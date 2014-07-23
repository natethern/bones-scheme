#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include <assert.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "bones.h"


Display     *display;
Window       top;
XFontStruct *fontst;
GC           gc;
Colormap     cmap;
XColor       black, white, rgb;
Atom         wm_protocols, wm_delete_window;
int size_x, size_y;
char *fontname = "-misc-fixed-bold-r-normal--18-120-100-100-c-90-iso8859-1";
int screen;
XColor colortable[ 256 ];
BONES_X key_event_vector[ 4 ];
int line_height = 10;		/* XXX */
char *window_name = "tw";
BONES_X topline = NULL;
int init_width = 500, init_height = 300;


void keypress(char c, KeySym sym, int state)
{
  BONES_X vec = (BONES_X)key_event_vector;
  vec->header = (BONES_VECTOR << 56) | 4;
  vec->slots[ 0 ] = BONES_int2fix(c);
  vec->slots[ 1 ] = BONES_int2fix(sym);
  vec->slots[ 2 ] = BONES_int2fix(state);
  vec->slots[ 3 ] = BONES_int2fix(1);
}


void keyrelease(char c, KeySym sym, int state)
{
  BONES_X vec = (BONES_X)key_event_vector;
  vec->header = (BONES_VECTOR << 56) | 4;
  vec->slots[ 0 ] = BONES_int2fix(c);
  vec->slots[ 1 ] = BONES_int2fix(sym);
  vec->slots[ 2 ] = BONES_int2fix(state);
  vec->slots[ 3 ] = BONES_int2fix(1);
}


void palette(BONES_X vec)
{
  int i, len = BONES_size_of(vec);
  XColor rgb;

  for(i = 0; i < len; ++i) {
    BONES_X str = BONES_slot_ref(vec, i);
    char *name = strndup(BONES_string(str), BONES_size_of(str));
    XAllocNamedColor(display, cmap, name, &(colortable[ i ]), &rgb);
  }
}


void render_lines(BONES_X topline)
{
  int y = 0;

  if(topline == NULL) return;

  while(y < size_y) {
    render_line(topline);
    y += line_height;
    topline = BONES_slot_ref(topline, 1); /* slot #1: next */
  }
}


void render_text(BONES_X str, int y)
{
  int len = BONES_size_of(str);
  XDrawString(display, top, gc, 0, y, BONES_string(str), len);
}


void render_line(BONES_X line, int y)
{
  BONES_X content = BONES_slot_ref(line, 2); /* slot #2: content */

  if(BONES_type_of(content) == BONES_STRING) 
    render_text(content, y);
  else {
    while(BONES_type_of(content) != BONES_NULL) {
      BONES_X cell = BONES_slot_ref(content, 0); /* car */
      
      if(BONES_type_of(cell) == BONES_STRING)
	render_text(cell, y);
      else  {
	/* otherwise it's #(FG BG STRING) */
	int col = BONES_fix2int(BONES_slot_ref(cell, 0)); /* FG */

	XSetForeground(display, gc, colortable[ col ].pixel);
	col = BONES_fix2int(BONES_slot_ref(cell, 1)); /* FG */
	XSetBackground(display, gc, colortable[ col ].pixel);
	render_text(BONES_slot_ref(cell, 2), y);
      }
    }
  }
}


int init(int w, int h, char *window_name)
{
  char  *server;
  XEvent ev;

  XSetWindowAttributes atr;
  server = (char *)getenv("DISPLAY");

  if (server == NULL) server = "localhost:0.0";

  display = XOpenDisplay(server);

  if (display == NULL) return 0;

  size_x = w;
  size_y = h;
  screen = DefaultScreen(display);
  cmap = DefaultColormap(display, screen);
  XAllocNamedColor(display, cmap, "white", &black, &rgb);
  XAllocNamedColor(display, cmap, "#102e4e", &white, &rgb);
  top = XCreateSimpleWindow(display, DefaultRootWindow(display), 0,
			    0, size_x, size_y, 2,
			    BlackPixel(display, screen),
			    white.pixel);

  if (window_name != NULL)
    XStoreName(display, top, window_name);

  gc = XCreateGC(display, top, 0, 0);
  fontst = XLoadQueryFont(display, fontname);

  if (fontst == NULL) return 0;

  XSetFont(display, gc, fontst->fid);
  wm_protocols = XInternAtom(display, "WM_PROTOCOLS", True);
  wm_delete_window = XInternAtom(display, "WM_DELETE_WINDOW", True);
  XSetWMProtocols(display, top, &wm_delete_window, 1);
  atr.backing_store = WhenMapped;
  XChangeWindowAttributes(display, top, CWBackingStore, &atr);
  XSelectInput(display, top,
	       ExposureMask | KeyPressMask | KeyReleaseMask | ButtonMotionMask |
	       OwnerGrabButtonMask | ButtonPressMask | ButtonReleaseMask |
	       StructureNotifyMask);
  XResizeWindow(display, top, size_x, size_y);
  XMapWindow(display, top);
  XFlush(display);

  do {
    XNextEvent(display, &ev);
  } while (ev.type != Expose);
}


void redraw()
{
  XSetBackground(display, gc, white.pixel);
  XSetForeground(display, gc, black.pixel);
  render_lines(topline);
}


int main()
{
  if(!init(init_width, init_height, window_name)) return 1;

  palette(bones(NULL));

  for (;;) {
    XEvent xevent;

    XNextEvent(display, &xevent);
    
    if (xevent.type == Expose) {
      redraw();
    } 
    else if (xevent.type == KeyPress || xevent.type == KeyRelease) {
      int ret;
      char c;
      KeySym keysym;
      if ((ret=XLookupString((XKeyEvent *)&xevent, &c, 1, &keysym, NULL)) == 1 ||
	  (XK_Home <= keysym && keysym <= XK_Down)) {
	if (xevent.type == KeyPress) 
	  keypress(c, keysym, xevent.xkey.state);
	else
	  keyrelease(c, keysym, xevent.xkey.state);
      }
    } 
    else if (xevent.type == ClientMessage) {
      if (xevent.xclient.message_type == wm_protocols &&
	  xevent.xclient.data.l[0] == wm_delete_window) {
	keypress(0, 0, 0);	/* c == 0; close */
      }
    }
    else if(xevent.type == ConfigureNotify) {
      int w = xevent.xconfigure.width;
      int h = xevent.xconfigure.height;

      if(w != size_x || h != size_y) {
	size_x = w;
	size_y = h;
	keypress(1, w, h);	/* c == 1: resize */
	redraw();
      }
    }
  }
}
