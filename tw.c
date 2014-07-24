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
BONES_X key_event_vector[ 5 ];
int line_height = 20;		/* XXX */
char *window_name = "tw";
BONES_X topline = NULL;
int init_width = 500, init_height = 300;


static void call_scheme(BONES_X arg)
{
  BONES_X result = scheme(arg);

  if(BONES_is_error_object(result)) {
    BONES_X msg = BONES_error_object_message(result);
    fprintf(stderr, "Scheme error: %.*s\n", (int)BONES_size_of(msg), BONES_string(msg));
    exit(1);
  }

  topline = result;
}


static void keypress(char c, KeySym sym, int state)
{
  BONES_X vec = (BONES_X)key_event_vector;
  BONES_header_set(vec, (BONES_VECTOR << 56) | 4);
  BONES_slot_set(vec, 0, BONES_int2fix(c));
  BONES_slot_set(vec, 1, BONES_int2fix(sym));
  BONES_slot_set(vec, 2, BONES_int2fix(state));
  BONES_slot_set(vec, 3, BONES_int2fix(1));
  call_scheme(vec);
}


static void keyrelease(char c, KeySym sym, int state)
{
  BONES_X vec = (BONES_X)key_event_vector;
  BONES_header_set(vec, (BONES_VECTOR << 56) | 4);
  BONES_slot_set(vec, 0, BONES_int2fix(c));
  BONES_slot_set(vec, 1, BONES_int2fix(sym));
  BONES_slot_set(vec, 2, BONES_int2fix(state));
  BONES_slot_set(vec, 3, BONES_int2fix(0));
  call_scheme(vec);
}


static void palette(BONES_X vec)
{
  int i, len = BONES_size_of(vec);
  XColor rgb;

  for(i = 0; i < len; ++i) {
    BONES_X str = BONES_slot_ref(vec, i);
    char *name = strndup(BONES_string(str), BONES_size_of(str));
    XAllocNamedColor(display, cmap, name, &(colortable[ i ]), &rgb);
  }
}


static void render_text(BONES_X str, int y)
{
  int len = BONES_size_of(str);

  XDrawString(display, top, gc, 0, y, BONES_string(str), len);
  //printf(">>%.*s<<\n", len, BONES_string(str));
}


static void render_line(BONES_X line, int y)
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

      content = BONES_slot_ref(content, 1); /* cdr */
    }
  }
}


static void render_lines(BONES_X topline)
{
  int y = 0;
  BONES_X ln = topline; 

  if(ln == NULL) return;

  while(BONES_type_of(ln) != BONES_BOOLEAN && y < size_y) {
    render_line(ln, y);
    y += line_height;
    ln = BONES_slot_ref(ln, 1); /* slot #1: next */
  }
}


static int init(int w, int h, char *window_name)
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


static void redraw()
{
  XSetBackground(display, gc, white.pixel);
  XSetForeground(display, gc, black.pixel);
  render_lines(topline);
}


int main()
{
  if(!init(init_width, init_height, window_name)) return 1;

  palette(scheme(NULL));

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

	redraw();
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
