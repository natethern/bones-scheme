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


void keypress(char c, KeySym sym, int state)
{
  fprintf(stderr, "Press: %d\n", c);
  //XXX
  ...
}


void keyrelease(char c, KeySym sym, int state)
{
  fprintf(stderr, "Release: %d\n", c);
  //XXX
  ...
}


void closewindow()
{
  exit(0);
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
  ...
}


void render_line(BONES_X line)
{
  ...
}


int init(int w, int h, char *window_name)
{
  char  *server;
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

  {
    XEvent ev;
    do {
      XNextEvent(display, &ev);
    } while (ev.type != Expose);
  }
}


void redraw()
{
  XSetBackground(display, gc, white.pixel);
  XSetForeground(display, gc, black.pixel);

  //XXX
  XDrawString(display, top, gc, 100, 50, "This is a test.", strlen("This is a test."));
  render_lines ...
}


int main()
{
  if(!init(500, 300, "oink")) return 1;

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
	closewindow();
      }
    }
    else if(xevent.type == ConfigureNotify) {
      int w = xevent.xconfigure.width;
      int h = xevent.xconfigure.height;

      if(w != size_x || h != size_y) {
	size_x = w;
	size_y = h;
	keypress(0, w, h);
	redraw();
      }
    }
  }
}
