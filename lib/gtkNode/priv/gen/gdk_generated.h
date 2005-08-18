/*******************************/
void Gdk_color_alloc(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* colormap;
  GdkColor* color;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  R = gdk_color_alloc(colormap, color);
  gn_put_longlong(XBUF,(long long)R);
}
/*******************************/
void Gdk_color_black(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* colormap;
  GdkColor* color;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  R = gdk_color_black(colormap, color);
  gn_put_longlong(XBUF,(long long)R);
}
/*******************************/
void Gdk_color_change(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* colormap;
  GdkColor* color;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  R = gdk_color_change(colormap, color);
  gn_put_longlong(XBUF,(long long)R);
}
/*******************************/
void Gdk_color_equal(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColor* object;
  GdkColor* colorb;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&colorb) ) return;
  R = gdk_color_equal(object, colorb);
  gn_put_boolean(XBUF,(int)R);
}
/*******************************/
void Gdk_color_free(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColor* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&object) ) return;
  gdk_color_free(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_color_hash(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColor* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&object) ) return;
  R = gdk_color_hash(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
}
/*******************************/
void Gdk_color_parse(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* spec;
  GdkColor* color;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &spec) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  R = gdk_color_parse(spec, color);
  free(spec);
  gn_put_longlong(XBUF,(long long)R);
}
/*******************************/
void Gdk_color_white(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* colormap;
  GdkColor* color;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  R = gdk_color_white(colormap, color);
  gn_put_longlong(XBUF,(long long)R);
}
/*******************************/
void Gdk_colormap_alloc_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* object;
  GdkColor* color;
  gboolean writeable;
  gboolean best_match;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &writeable) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &best_match) ) return;
  R = gdk_colormap_alloc_color(object, color, writeable, best_match);
  gn_put_boolean(XBUF,(int)R);
}
/*******************************/
void Gdk_colormap_change(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* object;
  gint ncolors;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ncolors) ) return;
  gdk_colormap_change(object, ncolors);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_colormap_free_colors(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* object;
  GdkColor* colors;
  gint ncolors;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&colors) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ncolors) ) return;
  gdk_colormap_free_colors(object, colors, ncolors);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_colormap_get_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&object) ) return;
  R = (GObject*)gdk_colormap_get_screen(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_colormap_get_system(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return;
  R = (GObject*)gdk_colormap_get_system();
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_colormap_get_system_size(int ARI, ei_x_buff *XBUF, char *B, int *I){


  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return;
  R = gdk_colormap_get_system_size();
  gn_put_longlong(XBUF,(long long)R);
}
/*******************************/
void Gdk_colormap_ref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&object) ) return;
  R = (GObject*)gdk_colormap_ref(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_colormap_unref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&object) ) return;
  gdk_colormap_unref(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_colors_store(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* colormap;
  GdkColor* colors;
  gint ncolors;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&colors) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ncolors) ) return;
  gdk_colors_store(colormap, colors, ncolors);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_arc(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  gboolean filled;
  gint x;
  gint y;
  gint width;
  gint height;
  gint angle1;
  gint angle2;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 9, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &filled) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &angle1) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &angle2) ) return;
  gdk_draw_arc(drawable, gc, filled, x, y, width, height, angle1, angle2);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_drawable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  GdkDrawable* src;
  gint xsrc;
  gint ysrc;
  gint xdest;
  gint ydest;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 9, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&src) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &xsrc) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ysrc) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &xdest) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ydest) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  gdk_draw_drawable(drawable, gc, src, xsrc, ysrc, xdest, ydest, width, height);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_image(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  GdkImage* image;
  gint xsrc;
  gint ysrc;
  gint xdest;
  gint ydest;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 9, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_IMAGE, (GObject**)&image) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &xsrc) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ysrc) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &xdest) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ydest) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  gdk_draw_image(drawable, gc, image, xsrc, ysrc, xdest, ydest, width, height);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  gint x1_;
  gint y1_;
  gint x2_;
  gint y2_;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x1_) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y1_) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x2_) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y2_) ) return;
  gdk_draw_line(drawable, gc, x1_, y1_, x2_, y2_);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_lines(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  GdkPoint* points;
  gint npoints;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkPoint", (void**)&points) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &npoints) ) return;
  gdk_draw_lines(drawable, gc, points, npoints);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_point(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  gdk_draw_point(drawable, gc, x, y);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_points(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  GdkPoint* points;
  gint npoints;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkPoint", (void**)&points) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &npoints) ) return;
  gdk_draw_points(drawable, gc, points, npoints);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_polygon(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  gboolean filled;
  GdkPoint* points;
  gint npoints;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &filled) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkPoint", (void**)&points) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &npoints) ) return;
  gdk_draw_polygon(drawable, gc, filled, points, npoints);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_draw_rectangle(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  GdkGC* gc;
  gboolean filled;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 7, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &filled) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  gdk_draw_rectangle(drawable, gc, filled, x, y, width, height);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_drawable_copy_to_image(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;
  GdkImage* image;
  gint src_x;
  gint src_y;
  gint dest_x;
  gint dest_y;
  gint width;
  gint height;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 8, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_IMAGE, (GObject**)&image) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &src_x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &src_y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &dest_x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &dest_y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  R = (GObject*)gdk_drawable_copy_to_image(object, image, src_x, src_y, dest_x, dest_y, width, height);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_drawable_get_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  R = (GObject*)gdk_drawable_get_colormap(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_drawable_get_depth(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  R = gdk_drawable_get_depth(object);
  gn_put_longlong(XBUF,(long long)R);
}
/*******************************/
void Gdk_drawable_get_display(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  R = (GObject*)gdk_drawable_get_display(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_drawable_get_image(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;
  gint x;
  gint y;
  gint width;
  gint height;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  R = (GObject*)gdk_drawable_get_image(object, x, y, width, height);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_drawable_get_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  R = (GObject*)gdk_drawable_get_screen(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_drawable_ref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  R = (GObject*)gdk_drawable_ref(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_drawable_set_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;
  GdkColormap* colormap;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return;
  gdk_drawable_set_colormap(object, colormap);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_drawable_unref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&object) ) return;
  gdk_drawable_unref(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_copy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkGC* src_gc;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&src_gc) ) return;
  gdk_gc_copy(object, src_gc);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_get_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  R = (GObject*)gdk_gc_get_colormap(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_gc_get_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  R = (GObject*)gdk_gc_get_screen(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_gc_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  R = (GObject*)gdk_gc_new(drawable);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_gc_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  gint x_offset;
  gint y_offset;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x_offset) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y_offset) ) return;
  gdk_gc_offset(object, x_offset, y_offset);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_ref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  R = (GObject*)gdk_gc_ref(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_gc_set_background(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  gdk_gc_set_background(object, color);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_clip_origin(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  gdk_gc_set_clip_origin(object, x, y);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_clip_rectangle(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkRectangle* rectangle;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&rectangle) ) return;
  gdk_gc_set_clip_rectangle(object, rectangle);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkColormap* colormap;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return;
  gdk_gc_set_colormap(object, colormap);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_exposures(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  gboolean exposures;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &exposures) ) return;
  gdk_gc_set_exposures(object, exposures);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_fill(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkFill fill;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkFill", (gint*)&fill) ) return;
  gdk_gc_set_fill(object, fill);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_foreground(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  gdk_gc_set_foreground(object, color);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_function(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkFunction function;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkFunction", (gint*)&function) ) return;
  gdk_gc_set_function(object, function);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_line_attributes(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  gint line_width;
  GdkLineStyle line_style;
  GdkCapStyle cap_style;
  GdkJoinStyle join_style;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &line_width) ) return;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkLineStyle", (gint*)&line_style) ) return;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkCapStyle", (gint*)&cap_style) ) return;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkJoinStyle", (gint*)&join_style) ) return;
  gdk_gc_set_line_attributes(object, line_width, line_style, cap_style, join_style);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_rgb_bg_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  gdk_gc_set_rgb_bg_color(object, color);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_rgb_fg_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  gdk_gc_set_rgb_fg_color(object, color);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_stipple(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkPixmap* stipple;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_PIXMAP, (GObject**)&stipple) ) return;
  gdk_gc_set_stipple(object, stipple);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_subwindow(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkSubwindowMode mode;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkSubwindowMode", (gint*)&mode) ) return;
  gdk_gc_set_subwindow(object, mode);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_tile(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  GdkPixmap* tile;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_PIXMAP, (GObject**)&tile) ) return;
  gdk_gc_set_tile(object, tile);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_set_ts_origin(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  gdk_gc_set_ts_origin(object, x, y);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_gc_unref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&object) ) return;
  gdk_gc_unref(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_get_default_root_window(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return;
  R = (GObject*)gdk_get_default_root_window();
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_pixmap_create_from_data(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  gchar* data;
  gint width;
  gint height;
  gint depth;
  GdkColor* fg;
  GdkColor* bg;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 7, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &data) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &depth) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&fg) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&bg) ) return;
  R = (GObject*)gdk_pixmap_create_from_data(drawable, data, width, height, depth, fg, bg);
  free(data);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_pixmap_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDrawable* drawable;
  gint width;
  gint height;
  gint depth;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &depth) ) return;
  R = (GObject*)gdk_pixmap_new(drawable, width, height, depth);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_set_sm_client_id(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* sm_client_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &sm_client_id) ) return;
  gdk_set_sm_client_id(sm_client_id);
  free(sm_client_id);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_begin_move_drag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gint button;
  gint root_x;
  gint root_y;
  guint32 timestamp;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &button) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &root_x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &root_y) ) return;
  if ( ! gn_get_arg_guint32(XBUF, B, I, &timestamp) ) return;
  gdk_window_begin_move_drag(object, button, root_x, root_y, timestamp);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_begin_paint_rect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkRectangle* rectangle;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&rectangle) ) return;
  gdk_window_begin_paint_rect(object, rectangle);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_begin_resize_drag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkWindowEdge edge;
  gint button;
  gint root_x;
  gint root_y;
  guint32 timestamp;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkWindowEdge", (gint*)&edge) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &button) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &root_x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &root_y) ) return;
  if ( ! gn_get_arg_guint32(XBUF, B, I, &timestamp) ) return;
  gdk_window_begin_resize_drag(object, edge, button, root_x, root_y, timestamp);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_clear(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_clear(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_clear_area(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  gdk_window_clear_area(object, x, y, width, height);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_clear_area_e(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  gdk_window_clear_area_e(object, x, y, width, height);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_deiconify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_deiconify(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_destroy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_destroy(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_end_paint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_end_paint(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  guint32 timestamp;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_guint32(XBUF, B, I, &timestamp) ) return;
  gdk_window_focus(object, timestamp);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_freeze_updates(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_freeze_updates(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_fullscreen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_fullscreen(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_get_events(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  GdkEventMask R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  R = gdk_window_get_events(object);
  gn_put_flags(XBUF,"GdkEventMask",R);
}
/*******************************/
void Gdk_window_get_frame_extents(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkRectangle* rect;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&rect) ) return;
  gdk_window_get_frame_extents(object, rect);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_get_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  R = (GObject*)gdk_window_get_group(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_window_get_parent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  R = (GObject*)gdk_window_get_parent(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_window_get_state(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  GdkWindowState R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  R = gdk_window_get_state(object);
  gn_put_flags(XBUF,"GdkWindowState",R);
}
/*******************************/
void Gdk_window_get_toplevel(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  R = (GObject*)gdk_window_get_toplevel(object);
  gn_put_object(XBUF,R);
}
/*******************************/
void Gdk_window_get_window_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  GdkWindowType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  R = gdk_window_get_window_type(object);
  gn_put_enum(XBUF,"GdkWindowType",R);
}
/*******************************/
void Gdk_window_hide(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_hide(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_iconify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_iconify(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_invalidate_rect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkRectangle* rect;
  gboolean invalidate_children;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&rect) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &invalidate_children) ) return;
  gdk_window_invalidate_rect(object, rect, invalidate_children);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_is_viewable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  R = gdk_window_is_viewable(object);
  gn_put_boolean(XBUF,(int)R);
}
/*******************************/
void Gdk_window_is_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  R = gdk_window_is_visible(object);
  gn_put_boolean(XBUF,(int)R);
}
/*******************************/
void Gdk_window_lower(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_lower(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_maximize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_maximize(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_merge_child_shapes(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_merge_child_shapes(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_move(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  gdk_window_move(object, x, y);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_move_resize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  gdk_window_move_resize(object, x, y, width, height);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_process_all_updates(int ARI, ei_x_buff *XBUF, char *B, int *I){


  /* no return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return;
  gdk_window_process_all_updates();
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_process_updates(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean update_children;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &update_children) ) return;
  gdk_window_process_updates(object, update_children);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_raise(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_raise(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_register_dnd(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_register_dnd(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_reparent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkWindow* new_parent;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&new_parent) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  gdk_window_reparent(object, new_parent, x, y);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_resize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return;
  gdk_window_resize(object, width, height);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_scroll(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gint dx;
  gint dy;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &dx) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &dy) ) return;
  gdk_window_scroll(object, dx, dy);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_accept_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean accept_focus;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &accept_focus) ) return;
  gdk_window_set_accept_focus(object, accept_focus);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_back_pixmap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkPixmap* pixmap;
  gboolean parent_relative;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_PIXMAP, (GObject**)&pixmap) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &parent_relative) ) return;
  gdk_window_set_back_pixmap(object, pixmap, parent_relative);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_background(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return;
  gdk_window_set_background(object, color);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_child_shapes(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_set_child_shapes(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_debug_updates(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return;
  gdk_window_set_debug_updates(setting);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_decorations(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkWMDecoration decorations;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkWMDecoration", (gint*)&decorations) ) return;
  gdk_window_set_decorations(object, decorations);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_events(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkEventMask event_mask;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkEventMask", (gint*)&event_mask) ) return;
  gdk_window_set_events(object, event_mask);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_functions(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkWMFunction functions;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkWMFunction", (gint*)&functions) ) return;
  gdk_window_set_functions(object, functions);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkWindow* leader;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&leader) ) return;
  gdk_window_set_group(object, leader);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_hints(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gint x;
  gint y;
  gint min_width;
  gint min_height;
  gint max_width;
  gint max_height;
  gint flags;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 8, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &min_width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &min_height) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &max_width) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &max_height) ) return;
  if ( ! gn_get_arg_gint(XBUF, B, I, &flags) ) return;
  gdk_window_set_hints(object, x, y, min_width, min_height, max_width, max_height, flags);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_icon_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gchar* name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return;
  gdk_window_set_icon_name(object, name);
  free(name);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_keep_above(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return;
  gdk_window_set_keep_above(object, setting);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_keep_below(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return;
  gdk_window_set_keep_below(object, setting);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_modal_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean modal;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &modal) ) return;
  gdk_window_set_modal_hint(object, modal);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_override_redirect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean override_redirect;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &override_redirect) ) return;
  gdk_window_set_override_redirect(object, override_redirect);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_role(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gchar* role;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &role) ) return;
  gdk_window_set_role(object, role);
  free(role);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_skip_pager_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean skips_pager;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &skips_pager) ) return;
  gdk_window_set_skip_pager_hint(object, skips_pager);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_skip_taskbar_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean skips_taskbar;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &skips_taskbar) ) return;
  gdk_window_set_skip_taskbar_hint(object, skips_taskbar);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_static_gravities(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gboolean use_static;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_static) ) return;
  R = gdk_window_set_static_gravities(object, use_static);
  gn_put_boolean(XBUF,(int)R);
}
/*******************************/
void Gdk_window_set_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  gchar* title;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return;
  gdk_window_set_title(object, title);
  free(title);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_transient_for(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkWindow* parent;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&parent) ) return;
  gdk_window_set_transient_for(object, parent);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_set_type_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;
  GdkWindowTypeHint hint;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkWindowTypeHint", (gint*)&hint) ) return;
  gdk_window_set_type_hint(object, hint);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_show(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_show(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_show_unraised(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_show_unraised(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_stick(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_stick(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_thaw_updates(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_thaw_updates(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_unfullscreen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_unfullscreen(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_unmaximize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_unmaximize(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_unstick(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_unstick(object);
  gn_put_void(XBUF);
}
/*******************************/
void Gdk_window_withdraw(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&object) ) return;
  gdk_window_withdraw(object);
  gn_put_void(XBUF);
}
