#include "gtkNode.h"
#include <string.h>

void GN_glade_init(int arity, ei_x_buff *xbuf, char *buff, int *index){
  char *xml_filename;
  
  if ( ! gn_check_arity(xbuf, 1, arity) ) return;
  if ( ! gn_get_arg_gchar(xbuf, buff, index, &xml_filename) ) return; /* free */
  
  if ( gn_glade_init(xml_filename) ) {
    gn_put_void(xbuf);
  }else{
    gn_enc_1_error(xbuf, "glade_init_failed");
  }
  free(xml_filename);
}

void GN_fname(int arity, ei_x_buff *xbuf, char *buff, int *index){
  extern GModule *gmod;
  gchar fname[MAXATOMLEN+1];
  gpointer fp;
  GType gtp;

  if ( ! gn_check_arity(xbuf, 1, arity) ) return;
  if ( ! gn_get_arg_gchar_fix(xbuf, buff, index, fname) ) return;
  
  if ( g_module_symbol(gmod, fname, &fp)  ) {
    gn_put_longlong(xbuf, (long long)(ulong)fp);
  }else if ( gtp = gn_GType_from_name(fname) ) {
    gn_put_longlong(xbuf, (long long)gtp);
  }else{
    gn_enc_2_error(xbuf, "bad_fname");
    ei_x_encode_atom(xbuf, fname);
  }
}


void GN_enum(int arity, ei_x_buff *xbuf, char *buff, int *index){
  gchar etype[MAXATOMLEN+1];
  gchar ename[MAXATOMLEN+1];
  gint eval;
  
  if ( ! gn_check_arity(xbuf, 2, arity) ) return;
  if ( ! gn_get_arg_gchar_fix(xbuf, buff, index, ename) ) return;
  if ( ! gn_get_arg_gchar_fix(xbuf, buff, index, etype) ) return;
  
  if ( gn_get_enum_val(xbuf, etype, ename, &eval) ) 
    gn_put_longlong(xbuf, (long long)eval);
}

void GN_new_gvalue(int arity, ei_x_buff *xbuf, char *buff, int *index){
  gchar name[MAXATOMLEN+1];
  gchar gtype[MAXATOMLEN+1];
  char key[200] = "GValue-";
  GType gtyp;
  GValue* gval;
  
  if ( ! gn_check_arity(xbuf, 2, arity) ) return;
  if ( ! gn_get_arg_gchar_fix(xbuf, buff, index, name) ) return;
  if ( ! gn_get_arg_gchar_fix(xbuf, buff, index, gtype) ) return;
  
  strcat(key, name);
  if ( hash_lookup(key) ) {
    gn_enc_2_error(xbuf, "already_exists");
    ei_x_encode_atom(xbuf, key);
    return;
  }
  
  if( ! (gtyp = gn_GType_from_name(gtype)) || ! G_TYPE_IS_VALUE_TYPE(gtyp) ) {
    gn_enc_2_error(xbuf, "not_value_type");
    ei_x_encode_atom(xbuf, gtype);
    return;
  }
  
  gval = g_new0(GValue,1);
  hash_insert(key, (void*)gval);
  g_value_init(gval, gtyp);
  gn_put_void(xbuf);
}

void GN_draw_arc(int ARI, ei_x_buff *XBUF, char *B, int *I) {

  GtkDrawingArea* object;
  GtkWidget* widget;
  GdkGC* gc;
  gchar colorspec[MAXATOMLEN];

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DRAWING_AREA, (GObject**)&object) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return;
  if ( ! gn_get_arg_gchar_fix(XBUF, B, I, colorspec) ) return;
  
  widget = GTK_WIDGET(object);

/*   gc = widget->style->fg_gc[GTK_WIDGET_STATE (widget)]; */
  GdkColor color; 
  GdkColormap* colormap = gdk_gc_get_colormap(gc); 
  g_assert(gdk_color_parse(colorspec,&color)); 
  g_assert(gdk_colormap_alloc_color(colormap, &color, FALSE, TRUE)); 
  gdk_gc_set_foreground(gc, &color); 
  
  gdk_draw_arc (widget->window,
		gc,
		TRUE,
		0, 0, widget->allocation.width, widget->allocation.height,
		0, 64 * 360);
  gn_put_ulonglong(XBUF,(unsigned long long) widget->allocation.width);
}

void GN_drawingarea_get_window(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  GtkDrawingArea* object;  
  GtkWidget* widget;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DRAWING_AREA, (GObject**)&object) ) return;

  widget = GTK_WIDGET(object);
  gn_put_object(XBUF,(GObject*) widget->window);
}

void GN_widget_get_attr(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  GtkWidget* widget;
  
  if ( ! gn_check_arity(XBUF, 1, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return;
  gn_put_tuple(XBUF, 5);
  gn_enc_object(XBUF,(GObject*) widget->window);
  gn_enc_longlong(XBUF,(long long)widget->allocation.x);
  gn_enc_longlong(XBUF,(long long)widget->allocation.y);
  gn_enc_longlong(XBUF,(long long)widget->allocation.width);
  gn_enc_longlong(XBUF,(long long)widget->allocation.height);
}
