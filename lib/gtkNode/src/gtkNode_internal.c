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

void GN_widget_get_attr(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  gchar attr[MAXATOMLEN+1];
  GtkWidget* widget;
  
  if ( ! gn_check_arity(XBUF, 2, ARI) ) return;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return;
  if ( ! gn_get_arg_gchar_fix(XBUF, B, I, attr) ) return;
  
  if ( strcmp("window",attr) == 0 ){
    gn_put_object(XBUF,(GObject*) widget->window);
  }else if ( strcmp("x",attr) == 0 ) {
    gn_put_longlong(XBUF,(long long)widget->allocation.x);
  }else if ( strcmp("y",attr) == 0 ) {
    gn_put_longlong(XBUF,(long long)widget->allocation.y);
  }else if ( strcmp("width",attr) == 0 ) {
    gn_put_longlong(XBUF,(long long)widget->allocation.width);
  }else if ( strcmp("height",attr) == 0 ) {
    gn_put_longlong(XBUF,(long long)widget->allocation.height);
  }else{
    gn_enc_2_error(XBUF, "no_such_attr");
    ei_x_encode_atom(XBUF, attr);
  }
}

