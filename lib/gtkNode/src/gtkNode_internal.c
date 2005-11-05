#include "gtkNode.h"
#include <string.h>

gboolean  GN_glade_init(int arity, ei_x_buff *XBUF, char *B, int *I){
  char *xml_filename;
  
  if ( ! gn_check_arity(XBUF, 1, arity) ) 
    return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &xml_filename) ) 
    return FALSE; /* free */
  
  if ( gn_glade_init(xml_filename) ) {
    gn_put_void(XBUF);
    free(xml_filename);
    return TRUE;
  }else{
    gn_enc_1_error(XBUF, "glade_init_failed");
    free(xml_filename);
    return FALSE;
  }
}

gboolean  GN_new_gvalue(int arity, ei_x_buff *XBUF, char *B, int *I){
  gchar name[MAXATOMLEN+1];
  gchar gtype[MAXATOMLEN+1];
  char key[200] = "GValue-";
  GType gtyp;
  GValue* gval;
  
  if ( ! gn_check_arity(XBUF, 2, arity) ) return FALSE;
  if ( ! gn_get_arg_gchar_fix(XBUF, B, I, name) ) return FALSE;
  if ( ! gn_get_arg_gchar_fix(XBUF, B, I, gtype) ) return FALSE;
  
  strcat(key, name);
  if ( hash_lookup(key) ) {
    gn_enc_2_error(XBUF, "already_exists");
    ei_x_encode_atom(XBUF, key);
    return FALSE;
  }
  
  if( ! (gtyp = gn_GType_from_name(gtype)) || ! G_TYPE_IS_VALUE_TYPE(gtyp) ) {
    gn_enc_2_error(XBUF, "not_value_type");
    ei_x_encode_atom(XBUF, gtype);
    return FALSE;
  }
  
  gval = g_new0(GValue,1);
  hash_insert(key, (void*)gval);
  g_value_init(gval, gtyp);
  gn_put_void(XBUF);
  return TRUE;
}

gboolean GN_pango_layout_set_text(int ARI, ei_x_buff *XBUF, char *B, int *I){
  char* text;
  char* descr_str;
  PangoLayout* layout;
  PangoFontDescription* descr;

  /* no return value */
  
  if ( ! gn_check_arity(XBUF, 3, ARI) ) 
    return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "PangoLayout", (void**)&layout) ) 
    return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) )  /* free */
    return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &descr_str) ) { /* free */
    free(text);
    return FALSE;
  }
  
  descr = pango_font_description_from_string(descr_str);
  pango_layout_set_font_description(layout, descr);
  pango_layout_set_text(layout,text,(int)strlen(text));
  pango_font_description_free(descr);
  
  gn_put_void(XBUF);
  free(text);
  free(descr_str);
  return TRUE;
}

gboolean GN_widget_get_attr(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  gchar attr[MAXATOMLEN+1];
  GtkWidget* widget;
  
  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) 
    return FALSE;
  if ( ! gn_get_arg_gchar_fix(XBUF, B, I, attr) ) return FALSE;
  
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
    return FALSE;
  }
  return TRUE;
}


