/* GTK version: 2.4.9 */
/*******************************/
gboolean G_strdup_value_contents(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* value;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&value) ) return FALSE;
  R = g_strdup_value_contents(value);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean G_value_copy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  GValue* dest_value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&dest_value) ) return FALSE;
  g_value_copy(object, dest_value);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_dup_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_dup_string(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean G_value_fits_pointer(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_fits_pointer(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean G_value_get_boolean(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_get_boolean(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean G_value_get_double(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  gdouble R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_get_double(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean G_value_get_float(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  gfloat R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_get_float(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean G_value_get_int(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_get_int(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean G_value_get_int64(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  gint64 R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_get_int64(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean G_value_get_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_get_string(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean G_value_get_uint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_get_uint(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean G_value_reset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  GValue* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  R = g_value_reset(object);
  gn_put_struct(XBUF,"GValue",(void*)R);
  return TRUE;
}
/*******************************/
gboolean G_value_set_boolean(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gboolean v_boolean;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &v_boolean) ) return FALSE;
  g_value_set_boolean(object, v_boolean);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_set_double(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gdouble v_double;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &v_double) ) return FALSE;
  g_value_set_double(object, v_double);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_set_float(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gfloat v_float;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &v_float) ) return FALSE;
  g_value_set_float(object, v_float);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_set_int(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gint v_int;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &v_int) ) return FALSE;
  g_value_set_int(object, v_int);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_set_int64(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gint64 v_int64;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint64(XBUF, B, I, &v_int64) ) return FALSE;
  g_value_set_int64(object, v_int64);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_set_static_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gchar* v_string;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &v_string) ) return FALSE;
  g_value_set_static_string(object, v_string);
  free(v_string);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_set_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gchar* v_string;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &v_string) ) return FALSE;
  g_value_set_string(object, v_string);
  free(v_string);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_set_string_take_ownership(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gchar* v_string;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &v_string) ) return FALSE;
  g_value_set_string_take_ownership(object, v_string);
  free(v_string);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_set_uint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  guint v_uint;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &v_uint) ) return FALSE;
  g_value_set_uint(object, v_uint);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_take_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  gchar* v_string;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &v_string) ) return FALSE;
  g_value_take_string(object, v_string);
  free(v_string);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean G_value_transform(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  GValue* dest_value;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&dest_value) ) return FALSE;
  R = g_value_transform(object, dest_value);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean G_value_unset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  g_value_unset(object);
  gn_put_void(XBUF);
  return TRUE;
}
