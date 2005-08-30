/* GTK version: 2.6.8 */
/*******************************/
gboolean Gtk_accel_group_disconnect_key(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAccelGroup* object;
  guint accel_key;
  GdkModifierType accel_mods;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &accel_key) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&accel_mods) ) return FALSE;
  R = gtk_accel_group_disconnect_key(object, accel_key, accel_mods);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_group_lock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAccelGroup* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&object) ) return FALSE;
  gtk_accel_group_lock(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_group_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_accel_group_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_group_unlock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAccelGroup* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&object) ) return FALSE;
  gtk_accel_group_unlock(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_label_get_accel_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAccelLabel* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_LABEL, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_accel_label_get_accel_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_label_get_accel_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAccelLabel* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_accel_label_get_accel_width(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_label_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* string;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &string) ) return FALSE;
  R = (GObject*)gtk_accel_label_new(string);
  free(string);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_label_refetch(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAccelLabel* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_accel_label_refetch(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_label_set_accel_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAccelLabel* object;
  GtkWidget* accel_widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&accel_widget) ) return FALSE;
  gtk_accel_label_set_accel_widget(object, accel_widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_add_entry(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* accel_path;
  guint accel_key;
  GdkModifierType accel_mods;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_path) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &accel_key) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&accel_mods) ) return FALSE;
  gtk_accel_map_add_entry(accel_path, accel_key, accel_mods);
  free(accel_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_add_filter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* filter_pattern;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &filter_pattern) ) return FALSE;
  gtk_accel_map_add_filter(filter_pattern);
  free(filter_pattern);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_change_entry(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* accel_path;
  guint accel_key;
  GdkModifierType accel_mods;
  gboolean replace;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_path) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &accel_key) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&accel_mods) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &replace) ) return FALSE;
  R = gtk_accel_map_change_entry(accel_path, accel_key, accel_mods, replace);
  free(accel_path);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_load(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* file_name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &file_name) ) return FALSE;
  gtk_accel_map_load(file_name);
  free(file_name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_load_fd(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gint fd;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &fd) ) return FALSE;
  gtk_accel_map_load_fd(fd);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_lock_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* accel_path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_path) ) return FALSE;
  gtk_accel_map_lock_path(accel_path);
  free(accel_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_save(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* file_name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &file_name) ) return FALSE;
  gtk_accel_map_save(file_name);
  free(file_name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_save_fd(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gint fd;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &fd) ) return FALSE;
  gtk_accel_map_save_fd(fd);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accel_map_unlock_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* accel_path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_path) ) return FALSE;
  gtk_accel_map_unlock_path(accel_path);
  free(accel_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accelerator_get_default_mod_mask(int ARI, ei_x_buff *XBUF, char *B, int *I){


  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_accelerator_get_default_mod_mask();
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accelerator_get_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  guint accelerator_key;
  GdkModifierType accelerator_mods;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &accelerator_key) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&accelerator_mods) ) return FALSE;
  R = gtk_accelerator_get_label(accelerator_key, accelerator_mods);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accelerator_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  guint accelerator_key;
  GdkModifierType accelerator_mods;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &accelerator_key) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&accelerator_mods) ) return FALSE;
  R = gtk_accelerator_name(accelerator_key, accelerator_mods);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_accelerator_set_default_mod_mask(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkModifierType default_mod_mask;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&default_mod_mask) ) return FALSE;
  gtk_accelerator_set_default_mod_mask(default_mod_mask);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_accelerator_valid(int ARI, ei_x_buff *XBUF, char *B, int *I){

  guint keyval;
  GdkModifierType modifiers;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &keyval) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&modifiers) ) return FALSE;
  R = gtk_accelerator_valid(keyval, modifiers);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_activate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  gtk_action_activate(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_block_activate_from(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  GtkWidget* proxy;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&proxy) ) return FALSE;
  gtk_action_block_activate_from(object, proxy);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_connect_accelerator(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  gtk_action_connect_accelerator(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_connect_proxy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  GtkWidget* proxy;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&proxy) ) return FALSE;
  gtk_action_connect_proxy(object, proxy);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_create_icon(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  GtkIconSize icon_size;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkIconSize", (gint*)&icon_size) ) return FALSE;
  R = (GObject*)gtk_action_create_icon(object, icon_size);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_create_menu_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_action_create_menu_item(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_create_tool_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_action_create_tool_item(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_disconnect_accelerator(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  gtk_action_disconnect_accelerator(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_disconnect_proxy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  GtkWidget* proxy;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&proxy) ) return FALSE;
  gtk_action_disconnect_proxy(object, proxy);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_get_accel_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_action_get_accel_path(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_get_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_action_get_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_get_sensitive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_action_get_sensitive(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_get_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_action_get_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_add_action(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;
  GtkAction* action;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&action) ) return FALSE;
  gtk_action_group_add_action(object, action);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_add_action_with_accel(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;
  GtkAction* action;
  gchar* accelerator;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&action) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accelerator) ) return FALSE;
  gtk_action_group_add_action_with_accel(object, action, accelerator);
  free(accelerator);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_get_action(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;
  gchar* action_name;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &action_name) ) return FALSE;
  R = (GObject*)gtk_action_group_get_action(object, action_name);
  free(action_name);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_get_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  R = gtk_action_group_get_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_get_sensitive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  R = gtk_action_group_get_sensitive(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_get_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  R = gtk_action_group_get_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* name;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  R = (GObject*)gtk_action_group_new(name);
  free(name);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_remove_action(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;
  GtkAction* action;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&action) ) return FALSE;
  gtk_action_group_remove_action(object, action);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_set_sensitive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;
  gboolean sensitive;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &sensitive) ) return FALSE;
  gtk_action_group_set_sensitive(object, sensitive);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_set_translation_domain(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;
  gchar* domain;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &domain) ) return FALSE;
  gtk_action_group_set_translation_domain(object, domain);
  free(domain);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_set_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;
  gboolean visible;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &visible) ) return FALSE;
  gtk_action_group_set_visible(object, visible);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_group_translate_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkActionGroup* object;
  gchar* string;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &string) ) return FALSE;
  R = gtk_action_group_translate_string(object, string);
  free(string);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_is_sensitive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_action_is_sensitive(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_is_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_action_is_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* name;
  gchar* label;
  gchar* tooltip;
  gchar* stock_id;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &tooltip) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  R = (GObject*)gtk_action_new(name, label, tooltip, stock_id);
  free(name);
  free(label);
  free(tooltip);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_set_accel_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  GtkAccelGroup* accel_group;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&accel_group) ) return FALSE;
  gtk_action_set_accel_group(object, accel_group);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_set_accel_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  gchar* accel_path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_path) ) return FALSE;
  gtk_action_set_accel_path(object, accel_path);
  free(accel_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_set_sensitive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  gboolean sensitive;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &sensitive) ) return FALSE;
  gtk_action_set_sensitive(object, sensitive);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_set_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  gboolean visible;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &visible) ) return FALSE;
  gtk_action_set_visible(object, visible);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_action_unblock_activate_from(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAction* object;
  GtkWidget* proxy;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&proxy) ) return FALSE;
  gtk_action_unblock_activate_from(object, proxy);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_adjustment_changed(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&object) ) return FALSE;
  gtk_adjustment_changed(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_adjustment_clamp_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* object;
  gdouble lower;
  gdouble upper;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &lower) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &upper) ) return FALSE;
  gtk_adjustment_clamp_page(object, lower, upper);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_adjustment_get_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* object;

  gdouble R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&object) ) return FALSE;
  R = gtk_adjustment_get_value(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_adjustment_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gdouble value;
  gdouble lower;
  gdouble upper;
  gdouble step_increment;
  gdouble page_increment;
  gdouble page_size;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &value) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &lower) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &upper) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &step_increment) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &page_increment) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &page_size) ) return FALSE;
  R = (GObject*)gtk_adjustment_new(value, lower, upper, step_increment, page_increment, page_size);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_adjustment_set_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* object;
  gdouble value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &value) ) return FALSE;
  gtk_adjustment_set_value(object, value);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_adjustment_value_changed(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&object) ) return FALSE;
  gtk_adjustment_value_changed(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_alignment_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gfloat xalign;
  gfloat yalign;
  gfloat xscale;
  gfloat yscale;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xscale) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yscale) ) return FALSE;
  R = (GObject*)gtk_alignment_new(xalign, yalign, xscale, yscale);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_alignment_set(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAlignment* object;
  gfloat xalign;
  gfloat yalign;
  gfloat xscale;
  gfloat yscale;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ALIGNMENT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xscale) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yscale) ) return FALSE;
  gtk_alignment_set(object, xalign, yalign, xscale, yscale);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_alignment_set_padding(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAlignment* object;
  guint padding_top;
  guint padding_bottom;
  guint padding_left;
  guint padding_right;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ALIGNMENT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &padding_top) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &padding_bottom) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &padding_left) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &padding_right) ) return FALSE;
  gtk_alignment_set_padding(object, padding_top, padding_bottom, padding_left, padding_right);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_alternative_dialog_button_order(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkScreen* screen;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_SCREEN, (GObject**)&screen) ) return FALSE;
  R = gtk_alternative_dialog_button_order(screen);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_arrow_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkArrowType arrow_type;
  GtkShadowType shadow_type;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkArrowType", (gint*)&arrow_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  R = (GObject*)gtk_arrow_new(arrow_type, shadow_type);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_arrow_set(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkArrow* object;
  GtkArrowType arrow_type;
  GtkShadowType shadow_type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ARROW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkArrowType", (gint*)&arrow_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  gtk_arrow_set(object, arrow_type, shadow_type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_aspect_frame_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;
  gfloat xalign;
  gfloat yalign;
  gfloat ratio;
  gboolean obey_child;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &ratio) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &obey_child) ) return FALSE;
  R = (GObject*)gtk_aspect_frame_new(label, xalign, yalign, ratio, obey_child);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_aspect_frame_set(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAspectFrame* object;
  gfloat xalign;
  gfloat yalign;
  gfloat ratio;
  gboolean obey_child;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ASPECT_FRAME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &ratio) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &obey_child) ) return FALSE;
  gtk_aspect_frame_set(object, xalign, yalign, ratio, obey_child);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_bin_get_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBin* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BIN, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_bin_get_child(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_bindings_activate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkObject* object;
  guint keyval;
  GdkModifierType modifiers;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_OBJECT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &keyval) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&modifiers) ) return FALSE;
  R = gtk_bindings_activate(object, keyval, modifiers);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_get_homogeneous(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_box_get_homogeneous(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_get_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_box_get_spacing(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_pack_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;
  GtkWidget* child;
  gboolean expand;
  gboolean fill;
  guint padding;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &fill) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &padding) ) return FALSE;
  gtk_box_pack_end(object, child, expand, fill, padding);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_pack_end_defaults(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;
  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_box_pack_end_defaults(object, widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_pack_start(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;
  GtkWidget* child;
  gboolean expand;
  gboolean fill;
  guint padding;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &fill) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &padding) ) return FALSE;
  gtk_box_pack_start(object, child, expand, fill, padding);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_pack_start_defaults(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;
  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_box_pack_start_defaults(object, widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_reorder_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;
  GtkWidget* child;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_box_reorder_child(object, child, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_set_child_packing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;
  GtkWidget* child;
  gboolean expand;
  gboolean fill;
  guint padding;
  GtkPackType pack_type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &fill) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &padding) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPackType", (gint*)&pack_type) ) return FALSE;
  gtk_box_set_child_packing(object, child, expand, fill, padding, pack_type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_set_homogeneous(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;
  gboolean homogeneous;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &homogeneous) ) return FALSE;
  gtk_box_set_homogeneous(object, homogeneous);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_box_set_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkBox* object;
  gint spacing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &spacing) ) return FALSE;
  gtk_box_set_spacing(object, spacing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_box_get_child_secondary(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButtonBox* object;
  GtkWidget* child;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  R = gtk_button_box_get_child_secondary(object, child);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_box_get_layout(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButtonBox* object;

  GtkButtonBoxStyle R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_button_box_get_layout(object);
  gn_put_enum(XBUF,"GtkButtonBoxStyle",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_box_set_child_secondary(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButtonBox* object;
  GtkWidget* child;
  gboolean is_secondary;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_secondary) ) return FALSE;
  gtk_button_box_set_child_secondary(object, child, is_secondary);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_box_set_layout(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButtonBox* object;
  GtkButtonBoxStyle layout_style;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkButtonBoxStyle", (gint*)&layout_style) ) return FALSE;
  gtk_button_box_set_layout(object, layout_style);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_clicked(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  gtk_button_clicked(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_enter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  gtk_button_enter(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_get_focus_on_click(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_button_get_focus_on_click(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_get_image(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_button_get_image(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_get_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_button_get_label(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_get_relief(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  GtkReliefStyle R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_button_get_relief(object);
  gn_put_enum(XBUF,"GtkReliefStyle",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_get_use_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_button_get_use_stock(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_get_use_underline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_button_get_use_underline(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_leave(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  gtk_button_leave(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_button_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_new_from_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* stock_id;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  R = (GObject*)gtk_button_new_from_stock(stock_id);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_new_with_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_button_new_with_label(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_new_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_button_new_with_mnemonic(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_pressed(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  gtk_button_pressed(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_released(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  gtk_button_released(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_set_alignment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;
  gfloat xalign;
  gfloat yalign;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yalign) ) return FALSE;
  gtk_button_set_alignment(object, xalign, yalign);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_set_focus_on_click(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;
  gboolean focus_on_click;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &focus_on_click) ) return FALSE;
  gtk_button_set_focus_on_click(object, focus_on_click);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_set_image(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;
  GtkWidget* image;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&image) ) return FALSE;
  gtk_button_set_image(object, image);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_set_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;
  gchar* label;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  gtk_button_set_label(object, label);
  free(label);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_set_relief(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;
  GtkReliefStyle newstyle;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkReliefStyle", (gint*)&newstyle) ) return FALSE;
  gtk_button_set_relief(object, newstyle);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_set_use_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;
  gboolean use_stock;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_stock) ) return FALSE;
  gtk_button_set_use_stock(object, use_stock);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_button_set_use_underline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkButton* object;
  gboolean use_underline;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_underline) ) return FALSE;
  gtk_button_set_use_underline(object, use_underline);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_clear_marks(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  gtk_calendar_clear_marks(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_freeze(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  gtk_calendar_freeze(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_get_display_options(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;

  GtkCalendarDisplayOptions R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  R = gtk_calendar_get_display_options(object);
  gn_put_flags(XBUF,"GtkCalendarDisplayOptions",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_mark_day(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;
  guint day;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &day) ) return FALSE;
  R = gtk_calendar_mark_day(object, day);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_calendar_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_select_day(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;
  guint day;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &day) ) return FALSE;
  gtk_calendar_select_day(object, day);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_select_month(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;
  guint month;
  guint year;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &month) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &year) ) return FALSE;
  R = gtk_calendar_select_month(object, month, year);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_set_display_options(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;
  GtkCalendarDisplayOptions flags;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GtkCalendarDisplayOptions", (gint*)&flags) ) return FALSE;
  gtk_calendar_set_display_options(object, flags);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_thaw(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  gtk_calendar_thaw(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_calendar_unmark_day(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCalendar* object;
  guint day;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CALENDAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &day) ) return FALSE;
  R = gtk_calendar_unmark_day(object, day);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_editable_editing_done(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellEditable* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_EDITABLE, (GObject**)&object) ) return FALSE;
  gtk_cell_editable_editing_done(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_editable_remove_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellEditable* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_EDITABLE, (GObject**)&object) ) return FALSE;
  gtk_cell_editable_remove_widget(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_layout_add_attribute(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellLayout* object;
  GtkCellRenderer* cell;
  gchar* attribute;
  gint column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &attribute) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column) ) return FALSE;
  gtk_cell_layout_add_attribute(object, cell, attribute, column);
  free(attribute);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_layout_clear(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellLayout* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_LAYOUT, (GObject**)&object) ) return FALSE;
  gtk_cell_layout_clear(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_layout_clear_attributes(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellLayout* object;
  GtkCellRenderer* cell;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell) ) return FALSE;
  gtk_cell_layout_clear_attributes(object, cell);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_layout_pack_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellLayout* object;
  GtkCellRenderer* cell;
  gboolean expand;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  gtk_cell_layout_pack_end(object, cell, expand);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_layout_pack_start(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellLayout* object;
  GtkCellRenderer* cell;
  gboolean expand;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  gtk_cell_layout_pack_start(object, cell, expand);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_layout_reorder(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellLayout* object;
  GtkCellRenderer* cell;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_cell_layout_reorder(object, cell, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_editing_canceled(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRenderer* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&object) ) return FALSE;
  gtk_cell_renderer_editing_canceled(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_pixbuf_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_cell_renderer_pixbuf_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_render(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRenderer* object;
  GdkWindow* window;
  GtkWidget* widget;
  GdkRectangle* background_area;
  GdkRectangle* cell_area;
  GdkRectangle* expose_area;
  GtkCellRendererState flags;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 7, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&background_area) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&cell_area) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&expose_area) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GtkCellRendererState", (gint*)&flags) ) return FALSE;
  gtk_cell_renderer_render(object, window, widget, background_area, cell_area, expose_area, flags);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_set_fixed_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRenderer* object;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_cell_renderer_set_fixed_size(object, width, height);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_stop_editing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRenderer* object;
  gboolean canceled;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &canceled) ) return FALSE;
  gtk_cell_renderer_stop_editing(object, canceled);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_text_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_cell_renderer_text_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_text_set_fixed_height_from_font(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRendererText* object;
  gint number_of_rows;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER_TEXT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &number_of_rows) ) return FALSE;
  gtk_cell_renderer_text_set_fixed_height_from_font(object, number_of_rows);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_toggle_get_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRendererToggle* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER_TOGGLE, (GObject**)&object) ) return FALSE;
  R = gtk_cell_renderer_toggle_get_active(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_toggle_get_radio(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRendererToggle* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER_TOGGLE, (GObject**)&object) ) return FALSE;
  R = gtk_cell_renderer_toggle_get_radio(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_toggle_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_cell_renderer_toggle_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_toggle_set_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRendererToggle* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER_TOGGLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_cell_renderer_toggle_set_active(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_cell_renderer_toggle_set_radio(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCellRendererToggle* object;
  gboolean radio;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER_TOGGLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &radio) ) return FALSE;
  gtk_cell_renderer_toggle_set_radio(object, radio);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_button_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_check_button_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_button_new_with_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_check_button_new_with_label(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_button_new_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_check_button_new_with_mnemonic(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_get_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCheckMenuItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CHECK_MENU_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_check_menu_item_get_active(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_get_draw_as_radio(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCheckMenuItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CHECK_MENU_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_check_menu_item_get_draw_as_radio(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_get_inconsistent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCheckMenuItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CHECK_MENU_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_check_menu_item_get_inconsistent(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_check_menu_item_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_new_with_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_check_menu_item_new_with_label(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_new_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_check_menu_item_new_with_mnemonic(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_set_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCheckMenuItem* object;
  gboolean is_active;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CHECK_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_active) ) return FALSE;
  gtk_check_menu_item_set_active(object, is_active);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_set_draw_as_radio(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCheckMenuItem* object;
  gboolean draw_as_radio;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CHECK_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &draw_as_radio) ) return FALSE;
  gtk_check_menu_item_set_draw_as_radio(object, draw_as_radio);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_set_inconsistent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCheckMenuItem* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CHECK_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_check_menu_item_set_inconsistent(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_check_menu_item_toggled(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkCheckMenuItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CHECK_MENU_ITEM, (GObject**)&object) ) return FALSE;
  gtk_check_menu_item_toggled(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_get_alpha(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorButton* object;

  guint16 R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_color_button_get_alpha(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_get_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorButton* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_color_button_get_color(object, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_get_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorButton* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_color_button_get_title(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_get_use_alpha(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_color_button_get_use_alpha(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_color_button_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_new_with_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColor* color;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  R = (GObject*)gtk_color_button_new_with_color(color);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_set_alpha(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorButton* object;
  guint16 alpha;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint16(XBUF, B, I, &alpha) ) return FALSE;
  gtk_color_button_set_alpha(object, alpha);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_set_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorButton* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_color_button_set_color(object, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_set_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorButton* object;
  gchar* title;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return FALSE;
  gtk_color_button_set_title(object, title);
  free(title);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_button_set_use_alpha(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorButton* object;
  gboolean use_alpha;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_alpha) ) return FALSE;
  gtk_color_button_set_use_alpha(object, use_alpha);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_dialog_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* title;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return FALSE;
  R = (GObject*)gtk_color_selection_dialog_new(title);
  free(title);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_get_current_alpha(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;

  guint16 R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_color_selection_get_current_alpha(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_get_current_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_color_selection_get_current_color(object, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_get_has_opacity_control(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_color_selection_get_has_opacity_control(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_get_has_palette(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_color_selection_get_has_palette(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_get_previous_alpha(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;

  guint16 R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_color_selection_get_previous_alpha(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_get_previous_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_color_selection_get_previous_color(object, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_is_adjusting(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_color_selection_is_adjusting(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_color_selection_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_palette_to_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColor* colors;
  gint n_colors;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&colors) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n_colors) ) return FALSE;
  R = gtk_color_selection_palette_to_string(colors, n_colors);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_set_current_alpha(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;
  guint16 alpha;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint16(XBUF, B, I, &alpha) ) return FALSE;
  gtk_color_selection_set_current_alpha(object, alpha);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_set_current_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_color_selection_set_current_color(object, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_set_has_opacity_control(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;
  gboolean has_opacity;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &has_opacity) ) return FALSE;
  gtk_color_selection_set_has_opacity_control(object, has_opacity);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_set_has_palette(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;
  gboolean has_palette;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &has_palette) ) return FALSE;
  gtk_color_selection_set_has_palette(object, has_palette);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_set_previous_alpha(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;
  guint16 alpha;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint16(XBUF, B, I, &alpha) ) return FALSE;
  gtk_color_selection_set_previous_alpha(object, alpha);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_color_selection_set_previous_color(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkColorSelection* object;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COLOR_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_color_selection_set_previous_color(object, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_append_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gchar* text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  gtk_combo_box_append_text(object, text);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_entry_get_text_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBoxEntry* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_combo_box_entry_get_text_column(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_entry_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_combo_box_entry_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_entry_new_text(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_combo_box_entry_new_text();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_entry_new_with_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* model;
  gint text_column;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&model) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &text_column) ) return FALSE;
  R = (GObject*)gtk_combo_box_entry_new_with_model(model, text_column);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_entry_set_text_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBoxEntry* object;
  gint text_column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &text_column) ) return FALSE;
  gtk_combo_box_entry_set_text_column(object, text_column);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_combo_box_get_active(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_active_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_combo_box_get_active_iter(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_active_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_combo_box_get_active_text(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_add_tearoffs(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_combo_box_get_add_tearoffs(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_column_span_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_combo_box_get_column_span_column(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_focus_on_click(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_combo_box_get_focus_on_click(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_combo_box_get_model(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_row_span_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_combo_box_get_row_span_column(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_get_wrap_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_combo_box_get_wrap_width(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_insert_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gint position;
  gchar* text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  gtk_combo_box_insert_text(object, position, text);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_combo_box_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_new_text(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_combo_box_new_text();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_new_with_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* model;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&model) ) return FALSE;
  R = (GObject*)gtk_combo_box_new_with_model(model);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_popdown(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  gtk_combo_box_popdown(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_popup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  gtk_combo_box_popup(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_prepend_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gchar* text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  gtk_combo_box_prepend_text(object, text);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_remove_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_combo_box_remove_text(object, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_set_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gint index_;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &index_) ) return FALSE;
  gtk_combo_box_set_active(object, index_);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_set_active_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_combo_box_set_active_iter(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_set_add_tearoffs(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gboolean add_tearoffs;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &add_tearoffs) ) return FALSE;
  gtk_combo_box_set_add_tearoffs(object, add_tearoffs);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_set_column_span_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gint column_span;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column_span) ) return FALSE;
  gtk_combo_box_set_column_span_column(object, column_span);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_set_focus_on_click(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gboolean focus_on_click;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &focus_on_click) ) return FALSE;
  gtk_combo_box_set_focus_on_click(object, focus_on_click);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_set_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  GtkTreeModel* model;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&model) ) return FALSE;
  gtk_combo_box_set_model(object, model);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_set_row_span_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gint row_span;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &row_span) ) return FALSE;
  gtk_combo_box_set_row_span_column(object, row_span);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_combo_box_set_wrap_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkComboBox* object;
  gint width;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_COMBO_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  gtk_combo_box_set_wrap_width(object, width);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_add(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_container_add(object, widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_check_resize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  gtk_container_check_resize(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_child_get_property(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  GtkWidget* child;
  gchar* property_name;
  GValue* value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &property_name) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&value) ) return FALSE;
  gtk_container_child_get_property(object, child, property_name, value);
  free(property_name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_child_set_property(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  GtkWidget* child;
  gchar* property_name;
  GValue* value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &property_name) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&value) ) return FALSE;
  gtk_container_child_set_property(object, child, property_name, value);
  free(property_name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_get_border_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  R = gtk_container_get_border_width(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_get_focus_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_container_get_focus_hadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_get_focus_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_container_get_focus_vadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_get_resize_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;

  GtkResizeMode R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  R = gtk_container_get_resize_mode(object);
  gn_put_enum(XBUF,"GtkResizeMode",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_remove(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_container_remove(object, widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_resize_children(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  gtk_container_resize_children(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_set_border_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  guint border_width;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &border_width) ) return FALSE;
  gtk_container_set_border_width(object, border_width);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_set_focus_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  GtkWidget* child;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  gtk_container_set_focus_child(object, child);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_set_focus_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_container_set_focus_hadjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_set_focus_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_container_set_focus_vadjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_set_reallocate_redraws(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  gboolean needs_redraws;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &needs_redraws) ) return FALSE;
  gtk_container_set_reallocate_redraws(object, needs_redraws);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_set_resize_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;
  GtkResizeMode resize_mode;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkResizeMode", (gint*)&resize_mode) ) return FALSE;
  gtk_container_set_resize_mode(object, resize_mode);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_container_unset_focus_chain(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkContainer* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CONTAINER, (GObject**)&object) ) return FALSE;
  gtk_container_unset_focus_chain(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_add_action_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkDialog* object;
  GtkWidget* child;
  gint response_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &response_id) ) return FALSE;
  gtk_dialog_add_action_widget(object, child, response_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_add_button(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkDialog* object;
  gchar* button_text;
  gint response_id;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &button_text) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &response_id) ) return FALSE;
  R = (GObject*)gtk_dialog_add_button(object, button_text, response_id);
  free(button_text);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_get_has_separator(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkDialog* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DIALOG, (GObject**)&object) ) return FALSE;
  R = gtk_dialog_get_has_separator(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_dialog_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_response(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkDialog* object;
  gint response_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &response_id) ) return FALSE;
  gtk_dialog_response(object, response_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_run(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkDialog* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DIALOG, (GObject**)&object) ) return FALSE;
  R = gtk_dialog_run(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_set_default_response(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkDialog* object;
  gint response_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &response_id) ) return FALSE;
  gtk_dialog_set_default_response(object, response_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_set_has_separator(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkDialog* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_dialog_set_has_separator(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_dialog_set_response_sensitive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkDialog* object;
  gint response_id;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &response_id) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_dialog_set_response_sensitive(object, response_id, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_check_threshold(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;
  gint start_x;
  gint start_y;
  gint current_x;
  gint current_y;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &start_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &start_y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &current_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &current_y) ) return FALSE;
  R = gtk_drag_check_threshold(widget, start_x, start_y, current_x, current_y);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_dest_add_image_targets(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_dest_add_image_targets(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_dest_add_text_targets(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_dest_add_text_targets(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_dest_add_uri_targets(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_dest_add_uri_targets(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_dest_set_proxy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;
  GdkWindow* proxy_window;
  GdkDragProtocol protocol;
  gboolean use_coordinates;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&proxy_window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkDragProtocol", (gint*)&protocol) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_coordinates) ) return FALSE;
  gtk_drag_dest_set_proxy(widget, proxy_window, protocol, use_coordinates);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_dest_unset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_dest_unset(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_finish(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDragContext* context;
  gboolean success;
  gboolean del;
  guint32 time_;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAG_CONTEXT, (GObject**)&context) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &success) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &del) ) return FALSE;
  if ( ! gn_get_arg_guint32(XBUF, B, I, &time_) ) return FALSE;
  gtk_drag_finish(context, success, del, time_);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_get_source_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDragContext* context;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAG_CONTEXT, (GObject**)&context) ) return FALSE;
  R = (GObject*)gtk_drag_get_source_widget(context);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_highlight(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_highlight(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_set_icon_default(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDragContext* context;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAG_CONTEXT, (GObject**)&context) ) return FALSE;
  gtk_drag_set_icon_default(context);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_set_icon_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDragContext* context;
  gchar* stock_id;
  gint hot_x;
  gint hot_y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAG_CONTEXT, (GObject**)&context) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &hot_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &hot_y) ) return FALSE;
  gtk_drag_set_icon_stock(context, stock_id, hot_x, hot_y);
  free(stock_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_set_icon_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkDragContext* context;
  GtkWidget* widget;
  gint hot_x;
  gint hot_y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAG_CONTEXT, (GObject**)&context) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &hot_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &hot_y) ) return FALSE;
  gtk_drag_set_icon_widget(context, widget, hot_x, hot_y);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_source_add_image_targets(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_source_add_image_targets(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_source_add_text_targets(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_source_add_text_targets(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_source_add_uri_targets(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_source_add_uri_targets(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_source_set_icon_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;
  gchar* stock_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  gtk_drag_source_set_icon_stock(widget, stock_id);
  free(stock_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_source_unset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_source_unset(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drag_unhighlight(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_drag_unhighlight(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_draw_insertion_cursor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;
  GdkDrawable* drawable;
  GdkRectangle* area;
  GdkRectangle* location;
  gboolean is_primary;
  GtkTextDirection direction;
  gboolean draw_arrow;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 7, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_DRAWABLE, (GObject**)&drawable) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&location) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_primary) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTextDirection", (gint*)&direction) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &draw_arrow) ) return FALSE;
  gtk_draw_insertion_cursor(widget, drawable, area, location, is_primary, direction, draw_arrow);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_drawing_area_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_drawing_area_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_copy_clipboard(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  gtk_editable_copy_clipboard(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_cut_clipboard(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  gtk_editable_cut_clipboard(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_delete_selection(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  gtk_editable_delete_selection(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_delete_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;
  gint start_pos;
  gint end_pos;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &start_pos) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &end_pos) ) return FALSE;
  gtk_editable_delete_text(object, start_pos, end_pos);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_get_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;
  gint start_pos;
  gint end_pos;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &start_pos) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &end_pos) ) return FALSE;
  R = gtk_editable_get_chars(object, start_pos, end_pos);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_get_editable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  R = gtk_editable_get_editable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_get_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  R = gtk_editable_get_position(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_paste_clipboard(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  gtk_editable_paste_clipboard(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_select_region(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;
  gint start;
  gint end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &start) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &end) ) return FALSE;
  gtk_editable_select_region(object, start, end);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_set_editable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;
  gboolean is_editable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_editable) ) return FALSE;
  gtk_editable_set_editable(object, is_editable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_editable_set_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEditable* object;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EDITABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_editable_set_position(object, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_complete(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  gtk_entry_completion_complete(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_delete_action(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;
  gint index_;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &index_) ) return FALSE;
  gtk_entry_completion_delete_action(object, index_);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_get_entry(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_entry_completion_get_entry(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_get_inline_completion(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  R = gtk_entry_completion_get_inline_completion(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_get_minimum_key_length(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  R = gtk_entry_completion_get_minimum_key_length(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_get_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_entry_completion_get_model(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_get_popup_completion(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  R = gtk_entry_completion_get_popup_completion(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_get_text_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  R = gtk_entry_completion_get_text_column(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_insert_action_markup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;
  gint index_;
  gchar* markup;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &index_) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &markup) ) return FALSE;
  gtk_entry_completion_insert_action_markup(object, index_, markup);
  free(markup);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_insert_action_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;
  gint index_;
  gchar* text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &index_) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  gtk_entry_completion_insert_action_text(object, index_, text);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_insert_prefix(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  gtk_entry_completion_insert_prefix(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_entry_completion_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_set_inline_completion(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;
  gboolean inline_completion;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &inline_completion) ) return FALSE;
  gtk_entry_completion_set_inline_completion(object, inline_completion);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_set_minimum_key_length(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;
  gint length;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &length) ) return FALSE;
  gtk_entry_completion_set_minimum_key_length(object, length);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_set_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;
  GtkTreeModel* model;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&model) ) return FALSE;
  gtk_entry_completion_set_model(object, model);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_set_popup_completion(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;
  gboolean popup_completion;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &popup_completion) ) return FALSE;
  gtk_entry_completion_set_popup_completion(object, popup_completion);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_completion_set_text_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntryCompletion* object;
  gint column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column) ) return FALSE;
  gtk_entry_completion_set_text_column(object, column);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_activates_default(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_entry_get_activates_default(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_alignment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  gfloat R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_entry_get_alignment(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_completion(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_entry_get_completion(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_has_frame(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_entry_get_has_frame(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_layout(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  PangoLayout* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_entry_get_layout(object);
  gn_put_struct(XBUF,"PangoLayout",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_max_length(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_entry_get_max_length(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_entry_get_text(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_visibility(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_entry_get_visibility(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_get_width_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  R = gtk_entry_get_width_chars(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_layout_index_to_text_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gint layout_index;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &layout_index) ) return FALSE;
  R = gtk_entry_layout_index_to_text_index(object, layout_index);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_entry_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_set_activates_default(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_entry_set_activates_default(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_set_alignment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gfloat xalign;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  gtk_entry_set_alignment(object, xalign);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_set_completion(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  GtkEntryCompletion* completion;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY_COMPLETION, (GObject**)&completion) ) return FALSE;
  gtk_entry_set_completion(object, completion);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_set_has_frame(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_entry_set_has_frame(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_set_max_length(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gint max;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &max) ) return FALSE;
  gtk_entry_set_max_length(object, max);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_set_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gchar* text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  gtk_entry_set_text(object, text);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_set_visibility(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gboolean visible;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &visible) ) return FALSE;
  gtk_entry_set_visibility(object, visible);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_set_width_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gint n_chars;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n_chars) ) return FALSE;
  gtk_entry_set_width_chars(object, n_chars);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_entry_text_index_to_layout_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEntry* object;
  gint text_index;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ENTRY, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &text_index) ) return FALSE;
  R = gtk_entry_text_index_to_layout_index(object, text_index);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_event_box_get_above_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEventBox* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EVENT_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_event_box_get_above_child(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_event_box_get_visible_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEventBox* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EVENT_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_event_box_get_visible_window(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_event_box_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_event_box_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_event_box_set_above_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEventBox* object;
  gboolean above_child;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EVENT_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &above_child) ) return FALSE;
  gtk_event_box_set_above_child(object, above_child);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_event_box_set_visible_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkEventBox* object;
  gboolean visible_window;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EVENT_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &visible_window) ) return FALSE;
  gtk_event_box_set_visible_window(object, visible_window);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_get_expanded(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  R = gtk_expander_get_expanded(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_get_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  R = gtk_expander_get_label(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_get_label_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_expander_get_label_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_get_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  R = gtk_expander_get_spacing(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_get_use_markup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  R = gtk_expander_get_use_markup(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_get_use_underline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  R = gtk_expander_get_use_underline(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_expander_new(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_new_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_expander_new_with_mnemonic(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_set_expanded(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;
  gboolean expanded;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expanded) ) return FALSE;
  gtk_expander_set_expanded(object, expanded);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_set_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;
  gchar* label;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  gtk_expander_set_label(object, label);
  free(label);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_set_label_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;
  GtkWidget* label_widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&label_widget) ) return FALSE;
  gtk_expander_set_label_widget(object, label_widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_set_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;
  gint spacing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &spacing) ) return FALSE;
  gtk_expander_set_spacing(object, spacing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_set_use_markup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;
  gboolean use_markup;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_markup) ) return FALSE;
  gtk_expander_set_use_markup(object, use_markup);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_expander_set_use_underline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkExpander* object;
  gboolean use_underline;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_EXPANDER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_underline) ) return FALSE;
  gtk_expander_set_use_underline(object, use_underline);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_chooser_widget_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileChooserAction action;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkFileChooserAction", (gint*)&action) ) return FALSE;
  R = (GObject*)gtk_file_chooser_widget_new(action);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_chooser_widget_new_with_backend(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileChooserAction action;
  gchar* backend;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkFileChooserAction", (gint*)&action) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &backend) ) return FALSE;
  R = (GObject*)gtk_file_chooser_widget_new_with_backend(action, backend);
  free(backend);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_selection_complete(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileSelection* object;
  gchar* pattern;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FILE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &pattern) ) return FALSE;
  gtk_file_selection_complete(object, pattern);
  free(pattern);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_selection_get_filename(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileSelection* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FILE_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_file_selection_get_filename(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_selection_get_select_multiple(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileSelection* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FILE_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_file_selection_get_select_multiple(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_selection_hide_fileop_buttons(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileSelection* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FILE_SELECTION, (GObject**)&object) ) return FALSE;
  gtk_file_selection_hide_fileop_buttons(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_selection_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* title;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return FALSE;
  R = (GObject*)gtk_file_selection_new(title);
  free(title);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_selection_set_filename(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileSelection* object;
  gchar* filename;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FILE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &filename) ) return FALSE;
  gtk_file_selection_set_filename(object, filename);
  free(filename);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_selection_set_select_multiple(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileSelection* object;
  gboolean select_multiple;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FILE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &select_multiple) ) return FALSE;
  gtk_file_selection_set_select_multiple(object, select_multiple);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_file_selection_show_fileop_buttons(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFileSelection* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FILE_SELECTION, (GObject**)&object) ) return FALSE;
  gtk_file_selection_show_fileop_buttons(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_fixed_get_has_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFixed* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FIXED, (GObject**)&object) ) return FALSE;
  R = gtk_fixed_get_has_window(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_fixed_move(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFixed* object;
  GtkWidget* widget;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FIXED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  gtk_fixed_move(object, widget, x, y);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_fixed_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_fixed_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_fixed_put(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFixed* object;
  GtkWidget* widget;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FIXED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  gtk_fixed_put(object, widget, x, y);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_fixed_set_has_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFixed* object;
  gboolean has_window;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FIXED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &has_window) ) return FALSE;
  gtk_fixed_set_has_window(object, has_window);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_get_font_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_font_button_get_font_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_get_show_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_font_button_get_show_size(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_get_show_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_font_button_get_show_style(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_get_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_font_button_get_title(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_get_use_font(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_font_button_get_use_font(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_get_use_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_font_button_get_use_size(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_font_button_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_new_with_font(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* fontname;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &fontname) ) return FALSE;
  R = (GObject*)gtk_font_button_new_with_font(fontname);
  free(fontname);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_set_font_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;
  gchar* fontname;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &fontname) ) return FALSE;
  R = gtk_font_button_set_font_name(object, fontname);
  free(fontname);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_set_show_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;
  gboolean show_size;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &show_size) ) return FALSE;
  gtk_font_button_set_show_size(object, show_size);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_set_show_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;
  gboolean show_style;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &show_style) ) return FALSE;
  gtk_font_button_set_show_style(object, show_style);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_set_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;
  gchar* title;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return FALSE;
  gtk_font_button_set_title(object, title);
  free(title);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_set_use_font(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;
  gboolean use_font;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_font) ) return FALSE;
  gtk_font_button_set_use_font(object, use_font);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_button_set_use_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontButton* object;
  gboolean use_size;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_size) ) return FALSE;
  gtk_font_button_set_use_size(object, use_size);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_dialog_get_font_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontSelectionDialog* object;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_SELECTION_DIALOG, (GObject**)&object) ) return FALSE;
  R = gtk_font_selection_dialog_get_font_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_dialog_get_preview_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontSelectionDialog* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_SELECTION_DIALOG, (GObject**)&object) ) return FALSE;
  R = gtk_font_selection_dialog_get_preview_text(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_dialog_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* title;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return FALSE;
  R = (GObject*)gtk_font_selection_dialog_new(title);
  free(title);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_dialog_set_font_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontSelectionDialog* object;
  gchar* fontname;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_SELECTION_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &fontname) ) return FALSE;
  R = gtk_font_selection_dialog_set_font_name(object, fontname);
  free(fontname);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_dialog_set_preview_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontSelectionDialog* object;
  gchar* text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_SELECTION_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  gtk_font_selection_dialog_set_preview_text(object, text);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_get_font_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontSelection* object;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_font_selection_get_font_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_get_preview_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontSelection* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_font_selection_get_preview_text(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_font_selection_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_set_font_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontSelection* object;
  gchar* fontname;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &fontname) ) return FALSE;
  R = gtk_font_selection_set_font_name(object, fontname);
  free(fontname);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_font_selection_set_preview_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFontSelection* object;
  gchar* text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FONT_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  gtk_font_selection_set_preview_text(object, text);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_frame_get_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFrame* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FRAME, (GObject**)&object) ) return FALSE;
  R = gtk_frame_get_label(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_frame_get_label_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFrame* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FRAME, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_frame_get_label_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_frame_get_shadow_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFrame* object;

  GtkShadowType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FRAME, (GObject**)&object) ) return FALSE;
  R = gtk_frame_get_shadow_type(object);
  gn_put_enum(XBUF,"GtkShadowType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_frame_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_frame_new(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_frame_set_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFrame* object;
  gchar* label;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FRAME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  gtk_frame_set_label(object, label);
  free(label);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_frame_set_label_align(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFrame* object;
  gfloat xalign;
  gfloat yalign;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FRAME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yalign) ) return FALSE;
  gtk_frame_set_label_align(object, xalign, yalign);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_frame_set_label_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFrame* object;
  GtkWidget* label_widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FRAME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&label_widget) ) return FALSE;
  gtk_frame_set_label_widget(object, label_widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_frame_set_shadow_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkFrame* object;
  GtkShadowType type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_FRAME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&type) ) return FALSE;
  gtk_frame_set_shadow_type(object, type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_gc_release(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkGC* gc;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_GC, (GObject**)&gc) ) return FALSE;
  gtk_gc_release(gc);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_handle_box_get_handle_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkHandleBox* object;

  GtkPositionType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_HANDLE_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_handle_box_get_handle_position(object);
  gn_put_enum(XBUF,"GtkPositionType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_handle_box_get_shadow_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkHandleBox* object;

  GtkShadowType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_HANDLE_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_handle_box_get_shadow_type(object);
  gn_put_enum(XBUF,"GtkShadowType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_handle_box_get_snap_edge(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkHandleBox* object;

  GtkPositionType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_HANDLE_BOX, (GObject**)&object) ) return FALSE;
  R = gtk_handle_box_get_snap_edge(object);
  gn_put_enum(XBUF,"GtkPositionType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_handle_box_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_handle_box_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_handle_box_set_handle_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkHandleBox* object;
  GtkPositionType position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_HANDLE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPositionType", (gint*)&position) ) return FALSE;
  gtk_handle_box_set_handle_position(object, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_handle_box_set_shadow_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkHandleBox* object;
  GtkShadowType type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_HANDLE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&type) ) return FALSE;
  gtk_handle_box_set_shadow_type(object, type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_handle_box_set_snap_edge(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkHandleBox* object;
  GtkPositionType edge;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_HANDLE_BOX, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPositionType", (gint*)&edge) ) return FALSE;
  gtk_handle_box_set_snap_edge(object, edge);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_hbox_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gboolean homogeneous;
  gint spacing;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &homogeneous) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &spacing) ) return FALSE;
  R = (GObject*)gtk_hbox_new(homogeneous, spacing);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_hbutton_box_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_hbutton_box_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_hpaned_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_hpaned_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_hruler_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_hruler_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_hscale_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* adjustment;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  R = (GObject*)gtk_hscale_new(adjustment);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_hscale_new_with_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gdouble min;
  gdouble max;
  gdouble step;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &min) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &max) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &step) ) return FALSE;
  R = (GObject*)gtk_hscale_new_with_range(min, max, step);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_hscrollbar_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* adjustment;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  R = (GObject*)gtk_hscrollbar_new(adjustment);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_hseparator_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_hseparator_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_factory_add_default(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconFactory* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_FACTORY, (GObject**)&object) ) return FALSE;
  gtk_icon_factory_add_default(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_factory_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_icon_factory_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_factory_remove_default(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconFactory* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_FACTORY, (GObject**)&object) ) return FALSE;
  gtk_icon_factory_remove_default(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_size_from_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* name;

  GtkIconSize R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  R = gtk_icon_size_from_name(name);
  free(name);
  gn_put_enum(XBUF,"GtkIconSize",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_size_register(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* name;
  gint width;
  gint height;

  GtkIconSize R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  R = gtk_icon_size_register(name, width, height);
  free(name);
  gn_put_enum(XBUF,"GtkIconSize",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_size_register_alias(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* alias;
  GtkIconSize target;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &alias) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkIconSize", (gint*)&target) ) return FALSE;
  gtk_icon_size_register_alias(alias, target);
  free(alias);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_append_search_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconTheme* object;
  gchar* path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_THEME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &path) ) return FALSE;
  gtk_icon_theme_append_search_path(object, path);
  free(path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_get_default(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_icon_theme_get_default();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_get_example_icon_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconTheme* object;

  char* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_THEME, (GObject**)&object) ) return FALSE;
  R = gtk_icon_theme_get_example_icon_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_get_for_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkScreen* screen;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_SCREEN, (GObject**)&screen) ) return FALSE;
  R = (GObject*)gtk_icon_theme_get_for_screen(screen);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_has_icon(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconTheme* object;
  gchar* icon_name;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_THEME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &icon_name) ) return FALSE;
  R = gtk_icon_theme_has_icon(object, icon_name);
  free(icon_name);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_icon_theme_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_prepend_search_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconTheme* object;
  gchar* path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_THEME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &path) ) return FALSE;
  gtk_icon_theme_prepend_search_path(object, path);
  free(path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_rescan_if_needed(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconTheme* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_THEME, (GObject**)&object) ) return FALSE;
  R = gtk_icon_theme_rescan_if_needed(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_set_custom_theme(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconTheme* object;
  gchar* theme_name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_THEME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &theme_name) ) return FALSE;
  gtk_icon_theme_set_custom_theme(object, theme_name);
  free(theme_name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_icon_theme_set_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIconTheme* object;
  GdkScreen* screen;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ICON_THEME, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_SCREEN, (GObject**)&screen) ) return FALSE;
  gtk_icon_theme_set_screen(object, screen);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_im_multicontext_append_menuitems(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkIMMulticontext* object;
  GtkMenuShell* menushell;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IM_MULTICONTEXT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&menushell) ) return FALSE;
  gtk_im_multicontext_append_menuitems(object, menushell);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_get_animation(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImage* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_image_get_animation(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_get_pixel_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImage* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE, (GObject**)&object) ) return FALSE;
  R = gtk_image_get_pixel_size(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_get_storage_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImage* object;

  GtkImageType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE, (GObject**)&object) ) return FALSE;
  R = gtk_image_get_storage_type(object);
  gn_put_enum(XBUF,"GtkImageType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_menu_item_get_image(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImageMenuItem* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_image_menu_item_get_image(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_menu_item_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_image_menu_item_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_menu_item_new_from_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* stock_id;
  GtkAccelGroup* accel_group;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&accel_group) ) return FALSE;
  R = (GObject*)gtk_image_menu_item_new_from_stock(stock_id, accel_group);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_menu_item_new_with_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_image_menu_item_new_with_label(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_menu_item_new_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_image_menu_item_new_with_mnemonic(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_menu_item_set_image(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImageMenuItem* object;
  GtkWidget* image;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&image) ) return FALSE;
  gtk_image_menu_item_set_image(object, image);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_image_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_new_from_animation(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkPixbufAnimation* animation;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_PIXBUF_ANIMATION, (GObject**)&animation) ) return FALSE;
  R = (GObject*)gtk_image_new_from_animation(animation);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_new_from_file(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* filename;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &filename) ) return FALSE;
  R = (GObject*)gtk_image_new_from_file(filename);
  free(filename);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_new_from_icon_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* icon_name;
  GtkIconSize size;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &icon_name) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkIconSize", (gint*)&size) ) return FALSE;
  R = (GObject*)gtk_image_new_from_icon_name(icon_name, size);
  free(icon_name);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_new_from_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* stock_id;
  GtkIconSize size;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkIconSize", (gint*)&size) ) return FALSE;
  R = (GObject*)gtk_image_new_from_stock(stock_id, size);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_set_from_animation(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImage* object;
  GdkPixbufAnimation* animation;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_PIXBUF_ANIMATION, (GObject**)&animation) ) return FALSE;
  gtk_image_set_from_animation(object, animation);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_set_from_file(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImage* object;
  gchar* filename;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &filename) ) return FALSE;
  gtk_image_set_from_file(object, filename);
  free(filename);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_set_from_icon_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImage* object;
  gchar* icon_name;
  GtkIconSize size;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &icon_name) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkIconSize", (gint*)&size) ) return FALSE;
  gtk_image_set_from_icon_name(object, icon_name, size);
  free(icon_name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_set_from_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImage* object;
  gchar* stock_id;
  GtkIconSize size;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkIconSize", (gint*)&size) ) return FALSE;
  gtk_image_set_from_stock(object, stock_id, size);
  free(stock_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_image_set_pixel_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkImage* object;
  gint pixel_size;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_IMAGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &pixel_size) ) return FALSE;
  gtk_image_set_pixel_size(object, pixel_size);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_input_dialog_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_input_dialog_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_invisible_get_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkInvisible* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_INVISIBLE, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_invisible_get_screen(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_invisible_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_invisible_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_invisible_new_for_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkScreen* screen;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_SCREEN, (GObject**)&screen) ) return FALSE;
  R = (GObject*)gtk_invisible_new_for_screen(screen);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_invisible_set_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkInvisible* object;
  GdkScreen* screen;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_INVISIBLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_SCREEN, (GObject**)&screen) ) return FALSE;
  gtk_invisible_set_screen(object, screen);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_item_deselect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ITEM, (GObject**)&object) ) return FALSE;
  gtk_item_deselect(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_item_select(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ITEM, (GObject**)&object) ) return FALSE;
  gtk_item_select(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_item_toggle(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ITEM, (GObject**)&object) ) return FALSE;
  gtk_item_toggle(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_angle(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  gdouble R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_angle(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_justify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  GtkJustification R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_justify(object);
  gn_put_enum(XBUF,"GtkJustification",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_label(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_layout(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  PangoLayout* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_layout(object);
  gn_put_struct(XBUF,"PangoLayout",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_line_wrap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_line_wrap(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_max_width_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_max_width_chars(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_mnemonic_keyval(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_mnemonic_keyval(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_mnemonic_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_label_get_mnemonic_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_selectable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_selectable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_single_line_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_single_line_mode(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_text(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_use_markup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_use_markup(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_use_underline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_use_underline(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_get_width_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  R = gtk_label_get_width_chars(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  char* str;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_char(XBUF, B, I, &str) ) return FALSE;
  R = (GObject*)gtk_label_new(str);
  free(str);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_new_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  char* str;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_char(XBUF, B, I, &str) ) return FALSE;
  R = (GObject*)gtk_label_new_with_mnemonic(str);
  free(str);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_select_region(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gint start_offset;
  gint end_offset;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &start_offset) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &end_offset) ) return FALSE;
  gtk_label_select_region(object, start_offset, end_offset);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_angle(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gdouble angle;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &angle) ) return FALSE;
  gtk_label_set_angle(object, angle);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_justify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  GtkJustification jtype;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkJustification", (gint*)&jtype) ) return FALSE;
  gtk_label_set_justify(object, jtype);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gchar* str;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &str) ) return FALSE;
  gtk_label_set_label(object, str);
  free(str);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_line_wrap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gboolean wrap;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &wrap) ) return FALSE;
  gtk_label_set_line_wrap(object, wrap);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_markup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gchar* str;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &str) ) return FALSE;
  gtk_label_set_markup(object, str);
  free(str);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_markup_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gchar* str;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &str) ) return FALSE;
  gtk_label_set_markup_with_mnemonic(object, str);
  free(str);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_max_width_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gint n_chars;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n_chars) ) return FALSE;
  gtk_label_set_max_width_chars(object, n_chars);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_mnemonic_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_label_set_mnemonic_widget(object, widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_pattern(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gchar* pattern;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &pattern) ) return FALSE;
  gtk_label_set_pattern(object, pattern);
  free(pattern);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_selectable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_label_set_selectable(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_single_line_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gboolean single_line_mode;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &single_line_mode) ) return FALSE;
  gtk_label_set_single_line_mode(object, single_line_mode);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  char* str;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_char(XBUF, B, I, &str) ) return FALSE;
  gtk_label_set_text(object, str);
  free(str);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_text_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gchar* str;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &str) ) return FALSE;
  gtk_label_set_text_with_mnemonic(object, str);
  free(str);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_use_markup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_label_set_use_markup(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_use_underline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_label_set_use_underline(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_label_set_width_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLabel* object;
  gint n_chars;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LABEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n_chars) ) return FALSE;
  gtk_label_set_width_chars(object, n_chars);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_layout_get_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLayout* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LAYOUT, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_layout_get_hadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_layout_get_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLayout* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LAYOUT, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_layout_get_vadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_layout_move(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLayout* object;
  GtkWidget* child_widget;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child_widget) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  gtk_layout_move(object, child_widget, x, y);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_layout_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* hadjustment;
  GtkAdjustment* vadjustment;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&hadjustment) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&vadjustment) ) return FALSE;
  R = (GObject*)gtk_layout_new(hadjustment, vadjustment);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_layout_put(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLayout* object;
  GtkWidget* child_widget;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child_widget) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  gtk_layout_put(object, child_widget, x, y);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_layout_set_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLayout* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_layout_set_hadjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_layout_set_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLayout* object;
  guint width;
  guint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &height) ) return FALSE;
  gtk_layout_set_size(object, width, height);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_layout_set_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkLayout* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LAYOUT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_layout_set_vadjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_append(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_list_store_append(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_clear(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  gtk_list_store_clear(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_insert(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_list_store_insert(object, iter, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_insert_after(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* sibling;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&sibling) ) return FALSE;
  gtk_list_store_insert_after(object, iter, sibling);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_insert_before(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* sibling;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&sibling) ) return FALSE;
  gtk_list_store_insert_before(object, iter, sibling);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_iter_is_valid(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_list_store_iter_is_valid(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_move_after(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&position) ) return FALSE;
  gtk_list_store_move_after(object, iter, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_move_before(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&position) ) return FALSE;
  gtk_list_store_move_before(object, iter, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_newv(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gint n_columns;
  GType* types;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n_columns) ) return FALSE;
  if ( ! gn_get_arg_list(XBUF, B, I, "GType", (void**)&types) ) return FALSE;
  R = (GObject*)gtk_list_store_newv(n_columns, types);
  g_free(types);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_prepend(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_list_store_prepend(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_remove(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_list_store_remove(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_set_column_types(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  gint n_columns;
  GType* types;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n_columns) ) return FALSE;
  if ( ! gn_get_arg_list(XBUF, B, I, "GType", (void**)&types) ) return FALSE;
  gtk_list_store_set_column_types(object, n_columns, types);
  g_free(types);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_set_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* iter;
  gint column;
  GValue* value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&value) ) return FALSE;
  gtk_list_store_set_value(object, iter, column, value);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_list_store_swap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkListStore* object;
  GtkTreeIter* a;
  GtkTreeIter* b;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_LIST_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&a) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&b) ) return FALSE;
  gtk_list_store_swap(object, a, b);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_attach(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  GtkWidget* child;
  guint left_attach;
  guint right_attach;
  guint top_attach;
  guint bottom_attach;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &left_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &right_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &top_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &bottom_attach) ) return FALSE;
  gtk_menu_attach(object, child, left_attach, right_attach, top_attach, bottom_attach);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_bar_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_menu_bar_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_detach(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  gtk_menu_detach(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_get_accel_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_menu_get_accel_group(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_get_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_menu_get_active(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_get_attach_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_menu_get_attach_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_get_tearoff_state(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  R = gtk_menu_get_tearoff_state(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_get_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  R = gtk_menu_get_title(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_activate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  gtk_menu_item_activate(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_deselect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  gtk_menu_item_deselect(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_get_right_justified(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_menu_item_get_right_justified(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_get_submenu(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_menu_item_get_submenu(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_menu_item_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_new_with_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_menu_item_new_with_label(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_new_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_menu_item_new_with_mnemonic(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_remove_submenu(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  gtk_menu_item_remove_submenu(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_select(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  gtk_menu_item_select(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_set_accel_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;
  gchar* accel_path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_path) ) return FALSE;
  gtk_menu_item_set_accel_path(object, accel_path);
  free(accel_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_set_right_justified(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;
  gboolean right_justified;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &right_justified) ) return FALSE;
  gtk_menu_item_set_right_justified(object, right_justified);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_set_submenu(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;
  GtkWidget* submenu;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&submenu) ) return FALSE;
  gtk_menu_item_set_submenu(object, submenu);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_item_toggle_size_allocate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuItem* object;
  gint allocation;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &allocation) ) return FALSE;
  gtk_menu_item_toggle_size_allocate(object, allocation);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_menu_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_popdown(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  gtk_menu_popdown(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_reorder_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  GtkWidget* child;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_menu_reorder_child(object, child, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_reposition(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  gtk_menu_reposition(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_set_accel_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  GtkAccelGroup* accel_group;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&accel_group) ) return FALSE;
  gtk_menu_set_accel_group(object, accel_group);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_set_accel_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  gchar* accel_path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_path) ) return FALSE;
  gtk_menu_set_accel_path(object, accel_path);
  free(accel_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_set_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  guint index_;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &index_) ) return FALSE;
  gtk_menu_set_active(object, index_);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_set_monitor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  gint monitor_num;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &monitor_num) ) return FALSE;
  gtk_menu_set_monitor(object, monitor_num);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_set_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  GdkScreen* screen;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_SCREEN, (GObject**)&screen) ) return FALSE;
  gtk_menu_set_screen(object, screen);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_set_tearoff_state(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  gboolean torn_off;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &torn_off) ) return FALSE;
  gtk_menu_set_tearoff_state(object, torn_off);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_set_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenu* object;
  gchar* title;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return FALSE;
  gtk_menu_set_title(object, title);
  free(title);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_activate_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;
  GtkWidget* menu_item;
  gboolean force_deactivate;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&menu_item) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &force_deactivate) ) return FALSE;
  gtk_menu_shell_activate_item(object, menu_item, force_deactivate);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_append(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;
  GtkWidget* child;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  gtk_menu_shell_append(object, child);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_cancel(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  gtk_menu_shell_cancel(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_deactivate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  gtk_menu_shell_deactivate(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_deselect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  gtk_menu_shell_deselect(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_insert(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;
  GtkWidget* child;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_menu_shell_insert(object, child, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_prepend(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;
  GtkWidget* child;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  gtk_menu_shell_prepend(object, child);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_select_first(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;
  gboolean search_sensitive;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &search_sensitive) ) return FALSE;
  gtk_menu_shell_select_first(object, search_sensitive);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_menu_shell_select_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMenuShell* object;
  GtkWidget* menu_item;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MENU_SHELL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&menu_item) ) return FALSE;
  gtk_menu_shell_select_item(object, menu_item);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_message_dialog_set_markup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMessageDialog* object;
  gchar* str;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MESSAGE_DIALOG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &str) ) return FALSE;
  gtk_message_dialog_set_markup(object, str);
  free(str);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_misc_set_alignment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMisc* object;
  gfloat xalign;
  gfloat yalign;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MISC, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &yalign) ) return FALSE;
  gtk_misc_set_alignment(object, xalign, yalign);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_misc_set_padding(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkMisc* object;
  gint xpad;
  gint ypad;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_MISC, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &xpad) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ypad) ) return FALSE;
  gtk_misc_set_padding(object, xpad, ypad);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_append_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  GtkWidget* tab_label;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&tab_label) ) return FALSE;
  R = gtk_notebook_append_page(object, child, tab_label);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_append_page_menu(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  GtkWidget* tab_label;
  GtkWidget* menu_label;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&tab_label) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&menu_label) ) return FALSE;
  R = gtk_notebook_append_page_menu(object, child, tab_label, menu_label);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_current_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  R = gtk_notebook_get_current_page(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_menu_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  R = (GObject*)gtk_notebook_get_menu_label(object, child);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_menu_label_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  R = gtk_notebook_get_menu_label_text(object, child);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_n_pages(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  R = gtk_notebook_get_n_pages(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_nth_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  gint page_num;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &page_num) ) return FALSE;
  R = (GObject*)gtk_notebook_get_nth_page(object, page_num);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_scrollable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  R = gtk_notebook_get_scrollable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_show_border(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  R = gtk_notebook_get_show_border(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_show_tabs(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  R = gtk_notebook_get_show_tabs(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_tab_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  R = (GObject*)gtk_notebook_get_tab_label(object, child);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_tab_label_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  R = gtk_notebook_get_tab_label_text(object, child);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_get_tab_pos(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  GtkPositionType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  R = gtk_notebook_get_tab_pos(object);
  gn_put_enum(XBUF,"GtkPositionType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_insert_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  GtkWidget* tab_label;
  gint position;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&tab_label) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  R = gtk_notebook_insert_page(object, child, tab_label, position);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_insert_page_menu(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  GtkWidget* tab_label;
  GtkWidget* menu_label;
  gint position;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&tab_label) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&menu_label) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  R = gtk_notebook_insert_page_menu(object, child, tab_label, menu_label, position);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_notebook_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_next_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  gtk_notebook_next_page(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_page_num(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  R = gtk_notebook_page_num(object, child);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_popup_disable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  gtk_notebook_popup_disable(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_popup_enable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  gtk_notebook_popup_enable(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_prepend_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  GtkWidget* tab_label;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&tab_label) ) return FALSE;
  R = gtk_notebook_prepend_page(object, child, tab_label);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_prepend_page_menu(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  GtkWidget* tab_label;
  GtkWidget* menu_label;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&tab_label) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&menu_label) ) return FALSE;
  R = gtk_notebook_prepend_page_menu(object, child, tab_label, menu_label);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_prev_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  gtk_notebook_prev_page(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_remove_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  gint page_num;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &page_num) ) return FALSE;
  gtk_notebook_remove_page(object, page_num);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_reorder_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_notebook_reorder_child(object, child, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_current_page(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  gint page_num;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &page_num) ) return FALSE;
  gtk_notebook_set_current_page(object, page_num);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_menu_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  GtkWidget* menu_label;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&menu_label) ) return FALSE;
  gtk_notebook_set_menu_label(object, child, menu_label);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_menu_label_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  gchar* menu_text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &menu_text) ) return FALSE;
  gtk_notebook_set_menu_label_text(object, child, menu_text);
  free(menu_text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_scrollable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  gboolean scrollable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &scrollable) ) return FALSE;
  gtk_notebook_set_scrollable(object, scrollable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_show_border(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  gboolean show_border;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &show_border) ) return FALSE;
  gtk_notebook_set_show_border(object, show_border);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_show_tabs(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  gboolean show_tabs;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &show_tabs) ) return FALSE;
  gtk_notebook_set_show_tabs(object, show_tabs);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_tab_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  GtkWidget* tab_label;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&tab_label) ) return FALSE;
  gtk_notebook_set_tab_label(object, child, tab_label);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_tab_label_packing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  gboolean expand;
  gboolean fill;
  GtkPackType pack_type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &fill) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPackType", (gint*)&pack_type) ) return FALSE;
  gtk_notebook_set_tab_label_packing(object, child, expand, fill, pack_type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_tab_label_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkWidget* child;
  gchar* tab_text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &tab_text) ) return FALSE;
  gtk_notebook_set_tab_label_text(object, child, tab_text);
  free(tab_text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_notebook_set_tab_pos(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkNotebook* object;
  GtkPositionType pos;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_NOTEBOOK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPositionType", (gint*)&pos) ) return FALSE;
  gtk_notebook_set_tab_pos(object, pos);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_object_destroy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkObject* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_OBJECT, (GObject**)&object) ) return FALSE;
  gtk_object_destroy(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_object_sink(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkObject* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_OBJECT, (GObject**)&object) ) return FALSE;
  gtk_object_sink(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_arrow(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  GtkArrowType arrow_type;
  gboolean fill;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 13, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkArrowType", (gint*)&arrow_type) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &fill) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_arrow(style, window, state_type, shadow_type, area, widget, detail, arrow_type, fill, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_box(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 11, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_box(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_box_gap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;
  GtkPositionType gap_side;
  gint gap_x;
  gint gap_width;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 14, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPositionType", (gint*)&gap_side) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &gap_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &gap_width) ) return FALSE;
  gtk_paint_box_gap(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height, gap_side, gap_x, gap_width);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_check(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 11, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_check(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_diamond(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 11, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_diamond(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_expander(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  GtkExpanderStyle expander_style;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 9, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkExpanderStyle", (gint*)&expander_style) ) return FALSE;
  gtk_paint_expander(style, window, state_type, area, widget, detail, x, y, expander_style);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_extension(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;
  GtkPositionType gap_side;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 12, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPositionType", (gint*)&gap_side) ) return FALSE;
  gtk_paint_extension(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height, gap_side);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_flat_box(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 11, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_flat_box(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 10, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_focus(style, window, state_type, area, widget, detail, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_handle(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;
  GtkOrientation orientation;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 12, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkOrientation", (gint*)&orientation) ) return FALSE;
  gtk_paint_handle(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height, orientation);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_hline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x1;
  gint x2;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 9, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x1) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x2) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  gtk_paint_hline(style, window, state_type, area, widget, detail, x1, x2, y);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_layout(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  gboolean use_text;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  PangoLayout* layout;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 10, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_text) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "PangoLayout", (void**)&layout) ) return FALSE;
  gtk_paint_layout(style, window, state_type, use_text, area, widget, detail, x, y, layout);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_option(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 11, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_option(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_polygon(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  GdkPoint* points;
  gint npoints;
  gboolean fill;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 10, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkPoint", (void**)&points) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &npoints) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &fill) ) return FALSE;
  gtk_paint_polygon(style, window, state_type, shadow_type, area, widget, detail, points, npoints, fill);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_resize_grip(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  GdkWindowEdge edge;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 11, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkWindowEdge", (gint*)&edge) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_resize_grip(style, window, state_type, area, widget, detail, edge, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_shadow(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 11, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_shadow(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_shadow_gap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;
  GtkPositionType gap_side;
  gint gap_x;
  gint gap_width;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 14, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPositionType", (gint*)&gap_side) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &gap_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &gap_width) ) return FALSE;
  gtk_paint_shadow_gap(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height, gap_side, gap_x, gap_width);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_slider(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;
  GtkOrientation orientation;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 12, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkOrientation", (gint*)&orientation) ) return FALSE;
  gtk_paint_slider(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height, orientation);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_tab(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GtkShadowType shadow_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 11, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&shadow_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_paint_tab(style, window, state_type, shadow_type, area, widget, detail, x, y, width, height);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paint_vline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* style;
  GdkWindow* window;
  GtkStateType state_type;
  GdkRectangle* area;
  GtkWidget* widget;
  gchar* detail;
  gint y1_;
  gint y2_;
  gint x;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 9, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &detail) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y1_) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y2_) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  gtk_paint_vline(style, window, state_type, area, widget, detail, y1_, y2_, x);
  free(detail);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_add1(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;
  GtkWidget* child;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  gtk_paned_add1(object, child);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_add2(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;
  GtkWidget* child;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  gtk_paned_add2(object, child);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_compute_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;
  gint allocation;
  gint child1_req;
  gint child2_req;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &allocation) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &child1_req) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &child2_req) ) return FALSE;
  gtk_paned_compute_position(object, allocation, child1_req, child2_req);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_get_child1(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_paned_get_child1(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_get_child2(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_paned_get_child2(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_get_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  R = gtk_paned_get_position(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_pack1(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;
  GtkWidget* child;
  gboolean resize;
  gboolean shrink;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &resize) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &shrink) ) return FALSE;
  gtk_paned_pack1(object, child, resize, shrink);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_pack2(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;
  GtkWidget* child;
  gboolean resize;
  gboolean shrink;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &resize) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &shrink) ) return FALSE;
  gtk_paned_pack2(object, child, resize, shrink);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_paned_set_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkPaned* object;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PANED, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_paned_set_position(object, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_get_fraction(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;

  gdouble R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  R = gtk_progress_bar_get_fraction(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_get_orientation(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;

  GtkProgressBarOrientation R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  R = gtk_progress_bar_get_orientation(object);
  gn_put_enum(XBUF,"GtkProgressBarOrientation",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_get_pulse_step(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;

  gdouble R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  R = gtk_progress_bar_get_pulse_step(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_get_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  R = gtk_progress_bar_get_text(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_progress_bar_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_pulse(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  gtk_progress_bar_pulse(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_set_fraction(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;
  gdouble fraction;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &fraction) ) return FALSE;
  gtk_progress_bar_set_fraction(object, fraction);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_set_orientation(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;
  GtkProgressBarOrientation orientation;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkProgressBarOrientation", (gint*)&orientation) ) return FALSE;
  gtk_progress_bar_set_orientation(object, orientation);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_set_pulse_step(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;
  gdouble fraction;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &fraction) ) return FALSE;
  gtk_progress_bar_set_pulse_step(object, fraction);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_progress_bar_set_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkProgressBar* object;
  gchar* text;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_PROGRESS_BAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  gtk_progress_bar_set_text(object, text);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_action_get_current_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioAction* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_radio_action_get_current_value(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_action_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* name;
  gchar* label;
  gchar* tooltip;
  gchar* stock_id;
  gint value;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &tooltip) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &value) ) return FALSE;
  R = (GObject*)gtk_radio_action_new(name, label, tooltip, stock_id, value);
  free(name);
  free(label);
  free(tooltip);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_button_new_from_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioButton* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_BUTTON, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_radio_button_new_from_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_button_new_with_label_from_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioButton* object;
  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_radio_button_new_with_label_from_widget(object, label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_button_new_with_mnemonic_from_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioButton* object;
  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_radio_button_new_with_mnemonic_from_widget(object, label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_menu_item_new_from_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioMenuItem* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_MENU_ITEM, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_radio_menu_item_new_from_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_menu_item_new_with_label_from_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioMenuItem* object;
  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_radio_menu_item_new_with_label_from_widget(object, label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_menu_item_new_with_mnemonic_from_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioMenuItem* object;
  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_MENU_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_radio_menu_item_new_with_mnemonic_from_widget(object, label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_tool_button_new_from_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioToolButton* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_radio_tool_button_new_from_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_radio_tool_button_new_with_stock_from_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRadioToolButton* object;
  gchar* stock_id;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RADIO_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  R = (GObject*)gtk_radio_tool_button_new_with_stock_from_widget(object, stock_id);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_get_adjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_range_get_adjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_get_inverted(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  R = gtk_range_get_inverted(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_get_update_policy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;

  GtkUpdateType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  R = gtk_range_get_update_policy(object);
  gn_put_enum(XBUF,"GtkUpdateType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_get_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;

  gdouble R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  R = gtk_range_get_value(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_set_adjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_range_set_adjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_set_increments(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;
  gdouble step;
  gdouble page;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &step) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &page) ) return FALSE;
  gtk_range_set_increments(object, step, page);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_set_inverted(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_range_set_inverted(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_set_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;
  gdouble min;
  gdouble max;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &min) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &max) ) return FALSE;
  gtk_range_set_range(object, min, max);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_set_update_policy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;
  GtkUpdateType policy;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkUpdateType", (gint*)&policy) ) return FALSE;
  gtk_range_set_update_policy(object, policy);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_range_set_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRange* object;
  gdouble value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RANGE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &value) ) return FALSE;
  gtk_range_set_value(object, value);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_add_default_file(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* filename;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &filename) ) return FALSE;
  gtk_rc_add_default_file(filename);
  free(filename);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_find_module_in_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* module_file;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &module_file) ) return FALSE;
  R = gtk_rc_find_module_in_path(module_file);
  free(module_file);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_get_im_module_file(int ARI, ei_x_buff *XBUF, char *B, int *I){


  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_rc_get_im_module_file();
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_get_im_module_path(int ARI, ei_x_buff *XBUF, char *B, int *I){


  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_rc_get_im_module_path();
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_get_module_dir(int ARI, ei_x_buff *XBUF, char *B, int *I){


  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_rc_get_module_dir();
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_get_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  R = (GObject*)gtk_rc_get_style(widget);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_get_theme_dir(int ARI, ei_x_buff *XBUF, char *B, int *I){


  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_rc_get_theme_dir();
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_parse(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* filename;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &filename) ) return FALSE;
  gtk_rc_parse(filename);
  free(filename);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_parse_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* rc_string;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &rc_string) ) return FALSE;
  gtk_rc_parse_string(rc_string);
  free(rc_string);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_reparse_all(int ARI, ei_x_buff *XBUF, char *B, int *I){


  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_rc_reparse_all();
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_reparse_all_for_settings(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSettings* settings;
  gboolean force_load;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SETTINGS, (GObject**)&settings) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &force_load) ) return FALSE;
  R = gtk_rc_reparse_all_for_settings(settings, force_load);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_reset_styles(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSettings* settings;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SETTINGS, (GObject**)&settings) ) return FALSE;
  gtk_rc_reset_styles(settings);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_style_copy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRcStyle* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RC_STYLE, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_rc_style_copy(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_style_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_rc_style_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_style_ref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRcStyle* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RC_STYLE, (GObject**)&object) ) return FALSE;
  gtk_rc_style_ref(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_rc_style_unref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRcStyle* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RC_STYLE, (GObject**)&object) ) return FALSE;
  gtk_rc_style_unref(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ruler_draw_pos(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRuler* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RULER, (GObject**)&object) ) return FALSE;
  gtk_ruler_draw_pos(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ruler_draw_ticks(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRuler* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RULER, (GObject**)&object) ) return FALSE;
  gtk_ruler_draw_ticks(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ruler_get_metric(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRuler* object;

  GtkMetricType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RULER, (GObject**)&object) ) return FALSE;
  R = gtk_ruler_get_metric(object);
  gn_put_enum(XBUF,"GtkMetricType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_ruler_set_metric(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRuler* object;
  GtkMetricType metric;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RULER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkMetricType", (gint*)&metric) ) return FALSE;
  gtk_ruler_set_metric(object, metric);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ruler_set_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkRuler* object;
  gdouble lower;
  gdouble upper;
  gdouble position;
  gdouble max_size;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RULER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &lower) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &upper) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &position) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &max_size) ) return FALSE;
  gtk_ruler_set_range(object, lower, upper, position, max_size);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scale_get_digits(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScale* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCALE, (GObject**)&object) ) return FALSE;
  R = gtk_scale_get_digits(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scale_get_draw_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScale* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCALE, (GObject**)&object) ) return FALSE;
  R = gtk_scale_get_draw_value(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scale_get_layout(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScale* object;

  PangoLayout* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCALE, (GObject**)&object) ) return FALSE;
  R = gtk_scale_get_layout(object);
  gn_put_struct(XBUF,"PangoLayout",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scale_get_value_pos(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScale* object;

  GtkPositionType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCALE, (GObject**)&object) ) return FALSE;
  R = gtk_scale_get_value_pos(object);
  gn_put_enum(XBUF,"GtkPositionType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scale_set_digits(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScale* object;
  gint digits;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCALE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &digits) ) return FALSE;
  gtk_scale_set_digits(object, digits);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scale_set_draw_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScale* object;
  gboolean draw_value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCALE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &draw_value) ) return FALSE;
  gtk_scale_set_draw_value(object, draw_value);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scale_set_value_pos(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScale* object;
  GtkPositionType pos;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCALE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPositionType", (gint*)&pos) ) return FALSE;
  gtk_scale_set_value_pos(object, pos);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_add_with_viewport(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;
  GtkWidget* child;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  gtk_scrolled_window_add_with_viewport(object, child);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_get_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_scrolled_window_get_hadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_get_placement(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;

  GtkCornerType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_scrolled_window_get_placement(object);
  gn_put_enum(XBUF,"GtkCornerType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_get_shadow_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;

  GtkShadowType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_scrolled_window_get_shadow_type(object);
  gn_put_enum(XBUF,"GtkShadowType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_get_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_scrolled_window_get_vadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* hadjustment;
  GtkAdjustment* vadjustment;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&hadjustment) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&vadjustment) ) return FALSE;
  R = (GObject*)gtk_scrolled_window_new(hadjustment, vadjustment);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_set_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;
  GtkAdjustment* hadjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&hadjustment) ) return FALSE;
  gtk_scrolled_window_set_hadjustment(object, hadjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_set_placement(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;
  GtkCornerType window_placement;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkCornerType", (gint*)&window_placement) ) return FALSE;
  gtk_scrolled_window_set_placement(object, window_placement);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_set_policy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;
  GtkPolicyType hscrollbar_policy;
  GtkPolicyType vscrollbar_policy;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPolicyType", (gint*)&hscrollbar_policy) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkPolicyType", (gint*)&vscrollbar_policy) ) return FALSE;
  gtk_scrolled_window_set_policy(object, hscrollbar_policy, vscrollbar_policy);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_set_shadow_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;
  GtkShadowType type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&type) ) return FALSE;
  gtk_scrolled_window_set_shadow_type(object, type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_scrolled_window_set_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkScrolledWindow* object;
  GtkAdjustment* hadjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SCROLLED_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&hadjustment) ) return FALSE;
  gtk_scrolled_window_set_vadjustment(object, hadjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_selection_remove_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_selection_remove_all(widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_separator_menu_item_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_separator_menu_item_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_separator_tool_item_get_draw(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSeparatorToolItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SEPARATOR_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_separator_tool_item_get_draw(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_separator_tool_item_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_separator_tool_item_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_separator_tool_item_set_draw(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSeparatorToolItem* object;
  gboolean draw;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SEPARATOR_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &draw) ) return FALSE;
  gtk_separator_tool_item_set_draw(object, draw);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_settings_get_default(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_settings_get_default();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_settings_get_for_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkScreen* screen;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_SCREEN, (GObject**)&screen) ) return FALSE;
  R = (GObject*)gtk_settings_get_for_screen(screen);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_settings_set_double_property(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSettings* object;
  gchar* name;
  gdouble v_double;
  gchar* origin;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SETTINGS, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &v_double) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &origin) ) return FALSE;
  gtk_settings_set_double_property(object, name, v_double, origin);
  free(name);
  free(origin);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_settings_set_string_property(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSettings* object;
  gchar* name;
  gchar* v_string;
  gchar* origin;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SETTINGS, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &v_string) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &origin) ) return FALSE;
  gtk_settings_set_string_property(object, name, v_string, origin);
  free(name);
  free(v_string);
  free(origin);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_size_group_add_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSizeGroup* object;
  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SIZE_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_size_group_add_widget(object, widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_size_group_get_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSizeGroup* object;

  GtkSizeGroupMode R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SIZE_GROUP, (GObject**)&object) ) return FALSE;
  R = gtk_size_group_get_mode(object);
  gn_put_enum(XBUF,"GtkSizeGroupMode",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_size_group_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSizeGroupMode mode;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkSizeGroupMode", (gint*)&mode) ) return FALSE;
  R = (GObject*)gtk_size_group_new(mode);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_size_group_remove_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSizeGroup* object;
  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SIZE_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_size_group_remove_widget(object, widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_size_group_set_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSizeGroup* object;
  GtkSizeGroupMode mode;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SIZE_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkSizeGroupMode", (gint*)&mode) ) return FALSE;
  gtk_size_group_set_mode(object, mode);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_socket_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_socket_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_configure(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  GtkAdjustment* adjustment;
  gdouble climb_rate;
  guint digits;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &climb_rate) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &digits) ) return FALSE;
  gtk_spin_button_configure(object, adjustment, climb_rate, digits);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_get_adjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_spin_button_get_adjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_get_digits(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_spin_button_get_digits(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_get_numeric(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_spin_button_get_numeric(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_get_snap_to_ticks(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_spin_button_get_snap_to_ticks(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_get_update_policy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  GtkSpinButtonUpdatePolicy R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_spin_button_get_update_policy(object);
  gn_put_enum(XBUF,"GtkSpinButtonUpdatePolicy",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_get_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  gdouble R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_spin_button_get_value(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_get_value_as_int(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_spin_button_get_value_as_int(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_get_wrap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_spin_button_get_wrap(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* adjustment;
  gdouble climb_rate;
  guint digits;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &climb_rate) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &digits) ) return FALSE;
  R = (GObject*)gtk_spin_button_new(adjustment, climb_rate, digits);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_new_with_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gdouble min;
  gdouble max;
  gdouble step;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &min) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &max) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &step) ) return FALSE;
  R = (GObject*)gtk_spin_button_new_with_range(min, max, step);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_adjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_spin_button_set_adjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_digits(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  guint digits;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &digits) ) return FALSE;
  gtk_spin_button_set_digits(object, digits);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_increments(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  gdouble step;
  gdouble page;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &step) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &page) ) return FALSE;
  gtk_spin_button_set_increments(object, step, page);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_numeric(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  gboolean numeric;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &numeric) ) return FALSE;
  gtk_spin_button_set_numeric(object, numeric);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  gdouble min;
  gdouble max;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &min) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &max) ) return FALSE;
  gtk_spin_button_set_range(object, min, max);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_snap_to_ticks(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  gboolean snap_to_ticks;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &snap_to_ticks) ) return FALSE;
  gtk_spin_button_set_snap_to_ticks(object, snap_to_ticks);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_update_policy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  GtkSpinButtonUpdatePolicy policy;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkSpinButtonUpdatePolicy", (gint*)&policy) ) return FALSE;
  gtk_spin_button_set_update_policy(object, policy);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  gdouble value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &value) ) return FALSE;
  gtk_spin_button_set_value(object, value);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_set_wrap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  gboolean wrap;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &wrap) ) return FALSE;
  gtk_spin_button_set_wrap(object, wrap);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_spin(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;
  GtkSpinType direction;
  gdouble increment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkSpinType", (gint*)&direction) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &increment) ) return FALSE;
  gtk_spin_button_spin(object, direction, increment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_spin_button_update(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkSpinButton* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_SPIN_BUTTON, (GObject**)&object) ) return FALSE;
  gtk_spin_button_update(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_statusbar_get_context_id(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStatusbar* object;
  gchar* context_description;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STATUSBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &context_description) ) return FALSE;
  R = gtk_statusbar_get_context_id(object, context_description);
  free(context_description);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_statusbar_get_has_resize_grip(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStatusbar* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STATUSBAR, (GObject**)&object) ) return FALSE;
  R = gtk_statusbar_get_has_resize_grip(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_statusbar_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_statusbar_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_statusbar_pop(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStatusbar* object;
  guint context_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STATUSBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &context_id) ) return FALSE;
  gtk_statusbar_pop(object, context_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_statusbar_push(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStatusbar* object;
  guint context_id;
  gchar* text;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STATUSBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &context_id) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  R = gtk_statusbar_push(object, context_id, text);
  free(text);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_statusbar_remove(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStatusbar* object;
  guint context_id;
  guint message_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STATUSBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &context_id) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &message_id) ) return FALSE;
  gtk_statusbar_remove(object, context_id, message_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_statusbar_set_has_resize_grip(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStatusbar* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STATUSBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_statusbar_set_has_resize_grip(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_style_apply_default_background(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* object;
  GdkWindow* window;
  gboolean set_bg;
  GtkStateType state_type;
  GdkRectangle* area;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 9, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &set_bg) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_style_apply_default_background(object, window, set_bg, state_type, area, x, y, width, height);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_style_attach(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* object;
  GdkWindow* window;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  R = (GObject*)gtk_style_attach(object, window);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_style_copy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_style_copy(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_style_detach(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&object) ) return FALSE;
  gtk_style_detach(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_style_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_style_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_style_set_background(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkStyle* object;
  GdkWindow* window;
  GtkStateType state_type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state_type) ) return FALSE;
  gtk_style_set_background(object, window, state_type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_attach(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  GtkWidget* child;
  guint left_attach;
  guint right_attach;
  guint top_attach;
  guint bottom_attach;
  GtkAttachOptions xoptions;
  GtkAttachOptions yoptions;
  guint xpadding;
  guint ypadding;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 10, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &left_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &right_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &top_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &bottom_attach) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GtkAttachOptions", (gint*)&xoptions) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GtkAttachOptions", (gint*)&yoptions) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &xpadding) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &ypadding) ) return FALSE;
  gtk_table_attach(object, child, left_attach, right_attach, top_attach, bottom_attach, xoptions, yoptions, xpadding, ypadding);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_attach_defaults(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  GtkWidget* widget;
  guint left_attach;
  guint right_attach;
  guint top_attach;
  guint bottom_attach;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &left_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &right_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &top_attach) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &bottom_attach) ) return FALSE;
  gtk_table_attach_defaults(object, widget, left_attach, right_attach, top_attach, bottom_attach);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_get_col_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  guint column;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &column) ) return FALSE;
  R = gtk_table_get_col_spacing(object, column);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_get_default_col_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  R = gtk_table_get_default_col_spacing(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_get_default_row_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  R = gtk_table_get_default_row_spacing(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_get_homogeneous(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  R = gtk_table_get_homogeneous(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_get_row_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  guint row;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &row) ) return FALSE;
  R = gtk_table_get_row_spacing(object, row);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  guint rows;
  guint columns;
  gboolean homogeneous;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &rows) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &columns) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &homogeneous) ) return FALSE;
  R = (GObject*)gtk_table_new(rows, columns, homogeneous);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_resize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  guint rows;
  guint columns;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &rows) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &columns) ) return FALSE;
  gtk_table_resize(object, rows, columns);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_set_col_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  guint column;
  guint spacing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &column) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &spacing) ) return FALSE;
  gtk_table_set_col_spacing(object, column, spacing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_set_col_spacings(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  guint spacing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &spacing) ) return FALSE;
  gtk_table_set_col_spacings(object, spacing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_set_homogeneous(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  gboolean homogeneous;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &homogeneous) ) return FALSE;
  gtk_table_set_homogeneous(object, homogeneous);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_set_row_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  guint row;
  guint spacing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &row) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &spacing) ) return FALSE;
  gtk_table_set_row_spacing(object, row, spacing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_table_set_row_spacings(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTable* object;
  guint spacing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &spacing) ) return FALSE;
  gtk_table_set_row_spacings(object, spacing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tearoff_menu_item_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_tearoff_menu_item_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_apply_tag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextTag* tag;
  GtkTextIter* start;
  GtkTextIter* end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  gtk_text_buffer_apply_tag(object, tag, start, end);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_apply_tag_by_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* name;
  GtkTextIter* start;
  GtkTextIter* end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  gtk_text_buffer_apply_tag_by_name(object, name, start, end);
  free(name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_backspace(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  gboolean interactive;
  gboolean default_editable;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &interactive) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &default_editable) ) return FALSE;
  R = gtk_text_buffer_backspace(object, iter, interactive, default_editable);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_begin_user_action(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  gtk_text_buffer_begin_user_action(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_create_child_anchor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  R = (GObject*)gtk_text_buffer_create_child_anchor(object, iter);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_create_mark(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* mark_name;
  GtkTextIter* where;
  gboolean left_gravity;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &mark_name) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&where) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &left_gravity) ) return FALSE;
  R = (GObject*)gtk_text_buffer_create_mark(object, mark_name, where, left_gravity);
  free(mark_name);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_delete(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* start;
  GtkTextIter* end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  gtk_text_buffer_delete(object, start, end);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_delete_interactive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* start_iter;
  GtkTextIter* end_iter;
  gboolean default_editable;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start_iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end_iter) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &default_editable) ) return FALSE;
  R = gtk_text_buffer_delete_interactive(object, start_iter, end_iter, default_editable);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_delete_mark(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextMark* mark;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&mark) ) return FALSE;
  gtk_text_buffer_delete_mark(object, mark);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_delete_mark_by_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  gtk_text_buffer_delete_mark_by_name(object, name);
  free(name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_delete_selection(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gboolean interactive;
  gboolean default_editable;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &interactive) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &default_editable) ) return FALSE;
  R = gtk_text_buffer_delete_selection(object, interactive, default_editable);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_end_user_action(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  gtk_text_buffer_end_user_action(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_bounds(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* start;
  GtkTextIter* end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  gtk_text_buffer_get_bounds(object, start, end);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_char_count(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  R = gtk_text_buffer_get_char_count(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_end_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  gtk_text_buffer_get_end_iter(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_insert(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_text_buffer_get_insert(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_iter_at_child_anchor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  GtkTextChildAnchor* anchor;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_CHILD_ANCHOR, (GObject**)&anchor) ) return FALSE;
  gtk_text_buffer_get_iter_at_child_anchor(object, iter, anchor);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_iter_at_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  gint line_number;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &line_number) ) return FALSE;
  gtk_text_buffer_get_iter_at_line(object, iter, line_number);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_iter_at_line_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  gint line_number;
  gint byte_index;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &line_number) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &byte_index) ) return FALSE;
  gtk_text_buffer_get_iter_at_line_index(object, iter, line_number, byte_index);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_iter_at_line_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  gint line_number;
  gint char_offset;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &line_number) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &char_offset) ) return FALSE;
  gtk_text_buffer_get_iter_at_line_offset(object, iter, line_number, char_offset);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_iter_at_mark(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  GtkTextMark* mark;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&mark) ) return FALSE;
  gtk_text_buffer_get_iter_at_mark(object, iter, mark);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_iter_at_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  gint char_offset;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &char_offset) ) return FALSE;
  gtk_text_buffer_get_iter_at_offset(object, iter, char_offset);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_line_count(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  R = gtk_text_buffer_get_line_count(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_mark(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* name;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  R = (GObject*)gtk_text_buffer_get_mark(object, name);
  free(name);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_modified(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  R = gtk_text_buffer_get_modified(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_selection_bound(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_text_buffer_get_selection_bound(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_selection_bounds(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* start;
  GtkTextIter* end;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  R = gtk_text_buffer_get_selection_bounds(object, start, end);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_slice(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* start;
  GtkTextIter* end;
  gboolean include_hidden_chars;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &include_hidden_chars) ) return FALSE;
  R = gtk_text_buffer_get_slice(object, start, end, include_hidden_chars);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_start_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  gtk_text_buffer_get_start_iter(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_tag_table(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_text_buffer_get_tag_table(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_get_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* start;
  GtkTextIter* end;
  gboolean include_hidden_chars;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &include_hidden_chars) ) return FALSE;
  R = gtk_text_buffer_get_text(object, start, end, include_hidden_chars);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_insert(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  gchar* text;
  gint len;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &len) ) return FALSE;
  gtk_text_buffer_insert(object, iter, text, len);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_insert_at_cursor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* text;
  gint len;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &len) ) return FALSE;
  gtk_text_buffer_insert_at_cursor(object, text, len);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_insert_child_anchor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  GtkTextChildAnchor* anchor;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_CHILD_ANCHOR, (GObject**)&anchor) ) return FALSE;
  gtk_text_buffer_insert_child_anchor(object, iter, anchor);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_insert_interactive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  gchar* text;
  gint len;
  gboolean default_editable;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &len) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &default_editable) ) return FALSE;
  R = gtk_text_buffer_insert_interactive(object, iter, text, len, default_editable);
  free(text);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_insert_interactive_at_cursor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* text;
  gint len;
  gboolean default_editable;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &len) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &default_editable) ) return FALSE;
  R = gtk_text_buffer_insert_interactive_at_cursor(object, text, len, default_editable);
  free(text);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_insert_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  GtkTextIter* start;
  GtkTextIter* end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  gtk_text_buffer_insert_range(object, iter, start, end);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_insert_range_interactive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* iter;
  GtkTextIter* start;
  GtkTextIter* end;
  gboolean default_editable;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &default_editable) ) return FALSE;
  R = gtk_text_buffer_insert_range_interactive(object, iter, start, end, default_editable);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_move_mark(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextMark* mark;
  GtkTextIter* where;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&mark) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&where) ) return FALSE;
  gtk_text_buffer_move_mark(object, mark, where);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_move_mark_by_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* name;
  GtkTextIter* where;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&where) ) return FALSE;
  gtk_text_buffer_move_mark_by_name(object, name, where);
  free(name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextTagTable* table;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG_TABLE, (GObject**)&table) ) return FALSE;
  R = (GObject*)gtk_text_buffer_new(table);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_place_cursor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* where;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&where) ) return FALSE;
  gtk_text_buffer_place_cursor(object, where);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_remove_all_tags(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* start;
  GtkTextIter* end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  gtk_text_buffer_remove_all_tags(object, start, end);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_remove_tag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextTag* tag;
  GtkTextIter* start;
  GtkTextIter* end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  gtk_text_buffer_remove_tag(object, tag, start, end);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_remove_tag_by_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* name;
  GtkTextIter* start;
  GtkTextIter* end;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  gtk_text_buffer_remove_tag_by_name(object, name, start, end);
  free(name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_select_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  GtkTextIter* ins;
  GtkTextIter* bound;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&ins) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&bound) ) return FALSE;
  gtk_text_buffer_select_range(object, ins, bound);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_set_modified(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_text_buffer_set_modified(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_buffer_set_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* object;
  gchar* text;
  gint len;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &len) ) return FALSE;
  gtk_text_buffer_set_text(object, text, len);
  free(text);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_child_anchor_get_deleted(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextChildAnchor* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_CHILD_ANCHOR, (GObject**)&object) ) return FALSE;
  R = gtk_text_child_anchor_get_deleted(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_child_anchor_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_text_child_anchor_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_char(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_backward_char(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_backward_chars(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_cursor_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_backward_cursor_position(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_cursor_positions(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_backward_cursor_positions(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_backward_line(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_lines(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_backward_lines(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_search(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gchar* str;
  GtkTextSearchFlags flags;
  GtkTextIter* match_start;
  GtkTextIter* match_end;
  GtkTextIter* limit;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &str) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GtkTextSearchFlags", (gint*)&flags) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&match_start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&match_end) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&limit) ) return FALSE;
  R = gtk_text_iter_backward_search(object, str, flags, match_start, match_end, limit);
  free(str);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_sentence_start(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_backward_sentence_start(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_sentence_starts(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_backward_sentence_starts(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_to_tag_toggle(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextTag* tag;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  R = gtk_text_iter_backward_to_tag_toggle(object, tag);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_visible_cursor_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_backward_visible_cursor_position(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_visible_cursor_positions(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_backward_visible_cursor_positions(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_visible_word_start(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_backward_visible_word_start(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_visible_word_starts(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_backward_visible_word_starts(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_word_start(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_backward_word_start(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_backward_word_starts(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_backward_word_starts(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_begins_tag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextTag* tag;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  R = gtk_text_iter_begins_tag(object, tag);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_can_insert(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gboolean default_editability;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &default_editability) ) return FALSE;
  R = gtk_text_iter_can_insert(object, default_editability);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_compare(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextIter* rhs;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&rhs) ) return FALSE;
  R = gtk_text_iter_compare(object, rhs);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_copy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  GtkTextIter* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_copy(object);
  gn_put_struct(XBUF,"GtkTextIter",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_editable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gboolean default_setting;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &default_setting) ) return FALSE;
  R = gtk_text_iter_editable(object, default_setting);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_ends_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_ends_line(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_ends_sentence(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_ends_sentence(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_ends_tag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextTag* tag;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  R = gtk_text_iter_ends_tag(object, tag);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_ends_word(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_ends_word(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_equal(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextIter* rhs;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&rhs) ) return FALSE;
  R = gtk_text_iter_equal(object, rhs);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_char(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_forward_char(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_chars(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_forward_chars(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_cursor_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_forward_cursor_position(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_cursor_positions(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_forward_cursor_positions(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_forward_line(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_lines(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_forward_lines(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_search(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gchar* str;
  GtkTextSearchFlags flags;
  GtkTextIter* match_start;
  GtkTextIter* match_end;
  GtkTextIter* limit;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &str) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GtkTextSearchFlags", (gint*)&flags) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&match_start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&match_end) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&limit) ) return FALSE;
  R = gtk_text_iter_forward_search(object, str, flags, match_start, match_end, limit);
  free(str);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_sentence_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_forward_sentence_end(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_sentence_ends(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_forward_sentence_ends(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_to_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  gtk_text_iter_forward_to_end(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_to_line_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_forward_to_line_end(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_to_tag_toggle(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextTag* tag;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  R = gtk_text_iter_forward_to_tag_toggle(object, tag);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_visible_cursor_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_forward_visible_cursor_position(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_visible_cursor_positions(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_forward_visible_cursor_positions(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_visible_word_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_forward_visible_word_end(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_visible_word_ends(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_forward_visible_word_ends(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_word_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_forward_word_end(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_forward_word_ends(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_iter_forward_word_ends(object, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_free(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  gtk_text_iter_free(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_buffer(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = (GObject*)gtk_text_iter_get_buffer(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_bytes_in_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_get_bytes_in_line(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_chars_in_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_get_chars_in_line(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_child_anchor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = (GObject*)gtk_text_iter_get_child_anchor(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_get_line(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_line_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_get_line_index(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_line_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_get_line_offset(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_get_offset(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_slice(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextIter* end;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  R = gtk_text_iter_get_slice(object, end);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextIter* end;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  R = gtk_text_iter_get_text(object, end);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_visible_line_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_get_visible_line_index(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_visible_line_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_get_visible_line_offset(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_visible_slice(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextIter* end;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  R = gtk_text_iter_get_visible_slice(object, end);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_get_visible_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextIter* end;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  R = gtk_text_iter_get_visible_text(object, end);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_has_tag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextTag* tag;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  R = gtk_text_iter_has_tag(object, tag);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_in_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextIter* start;
  GtkTextIter* end;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&start) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&end) ) return FALSE;
  R = gtk_text_iter_in_range(object, start, end);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_inside_sentence(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_inside_sentence(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_inside_word(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_inside_word(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_is_cursor_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_is_cursor_position(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_is_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_is_end(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_is_start(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_is_start(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_order(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextIter* second;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&second) ) return FALSE;
  gtk_text_iter_order(object, second);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_set_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint line_number;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &line_number) ) return FALSE;
  gtk_text_iter_set_line(object, line_number);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_set_line_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint byte_on_line;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &byte_on_line) ) return FALSE;
  gtk_text_iter_set_line_index(object, byte_on_line);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_set_line_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint char_on_line;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &char_on_line) ) return FALSE;
  gtk_text_iter_set_line_offset(object, char_on_line);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_set_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint char_offset;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &char_offset) ) return FALSE;
  gtk_text_iter_set_offset(object, char_offset);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_set_visible_line_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint byte_on_line;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &byte_on_line) ) return FALSE;
  gtk_text_iter_set_visible_line_index(object, byte_on_line);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_set_visible_line_offset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  gint char_on_line;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &char_on_line) ) return FALSE;
  gtk_text_iter_set_visible_line_offset(object, char_on_line);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_starts_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_starts_line(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_starts_sentence(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_starts_sentence(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_starts_word(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  R = gtk_text_iter_starts_word(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_iter_toggles_tag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextIter* object;
  GtkTextTag* tag;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  R = gtk_text_iter_toggles_tag(object, tag);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_mark_get_buffer(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextMark* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_text_mark_get_buffer(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_mark_get_deleted(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextMark* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&object) ) return FALSE;
  R = gtk_text_mark_get_deleted(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_mark_get_left_gravity(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextMark* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&object) ) return FALSE;
  R = gtk_text_mark_get_left_gravity(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_mark_get_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextMark* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&object) ) return FALSE;
  R = gtk_text_mark_get_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_mark_get_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextMark* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&object) ) return FALSE;
  R = gtk_text_mark_get_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_mark_set_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextMark* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_text_mark_set_visible(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_tag_get_priority(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextTag* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&object) ) return FALSE;
  R = gtk_text_tag_get_priority(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_tag_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* name;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  R = (GObject*)gtk_text_tag_new(name);
  free(name);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_tag_set_priority(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextTag* object;
  gint priority;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &priority) ) return FALSE;
  gtk_text_tag_set_priority(object, priority);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_tag_table_add(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextTagTable* object;
  GtkTextTag* tag;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  gtk_text_tag_table_add(object, tag);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_tag_table_get_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextTagTable* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG_TABLE, (GObject**)&object) ) return FALSE;
  R = gtk_text_tag_table_get_size(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_tag_table_lookup(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextTagTable* object;
  gchar* name;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  R = (GObject*)gtk_text_tag_table_lookup(object, name);
  free(name);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_tag_table_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_text_tag_table_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_tag_table_remove(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextTagTable* object;
  GtkTextTag* tag;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG_TABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_TAG, (GObject**)&tag) ) return FALSE;
  gtk_text_tag_table_remove(object, tag);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_add_child_at_anchor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkWidget* child;
  GtkTextChildAnchor* anchor;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_CHILD_ANCHOR, (GObject**)&anchor) ) return FALSE;
  gtk_text_view_add_child_at_anchor(object, child, anchor);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_add_child_in_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkWidget* child;
  GtkTextWindowType which_window;
  gint xpos;
  gint ypos;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTextWindowType", (gint*)&which_window) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &xpos) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ypos) ) return FALSE;
  gtk_text_view_add_child_in_window(object, child, which_window, xpos, ypos);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_backward_display_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  R = gtk_text_view_backward_display_line(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_backward_display_line_start(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  R = gtk_text_view_backward_display_line_start(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_forward_display_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  R = gtk_text_view_forward_display_line(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_forward_display_line_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  R = gtk_text_view_forward_display_line_end(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_accepts_tab(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_accepts_tab(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_border_window_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextWindowType type;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTextWindowType", (gint*)&type) ) return FALSE;
  R = gtk_text_view_get_border_window_size(object, type);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_buffer(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_text_view_get_buffer(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_cursor_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_cursor_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_editable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_editable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_indent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_indent(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_iter_at_location(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  gtk_text_view_get_iter_at_location(object, iter, x, y);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_iter_location(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;
  GdkRectangle* location;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&location) ) return FALSE;
  gtk_text_view_get_iter_location(object, iter, location);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_justification(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  GtkJustification R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_justification(object);
  gn_put_enum(XBUF,"GtkJustification",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_left_margin(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_left_margin(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_overwrite(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_overwrite(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_pixels_above_lines(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_pixels_above_lines(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_pixels_below_lines(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_pixels_below_lines(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_pixels_inside_wrap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_pixels_inside_wrap(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_right_margin(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_right_margin(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_visible_rect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GdkRectangle* visible_rect;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&visible_rect) ) return FALSE;
  gtk_text_view_get_visible_rect(object, visible_rect);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextWindowType win;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTextWindowType", (gint*)&win) ) return FALSE;
  R = (GObject*)gtk_text_view_get_window(object, win);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_window_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GdkWindow* window;

  GtkTextWindowType R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  R = gtk_text_view_get_window_type(object, window);
  gn_put_enum(XBUF,"GtkTextWindowType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_get_wrap_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  GtkWrapMode R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_get_wrap_mode(object);
  gn_put_enum(XBUF,"GtkWrapMode",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_move_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkWidget* child;
  gint xpos;
  gint ypos;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&child) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &xpos) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &ypos) ) return FALSE;
  gtk_text_view_move_child(object, child, xpos, ypos);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_move_mark_onscreen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextMark* mark;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&mark) ) return FALSE;
  R = gtk_text_view_move_mark_onscreen(object, mark);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_move_visually(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;
  gint count;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &count) ) return FALSE;
  R = gtk_text_view_move_visually(object, iter, count);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_text_view_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_new_with_buffer(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextBuffer* buffer;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&buffer) ) return FALSE;
  R = (GObject*)gtk_text_view_new_with_buffer(buffer);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_place_cursor_onscreen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_text_view_place_cursor_onscreen(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_scroll_mark_onscreen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextMark* mark;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&mark) ) return FALSE;
  gtk_text_view_scroll_mark_onscreen(object, mark);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_scroll_to_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;
  gdouble within_margin;
  gboolean use_align;
  gdouble xalign;
  gdouble yalign;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &within_margin) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_align) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &yalign) ) return FALSE;
  R = gtk_text_view_scroll_to_iter(object, iter, within_margin, use_align, xalign, yalign);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_scroll_to_mark(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextMark* mark;
  gdouble within_margin;
  gboolean use_align;
  gdouble xalign;
  gdouble yalign;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_MARK, (GObject**)&mark) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &within_margin) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_align) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &xalign) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &yalign) ) return FALSE;
  gtk_text_view_scroll_to_mark(object, mark, within_margin, use_align, xalign, yalign);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_accepts_tab(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gboolean accepts_tab;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &accepts_tab) ) return FALSE;
  gtk_text_view_set_accepts_tab(object, accepts_tab);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_border_window_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextWindowType type;
  gint size;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTextWindowType", (gint*)&type) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &size) ) return FALSE;
  gtk_text_view_set_border_window_size(object, type, size);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_buffer(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextBuffer* buffer;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_BUFFER, (GObject**)&buffer) ) return FALSE;
  gtk_text_view_set_buffer(object, buffer);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_cursor_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_text_view_set_cursor_visible(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_editable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_text_view_set_editable(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_indent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gint indent;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &indent) ) return FALSE;
  gtk_text_view_set_indent(object, indent);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_justification(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkJustification justification;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkJustification", (gint*)&justification) ) return FALSE;
  gtk_text_view_set_justification(object, justification);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_left_margin(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gint left_margin;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &left_margin) ) return FALSE;
  gtk_text_view_set_left_margin(object, left_margin);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_overwrite(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gboolean overwrite;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &overwrite) ) return FALSE;
  gtk_text_view_set_overwrite(object, overwrite);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_pixels_above_lines(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gint pixels_above_lines;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &pixels_above_lines) ) return FALSE;
  gtk_text_view_set_pixels_above_lines(object, pixels_above_lines);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_pixels_below_lines(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gint pixels_below_lines;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &pixels_below_lines) ) return FALSE;
  gtk_text_view_set_pixels_below_lines(object, pixels_below_lines);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_pixels_inside_wrap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gint pixels_inside_wrap;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &pixels_inside_wrap) ) return FALSE;
  gtk_text_view_set_pixels_inside_wrap(object, pixels_inside_wrap);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_right_margin(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  gint right_margin;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &right_margin) ) return FALSE;
  gtk_text_view_set_right_margin(object, right_margin);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_set_wrap_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkWrapMode wrap_mode;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkWrapMode", (gint*)&wrap_mode) ) return FALSE;
  gtk_text_view_set_wrap_mode(object, wrap_mode);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_text_view_starts_display_line(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextView* object;
  GtkTextIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TEXT_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTextIter", (void**)&iter) ) return FALSE;
  R = gtk_text_view_starts_display_line(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_action_get_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleAction* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_toggle_action_get_active(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_action_get_draw_as_radio(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleAction* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_ACTION, (GObject**)&object) ) return FALSE;
  R = gtk_toggle_action_get_draw_as_radio(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_action_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* name;
  gchar* label;
  gchar* tooltip;
  gchar* stock_id;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &tooltip) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  R = (GObject*)gtk_toggle_action_new(name, label, tooltip, stock_id);
  free(name);
  free(label);
  free(tooltip);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_action_set_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleAction* object;
  gboolean is_active;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_active) ) return FALSE;
  gtk_toggle_action_set_active(object, is_active);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_action_set_draw_as_radio(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleAction* object;
  gboolean draw_as_radio;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_ACTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &draw_as_radio) ) return FALSE;
  gtk_toggle_action_set_draw_as_radio(object, draw_as_radio);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_action_toggled(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleAction* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_ACTION, (GObject**)&object) ) return FALSE;
  gtk_toggle_action_toggled(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_get_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_toggle_button_get_active(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_get_inconsistent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_toggle_button_get_inconsistent(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_get_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_toggle_button_get_mode(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_toggle_button_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_new_with_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_toggle_button_new_with_label(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_new_with_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_toggle_button_new_with_mnemonic(label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_set_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleButton* object;
  gboolean is_active;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_active) ) return FALSE;
  gtk_toggle_button_set_active(object, is_active);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_set_inconsistent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleButton* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_toggle_button_set_inconsistent(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_set_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleButton* object;
  gboolean draw_indicator;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &draw_indicator) ) return FALSE;
  gtk_toggle_button_set_mode(object, draw_indicator);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_button_toggled(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleButton* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_BUTTON, (GObject**)&object) ) return FALSE;
  gtk_toggle_button_toggled(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_tool_button_get_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleToolButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_toggle_tool_button_get_active(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_tool_button_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_toggle_tool_button_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_tool_button_new_from_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* stock_id;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  R = (GObject*)gtk_toggle_tool_button_new_from_stock(stock_id);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toggle_tool_button_set_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToggleToolButton* object;
  gboolean is_active;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOGGLE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_active) ) return FALSE;
  gtk_toggle_tool_button_set_active(object, is_active);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_get_icon_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tool_button_get_icon_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_get_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_tool_button_get_label(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_get_label_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tool_button_get_label_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_get_stock_id(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_tool_button_get_stock_id(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_get_use_underline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  R = gtk_tool_button_get_use_underline(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* icon_widget;
  gchar* label;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&icon_widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  R = (GObject*)gtk_tool_button_new(icon_widget, label);
  free(label);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_new_from_stock(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* stock_id;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  R = (GObject*)gtk_tool_button_new_from_stock(stock_id);
  free(stock_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_set_icon_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;
  GtkWidget* icon_widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&icon_widget) ) return FALSE;
  gtk_tool_button_set_icon_widget(object, icon_widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_set_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;
  gchar* label;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &label) ) return FALSE;
  gtk_tool_button_set_label(object, label);
  free(label);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_set_label_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;
  GtkWidget* label_widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&label_widget) ) return FALSE;
  gtk_tool_button_set_label_widget(object, label_widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_set_stock_id(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;
  gchar* stock_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &stock_id) ) return FALSE;
  gtk_tool_button_set_stock_id(object, stock_id);
  free(stock_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_button_set_use_underline(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolButton* object;
  gboolean use_underline;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_BUTTON, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_underline) ) return FALSE;
  gtk_tool_button_set_use_underline(object, use_underline);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_expand(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_expand(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_homogeneous(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_homogeneous(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_icon_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  GtkIconSize R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_icon_size(object);
  gn_put_enum(XBUF,"GtkIconSize",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_is_important(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_is_important(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_orientation(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  GtkOrientation R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_orientation(object);
  gn_put_enum(XBUF,"GtkOrientation",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_proxy_menu_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  gchar* menu_item_id;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &menu_item_id) ) return FALSE;
  R = (GObject*)gtk_tool_item_get_proxy_menu_item(object, menu_item_id);
  free(menu_item_id);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_relief_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  GtkReliefStyle R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_relief_style(object);
  gn_put_enum(XBUF,"GtkReliefStyle",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_toolbar_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  GtkToolbarStyle R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_toolbar_style(object);
  gn_put_enum(XBUF,"GtkToolbarStyle",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_use_drag_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_use_drag_window(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_visible_horizontal(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_visible_horizontal(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_get_visible_vertical(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = gtk_tool_item_get_visible_vertical(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_tool_item_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_rebuild_menu(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  gtk_tool_item_rebuild_menu(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_retrieve_proxy_menu_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tool_item_retrieve_proxy_menu_item(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_set_expand(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  gboolean expand;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  gtk_tool_item_set_expand(object, expand);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_set_homogeneous(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  gboolean homogeneous;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &homogeneous) ) return FALSE;
  gtk_tool_item_set_homogeneous(object, homogeneous);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_set_is_important(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  gboolean is_important;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_important) ) return FALSE;
  gtk_tool_item_set_is_important(object, is_important);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_set_proxy_menu_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  gchar* menu_item_id;
  GtkWidget* menu_item;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &menu_item_id) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&menu_item) ) return FALSE;
  gtk_tool_item_set_proxy_menu_item(object, menu_item_id, menu_item);
  free(menu_item_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_set_tooltip(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  GtkTooltips* tooltips;
  gchar* tip_text;
  gchar* tip_private;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLTIPS, (GObject**)&tooltips) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &tip_text) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &tip_private) ) return FALSE;
  gtk_tool_item_set_tooltip(object, tooltips, tip_text, tip_private);
  free(tip_text);
  free(tip_private);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_set_use_drag_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  gboolean use_drag_window;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_drag_window) ) return FALSE;
  gtk_tool_item_set_use_drag_window(object, use_drag_window);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_set_visible_horizontal(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  gboolean visible_horizontal;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &visible_horizontal) ) return FALSE;
  gtk_tool_item_set_visible_horizontal(object, visible_horizontal);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tool_item_set_visible_vertical(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolItem* object;
  gboolean visible_vertical;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &visible_vertical) ) return FALSE;
  gtk_tool_item_set_visible_vertical(object, visible_vertical);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_drop_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  gint x;
  gint y;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  R = gtk_toolbar_get_drop_index(object, x, y);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_icon_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;

  GtkIconSize R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  R = gtk_toolbar_get_icon_size(object);
  gn_put_enum(XBUF,"GtkIconSize",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_item_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  GtkToolItem* item;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&item) ) return FALSE;
  R = gtk_toolbar_get_item_index(object, item);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_n_items(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  R = gtk_toolbar_get_n_items(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_nth_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  gint n;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n) ) return FALSE;
  R = (GObject*)gtk_toolbar_get_nth_item(object, n);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_orientation(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;

  GtkOrientation R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  R = gtk_toolbar_get_orientation(object);
  gn_put_enum(XBUF,"GtkOrientation",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_relief_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;

  GtkReliefStyle R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  R = gtk_toolbar_get_relief_style(object);
  gn_put_enum(XBUF,"GtkReliefStyle",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_show_arrow(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  R = gtk_toolbar_get_show_arrow(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;

  GtkToolbarStyle R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  R = gtk_toolbar_get_style(object);
  gn_put_enum(XBUF,"GtkToolbarStyle",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_get_tooltips(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  R = gtk_toolbar_get_tooltips(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_insert(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  GtkToolItem* item;
  gint pos;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&item) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &pos) ) return FALSE;
  gtk_toolbar_insert(object, item, pos);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_toolbar_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_set_drop_highlight_item(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  GtkToolItem* tool_item;
  gint index_;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOL_ITEM, (GObject**)&tool_item) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &index_) ) return FALSE;
  gtk_toolbar_set_drop_highlight_item(object, tool_item, index_);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_set_orientation(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  GtkOrientation orientation;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkOrientation", (gint*)&orientation) ) return FALSE;
  gtk_toolbar_set_orientation(object, orientation);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_set_show_arrow(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  gboolean show_arrow;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &show_arrow) ) return FALSE;
  gtk_toolbar_set_show_arrow(object, show_arrow);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_set_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  GtkToolbarStyle style;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkToolbarStyle", (gint*)&style) ) return FALSE;
  gtk_toolbar_set_style(object, style);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_set_tooltips(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;
  gboolean enable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &enable) ) return FALSE;
  gtk_toolbar_set_tooltips(object, enable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_toolbar_unset_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkToolbar* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLBAR, (GObject**)&object) ) return FALSE;
  gtk_toolbar_unset_style(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tooltips_disable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTooltips* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLTIPS, (GObject**)&object) ) return FALSE;
  gtk_tooltips_disable(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tooltips_enable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTooltips* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLTIPS, (GObject**)&object) ) return FALSE;
  gtk_tooltips_enable(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tooltips_force_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTooltips* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLTIPS, (GObject**)&object) ) return FALSE;
  gtk_tooltips_force_window(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tooltips_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_tooltips_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tooltips_set_tip(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTooltips* object;
  GtkWidget* widget;
  gchar* tip_text;
  gchar* tip_private;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TOOLTIPS, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &tip_text) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &tip_private) ) return FALSE;
  gtk_tooltips_set_tip(object, widget, tip_text, tip_private);
  free(tip_text);
  free(tip_private);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_drag_source_drag_data_delete(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeDragSource* object;
  GtkTreePath* path;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_DRAG_SOURCE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  R = gtk_tree_drag_source_drag_data_delete(object, path);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_drag_source_row_draggable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeDragSource* object;
  GtkTreePath* path;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_DRAG_SOURCE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  R = gtk_tree_drag_source_row_draggable(object, path);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_iter_copy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeIter* object;

  GtkTreeIter* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&object) ) return FALSE;
  R = gtk_tree_iter_copy(object);
  gn_put_struct(XBUF,"GtkTreeIter",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_iter_free(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeIter* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&object) ) return FALSE;
  gtk_tree_iter_free(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_clear_cache(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelFilter* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_FILTER, (GObject**)&object) ) return FALSE;
  gtk_tree_model_filter_clear_cache(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_convert_child_iter_to_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelFilter* object;
  GtkTreeIter* filter_iter;
  GtkTreeIter* child_iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_FILTER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&filter_iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&child_iter) ) return FALSE;
  gtk_tree_model_filter_convert_child_iter_to_iter(object, filter_iter, child_iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_convert_child_path_to_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelFilter* object;
  GtkTreePath* child_path;

  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_FILTER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&child_path) ) return FALSE;
  R = gtk_tree_model_filter_convert_child_path_to_path(object, child_path);
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_convert_iter_to_child_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelFilter* object;
  GtkTreeIter* child_iter;
  GtkTreeIter* filter_iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_FILTER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&child_iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&filter_iter) ) return FALSE;
  gtk_tree_model_filter_convert_iter_to_child_iter(object, child_iter, filter_iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_convert_path_to_child_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelFilter* object;
  GtkTreePath* filter_path;

  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_FILTER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&filter_path) ) return FALSE;
  R = gtk_tree_model_filter_convert_path_to_child_path(object, filter_path);
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_get_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelFilter* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_FILTER, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_model_filter_get_model(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreePath* root;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&root) ) return FALSE;
  R = (GObject*)gtk_tree_model_filter_new(object, root);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_refilter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelFilter* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_FILTER, (GObject**)&object) ) return FALSE;
  gtk_tree_model_filter_refilter(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_filter_set_visible_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelFilter* object;
  gint column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_FILTER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column) ) return FALSE;
  gtk_tree_model_filter_set_visible_column(object, column);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_get_flags(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;

  GtkTreeModelFlags R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  R = gtk_tree_model_get_flags(object);
  gn_put_flags(XBUF,"GtkTreeModelFlags",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_get_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;
  GtkTreePath* path;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  R = gtk_tree_model_get_iter(object, iter, path);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_get_iter_first(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_model_get_iter_first(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_get_iter_from_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;
  gchar* path_string;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &path_string) ) return FALSE;
  R = gtk_tree_model_get_iter_from_string(object, iter, path_string);
  free(path_string);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_get_n_columns(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  R = gtk_tree_model_get_n_columns(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_get_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;

  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_model_get_path(object, iter);
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_get_string_from_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_model_get_string_from_iter(object, iter);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_get_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;
  gint column;
  GValue* value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&value) ) return FALSE;
  gtk_tree_model_get_value(object, iter, column, value);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_iter_children(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;
  GtkTreeIter* parent;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&parent) ) return FALSE;
  R = gtk_tree_model_iter_children(object, iter, parent);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_iter_has_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_model_iter_has_child(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_iter_n_children(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_model_iter_n_children(object, iter);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_iter_next(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_model_iter_next(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_iter_nth_child(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;
  GtkTreeIter* parent;
  gint n;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&parent) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n) ) return FALSE;
  R = gtk_tree_model_iter_nth_child(object, iter, parent, n);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_iter_parent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;
  GtkTreeIter* child;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&child) ) return FALSE;
  R = gtk_tree_model_iter_parent(object, iter, child);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_ref_node(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_tree_model_ref_node(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_row_changed(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreePath* path;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_tree_model_row_changed(object, path, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_row_deleted(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreePath* path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  gtk_tree_model_row_deleted(object, path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_row_has_child_toggled(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreePath* path;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_tree_model_row_has_child_toggled(object, path, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_row_inserted(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreePath* path;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_tree_model_row_inserted(object, path, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_clear_cache(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelSort* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_SORT, (GObject**)&object) ) return FALSE;
  gtk_tree_model_sort_clear_cache(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_convert_child_iter_to_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelSort* object;
  GtkTreeIter* sort_iter;
  GtkTreeIter* child_iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_SORT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&sort_iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&child_iter) ) return FALSE;
  gtk_tree_model_sort_convert_child_iter_to_iter(object, sort_iter, child_iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_convert_child_path_to_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelSort* object;
  GtkTreePath* child_path;

  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_SORT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&child_path) ) return FALSE;
  R = gtk_tree_model_sort_convert_child_path_to_path(object, child_path);
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_convert_iter_to_child_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelSort* object;
  GtkTreeIter* child_iter;
  GtkTreeIter* sorted_iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_SORT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&child_iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&sorted_iter) ) return FALSE;
  gtk_tree_model_sort_convert_iter_to_child_iter(object, child_iter, sorted_iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_convert_path_to_child_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelSort* object;
  GtkTreePath* sorted_path;

  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_SORT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&sorted_path) ) return FALSE;
  R = gtk_tree_model_sort_convert_path_to_child_path(object, sorted_path);
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_get_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelSort* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_SORT, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_model_sort_get_model(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_iter_is_valid(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelSort* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_SORT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_model_sort_iter_is_valid(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_new_with_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_model_sort_new_with_model(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_sort_reset_default_sort_func(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModelSort* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL_SORT, (GObject**)&object) ) return FALSE;
  gtk_tree_model_sort_reset_default_sort_func(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_model_unref_node(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* object;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_tree_model_unref_node(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_append_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;
  gint index_;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &index_) ) return FALSE;
  gtk_tree_path_append_index(object, index_);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_compare(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;
  GtkTreePath* b;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&b) ) return FALSE;
  R = gtk_tree_path_compare(object, b);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_copy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;

  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  R = gtk_tree_path_copy(object);
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_down(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  gtk_tree_path_down(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_free(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  gtk_tree_path_free(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_get_depth(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  R = gtk_tree_path_get_depth(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_is_ancestor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;
  GtkTreePath* descendant;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&descendant) ) return FALSE;
  R = gtk_tree_path_is_ancestor(object, descendant);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_is_descendant(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;
  GtkTreePath* ancestor;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&ancestor) ) return FALSE;
  R = gtk_tree_path_is_descendant(object, ancestor);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_tree_path_new();
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_new_first(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_tree_path_new_first();
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_new_from_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* path;

  GtkTreePath* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &path) ) return FALSE;
  R = gtk_tree_path_new_from_string(path);
  free(path);
  gn_put_struct(XBUF,"GtkTreePath",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_next(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  gtk_tree_path_next(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_prepend_index(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;
  gint index_;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &index_) ) return FALSE;
  gtk_tree_path_prepend_index(object, index_);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_prev(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  R = gtk_tree_path_prev(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_to_string(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  R = gtk_tree_path_to_string(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_path_up(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreePath* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&object) ) return FALSE;
  R = gtk_tree_path_up(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_count_selected_rows(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_tree_selection_count_selected_rows(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_get_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;

  GtkSelectionMode R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  R = gtk_tree_selection_get_mode(object);
  gn_put_enum(XBUF,"GtkSelectionMode",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_get_tree_view(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_selection_get_tree_view(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_iter_is_selected(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_selection_iter_is_selected(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_path_is_selected(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkTreePath* path;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  R = gtk_tree_selection_path_is_selected(object, path);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_select_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  gtk_tree_selection_select_all(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_select_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_tree_selection_select_iter(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_select_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkTreePath* path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  gtk_tree_selection_select_path(object, path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_select_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkTreePath* start_path;
  GtkTreePath* end_path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&start_path) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&end_path) ) return FALSE;
  gtk_tree_selection_select_range(object, start_path, end_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_set_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkSelectionMode type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkSelectionMode", (gint*)&type) ) return FALSE;
  gtk_tree_selection_set_mode(object, type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_unselect_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  gtk_tree_selection_unselect_all(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_unselect_iter(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkTreeIter* iter;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  gtk_tree_selection_unselect_iter(object, iter);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_unselect_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkTreePath* path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  gtk_tree_selection_unselect_path(object, path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_selection_unselect_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSelection* object;
  GtkTreePath* start_path;
  GtkTreePath* end_path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SELECTION, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&start_path) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&end_path) ) return FALSE;
  gtk_tree_selection_unselect_range(object, start_path, end_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_sortable_has_default_sort_func(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSortable* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SORTABLE, (GObject**)&object) ) return FALSE;
  R = gtk_tree_sortable_has_default_sort_func(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_sortable_set_sort_column_id(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSortable* object;
  gint sort_column_id;
  GtkSortType order;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SORTABLE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &sort_column_id) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkSortType", (gint*)&order) ) return FALSE;
  gtk_tree_sortable_set_sort_column_id(object, sort_column_id, order);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_sortable_sort_column_changed(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeSortable* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_SORTABLE, (GObject**)&object) ) return FALSE;
  gtk_tree_sortable_sort_column_changed(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_append(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* parent;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&parent) ) return FALSE;
  gtk_tree_store_append(object, iter, parent);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_clear(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  gtk_tree_store_clear(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_insert(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* parent;
  gint position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&parent) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  gtk_tree_store_insert(object, iter, parent, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_insert_after(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* parent;
  GtkTreeIter* sibling;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&parent) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&sibling) ) return FALSE;
  gtk_tree_store_insert_after(object, iter, parent, sibling);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_insert_before(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* parent;
  GtkTreeIter* sibling;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&parent) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&sibling) ) return FALSE;
  gtk_tree_store_insert_before(object, iter, parent, sibling);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_is_ancestor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* descendant;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&descendant) ) return FALSE;
  R = gtk_tree_store_is_ancestor(object, iter, descendant);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_iter_depth(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_store_iter_depth(object, iter);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_iter_is_valid(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_store_iter_is_valid(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_move_after(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&position) ) return FALSE;
  gtk_tree_store_move_after(object, iter, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_move_before(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&position) ) return FALSE;
  gtk_tree_store_move_before(object, iter, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_newv(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gint n_columns;
  GType* types;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n_columns) ) return FALSE;
  if ( ! gn_get_arg_list(XBUF, B, I, "GType", (void**)&types) ) return FALSE;
  R = (GObject*)gtk_tree_store_newv(n_columns, types);
  g_free(types);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_prepend(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  GtkTreeIter* parent;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&parent) ) return FALSE;
  gtk_tree_store_prepend(object, iter, parent);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_remove(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  R = gtk_tree_store_remove(object, iter);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_set_column_types(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  gint n_columns;
  GType* types;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n_columns) ) return FALSE;
  if ( ! gn_get_arg_list(XBUF, B, I, "GType", (void**)&types) ) return FALSE;
  gtk_tree_store_set_column_types(object, n_columns, types);
  g_free(types);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_set_value(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* iter;
  gint column;
  GValue* value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&value) ) return FALSE;
  gtk_tree_store_set_value(object, iter, column, value);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_store_swap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeStore* object;
  GtkTreeIter* a;
  GtkTreeIter* b;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_STORE, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&a) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&b) ) return FALSE;
  gtk_tree_store_swap(object, a, b);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_append_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreeViewColumn* column;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  R = gtk_tree_view_append_column(object, column);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_collapse_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  gtk_tree_view_collapse_all(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_collapse_row(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  R = gtk_tree_view_collapse_row(object, path);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_add_attribute(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkCellRenderer* cell_renderer;
  gchar* attribute;
  gint column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell_renderer) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &attribute) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column) ) return FALSE;
  gtk_tree_view_column_add_attribute(object, cell_renderer, attribute, column);
  free(attribute);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_cell_is_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_cell_is_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_cell_set_cell_data(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkTreeModel* tree_model;
  GtkTreeIter* iter;
  gboolean is_expander;
  gboolean is_expanded;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&tree_model) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreeIter", (void**)&iter) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_expander) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_expanded) ) return FALSE;
  gtk_tree_view_column_cell_set_cell_data(object, tree_model, iter, is_expander, is_expanded);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_clear(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  gtk_tree_view_column_clear(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_clear_attributes(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkCellRenderer* cell_renderer;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell_renderer) ) return FALSE;
  gtk_tree_view_column_clear_attributes(object, cell_renderer);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_clicked(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  gtk_tree_view_column_clicked(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_focus_cell(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkCellRenderer* cell;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell) ) return FALSE;
  gtk_tree_view_column_focus_cell(object, cell);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_alignment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gfloat R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_alignment(object);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_clickable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_clickable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_expand(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_expand(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_fixed_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_fixed_width(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_max_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_max_width(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_min_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_min_width(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_reorderable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_reorderable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_resizable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_resizable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_sizing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  GtkTreeViewColumnSizing R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_sizing(object);
  gn_put_enum(XBUF,"GtkTreeViewColumnSizing",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_sort_column_id(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_sort_column_id(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_sort_indicator(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_sort_indicator(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_sort_order(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  GtkSortType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_sort_order(object);
  gn_put_enum(XBUF,"GtkSortType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_spacing(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_title(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_view_column_get_widget(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_get_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_column_get_width(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_tree_view_column_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_pack_end(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkCellRenderer* cell;
  gboolean expand;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  gtk_tree_view_column_pack_end(object, cell, expand);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_pack_start(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkCellRenderer* cell;
  gboolean expand;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&cell) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  gtk_tree_view_column_pack_start(object, cell, expand);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_alignment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gfloat xalign;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &xalign) ) return FALSE;
  gtk_tree_view_column_set_alignment(object, xalign);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_clickable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gboolean clickable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &clickable) ) return FALSE;
  gtk_tree_view_column_set_clickable(object, clickable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_expand(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gboolean expand;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  gtk_tree_view_column_set_expand(object, expand);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_fixed_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gint fixed_width;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &fixed_width) ) return FALSE;
  gtk_tree_view_column_set_fixed_width(object, fixed_width);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_max_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gint max_width;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &max_width) ) return FALSE;
  gtk_tree_view_column_set_max_width(object, max_width);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_min_width(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gint min_width;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &min_width) ) return FALSE;
  gtk_tree_view_column_set_min_width(object, min_width);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_reorderable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gboolean reorderable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &reorderable) ) return FALSE;
  gtk_tree_view_column_set_reorderable(object, reorderable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_resizable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gboolean resizable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &resizable) ) return FALSE;
  gtk_tree_view_column_set_resizable(object, resizable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_sizing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkTreeViewColumnSizing type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTreeViewColumnSizing", (gint*)&type) ) return FALSE;
  gtk_tree_view_column_set_sizing(object, type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_sort_column_id(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gint sort_column_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &sort_column_id) ) return FALSE;
  gtk_tree_view_column_set_sort_column_id(object, sort_column_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_sort_indicator(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_tree_view_column_set_sort_indicator(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_sort_order(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkSortType order;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkSortType", (gint*)&order) ) return FALSE;
  gtk_tree_view_column_set_sort_order(object, order);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_spacing(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gint spacing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &spacing) ) return FALSE;
  gtk_tree_view_column_set_spacing(object, spacing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gchar* title;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return FALSE;
  gtk_tree_view_column_set_title(object, title);
  free(title);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  gboolean visible;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &visible) ) return FALSE;
  gtk_tree_view_column_set_visible(object, visible);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_column_set_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeViewColumn* object;
  GtkWidget* widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) return FALSE;
  gtk_tree_view_column_set_widget(object, widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_columns_autosize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  gtk_tree_view_columns_autosize(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_create_row_drag_icon(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  R = (GObject*)gtk_tree_view_create_row_drag_icon(object, path);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_expand_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  gtk_tree_view_expand_all(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_expand_row(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;
  gboolean open_all;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &open_all) ) return FALSE;
  R = gtk_tree_view_expand_row(object, path, open_all);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_expand_to_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  gtk_tree_view_expand_to_path(object, path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_background_area(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;
  GtkTreeViewColumn* column;
  GdkRectangle* rect;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&rect) ) return FALSE;
  gtk_tree_view_get_background_area(object, path, column, rect);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_bin_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_view_get_bin_window(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_cell_area(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;
  GtkTreeViewColumn* column;
  GdkRectangle* rect;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&rect) ) return FALSE;
  gtk_tree_view_get_cell_area(object, path, column, rect);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gint n;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &n) ) return FALSE;
  R = (GObject*)gtk_tree_view_get_column(object, n);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_enable_search(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_get_enable_search(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_expander_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_view_get_expander_column(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_fixed_height_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_get_fixed_height_mode(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_view_get_hadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_headers_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_get_headers_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_hover_expand(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_get_hover_expand(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_hover_selection(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_get_hover_selection(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_view_get_model(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_reorderable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_get_reorderable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_rules_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_get_rules_hint(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_search_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = gtk_tree_view_get_search_column(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_selection(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_view_get_selection(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_tree_view_get_vadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_get_visible_rect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GdkRectangle* visible_rect;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&visible_rect) ) return FALSE;
  gtk_tree_view_get_visible_rect(object, visible_rect);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_insert_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreeViewColumn* column;
  gint position;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &position) ) return FALSE;
  R = gtk_tree_view_insert_column(object, column, position);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_move_column_after(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreeViewColumn* column;
  GtkTreeViewColumn* base_column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&base_column) ) return FALSE;
  gtk_tree_view_move_column_after(object, column, base_column);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_tree_view_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_new_with_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeModel* model;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&model) ) return FALSE;
  R = (GObject*)gtk_tree_view_new_with_model(model);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_remove_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreeViewColumn* column;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  R = gtk_tree_view_remove_column(object, column);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_row_activated(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;
  GtkTreeViewColumn* column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  gtk_tree_view_row_activated(object, path, column);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_row_expanded(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  R = gtk_tree_view_row_expanded(object, path);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_scroll_to_cell(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;
  GtkTreeViewColumn* column;
  gboolean use_align;
  gfloat row_align;
  gfloat col_align;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &use_align) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &row_align) ) return FALSE;
  if ( ! gn_get_arg_gfloat(XBUF, B, I, &col_align) ) return FALSE;
  gtk_tree_view_scroll_to_cell(object, path, column, use_align, row_align, col_align);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_scroll_to_point(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gint tree_x;
  gint tree_y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &tree_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &tree_y) ) return FALSE;
  gtk_tree_view_scroll_to_point(object, tree_x, tree_y);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_cursor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;
  GtkTreeViewColumn* focus_column;
  gboolean start_editing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&focus_column) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &start_editing) ) return FALSE;
  gtk_tree_view_set_cursor(object, path, focus_column, start_editing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_cursor_on_cell(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;
  GtkTreeViewColumn* focus_column;
  GtkCellRenderer* focus_cell;
  gboolean start_editing;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&focus_column) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_CELL_RENDERER, (GObject**)&focus_cell) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &start_editing) ) return FALSE;
  gtk_tree_view_set_cursor_on_cell(object, path, focus_column, focus_cell, start_editing);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_drag_dest_row(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreePath* path;
  GtkTreeViewDropPosition pos;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GtkTreePath", (void**)&path) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTreeViewDropPosition", (gint*)&pos) ) return FALSE;
  gtk_tree_view_set_drag_dest_row(object, path, pos);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_enable_search(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gboolean enable_search;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &enable_search) ) return FALSE;
  gtk_tree_view_set_enable_search(object, enable_search);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_expander_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreeViewColumn* column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW_COLUMN, (GObject**)&column) ) return FALSE;
  gtk_tree_view_set_expander_column(object, column);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_fixed_height_mode(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gboolean enable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &enable) ) return FALSE;
  gtk_tree_view_set_fixed_height_mode(object, enable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_tree_view_set_hadjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_headers_clickable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_tree_view_set_headers_clickable(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_headers_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gboolean headers_visible;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &headers_visible) ) return FALSE;
  gtk_tree_view_set_headers_visible(object, headers_visible);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_hover_expand(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gboolean expand;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &expand) ) return FALSE;
  gtk_tree_view_set_hover_expand(object, expand);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_hover_selection(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gboolean hover;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &hover) ) return FALSE;
  gtk_tree_view_set_hover_selection(object, hover);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_model(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkTreeModel* model;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_MODEL, (GObject**)&model) ) return FALSE;
  gtk_tree_view_set_model(object, model);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_reorderable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gboolean reorderable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &reorderable) ) return FALSE;
  gtk_tree_view_set_reorderable(object, reorderable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_rules_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_tree_view_set_rules_hint(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_search_column(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  gint column;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &column) ) return FALSE;
  gtk_tree_view_set_search_column(object, column);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_set_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_tree_view_set_vadjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_unset_rows_drag_dest(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  gtk_tree_view_unset_rows_drag_dest(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_tree_view_unset_rows_drag_source(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTreeView* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&object) ) return FALSE;
  gtk_tree_view_unset_rows_drag_source(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_add_ui(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;
  guint merge_id;
  gchar* path;
  gchar* name;
  gchar* action;
  GtkUIManagerItemType type;
  gboolean top;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 7, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &merge_id) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &path) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &action) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GtkUIManagerItemType", (gint*)&type) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &top) ) return FALSE;
  gtk_ui_manager_add_ui(object, merge_id, path, name, action, type, top);
  free(path);
  free(name);
  free(action);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_ensure_update(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  gtk_ui_manager_ensure_update(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_get_accel_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_ui_manager_get_accel_group(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_get_action(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;
  gchar* path;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &path) ) return FALSE;
  R = (GObject*)gtk_ui_manager_get_action(object, path);
  free(path);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_get_add_tearoffs(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  R = gtk_ui_manager_get_add_tearoffs(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_get_ui(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  R = gtk_ui_manager_get_ui(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_get_widget(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;
  gchar* path;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &path) ) return FALSE;
  R = (GObject*)gtk_ui_manager_get_widget(object, path);
  free(path);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_insert_action_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;
  GtkActionGroup* action_group;
  gint pos;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&action_group) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &pos) ) return FALSE;
  gtk_ui_manager_insert_action_group(object, action_group, pos);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_ui_manager_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_new_merge_id(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;

  guint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  R = gtk_ui_manager_new_merge_id(object);
  gn_put_ulonglong(XBUF,(unsigned long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_remove_action_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;
  GtkActionGroup* action_group;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACTION_GROUP, (GObject**)&action_group) ) return FALSE;
  gtk_ui_manager_remove_action_group(object, action_group);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_remove_ui(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;
  guint merge_id;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &merge_id) ) return FALSE;
  gtk_ui_manager_remove_ui(object, merge_id);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_ui_manager_set_add_tearoffs(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkUIManager* object;
  gboolean add_tearoffs;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_UI_MANAGER, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &add_tearoffs) ) return FALSE;
  gtk_ui_manager_set_add_tearoffs(object, add_tearoffs);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_vbox_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gboolean homogeneous;
  gint spacing;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &homogeneous) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &spacing) ) return FALSE;
  R = (GObject*)gtk_vbox_new(homogeneous, spacing);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_vbutton_box_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_vbutton_box_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_viewport_get_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkViewport* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_VIEWPORT, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_viewport_get_hadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_viewport_get_shadow_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkViewport* object;

  GtkShadowType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_VIEWPORT, (GObject**)&object) ) return FALSE;
  R = gtk_viewport_get_shadow_type(object);
  gn_put_enum(XBUF,"GtkShadowType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_viewport_get_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkViewport* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_VIEWPORT, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_viewport_get_vadjustment(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_viewport_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* hadjustment;
  GtkAdjustment* vadjustment;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&hadjustment) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&vadjustment) ) return FALSE;
  R = (GObject*)gtk_viewport_new(hadjustment, vadjustment);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_viewport_set_hadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkViewport* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_VIEWPORT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_viewport_set_hadjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_viewport_set_shadow_type(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkViewport* object;
  GtkShadowType type;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_VIEWPORT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkShadowType", (gint*)&type) ) return FALSE;
  gtk_viewport_set_shadow_type(object, type);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_viewport_set_vadjustment(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkViewport* object;
  GtkAdjustment* adjustment;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_VIEWPORT, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  gtk_viewport_set_vadjustment(object, adjustment);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_vpaned_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_vpaned_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_vruler_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_vruler_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_vscale_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* adjustment;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  R = (GObject*)gtk_vscale_new(adjustment);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_vscale_new_with_range(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gdouble min;
  gdouble max;
  gdouble step;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &min) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &max) ) return FALSE;
  if ( ! gn_get_arg_gdouble(XBUF, B, I, &step) ) return FALSE;
  R = (GObject*)gtk_vscale_new_with_range(min, max, step);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_vscrollbar_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkAdjustment* adjustment;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&adjustment) ) return FALSE;
  R = (GObject*)gtk_vscrollbar_new(adjustment);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_vseparator_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_vseparator_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_activate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_activate(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_add_accelerator(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gchar* accel_signal;
  GtkAccelGroup* accel_group;
  guint accel_key;
  GdkModifierType accel_mods;
  GtkAccelFlags accel_flags;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_signal) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&accel_group) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &accel_key) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&accel_mods) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GtkAccelFlags", (gint*)&accel_flags) ) return FALSE;
  gtk_widget_add_accelerator(object, accel_signal, accel_group, accel_key, accel_mods, accel_flags);
  free(accel_signal);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_add_events(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gint events;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &events) ) return FALSE;
  gtk_widget_add_events(object, events);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_add_mnemonic_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkWidget* label;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&label) ) return FALSE;
  gtk_widget_add_mnemonic_label(object, label);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_can_activate_accel(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  guint signal_id;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &signal_id) ) return FALSE;
  R = gtk_widget_can_activate_accel(object, signal_id);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_child_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkDirectionType direction;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkDirectionType", (gint*)&direction) ) return FALSE;
  R = gtk_widget_child_focus(object, direction);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_child_notify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gchar* child_property;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &child_property) ) return FALSE;
  gtk_widget_child_notify(object, child_property);
  free(child_property);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_create_pango_context(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  PangoContext* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_create_pango_context(object);
  gn_put_struct(XBUF,"PangoContext",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_create_pango_layout(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gchar* text;

  PangoLayout* R;

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) ) return FALSE;
  R = gtk_widget_create_pango_layout(object, text);
  free(text);
  gn_put_struct(XBUF,"PangoLayout",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_destroy(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_destroy(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_ensure_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_ensure_style(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_freeze_child_notify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_freeze_child_notify(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_child_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_get_child_visible(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_colormap(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_composite_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_get_composite_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_default_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_widget_get_default_colormap();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_default_direction(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GtkTextDirection R; /* return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = gtk_widget_get_default_direction();
  gn_put_enum(XBUF,"GtkTextDirection",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_default_style(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_widget_get_default_style();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_direction(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GtkTextDirection R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_get_direction(object);
  gn_put_enum(XBUF,"GtkTextDirection",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_display(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_display(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_events(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  gint R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_get_events(object);
  gn_put_longlong(XBUF,(long long)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_extension_events(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GdkExtensionMode R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_get_extension_events(object);
  gn_put_enum(XBUF,"GdkExtensionMode",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_modifier_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_modifier_style(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_get_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_no_show_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_get_no_show_all(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_pango_context(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  PangoContext* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_get_pango_context(object);
  gn_put_struct(XBUF,"PangoContext",(void*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_parent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_parent(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_parent_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_parent_window(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_root_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_root_window(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_screen(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_settings(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_settings(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_style(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_get_toplevel(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_get_toplevel(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_grab_default(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_grab_default(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_grab_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_grab_focus(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_has_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_has_screen(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_hide(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_hide(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_hide_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_hide_all(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_hide_on_delete(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_hide_on_delete(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_intersect(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GdkRectangle* area;
  GdkRectangle* intersection;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&area) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkRectangle", (void**)&intersection) ) return FALSE;
  R = gtk_widget_intersect(object, area, intersection);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_is_ancestor(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkWidget* ancestor;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&ancestor) ) return FALSE;
  R = gtk_widget_is_ancestor(object, ancestor);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_is_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = gtk_widget_is_focus(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_map(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_map(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_mnemonic_activate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gboolean group_cycling;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &group_cycling) ) return FALSE;
  R = gtk_widget_mnemonic_activate(object, group_cycling);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_modify_base(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkStateType state;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_widget_modify_base(object, state, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_modify_bg(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkStateType state;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_widget_modify_bg(object, state, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_modify_fg(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkStateType state;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_widget_modify_fg(object, state, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_modify_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkRcStyle* style;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_RC_STYLE, (GObject**)&style) ) return FALSE;
  gtk_widget_modify_style(object, style);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_modify_text(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkStateType state;
  GdkColor* color;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GdkColor", (void**)&color) ) return FALSE;
  gtk_widget_modify_text(object, state, color);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_pop_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){


  /* no return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  gtk_widget_pop_colormap();
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_pop_composite_child(int ARI, ei_x_buff *XBUF, char *B, int *I){


  /* no return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  gtk_widget_pop_composite_child();
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_push_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* cmap;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&cmap) ) return FALSE;
  gtk_widget_push_colormap(cmap);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_push_composite_child(int ARI, ei_x_buff *XBUF, char *B, int *I){


  /* no return value */

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  gtk_widget_push_composite_child();
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_queue_draw(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_queue_draw(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_queue_draw_area(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gint x;
  gint y;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_widget_queue_draw_area(object, x, y, width, height);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_queue_resize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_queue_resize(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_queue_resize_no_redraw(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_queue_resize_no_redraw(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_realize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_realize(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_ref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_widget_ref(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_remove_accelerator(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkAccelGroup* accel_group;
  guint accel_key;
  GdkModifierType accel_mods;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 4, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&accel_group) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &accel_key) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&accel_mods) ) return FALSE;
  R = gtk_widget_remove_accelerator(object, accel_group, accel_key, accel_mods);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_remove_mnemonic_label(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkWidget* label;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&label) ) return FALSE;
  gtk_widget_remove_mnemonic_label(object, label);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_reparent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkWidget* new_parent;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&new_parent) ) return FALSE;
  gtk_widget_reparent(object, new_parent);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_reset_rc_styles(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_reset_rc_styles(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_reset_shapes(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_reset_shapes(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_accel_path(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gchar* accel_path;
  GtkAccelGroup* accel_group;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &accel_path) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&accel_group) ) return FALSE;
  gtk_widget_set_accel_path(object, accel_path, accel_group);
  free(accel_path);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_app_paintable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gboolean app_paintable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &app_paintable) ) return FALSE;
  gtk_widget_set_app_paintable(object, app_paintable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_child_visible(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gboolean is_visible;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &is_visible) ) return FALSE;
  gtk_widget_set_child_visible(object, is_visible);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GdkColormap* colormap;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return FALSE;
  gtk_widget_set_colormap(object, colormap);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_composite_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gchar* name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  gtk_widget_set_composite_name(object, name);
  free(name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_default_colormap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GdkColormap* colormap;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_COLORMAP, (GObject**)&colormap) ) return FALSE;
  gtk_widget_set_default_colormap(colormap);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_default_direction(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkTextDirection dir;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTextDirection", (gint*)&dir) ) return FALSE;
  gtk_widget_set_default_direction(dir);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_direction(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkTextDirection dir;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkTextDirection", (gint*)&dir) ) return FALSE;
  gtk_widget_set_direction(object, dir);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_double_buffered(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gboolean double_buffered;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &double_buffered) ) return FALSE;
  gtk_widget_set_double_buffered(object, double_buffered);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_events(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gint events;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &events) ) return FALSE;
  gtk_widget_set_events(object, events);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_extension_events(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GdkExtensionMode mode;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkExtensionMode", (gint*)&mode) ) return FALSE;
  gtk_widget_set_extension_events(object, mode);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gchar* name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  gtk_widget_set_name(object, name);
  free(name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_no_show_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gboolean no_show_all;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &no_show_all) ) return FALSE;
  gtk_widget_set_no_show_all(object, no_show_all);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_parent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkWidget* parent;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&parent) ) return FALSE;
  gtk_widget_set_parent(object, parent);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_parent_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GdkWindow* parent_window;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_WINDOW, (GObject**)&parent_window) ) return FALSE;
  gtk_widget_set_parent_window(object, parent_window);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_redraw_on_allocate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gboolean redraw_on_allocate;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &redraw_on_allocate) ) return FALSE;
  gtk_widget_set_redraw_on_allocate(object, redraw_on_allocate);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_scroll_adjustments(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkAdjustment* hadjustment;
  GtkAdjustment* vadjustment;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&hadjustment) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ADJUSTMENT, (GObject**)&vadjustment) ) return FALSE;
  R = gtk_widget_set_scroll_adjustments(object, hadjustment, vadjustment);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_sensitive(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gboolean sensitive;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &sensitive) ) return FALSE;
  gtk_widget_set_sensitive(object, sensitive);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_size_request(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_widget_set_size_request(object, width, height);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_state(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkStateType state;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkStateType", (gint*)&state) ) return FALSE;
  gtk_widget_set_state(object, state);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_set_style(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  GtkStyle* style;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_STYLE, (GObject**)&style) ) return FALSE;
  gtk_widget_set_style(object, style);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_show(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_show(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_show_all(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_show_all(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_show_now(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_show_now(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_style_get_property(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;
  gchar* property_name;
  GValue* value;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &property_name) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&value) ) return FALSE;
  gtk_widget_style_get_property(object, property_name, value);
  free(property_name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_thaw_child_notify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_thaw_child_notify(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_unmap(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_unmap(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_unparent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_unparent(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_unrealize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_unrealize(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_widget_unref(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWidget* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&object) ) return FALSE;
  gtk_widget_unref(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_activate_default(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_activate_default(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_activate_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_activate_focus(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_add_accel_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GtkAccelGroup* accel_group;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&accel_group) ) return FALSE;
  gtk_window_add_accel_group(object, accel_group);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_add_embedded_xid(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  guint xid;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &xid) ) return FALSE;
  gtk_window_add_embedded_xid(object, xid);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_add_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  guint keyval;
  GtkWidget* target;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &keyval) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&target) ) return FALSE;
  gtk_window_add_mnemonic(object, keyval, target);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_begin_move_drag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gint button;
  gint root_x;
  gint root_y;
  guint32 timestamp;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &button) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &root_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &root_y) ) return FALSE;
  if ( ! gn_get_arg_guint32(XBUF, B, I, &timestamp) ) return FALSE;
  gtk_window_begin_move_drag(object, button, root_x, root_y, timestamp);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_begin_resize_drag(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GdkWindowEdge edge;
  gint button;
  gint root_x;
  gint root_y;
  guint32 timestamp;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 6, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkWindowEdge", (gint*)&edge) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &button) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &root_x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &root_y) ) return FALSE;
  if ( ! gn_get_arg_guint32(XBUF, B, I, &timestamp) ) return FALSE;
  gtk_window_begin_resize_drag(object, edge, button, root_x, root_y, timestamp);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_deiconify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_deiconify(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_fullscreen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_fullscreen(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_accept_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_accept_focus(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_decorated(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_decorated(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_destroy_with_parent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_destroy_with_parent(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_window_get_focus(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_focus_on_map(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_focus_on_map(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_gravity(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  GdkGravity R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_gravity(object);
  gn_put_enum(XBUF,"GdkGravity",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_has_frame(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_has_frame(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_icon_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_icon_name(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_mnemonic_modifier(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  GdkModifierType R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_mnemonic_modifier(object);
  gn_put_flags(XBUF,"GdkModifierType",R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_modal(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_modal(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_resizable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_resizable(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_role(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_role(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_window_get_screen(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_skip_pager_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_skip_pager_hint(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_skip_taskbar_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_skip_taskbar_hint(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  const gchar* R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_get_title(object);
  gn_put_string(XBUF,(char*)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_get_transient_for(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = (GObject*)gtk_window_get_transient_for(object);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_group_add_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindowGroup* object;
  GtkWindow* window;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  gtk_window_group_add_window(object, window);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_group_new(int ARI, ei_x_buff *XBUF, char *B, int *I){


  GObject* R;

  if ( ! gn_check_arity(XBUF, 0, ARI) ) return FALSE;
  R = (GObject*)gtk_window_group_new();
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_group_remove_window(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindowGroup* object;
  GtkWindow* window;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW_GROUP, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&window) ) return FALSE;
  gtk_window_group_remove_window(object, window);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_has_toplevel_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_has_toplevel_focus(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_iconify(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_iconify(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_is_active(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  R = gtk_window_is_active(object);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_maximize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_maximize(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_mnemonic_activate(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  guint keyval;
  GdkModifierType modifier;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &keyval) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&modifier) ) return FALSE;
  R = gtk_window_mnemonic_activate(object, keyval, modifier);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_move(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gint x;
  gint y;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &x) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &y) ) return FALSE;
  gtk_window_move(object, x, y);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_new(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindowType type;

  GObject* R;

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkWindowType", (gint*)&type) ) return FALSE;
  R = (GObject*)gtk_window_new(type);
  gn_put_object(XBUF,R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_parse_geometry(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gchar* geometry;

  gboolean R; /* return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &geometry) ) return FALSE;
  R = gtk_window_parse_geometry(object, geometry);
  free(geometry);
  gn_put_boolean(XBUF,(int)R);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_present(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_present(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_remove_accel_group(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GtkAccelGroup* accel_group;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_ACCEL_GROUP, (GObject**)&accel_group) ) return FALSE;
  gtk_window_remove_accel_group(object, accel_group);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_remove_embedded_xid(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  guint xid;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &xid) ) return FALSE;
  gtk_window_remove_embedded_xid(object, xid);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_remove_mnemonic(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  guint keyval;
  GtkWidget* target;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_guint(XBUF, B, I, &keyval) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&target) ) return FALSE;
  gtk_window_remove_mnemonic(object, keyval, target);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_reshow_with_initial_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_reshow_with_initial_size(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_resize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_window_resize(object, width, height);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_accept_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_accept_focus(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_auto_startup_notification(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_auto_startup_notification(setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_decorated(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_decorated(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_default(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GtkWidget* default_widget;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&default_widget) ) return FALSE;
  gtk_window_set_default(object, default_widget);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_default_icon_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  gchar* name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  gtk_window_set_default_icon_name(name);
  free(name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_default_size(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gint width;
  gint height;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &width) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &height) ) return FALSE;
  gtk_window_set_default_size(object, width, height);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_destroy_with_parent(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_destroy_with_parent(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_focus(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GtkWidget* focus;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&focus) ) return FALSE;
  gtk_window_set_focus(object, focus);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_focus_on_map(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_focus_on_map(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_frame_dimensions(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gint left;
  gint top;
  gint right;
  gint bottom;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 5, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &left) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &top) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &right) ) return FALSE;
  if ( ! gn_get_arg_gint(XBUF, B, I, &bottom) ) return FALSE;
  gtk_window_set_frame_dimensions(object, left, top, right, bottom);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_gravity(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GdkGravity gravity;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkGravity", (gint*)&gravity) ) return FALSE;
  gtk_window_set_gravity(object, gravity);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_has_frame(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_has_frame(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_icon_name(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gchar* name;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &name) ) return FALSE;
  gtk_window_set_icon_name(object, name);
  free(name);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_keep_above(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_keep_above(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_keep_below(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_keep_below(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_mnemonic_modifier(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GdkModifierType modifier;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_flags(XBUF, B, I, "GdkModifierType", (gint*)&modifier) ) return FALSE;
  gtk_window_set_mnemonic_modifier(object, modifier);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_modal(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean modal;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &modal) ) return FALSE;
  gtk_window_set_modal(object, modal);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_position(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GtkWindowPosition position;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GtkWindowPosition", (gint*)&position) ) return FALSE;
  gtk_window_set_position(object, position);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_resizable(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean resizable;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &resizable) ) return FALSE;
  gtk_window_set_resizable(object, resizable);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_role(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gchar* role;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &role) ) return FALSE;
  gtk_window_set_role(object, role);
  free(role);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_screen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GdkScreen* screen;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GDK_TYPE_SCREEN, (GObject**)&screen) ) return FALSE;
  gtk_window_set_screen(object, screen);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_skip_pager_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_skip_pager_hint(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_skip_taskbar_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gboolean setting;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gboolean(XBUF, B, I, &setting) ) return FALSE;
  gtk_window_set_skip_taskbar_hint(object, setting);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_title(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gchar* title;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &title) ) return FALSE;
  gtk_window_set_title(object, title);
  free(title);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_transient_for(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GtkWindow* parent;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&parent) ) return FALSE;
  gtk_window_set_transient_for(object, parent);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_type_hint(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  GdkWindowTypeHint hint;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_enum(XBUF, B, I, "GdkWindowTypeHint", (gint*)&hint) ) return FALSE;
  gtk_window_set_type_hint(object, hint);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_set_wmclass(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;
  gchar* wmclass_name;
  gchar* wmclass_class;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 3, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &wmclass_name) ) return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &wmclass_class) ) return FALSE;
  gtk_window_set_wmclass(object, wmclass_name, wmclass_class);
  free(wmclass_name);
  free(wmclass_class);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_stick(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_stick(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_unfullscreen(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_unfullscreen(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_unmaximize(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_unmaximize(object);
  gn_put_void(XBUF);
  return TRUE;
}
/*******************************/
gboolean Gtk_window_unstick(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GtkWindow* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WINDOW, (GObject**)&object) ) return FALSE;
  gtk_window_unstick(object);
  gn_put_void(XBUF);
  return TRUE;
}
