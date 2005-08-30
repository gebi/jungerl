#include "ei.h" 
#include <gtk/gtk.h>
#include <gmodule.h>
#include <glade/glade.h>
#include <pango/pango.h>

/* starts the cnode, returns file descriptor*/
int gn_start_cnode(char **argv);

/* called when there's data on the erlang node socket */
gboolean gn_handle_read(GIOChannel *source,GIOCondition cond,gpointer data);

/* called by all signals */
void gn_sighandler(GtkWidget *widget);/* , gpointer user_data); */

/* inits libglade */
gboolean gn_glade_init(char *filename);

/*   marshalling functions (gtkNode_marshal.c) */
void gn_wrap_ans(char* tag, ei_x_buff* xbuf);
void gn_enc_1_error(ei_x_buff *xbuf, char *err);
void gn_enc_2_error(ei_x_buff *xbuf, char *err);

void gn_put_tuple(ei_x_buff *xbuf, int N);
void gn_put_void(ei_x_buff *xbuf);
void gn_put_pid(ei_x_buff *xbuf, erlang_pid *p);
void gn_put_boolean(ei_x_buff *xbuf, int i);
void gn_put_double(ei_x_buff *xbuf, double d);
void gn_put_longlong(ei_x_buff *xbuf, long long l);
void gn_put_ulonglong(ei_x_buff *xbuf, unsigned long long l);
void gn_put_string(ei_x_buff *xbuf, char *p);
void gn_put_object(ei_x_buff *xbuf, GObject *w);
void gn_put_enum(ei_x_buff *xbuf, char* type_name, gint enum_val);
void gn_put_flags(ei_x_buff *xbuf, char* type_name, guint flags);

gboolean gn_get_arg_pid(ei_x_buff *xbuf, char *buff, int *index, erlang_pid *p);
gboolean gn_get_arg_gboolean(ei_x_buff *xbuf, char *B, int *I, gboolean *a);
gboolean gn_get_arg_char(ei_x_buff *xbuf, char *B, int *I, char **a);
gboolean gn_get_arg_gchar(ei_x_buff *xbuf, char *B, int *I, gchar **a);
gboolean gn_get_arg_gchar_fix(ei_x_buff *xbuf, char *B, int *I, gchar *a);
gboolean gn_get_arg_gint(ei_x_buff *xbuf, char *buff, int *index, gint *a);
gboolean gn_get_arg_int(ei_x_buff *xbuf, char *buff, int *index, int *a);
gboolean gn_get_arg_gint64(ei_x_buff *xbuf, char *B, int *I, gint64 *a);
gboolean gn_get_arg_glong(ei_x_buff *xbuf, char *B, int *I, glong *a);
gboolean gn_get_arg_guint16(ei_x_buff *xbuf, char *B, int *I, guint16 *a);
gboolean gn_get_arg_guint32(ei_x_buff *xbuf, char *B, int *I, guint32 *a);
gboolean gn_get_arg_guint(ei_x_buff *xbuf, char *B, int *I, guint *a);
gboolean gn_get_arg_gfloat(ei_x_buff *xbuf, char *B, int *I, gfloat *a);
gboolean gn_get_arg_gdouble(ei_x_buff *xbuf, char *B, int *I, gdouble *a);
gboolean gn_get_arg_enum(ei_x_buff *xbuf, char *B, int *I, gchar* ec, gint *i);
gboolean gn_get_arg_flags(ei_x_buff *xbuf, char *B, int *I, 
			  gchar* type_name, gint *flags);
gboolean gn_get_arg_list(ei_x_buff *xbuf, char *B, int *I, 
			 char* type, void** go);
gboolean gn_get_arg_object(ei_x_buff *xbuf, char *B, int *I, 
			   GType type, GObject** go);
gboolean gn_get_arg_struct(ei_x_buff *xbuf, char *B, int *I, 
			   char* type, void** go);

gboolean gn_check_object(ei_x_buff *xbuf, gchar* object_name, 
			 GType type, GObject** object);
gboolean gn_check_struct(ei_x_buff *xbuf, 
			 gchar* struct_name, gchar* struct_type, void** pp);
gboolean gn_check_arity(ei_x_buff *xbuf, int a1, int a2);

gint gn_get_list(ei_x_buff *xbuf, char *buff, int *index);
gint gn_get_tuple(ei_x_buff *xbuf, char *buff, int *index);

gboolean gn_get_enum_name(const gchar *type_name, gint i, gchar *enum_name);
gboolean gn_get_enum_val(ei_x_buff *xbuf, 
			 const gchar *type_name, gchar *enum_name, gint *i);

GType gn_GType_from_name(const gchar* name);

void gn_send(ei_x_buff *xbuf);
void gn_send_signal(const char *widgetname, char *evtyp);

GtkWidget* gn_check_widget_name(char* widget_name);
