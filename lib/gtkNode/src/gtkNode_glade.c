#include "gtkNode.h"

gboolean gn_glade_init(char *filename) {
  const char *rootnode = NULL;
  extern GladeXML *xml;
  
  if ( ! (xml = glade_xml_new(filename, rootnode, NULL)) ) 
    return FALSE;
  glade_xml_signal_autoconnect(xml);
  return TRUE;
}

void gn_sighandler(GtkWidget *widget){
  GdkEvent *gdk_event;
  GtkWidget *wid;
  GType gtyp;
  char evtype[100];
  const char *widgetname;
  
  /* instantiate the GdkEventType */
  g_assert( gn_GType_from_name("GdkEventType") ); 

  if ( ! (gdk_event = gtk_get_current_event()) )
    return;
  wid = gtk_get_event_widget(gdk_event);
  g_assert( gn_get_enum_name("GdkEventType", gdk_event->type, evtype));
  if ( ! (widgetname = glade_get_widget_name(widget)) )
    widgetname = "UNKNOWN";
  gn_send_signal(widgetname, evtype);
  gdk_event_free(gdk_event);
}

GtkWidget* gn_check_widget_name(char* widget_name) {
  extern GladeXML *xml;
  return glade_xml_get_widget(xml, widget_name);
}
