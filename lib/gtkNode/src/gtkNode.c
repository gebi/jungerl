#include <stdio.h>
#include "gtkNode.h"

GHashTable* ghash = NULL;	/* the hash table */
GModule *gmod = NULL;		/* the available functions */
GladeXML *xml = NULL;		/* the Glade design */
ei_cnode ec;			/* our cnode struct */


static void start_gtk(int argc, char **argv, int ErlFd){
  GIOChannel *channel;
  GIOCondition condition = G_IO_IN;
  GIOFunc func = gn_handle_read;
  gpointer user_data = NULL;
  
  /* initialise libraries */
  gtk_init(&argc, &argv);

  channel = g_io_channel_unix_new(ErlFd);
  g_io_add_watch(channel, condition, func, user_data); 

  gtk_main();			/* start the event loop */
}

int main(int argc, char **argv){
  int fd;

  if ( argc != 5 ){
    g_print("Usage: %s node host erlang_regname cookie\n", argv[0]);
    return 1;
  }
  
  fd = gn_start_cnode(argv);
  start_gtk(argc, argv, fd);

  return 0;
}
