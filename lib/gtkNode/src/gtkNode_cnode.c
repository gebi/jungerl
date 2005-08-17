#include <string.h> 
#include "gtkNode.h"

#define CREATION 1
#define REMNODE argv[1]
#define REMHOST argv[2]
#define REMREG argv[3]
#define COOKIE argv[4]

static int fd;			/* fd to remote Erlang node */
static gchar* rem_regname;    /* process name on remote Erlang node */

int gn_start_cnode(char **argv) { 
  extern ei_cnode ec;
  char node_name[MAXATOMLEN] = "gtk_"; /* our node name */
  char rem_node_name[MAXATOMLEN] = "";	/* other node name */
  ei_x_buff xbuf;
  erlang_pid *self = ei_self(&ec);

  strcat(node_name,REMNODE);
  strcat(rem_node_name,REMNODE);
  strcat(rem_node_name,"@");
  strcat(rem_node_name,REMHOST);
  rem_regname = g_strdup(REMREG);
  g_print("I am %s, you are %s\n", node_name, rem_node_name);

  if ( ei_connect_init(&ec, node_name, COOKIE, CREATION) < 0 )
    g_critical("ei_connect_init");

  if ( (fd = ei_connect(&ec, rem_node_name)) < 0 )
    g_critical("ei_connect");
  
  self->num = fd;		/* bug?? in ei_reg_send_tmo */

  ei_x_new_with_version(&xbuf);
  gn_wrap_ans("handshake", &xbuf);
  ei_x_encode_empty_list(&xbuf);
  gn_send(&xbuf);
  ei_x_free(&xbuf);
  
  return fd;
} 

void gn_send(ei_x_buff *xbuf) {
  extern ei_cnode ec;
  
  if ( ei_reg_send(&ec, fd, rem_regname, xbuf->buff, xbuf->index) )
    g_critical("bad send");
}

static gboolean get_fpointer(char *cmd, ei_x_buff *xbuf, gpointer* poi) {
  extern GModule* gmod;
  
  if ( ! g_module_supported() ) g_error("gtkNode requires working gmodule");
  
  if ( ! gmod ) gmod = g_module_open(NULL,0); /* "gtkNode_gtk.so" */
  
  g_module_symbol(gmod, cmd, poi);
  if ( ! poi ) {
    g_warning("could not find '%s'.", cmd);
    gn_enc_2_error(xbuf, "no_such_function");
    ei_x_encode_atom(xbuf, cmd);
    return FALSE;
  }
  return TRUE;
}

static void call_fpointer(char *cmd, char *buff, int *index, 
			  int length, ei_x_buff *xbuf) {
  
  void (*funcp)(int, ei_x_buff *, char *, int *);
  
  if ( get_fpointer(cmd, xbuf, (gpointer *)&funcp) )
    (*funcp)(length, xbuf, buff, index);
}

static void make_reply(ei_x_buff *xbuf, char *buff, int *index) {
  
  gint arity;
  gchar cmd[MAXATOMLEN+1];
  
  if ( ! ((arity = gn_get_tuple(xbuf, buff, index)) > -1) ) 
    return;

  if ( ! gn_check_arity(xbuf,2,arity) )
    return;
  
  if ( ! gn_get_arg_gchar_fix(xbuf, buff, index, cmd) )
    return;
  
  if ( ! ((arity = gn_get_list(xbuf, buff, index)) > -1) ){
    free(cmd);
    return;
  }
  
  call_fpointer(cmd, buff, index, arity, xbuf);
}

static void remove_list_tail(char *buff, int *index) {
  int k;
  g_assert( ei_decode_list_header(buff,index,&k) == 0 );
  g_assert( k == 0 );
}

static void make_reply_list(ei_x_buff *xbuf, char *buff, int *index) {
  
  gint arity, i;

  if ( ! ((arity = gn_get_list(xbuf, buff, index)) > -1) )
    return;
  
  gn_wrap_ans("reply",xbuf);
  ei_x_encode_list_header(xbuf, arity);
  for (i = 0; i < arity; i++) {
    make_reply(xbuf, buff, index);
    remove_list_tail(buff, index);
  }
  ei_x_encode_empty_list(xbuf);
}


static void reply(erlang_msg *msg, ei_x_buff *recv_x_buf) {
  int i = 0;
  int version;
  ei_x_buff xbuf;
  
  ei_decode_version(recv_x_buf->buff, &i, &version);
  ei_x_new_with_version(&xbuf);
  make_reply_list(&xbuf, recv_x_buf->buff, &i);
  gn_send(&xbuf);
  ei_x_free(&xbuf);
}

/* called from gtk main loop when there's data on the cnode socket */
gboolean gn_handle_read(GIOChannel *source,GIOCondition cond,gpointer data){
  erlang_msg msg;
  ei_x_buff xbuf;
  
  /*   fd = g_io_channel_unix_get_fd(source); */
  ei_x_new_with_version(&xbuf);
  switch ( ei_xreceive_msg(fd, &msg, &xbuf) ){
  case ERL_TICK:
    break;			/* ignore */
  case ERL_MSG:
    switch (msg.msgtype) {
    case ERL_SEND:
    case ERL_REG_SEND:
      reply(&msg, &xbuf);
      break;
    case ERL_LINK:
    case ERL_UNLINK:
    case ERL_GROUP_LEADER:
      break;			/* ignore */
    case ERL_EXIT:
    case ERL_EXIT2:
    case ERL_NODE_LINK:
      gtk_main_quit();		/* crash and burn */
      return FALSE;
    }
    break;
  case ERL_ERROR:
    gtk_main_quit();		/* crash and burn */
    return FALSE;
  }
  ei_x_free(&xbuf);
  return TRUE;
}
