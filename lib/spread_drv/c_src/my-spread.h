void my_ready_input(ErlDrvData data, ErlDrvEvent event);
int my_set_active(ErlDrvData data, int value);
int my_multigroup_multicast(mailbox mbox, service service_type, char *groups, int16 mess_type, int mess_len, char *mess);

int make_xret_group_list(struct descriptor *desc, callstate_t *c, ErlDrvTermData *msg, int *members, int *msgcount);
