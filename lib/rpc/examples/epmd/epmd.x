/*
** Erlang port mapper deamon (EPMD) protocol
** Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
*/

struct mapping {
    string node<>;
    string service<>;
    unsigned int vers;
    unsigned int prot;
    unsigned int port;
};

/*
 * Supported values for the "prot" field
 */
const IPPROTO_TCP = 6;      /* protocol number for TCP/IP */
const IPPROTO_UDP = 17;     /* protocol number for UDP/IP */

/*
** A list of mappings
*/
struct *pmaplist {
    mapping map;
    pmaplist next;
};

/*
** EPMD procedures (NOTE change program number when registered!!!)
*/

program EPMD_PROG {
  version EPMD_VERS {
       void EPMDPROC_NULL(void)                 = 0;
       bool EPMDPROC_SET(mapping)               = 1;
       bool EPMDPROC_UNSET(mapping)             = 2;
       unsigned int EPMDPROC_GETPORT(mapping)   = 3;
       pmaplist EPMDPROC_DUMP(void)             = 4;
  } = 1;
} = 0x20000002;

