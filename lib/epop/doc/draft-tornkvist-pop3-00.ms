.pl 10.0i
.po 0
.ll 7.2i
.lt 7.2i
.nr LL 7.2i
.nr LT 7.2i
.ds LF Tornkvist
.ds RF [Page %]
.ds CF
.ds LH INTERNET DRAFT
.ds RH 10 September 1998
.ds CH Notification - An extension to POP3
.hy 0
.ad 1
.in 0
Network Working Group                                       T. Tornkvist
INTERNET-DRAFT                                           SERC, Melbourne
                                                       10 September 1998

.ce Notification - An extension to the Post Office Protocol version 3

.in 0
Abstract

.in 3
This memo describes an optional extension to the Post Office Protocol 
version 3 (POP3), which introduces a possibility for a POP3 client
to be notified by a POP3 server whenever the clients maildrop is
accessed.

.in 0
Status of this Memo

.in 3
This document is an Internet-Draft.  Internet-Drafts are working
documents of the Internet Engineering Task Force (IETF), its
areas, and its working groups.  Note that other groups may also
distribute working documents as Internet-Drafts.

Internet-Drafts are draft documents valid for a maximum of six
months and may be updated, replaced, or obsoleted by other
documents at any time.  It is inappropriate to use Internet-
Drafts as reference material or to cite them other than as
"work in progress."

To view the entire list of current Internet-Drafts, please check
the "1id-abstracts.txt" listing contained in the Internet-Drafts
Shadow Directories on ftp.is.co.za (Africa), ftp.nordu.net
(Northern Europe), ftp.nis.garr.it (Southern Europe), munnari.oz.au
(Pacific Rim), ftp.ietf.org (US East Coast), or ftp.isi.edu
(US West Coast).

.in 0
Table of Contents

.in 3
.fi
1. Introduction ................................................    1
2. Operation ...................................................    2
3. NTFY sent by the client .....................................    2
4. NTFY sent by the server .....................................    3
5. Alteration to the RSET command ..............................    3
6. Alteration to the UPDATE state ..............................    4
7. Author's address ............................................    4
.ni

.in 0

1. Introduction

.in 3 
The Post Office Protocol version 3, as described in RFC 1939, is a
simple and low-cost method of enabling mail access. The host making
use of the POP3 service is referred to here as the "client", and 
the host providing the service as the "server". When a client
wants to retrieve mail from a server he establish a TCP connection
to the server and then uses various POP3 commands to access
the mail-drop. Thus every time a client wants to check for
new mail he has to poll the server. For a mail-drop
which seldom receives new mails this is obvious not economical.
It may also be a security issue since repeated attempts to
access the server are more vulnerable to interception. This
memo tries to remedy this by introducing the concept of notification.
This memo describes how a client can request that the server
notify the client when any changes have been made to the clients 
mail-drop.

.in 0 
2. Operation

.in 3
A new POP3 command named NTFY is added. It is used both by the
client and the server. When a client wants to be notified by the
server he sends a NTFY command together with a host name and port
number. When the client's mail-drop has been modified the server
establishes a connection to the specified host name and port number.
The server begin by sending a NTFY command together with the
name of the mail-drop. After this both the client and the server
enter the AUTHORIZATION state, as described in RFC 1939. Thus,
the client has to continue the exchange of commands to identify 
and authenticate itself accordingly.

As soon as a server tries to establish a TCP connection to be
used for notification, it also removes the request for notification.
It is up to the client to issue a new NTFY command if he wants
to be notified again. The client can only issue the NTFY command
in the TRANSACTION state.

A request for notification will not be activated until the
POP3 session enters the UPDATE state. During the TRANSACTION
state it is possible to cancel the request for notification by
using the RSET command.

.in 0
3. NTFY sent by the client

.in 3
This command can only be sent when the client is in the TRANSACTION
state.

.in 0
.fi
      NTFY hostname port-number

         Arguments: 
             a hostname argument that fully specifies the host
             to where the notification shall be delivered, and
             a port number to be used

         Restrictions:
             may only be given in the TRANSACTION state

         Discussion:
             The POP3 server issues a positive response if requests
             for notification can be serviced. However, the request
             for notification will not be valid until the POP3
             session enters the UPDATE state.

         Possible Responses:
             +OK 
             -ERR

         Examples:
             C: NTFY campari.rmit.edu.au 9411
             S: +OK
.ni

.in 0 
4. NTFY sent by the server

.in 3
When a client is to be notified about changes made to his mail-drop,
the server establish a TCP connection to the specified host and port
number. As soon this is done, the request for notification is erased
from the server. This is done regardless of if the connection was
successful or not.

.in 0
.fi
      NTFY name [timestamp]

         Arguments: 
             a name specifying the mail-drop in question and
             an optional timestamp if APOP authentication
             is to be used

         Restrictions:
             only one attempt to contact the client will be made

         Discussion:
             As soon the server has sent this command, both the 
             client and the server are supposed to enter the 
             AUTHORIZATION state.

         Examples:
             S: NTFY pop5432 <1896.697170952@campari.rmit.edu.au>
.ni

.in 0
5. Alteration to the RSET command

.in 3
A server which support this extension shall erase a request for
notification as a result to the RSET command.

.in 0 
6. Alteration to the UPDATE state

.in 3
A server which has received a request for notification shall not
make it valid until the UPDATE state has been entered.

.in 0 
7. Author's address

.in 3
.fi
Torbjorn Tornkvist
.br
SERC (Software Engineering Research Center)
.br
110 Victoria St, Carlton
.br
Victoria 3053
.br
AUSTRALIA
.br

Phone: +61 3 9925 4089
.br
Fax:   +61 3 9925 4094
.br
Email: tobbe@serc.rmit.edu.au
.ni
