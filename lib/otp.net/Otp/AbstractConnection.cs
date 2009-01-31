/*``The contents of this file are subject to the Erlang Public License,
* Version 1.1, (the "License"); you may not use this file except in
* compliance with the License. You should have received a copy of the
* Erlang Public License along with this software. If not, it can be
* retrieved via the world wide web at http://www.erlang.org/.
* 
* Software distributed under the License is distributed on an "AS IS"
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
* the License for the specific language governing rights and limitations
* under the License.
* 
* The Initial Developer of the Original Code is Ericsson Utvecklings AB.
* Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
* AB. All Rights Reserved.''
* 
 * Converted from Java to C# by Vlad Dumitrescu (vlad_Dumitrescu@hotmail.com)
*/
using System;
using System.Diagnostics;

namespace Otp
{

    /*
    * Maintains a connection between a C# process and a remote Erlang,
    * C# or C node. The object maintains connection state and allows
    * data to be sent to and received from the peer.
    *
    * <p> This abstract class provides the neccesary methods to maintain
    * the actual connection and encode the messages and headers in the
    * proper format according to the Erlang distribution protocol.
    * Subclasses can use these methods to provide a more or less
    * transparent communication channel as desired. </p>
    *
    * <p> Note that no receive methods are provided. Subclasses must
    * provide methods for message delivery, and may implement their own
    * receive methods. <p>
    *
    * <p> If an exception occurs in any of the methods in this class, the
    * connection will be closed and must be reopened in order to resume
    * communication with the peer. This will be indicated to the subclass
    * by passing the exception to its delivery() method. </p>
    * 
    * <p> The System property OtpConnection.trace can be used to change
    * the initial trace level setting for all connections. Normally the
    * initial trace level is 0 and connections are not traced unless
    * {@link #setTraceLevel setTraceLevel()} is used to change the
    * setting for a particular connection. OtpConnection.trace can be
    * used to turn on tracing by default for all connections. </p>
    **/
    public abstract class AbstractConnection
    {

        static AbstractConnection()
        {
            random = new System.Random();
        }
        protected internal const int headerLen = 2048; // more than enough
        
        protected internal static readonly byte passThrough = 0x70;
        protected internal static readonly byte version = 0x83;
        
        // MD5 challenge messsage tags
        protected internal const int ChallengeReply = 'r';
        protected internal const int ChallengeAck = 'a';
        protected internal const int ChallengeStatus = 's';

        private bool done = false;

        protected internal bool connected = false; // connection status
        protected internal System.Net.Sockets.TcpClient socket; // communication channel
        protected internal OtpPeer peer; // who are we connected to
        protected internal OtpLocalNode self; // this nodes id
        internal System.String name; // local name of this connection

        protected internal bool cookieOk = false; // already checked the cookie for this connection
        protected internal bool sendCookie = true; // Send cookies in messages?

        protected internal System.Threading.Thread thread;

        // tracelevel constants

        /*
        * <p> Set the trace level for this connection. Normally tracing is
        * off by default unless System property OtpConnection.trace was
        * set. </p>
        *
        * <p> The following levels are valid: 0 turns off tracing
        * completely, 1 shows ordinary send and receive messages, 2 shows
        * control messages such as link and unlink, 3 shows handshaking at
        * connection setup, and 4 shows communication with Epmd. Each level
        * includes the information shown by the lower ones. </p>
        *
        **/
        public static OtpTrace.Type traceLevel = OtpTrace.traceLevel;

        protected internal static System.Random random = null;

        /*
        * Accept an incoming connection from a remote node. Used by {@link
        * OtpSelf#accept() OtpSelf.accept()} to create a connection
        * based on data received when handshaking with the peer node, when
        * the remote node is the connection intitiator.
        *
        * @exception C#.io.IOException if it was not possible to connect to the peer.
        * @exception OtpAuthException if handshake resulted in an authentication error
        */
        protected internal AbstractConnection(OtpLocalNode self, System.Net.Sockets.TcpClient s)
        {
            this.self = self;
            this.peer = new OtpPeer();
            this.socket = s;

            this.socket.NoDelay = true;
            // Use keepalive timer
            this.socket.Client.SetSocketOption(
                System.Net.Sockets.SocketOptionLevel.Socket,
                System.Net.Sockets.SocketOptionName.KeepAlive, true);
            // Close socket gracefully
            this.socket.Client.SetSocketOption(
                System.Net.Sockets.SocketOptionLevel.Socket,
                System.Net.Sockets.SocketOptionName.DontLinger, true);

            //this.socket.ReceiveTimeout = 5000;

            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("<- ACCEPT FROM " + 
                    System.Net.IPAddress.Parse(s.Client.RemoteEndPoint.ToString()).ToString() + ":" +
                    (s.Client.RemoteEndPoint as System.Net.IPEndPoint).Port.ToString());
            }
            
            // get his info
            recvName(this.peer);
            
            // now find highest common dist value
            if ((peer._proto != self._proto) || (self._distHigh < peer._distLow) || (self._distLow > peer._distHigh))
            {
                close();
                throw new System.IO.IOException("No common protocol found - cannot accept connection");
            }
            // highest common version: min(peer.distHigh, self.distHigh)
            peer.distChoose = (peer._distHigh > self._distHigh?self._distHigh:peer._distHigh);
            
            doAccept();
            this.name = peer.node();
        }
        
        /*
        * Intiate and open a connection to a remote node.
        *
        * @exception C#.io.IOException if it was not possible to connect to the peer.
        * @exception OtpAuthException if handshake resulted in an authentication error.
        */
        protected internal AbstractConnection(OtpLocalNode self, OtpPeer other)
        {
            this.peer = other;
            this.self = self;
            this.socket = null;
            int port;
            
            //this.IsBackground = true;
            
            // now get a connection between the two...
            port = OtpEpmd.lookupPort(peer);
            
            // now find highest common dist value
            if ((peer._proto != self._proto) || (self._distHigh < peer._distLow) || (self._distLow > peer._distHigh))
            {
                throw new System.IO.IOException("No common protocol found - cannot connect");
            }
            
            // highest common version: min(peer.distHigh, self.distHigh)
            peer.distChoose = (peer._distHigh > self._distHigh ? self._distHigh : peer._distHigh);
            
            doConnect(port);
            
            this.name = peer.node();
            this.connected = true;
        }
        
        /*
        * Deliver communication exceptions to the recipient.
        **/
        public abstract void  deliver(System.Exception e);
        
        /*
        * Deliver messages to the recipient.
        **/
        public abstract void  deliver(OtpMsg msg);
        
        /*
        * Send a pre-encoded message to a named process on a remote node.
        *
        * @param dest the name of the remote process.
        * @param payload the encoded message to send.
        *
        * @exception C#.io.IOException if the connection is not active or
        * a communication error occurs.
        **/
        protected internal virtual void  sendBuf(Erlang.Pid from, System.String dest, OtpOutputStream payload)
        {
            if (!connected)
            {
                throw new System.IO.IOException("Not connected");
            }
            OtpOutputStream header = new OtpOutputStream(headerLen);
            
            // preamble: 4 byte length + "passthrough" tag + version
            header.write4BE(0); // reserve space for length
            header.write1(passThrough);
            header.write1(version);
            
            // header info
            header.write_tuple_head(4);
            header.write_long((long)OtpMsg.Tag.regSendTag);
            header.write_any(from);
            if (sendCookie)
                header.write_atom(self.cookie());
            else
                header.write_atom("");
            header.write_atom(dest);
            
            // version for payload
            header.write1(version);
            
            // fix up length in preamble
            header.poke4BE(0, header.count() + payload.count() - 4);
            
            do_send(header, payload);
        }
        
        /*
        * Send a pre-encoded message to a process on a remote node.
        *
        * @param dest the Erlang PID of the remote process.
        * @param msg the encoded message to send.
        *
        * @exception C#.io.IOException if the connection is not active
        * or a communication error occurs.
        **/
        protected internal virtual void  sendBuf(Erlang.Pid from, Erlang.Pid dest, OtpOutputStream payload)
        {
            if (!connected)
            {
                throw new System.IO.IOException("Not connected");
            }
            OtpOutputStream header = new OtpOutputStream(headerLen);
            
            // preamble: 4 byte length + "passthrough" tag + version
            header.write4BE(0); // reserve space for length
            header.write1(passThrough);
            header.write1(version);
            
            // header info
            header.write_tuple_head(3);
            header.write_long((long)OtpMsg.Tag.sendTag);
            if (sendCookie)
                header.write_atom(self.cookie());
            else
                header.write_atom("");
            header.write_any(dest);
            
            // version for payload
            header.write1(version);
            
            // fix up length in preamble
            header.poke4BE(0, header.count() + payload.count() - 4);
            
            do_send(header, payload);
        }

        /*Send an auth error to peer because he sent a bad cookie.
        * The auth error uses his cookie (not revealing ours).
        * This is just like send_reg otherwise
        */
        private void  cookieError(OtpLocalNode local, Erlang.Atom cookie)
        {
            try
            {
                OtpOutputStream header = new OtpOutputStream(headerLen);
                
                // preamble: 4 byte length + "passthrough" tag + version
                header.write4BE(0); // reserve space for length
                header.write1(passThrough);
                header.write1(version);
                
                header.write_tuple_head(4);
                header.write_long((long)OtpMsg.Tag.regSendTag);
                header.write_any(local.createPid()); // disposable pid
                header.write_atom(cookie.atomValue()); // important: his cookie, not mine...
                header.write_atom("auth");
                
                // version for payload
                header.write1(version);
                
                // the payload
                
                // the no_auth message (copied from Erlang) Don't change this (Erlang will crash)
                // {$gen_cast, {print, "~n** Unauthorized cookie ~w **~n", [foo@aule]}}
                Erlang.Object[] msg = new Erlang.Object[2];
                Erlang.Object[] msgbody = new Erlang.Object[3];
                
                msgbody[0] = new Erlang.Atom("print");
                msgbody[1] = new Erlang.String("~n** Bad cookie sent to " + local + " **~n");
                // Erlang will crash and burn if there is no third argument here...
                msgbody[2] = new Erlang.List(); // empty list
                
                msg[0] = new Erlang.Atom("$gen_cast");
                msg[1] = new Erlang.Tuple(msgbody);
                
                OtpOutputStream payload = new OtpOutputStream(new Erlang.Tuple(msg));
                
                // fix up length in preamble
                header.poke4BE(0, header.count() + payload.count() - 4);
                
                try
                {
                    do_send(header, payload);
                }
                catch (System.IO.IOException)
                {
                } // ignore
            }
            finally
            {
                close();
                throw new OtpAuthException("Remote cookie not authorized: " + cookie.atomValue());
            }
        }
        
        // link to pid
        
        /*
        * Create a link between the local node and the specified process on
        * the remote node. If the link is still active when the remote
        * process terminates, an exit signal will be sent to this
        * connection. Use {@link #sendUnlink unlink()} to remove the link.
        *
        * @param dest the Erlang PID of the remote process.
        *
        * @exception C#.io.IOException if the connection is not active
        * or a communication error occurs.
        **/
        protected internal virtual void  sendLink(Erlang.Pid from, Erlang.Pid dest)
        {
            if (!connected)
            {
                throw new System.IO.IOException("Not connected");
            }
            OtpOutputStream header = new OtpOutputStream(headerLen);
            
            // preamble: 4 byte length + "passthrough" tag
            header.write4BE(0); // reserve space for length
            header.write1(passThrough);
            header.write1(version);
            
            // header
            header.write_tuple_head(3);
            header.write_long((long)OtpMsg.Tag.linkTag);
            header.write_any(from);
            header.write_any(dest);
            
            // fix up length in preamble
            header.poke4BE(0, header.count() - 4);
            
            do_send(header);
        }
        
        /*
        * Remove a link between the local node and the specified process on
        * the remote node. This method deactivates links created with
        * {@link #sendLink link()}.
        *
        * @param dest the Erlang PID of the remote process.
        *
        * @exception C#.io.IOException if the connection is not active or
        * a communication error occurs.
        **/
        protected internal virtual void  sendUnlink(Erlang.Pid from, Erlang.Pid dest)
        {
            if (!connected)
            {
                throw new System.IO.IOException("Not connected");
            }
            OtpOutputStream header = new OtpOutputStream(headerLen);
            
            // preamble: 4 byte length + "passthrough" tag
            header.write4BE(0); // reserve space for length
            header.write1(passThrough);
            header.write1(version);
            
            // header
            header.write_tuple_head(3);
            header.write_long((long)OtpMsg.Tag.unlinkTag);
            header.write_any(from);
            header.write_any(dest);
            
            // fix up length in preamble
            header.poke4BE(0, header.count() - 4);
            
            do_send(header);
        }
        
        /*used internally when "processes" terminate */
        protected internal virtual void  sendExit(Erlang.Pid from, Erlang.Pid dest, System.String reason)
        {
            sendExit((int)OtpMsg.Tag.exitTag, from, dest, reason);
        }
        
        /*
        * Send an exit signal to a remote process.
        * 
        * @param dest the Erlang PID of the remote process.
        * @param reason a string describing the exit reason.
        * 
        * @exception C#.io.IOException if the connection is not active or
        * a communication error occurs.
        **/
        protected internal virtual void  sendExit2(Erlang.Pid from, Erlang.Pid dest, System.String reason)
        {
            sendExit((int)OtpMsg.Tag.exit2Tag, from, dest, reason);
        }

        private void  sendExit(int tag, Erlang.Pid from, Erlang.Pid dest, System.String reason)
        {
            if (!connected)
            {
                throw new System.IO.IOException("Not connected");
            }
            OtpOutputStream header = new OtpOutputStream(headerLen);
            
            // preamble: 4 byte length + "passthrough" tag
            header.write4BE(0); // reserve space for length
            header.write1(passThrough);
            header.write1(version);
            
            // header
            header.write_tuple_head(4);
            header.write_long(tag);
            header.write_any(from);
            header.write_any(dest);
            header.write_string(reason);
            
            // fix up length in preamble
            header.poke4BE(0, header.count() - 4);
            
            do_send(header);
        }
        
        public virtual void Start()
        {
            if (!connected)
            {
                deliver(new System.IO.IOException("Not connected"));
                return ;
            }

            byte[] lbuf = new byte[4];
            OtpInputStream ibuf;
            Erlang.Object traceobj;
            int len;
            byte[] tock = new byte[]{0, 0, 0, 0};
            
            try
            {
                while (!done)
                {
                    // don't return until we get a real message
                    // or a failure of some kind (e.g. EXIT)
                    // read length and read buffer must be atomic!
                    do
                    {
                        // read 4 bytes - get length of incoming packet
                        // socket.getInputStream().read(lbuf);
                        int n;
                        if ((n = readSock(socket, lbuf)) < lbuf.Length)
                            throw new System.Exception("Read " + n + " out of " + lbuf.Length + " bytes!");

                        ibuf = new OtpInputStream(lbuf);
                        len = ibuf.read4BE();
                        
                        //  received tick? send tock!
                        if (len == 0)
                            lock(this)
                            {
                                System.Byte[] temp_bytearray;
                                temp_bytearray = tock;
                                if (socket != null)
                                    ((System.IO.Stream) socket.GetStream()).Write(temp_bytearray, 0, temp_bytearray.Length);
                            }
                        
                    }
                    while (len == 0); // tick_loop
                    
                    // got a real message (maybe) - read len bytes
                    byte[] tmpbuf = new byte[len];
                    // i = socket.getInputStream().read(tmpbuf);
                    int m = readSock(socket, tmpbuf);
                    if (m < len)
                        throw new System.Exception("Read " + m + " out of " + len + " bytes!");

                    ibuf = new OtpInputStream(tmpbuf);
                    
                    if (ibuf.read1() != passThrough)
                    {
                        goto receive_loop_brk;
                    }
                    
                    // got a real message (really)
                    Erlang.Atom reason = null;
                    Erlang.Atom cookie = null;
                    Erlang.Object tmp = null;
                    Erlang.Tuple head = null;
                    Erlang.Atom toName;
                    Erlang.Pid to;
                    Erlang.Pid from;
                    Erlang.Ref eref;
                    
                    // decode the header
                    tmp = ibuf.read_any();
                    if (!(tmp is Erlang.Tuple))
                    {
                        goto receive_loop_brk;
                    }
                    
                    head = (Erlang.Tuple) tmp;
                    if (!(head.elementAt(0) is Erlang.Long))
                    {
                        goto receive_loop_brk;
                    }
                    
                    // lets see what kind of message this is
                    OtpMsg.Tag tag = (OtpMsg.Tag)((Erlang.Long)(head.elementAt(0))).longValue();
                    
                    switch (tag)
                    {
                        case OtpMsg.Tag.sendTag:
                        case OtpMsg.Tag.sendTTTag: 
                            // { SEND, Cookie, ToPid, TraceToken }
                            if (!cookieOk)
                            {
                                // we only check this once, he can send us bad cookies later if he likes
                                if (!(head.elementAt(1) is Erlang.Atom))
                                {
                                    goto receive_loop_brk;
                                }
                                cookie = (Erlang.Atom) head.elementAt(1);
                                if (sendCookie)
                                {
                                    if (!cookie.atomValue().Equals(self.cookie()))
                                    {
                                        cookieError(self, cookie);
                                    }
                                }
                                else
                                {
                                    if (!cookie.atomValue().Equals(""))
                                    {
                                        cookieError(self, cookie);
                                    }
                                }
                                cookieOk = true;
                            }

                            if (traceLevel >= OtpTrace.Type.sendThreshold)
                            {
                                OtpTrace.TraceEvent("<- " + headerType(head) + " " + head.ToString());

                                /*show received payload too */
                                long mark = ibuf.Position;
                                traceobj = ibuf.read_any();

                                if (traceobj != null)
                                    OtpTrace.TraceEvent("   " + traceobj.ToString());
                                else
                                    OtpTrace.TraceEvent("   (null)");
                                ibuf.Seek(mark, System.IO.SeekOrigin.Begin);
                            }

                            to = (Erlang.Pid) (head.elementAt(2));

                            deliver(new OtpMsg(to, ibuf));
                            break;

                        case OtpMsg.Tag.regSendTag:
                        case OtpMsg.Tag.regSendTTTag:
                            // { REG_SEND, FromPid, Cookie, ToName, TraceToken }
                            if (!cookieOk)
                            {
                                // we only check this once, he can send us bad cookies later if he likes
                                if (!(head.elementAt(2) is Erlang.Atom))
                                {
                                    goto receive_loop_brk;
                                }
                                cookie = (Erlang.Atom) head.elementAt(2);
                                if (sendCookie)
                                {
                                    if (!cookie.atomValue().Equals(self.cookie()))
                                    {
                                        cookieError(self, cookie);
                                    }
                                }
                                else
                                {
                                    if (!cookie.atomValue().Equals(""))
                                    {
                                        cookieError(self, cookie);
                                    }
                                }
                                cookieOk = true;
                            }

                            if (traceLevel >= OtpTrace.Type.sendThreshold)
                            {
                                OtpTrace.TraceEvent("<- " + headerType(head) + " " + head.ToString());
                                
                                /*show received payload too */
                                long mark = ibuf.Position;
                                traceobj = ibuf.read_any();

                                if (traceobj != null)
                                    OtpTrace.TraceEvent("   " + traceobj.ToString());
                                else
                                    OtpTrace.TraceEvent("   (null)");
                                ibuf.Seek(mark, System.IO.SeekOrigin.Begin);
                            }

                            from = (Erlang.Pid) (head.elementAt(1));
                            toName = (Erlang.Atom) (head.elementAt(3));

                            deliver(new OtpMsg(from, toName.atomValue(), ibuf));
                            break;

                        case OtpMsg.Tag.exitTag:
                        case OtpMsg.Tag.exit2Tag:
                            // { EXIT2, FromPid, ToPid, Reason }
                            if (!(head.elementAt(3) is Erlang.Atom))
                            {
                                goto receive_loop_brk;
                            }
                            if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                            {
                                OtpTrace.TraceEvent("<- " + headerType(head) + " " + head.ToString());
                            }
                            
                            from = (Erlang.Pid) (head.elementAt(1));
                            to = (Erlang.Pid) (head.elementAt(2));
                            reason = (Erlang.Atom) head.elementAt(3);
                            
                            deliver(new OtpMsg(tag, from, to, reason));
                            break;

                        case OtpMsg.Tag.exitTTTag:
                        case OtpMsg.Tag.exit2TTTag:
                            // { EXIT2, FromPid, ToPid, TraceToken, Reason }
                            // as above, but bifferent element number
                            if (!(head.elementAt(4) is Erlang.Atom))
                            {
                                goto receive_loop_brk;
                            }
                            if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                            {
                                OtpTrace.TraceEvent("<- " + headerType(head) + " " + head.ToString());
                            }

                            from = (Erlang.Pid) (head.elementAt(1));
                            to = (Erlang.Pid) (head.elementAt(2));
                            reason = (Erlang.Atom) head.elementAt(4);

                            deliver(new OtpMsg(tag, from, to, reason));
                            break;

                        case OtpMsg.Tag.linkTag:
                        case OtpMsg.Tag.unlinkTag:
                            // { UNLINK, FromPid, ToPid}
                            if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                            {
                                OtpTrace.TraceEvent("<- " + headerType(head) + " " + head.ToString());
                            }
                            
                            from = (Erlang.Pid) (head.elementAt(1));
                            to = (Erlang.Pid) (head.elementAt(2));

                            deliver(new OtpMsg(tag, from, to));
                            break;

                        // absolutely no idea what to do with these, so we ignore them...
                        case OtpMsg.Tag.groupLeaderTag:
                        case OtpMsg.Tag.nodeLinkTag:
                            // { NODELINK }
                            // (just show trace)
                            if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                            {
                                OtpTrace.TraceEvent("<- " + headerType(head) + " " + head.ToString());
                            }
                            break;

                        case OtpMsg.Tag.monitorPTag:
                            // {MONITOR_P, FromPid, ToProc, Ref}
                        case OtpMsg.Tag.demonitorPTag:
                            // {DEMONITOR_P, FromPid, ToProc, Ref}
                            if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                            {
                                OtpTrace.TraceEvent("<- " + headerType(head) + " " + head.ToString());
                            }
                            from = (Erlang.Pid)(head.elementAt(1));
                            to = (Erlang.Pid)(head.elementAt(2));
                            eref = (Erlang.Ref)(head.elementAt(3));
                            deliver(new OtpMsg(tag, from, to, eref));
                            break;

                        case OtpMsg.Tag.monitorPexitTag:
                            // {MONITOR_P_EXIT, FromPid, ToProc, Ref, Reason}
                            if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                            {
                                OtpTrace.TraceEvent("<- " + headerType(head) + " " + head.ToString());
                            }
                            from = (Erlang.Pid)(head.elementAt(1));
                            to = (Erlang.Pid)(head.elementAt(2));
                            eref = (Erlang.Ref)(head.elementAt(3));

                            deliver(new OtpMsg(tag, from, to, eref, reason));
                            break;

                        default:
                            // garbage?
                            if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                            {
                                OtpTrace.TraceEvent("<- Unknown tag " + headerType(head) + " " + head.ToString());
                            }
                            goto receive_loop_brk;
                    }
                }
receive_loop_brk: ;
                 // end receive_loop

                // this section reachable only with break
                // we have received garbage from peer
                deliver(new Erlang.Exit("Remote is sending garbage"));

            }
            catch (OtpAuthException e)
            {
                deliver(e);
            }
            catch (Erlang.DecodeException e)
            {
                OtpTrace.TraceEvent(e.ToString());
                deliver(new Erlang.Exit("Remote is sending garbage: " + e.ToString()));
            }
            catch (System.Exception e)
            {
                deliver(new Erlang.Exit("Remote has closed connection: " + e.ToString()));
            }
            finally
            {
                close();
                OtpTrace.TraceEvent("exit connection "+System.Threading.Thread.CurrentThread.Name);
            }
        }

        /*
        * Close the connection to the remote node.
        **/
        public virtual void  close()
        {
            done = true;
            connected = false;
            lock(this)
            {
                try
                {
                    if (socket != null)
                    {
                        if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                        {
                            OtpTrace.TraceEvent("-> CLOSE");
                        }
                        socket.Close();
                        //thread.Interrupt();
                    }
                }
                catch (System.Net.Sockets.SocketException)
                {
                    /*ignore socket close errors */
                }
                finally
                {
                    socket = null;
                }
            }
        }

        ~AbstractConnection()
        {
            close();
        }

        /*
        * Determine if the connection is still alive. Note that this method
        * only reports the status of the connection, and that it is
        * possible that there are unread messages waiting in the receive
        * queue.
        *
        * @return true if the connection is alive.
        **/
        public virtual bool isConnected()
        {
            return connected;
        }

        /*
        * Send an RPC request to the remote Erlang node. This convenience
        * function creates the following message and sends it to 'rex' on
        * the remote node:
        * 
        * <pre>
        * { self, { call, Mod, Fun, Args, user }}
        * </pre>
        *
        * <p> Note that this method has unpredicatble results if the remote
        * node is not an Erlang node. </p>
        *
        * @param mod the name of the Erlang module containing the function to be called.
        * @param fun the name of the function to call.
        * @param args a list of Erlang terms, to be used as arguments to the function.
        *
        * @exception C#.io.IOException if the connection is not active
        * or a communication error occurs.
        **/
        public virtual void sendRPC(Erlang.Pid from, System.String mod, System.String fun, Erlang.List args)
        {
            Erlang.Object rpc = encodeRPC(from, mod, fun, args, new Erlang.Atom("user"));
            sendBuf(from, "rex", new OtpOutputStream(rpc));
        }

        internal static Erlang.Tuple encodeRPC(
            Erlang.Pid from, string mod, string fun, Erlang.List args, Erlang.Object gleader)
        {
            return encodeRPC(from, new Erlang.Atom(mod), new Erlang.Atom(fun), args, gleader);
        }

        internal static Erlang.Tuple encodeRPC(
            Erlang.Pid from, Erlang.Atom mod, Erlang.Atom fun, Erlang.List args, Erlang.Object gleader)
        {
            /*{self, { call, Mod, Fun, Args, user}} */
            return new Erlang.Tuple(
                from,
                new Erlang.Tuple(new Erlang.Atom("call"), mod, fun, args, gleader)
            );
        }

        internal static Erlang.Object decodeRPC(Erlang.Object msg)
        {
            if (msg is Erlang.Tuple)
            {
                Erlang.Tuple t = (Erlang.Tuple)msg;
                if (t.arity() == 2)
                {
                    Erlang.Atom rex = t[0] as Erlang.Atom;
                    if (rex != null && rex.atomValue() == "rex")
                        return t[1];
                    // obs: second element
                }
            }
            return null;
        }

        internal static string decodeIO(Erlang.Object msg)
        {
            if (msg is Erlang.Tuple)
            {
                Erlang.Tuple t = (Erlang.Tuple)msg;
                if (t.arity() == 4)
                {
                    Erlang.Atom  ios = t[0] as Erlang.Atom;
                    Erlang.Tuple data = t[3] as Erlang.Tuple;
                    if (ios != null && ios.atomValue() == "io_request" && data != null)
                    {
                        if ((data[0] as Erlang.Atom).atomValue() == "put_chars")
                        {
                            if (data[1] is Erlang.String)
                                return (data[1] as Erlang.String).stringValue();
                            else if (data[1] is Erlang.Binary)
                            {
                                byte[] bin = (data[1] as Erlang.Binary).binaryValue();
                                char[] s = new char[bin.Length];
                                bin.CopyTo(s, 0);
                                return new string(s);
                            }
                            else
                                return data[1].ToString();
                        }
                    }
                }
            }
            return null;
        }

        // used by  send and send_reg (message types with payload)
        protected internal virtual void  do_send(OtpOutputStream header, OtpOutputStream payload)
        {
            lock(this)
            {
                try
                {
                    if (traceLevel >= OtpTrace.Type.sendThreshold)
                    {
                        // Need to decode header and output buffer to show trace message!
                        // First make OtpInputStream, then decode.
                        try
                        {
                            Erlang.Object h = (header.getOtpInputStream(5)).read_any();
                            OtpTrace.TraceEvent("-> " + headerType(h) + " " + h.ToString());
                            
                            Erlang.Object o = (payload.getOtpInputStream(0)).read_any();
                            OtpTrace.TraceEvent("   " + o.ToString());
                            o = null;
                        }
                        catch (Erlang.DecodeException e)
                        {
                            OtpTrace.TraceEvent("   " + "can't decode output buffer:" + e);
                        }
                    }
                    
                    header.writeTo((System.IO.Stream) socket.GetStream());
                    payload.writeTo((System.IO.Stream) socket.GetStream());
                }
                catch (System.Net.Sockets.SocketException e)
                {
                    close();
                    throw e;
                }
                catch (System.IO.IOException e)
                {
                    close();
                    throw e;
                }
            }
        }
        
        // used by the other message types
        protected internal virtual void  do_send(OtpOutputStream header)
        {
            lock(this)
            {
                try
                {
                    if (traceLevel >= OtpTrace.Type.ctrlThreshold)
                    {
                        try
                        {
                            Erlang.Object h = (header.getOtpInputStream(5)).read_any();
                            OtpTrace.TraceEvent("-> " + headerType(h) + " " + h);
                        }
                        catch (Erlang.DecodeException e)
                        {
                            OtpTrace.TraceEvent("   " + "can't decode output buffer: " + e);
                        }
                    }
                    header.writeTo((System.IO.Stream) socket.GetStream());
                }
                catch (System.Net.Sockets.SocketException e)
                {
                    close();
                    throw e;
                }
                catch (System.IO.IOException e)
                {
                    close();
                    throw e;
                }
            }
        }

        protected internal virtual System.String headerType(Erlang.Object h)
        {
            OtpMsg.Tag tag = OtpMsg.Tag.undefined;

            if (h is Erlang.Tuple)
            {
                tag = (OtpMsg.Tag) (((Erlang.Long) (((Erlang.Tuple) h).elementAt(0))).longValue());
            }

            switch (tag)
            {
                case OtpMsg.Tag.linkTag:
                    return "LINK";

                case OtpMsg.Tag.sendTag:
                    return "SEND";

                case OtpMsg.Tag.exitTag:
                    return "EXIT";

                case OtpMsg.Tag.unlinkTag:
                    return "UNLINK";

                case OtpMsg.Tag.nodeLinkTag:
                    return "NODELINK";

                case OtpMsg.Tag.regSendTag:
                    return "REG_SEND";

                case OtpMsg.Tag.groupLeaderTag:
                    return "GROUP_LEADER";

                case OtpMsg.Tag.exit2Tag:
                    return "EXIT2";

                case OtpMsg.Tag.sendTTTag:
                    return "SEND_TT";

                case OtpMsg.Tag.exitTTTag:
                    return "EXIT_TT";

                case OtpMsg.Tag.regSendTTTag:
                    return "REG_SEND_TT";

                case OtpMsg.Tag.exit2TTTag:
                    return "EXIT2_TT";

                case OtpMsg.Tag.monitorPTag:
                    return "MONITOR_P";

                case OtpMsg.Tag.demonitorPTag:
                    return "DEMONITOR_P";

                case OtpMsg.Tag.monitorPexitTag:
                    return "MONITOR_P_EXIT";
            }
            return "(unknown type)";
        }
        
        /*this method now throws exception if we don't get full read */
        protected internal virtual int readSock(System.Net.Sockets.TcpClient s, byte[] b)
        {
            int got = 0;
            int len = (int) (b.Length);
            int i;
            System.IO.Stream is_Renamed = null;
            
            lock (this)
            {
                if (s == null)
                {
                    throw new System.IO.IOException("expected " + len + " bytes, socket was closed");
                }
                is_Renamed = (System.IO.Stream)s.GetStream();
            }

            while (got < len && is_Renamed.CanRead)
            {
                i = is_Renamed.Read(b, got, len - got);

                if (i < 0)
                {
                    throw new System.IO.IOException("Expected " + len + " bytes, got EOF after " + got + " bytes");
                }
                else if (i == 0)
                {
                    throw new System.IO.IOException("Remote connection closed");
                }
                else
                    got += i;
            }
            return got;
        }

        protected internal virtual void  doAccept()
        {
            try
            {
                sendStatus("ok");
                int our_challenge = genChallenge();
                sendChallenge(peer.distChoose, self.flags, our_challenge);
                int her_challenge = recvChallengeReply(our_challenge);
                byte[] our_digest = genDigest(her_challenge, self.cookie());
                sendChallengeAck(our_digest);
                connected = true;
                cookieOk = true;
                sendCookie = false;
            }
            catch (System.Net.Sockets.SocketException ie)
            {
                close();
                throw new System.IO.IOException(ie.ToString());
            }
            catch (OtpAuthException ae)
            {
                close();
                throw ae;
            }
            catch (System.Exception)
            {
                System.String nn = peer.node();
                close();
                throw new System.IO.IOException("Error accepting connection from " + nn);
            }
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
                OtpTrace.TraceEvent("<- MD5 ACCEPTED " + peer.host());
        }
        
        protected internal virtual void  doConnect(int port)
        {
            try
            {
                socket = new System.Net.Sockets.TcpClient(peer.host(), port);
                socket.NoDelay = true;
                
                Debug.WriteLine("-> MD5 CONNECT TO " + peer.host() + ": " + port);
                
                if (traceLevel >= OtpTrace.Type.handshakeThreshold)
                    OtpTrace.TraceEvent("-> MD5 CONNECT TO " + peer.host() + ":" + port);
                sendName(peer.distChoose, self.flags);
                recvStatus();
                int her_challenge = recvChallenge();
                byte[] our_digest = genDigest(her_challenge, self.cookie());
                int our_challenge = genChallenge();
                sendChallengeReply(our_challenge, our_digest);
                recvChallengeAck(our_challenge);
                cookieOk = true;
                sendCookie = false;
            }
            catch (OtpAuthException ae)
            {
                close();
                throw ae;
            }
            catch (System.Net.Sockets.SocketException e)
            {
                close();
                throw new System.IO.IOException("Cannot connect to peer node: " + e.ToString());
            }
        }
        
        // This is nooo good as a challenge,
        // XXX fix me.
        static protected internal int genChallenge()
        {
            return random.Next();
        }
        
        // Used to debug print a message digest
        internal static System.String hex0(byte x)
        {
            char[] tab = new char[]{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
            int uint_Renamed;
            if (x < 0)
            {
                uint_Renamed = x & 0x7F;
                uint_Renamed |= (1 << 7);
            }
            else
            {
                uint_Renamed = (int) x;
            }
            return "" + tab[SupportClass.URShift(uint_Renamed, 4)] + tab[uint_Renamed & 0xF];
        }
        
        internal static System.String hex(byte[] b)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            try
            {
                int i;
                 for (i = 0; i < b.Length; ++i)
                    sb.Append(hex0(b[i]));
            }
            catch (System.Exception)
            {
                // Debug function, ignore errors.
            }
            return sb.ToString();
            
        }
        
        protected internal virtual byte[] genDigest(int challenge, System.String cookie)
        {
            int i;
            long ch2;
            
            if (challenge < 0)
            {
                ch2 = 1L << 31;
                ch2 |= (long) (challenge & 0x7FFFFFFFL);
            }
            else
            {
                ch2 = (long) challenge;
            }
            System.Security.Cryptography.MD5CryptoServiceProvider context 
                = new System.Security.Cryptography.MD5CryptoServiceProvider();

            byte[] tmp = context.ComputeHash(System.Text.Encoding.UTF8.GetBytes(cookie+System.Convert.ToString(ch2)));
            byte[] res = new byte[tmp.Length];
            for (i = 0; i < tmp.Length; ++i)
            {
                res[i] = (byte) (tmp[i] & 0xFF);
            }
            return res;
        }
        
        protected internal virtual void  sendName(int dist, int flags)
        {
            
            OtpOutputStream obuf = new OtpOutputStream();
            System.String str = self.node();
            obuf.write2BE(str.Length + 7); // 7 bytes + nodename
            obuf.write1(AbstractNode.NTYPE_R6);
            obuf.write2BE(dist);
            obuf.write4BE(flags);
            //UPGRADE_NOTE: This code will be optimized in the future;
            byte[] tmpBytes;
            int i;
            string tmpStr;
            tmpStr = str;
            tmpBytes = new byte[tmpStr.Length];
            i = 0;
            while (i < tmpStr.Length)
            {
                tmpBytes[i] = (byte) tmpStr[i];
                i++;
            }
            obuf.write(tmpBytes);
            
            obuf.writeTo((System.IO.Stream) socket.GetStream());
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("-> " + "HANDSHAKE sendName" + " flags=" + flags + " dist=" + dist + " local=" + self);
            }
        }
        
        protected internal virtual void  sendChallenge(int dist, int flags, int challenge)
        {
            
            OtpOutputStream obuf = new OtpOutputStream();
            System.String str = self.node();
            obuf.write2BE(str.Length + 11); // 11 bytes + nodename
            obuf.write1(AbstractNode.NTYPE_R6);
            obuf.write2BE(dist);
            obuf.write4BE(flags);
            obuf.write4BE(challenge);
            //UPGRADE_NOTE: This code will be optimized in the future;
            byte[] tmpBytes;
            int i;
            string tmpStr;
            tmpStr = str;
            tmpBytes = new byte[tmpStr.Length];
            i = 0;
            while (i < tmpStr.Length)
            {
                tmpBytes[i] = (byte) tmpStr[i];
                i++;
            }
            obuf.write(tmpBytes);
            
            obuf.writeTo((System.IO.Stream) socket.GetStream());
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("-> " + "HANDSHAKE sendChallenge" + " flags=" + flags + " dist=" + dist + " challenge=" + challenge + " local=" + self);
            }
        }
        
        protected internal virtual byte[] read2BytePackage()
        {
            
            byte[] lbuf = new byte[2];
            byte[] tmpbuf;
            
            readSock(socket, lbuf);
            OtpInputStream ibuf = new OtpInputStream(lbuf);
            int len = ibuf.read2BE();
            tmpbuf = new byte[len];
            readSock(socket, tmpbuf);
            return tmpbuf;
        }
        
        
        protected internal virtual void  recvName(OtpPeer peer)
        {
            
            System.String hisname = "";
            
            try
            {
                byte[] tmpbuf = read2BytePackage();
                OtpInputStream ibuf = new OtpInputStream(tmpbuf);
                byte[] tmpname;
                int len = (int) (tmpbuf.Length);
                peer.ntype = ibuf.read1();
                if (peer.ntype != AbstractNode.NTYPE_R6)
                {
                    throw new System.IO.IOException("Unknown remote node type");
                }
                peer._distLow = (peer._distHigh = ibuf.read2BE());
                if (peer._distLow < 5)
                {
                    throw new System.IO.IOException("Unknown remote node type");
                }
                peer.flags = ibuf.read4BE();
                tmpname = new byte[len - 7];
                ibuf.readN(tmpname);
                char[] tmpChar;
                tmpChar = new char[tmpname.Length];
                tmpname.CopyTo(tmpChar, 0);
                hisname = new System.String(tmpChar);
                // Set the old nodetype parameter to indicate hidden/normal status
                // When the old handshake is removed, the ntype should also be.
                if ((peer.flags & AbstractNode.dFlagPublished) != 0)
                    peer.ntype = AbstractNode.NTYPE_R4_ERLANG;
                else
                    peer.ntype = AbstractNode.NTYPE_R4_HIDDEN;


                if ((peer.flags & AbstractNode.dFlagExtendedReferences) == 0) 
                {
                    throw new System.IO.IOException("Handshake failed - peer cannot handle extended references");
                }
                  
                if (OtpSystem.useExtendedPidsPorts() && (peer.flags & AbstractNode.dFlagExtendedPidsPorts) == 0) 
                if (true && (peer.flags & AbstractNode.dFlagExtendedPidsPorts) == 0) 
                {
                    throw new System.IO.IOException("Handshake failed - peer cannot handle extended pids and ports");
                }
            }
            catch (Erlang.DecodeException)
            {
                throw new System.IO.IOException("Handshake failed - not enough data");
            }
            
            
            int i = hisname.IndexOf((System.Char) '@', 0);
            peer._node = hisname;
            peer._alive = hisname.Substring(0, (i) - (0));
            peer._host = hisname.Substring(i + 1, (hisname.Length) - (i + 1));
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("<- " + "HANDSHAKE" + " ntype=" + peer.ntype + " dist=" + peer._distHigh + " remote=" + peer);
            }
        }
        
        protected internal virtual int recvChallenge()
        {
            
            int challenge;
            
            try
            {
                byte[] buf = read2BytePackage();
                OtpInputStream ibuf = new OtpInputStream(buf);
                peer.ntype = ibuf.read1();
                if (peer.ntype != AbstractNode.NTYPE_R6)
                {
                    throw new System.IO.IOException("Unexpected peer type");
                }
                peer._distLow = (peer._distHigh = ibuf.read2BE());
                peer.flags = ibuf.read4BE();
                challenge = ibuf.read4BE();
                byte[] tmpname = new byte[buf.Length - 11];
                ibuf.readN(tmpname);
                char[] tmpChar;
                tmpChar = new char[tmpname.Length];
                tmpname.CopyTo(tmpChar, 0);
                System.String hisname = new System.String(tmpChar);
                int i = hisname.IndexOf((System.Char) '@', 0);
                peer._node = hisname;
                peer._alive = hisname.Substring(0, (i) - (0));
                peer._host = hisname.Substring(i + 1, (hisname.Length) - (i + 1));
            }
            catch (Erlang.DecodeException)
            {
                throw new System.IO.IOException("Handshake failed - not enough data");
            }
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("<- " + "HANDSHAKE recvChallenge" + " from=" + peer._node + " challenge=" + challenge + " local=" + self);
            }
            
            return challenge;
        }
        
        protected internal virtual void  sendChallengeReply(int challenge, byte[] digest)
        {
            
            OtpOutputStream obuf = new OtpOutputStream();
            obuf.write2BE(21);
            obuf.write1(ChallengeReply);
            obuf.write4BE(challenge);
            obuf.write(digest);
            obuf.writeTo((System.IO.Stream) socket.GetStream());
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("-> " + "HANDSHAKE sendChallengeReply" + " challenge=" + challenge + " digest=" + hex(digest) + " local=" + self);
            }
        }
        

        private bool digests_equals(byte[] a, byte[] b)
        {
            int i;
             for (i = 0; i < 16; ++i)
                if (a[i] != b[i])
                    return false;
            return true;
        }
        
        protected internal virtual int recvChallengeReply(int our_challenge)
        {
            
            int challenge;
            byte[] her_digest = new byte[16];
            
            try
            {
                byte[] buf = read2BytePackage();
                OtpInputStream ibuf = new OtpInputStream(buf);
                int tag = ibuf.read1();
                if (tag != ChallengeReply)
                {
                    throw new System.IO.IOException("Handshake protocol error");
                }
                challenge = ibuf.read4BE();
                ibuf.readN(her_digest);
                byte[] our_digest = genDigest(our_challenge, self.cookie());
                if (!digests_equals(her_digest, our_digest))
                {
                    throw new OtpAuthException("Peer authentication error.");
                }
            }
            catch (Erlang.DecodeException)
            {
                throw new System.IO.IOException("Handshake failed - not enough data");
            }
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("<- " + "HANDSHAKE recvChallengeReply" + " from=" + peer._node + " challenge=" + challenge + " digest=" + hex(her_digest) + " local=" + self);
            }
            
            return challenge;
        }
        
        protected internal virtual void  sendChallengeAck(byte[] digest)
        {
            
            OtpOutputStream obuf = new OtpOutputStream();
            obuf.write2BE(17);
            obuf.write1(ChallengeAck);
            obuf.write(digest);
            
            obuf.writeTo((System.IO.Stream) socket.GetStream());
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("-> " + "HANDSHAKE sendChallengeAck" + " digest=" + hex(digest) + " local=" + self);
            }
        }
        
        protected internal virtual void  recvChallengeAck(int our_challenge)
        {
            
            byte[] her_digest = new byte[16];
            try
            {
                byte[] buf = read2BytePackage();
                OtpInputStream ibuf = new OtpInputStream(buf);
                int tag = ibuf.read1();
                if (tag != ChallengeAck)
                {
                    throw new System.IO.IOException("Handshake protocol error");
                }
                ibuf.readN(her_digest);
                byte[] our_digest = genDigest(our_challenge, self.cookie());
                if (!digests_equals(her_digest, our_digest))
                {
                    throw new OtpAuthException("Peer authentication error.");
                }
            }
            catch (Erlang.DecodeException)
            {
                throw new System.IO.IOException("Handshake failed - not enough data");
            }
            catch (System.Exception)
            {
                throw new OtpAuthException("Peer authentication error.");
            }
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("<- " + "HANDSHAKE recvChallengeAck" + " from=" + peer._node + " digest=" + hex(her_digest) + " local=" + self);
            }
        }
        
        protected internal virtual void  sendStatus(System.String status)
        {
            
            OtpOutputStream obuf = new OtpOutputStream();
            obuf.write2BE(status.Length + 1);
            obuf.write1(ChallengeStatus);
            //UPGRADE_NOTE: This code will be optimized in the future;
            byte[] tmpBytes;
            int i;
            string tmpStr;
            tmpStr = status;
            tmpBytes = new byte[tmpStr.Length];
            i = 0;
            while (i < tmpStr.Length)
            {
                tmpBytes[i] = (byte) tmpStr[i];
                i++;
            }
            obuf.write(tmpBytes);
            
            obuf.writeTo((System.IO.Stream) socket.GetStream());
            
            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("-> " + "HANDSHAKE sendStatus" + " status=" + status + " local=" + self);
            }
        }
        
        protected internal virtual void  recvStatus()
        {

            try
            {
                byte[] buf = read2BytePackage();
                OtpInputStream ibuf = new OtpInputStream(buf);
                int tag = ibuf.read1();
                if (tag != ChallengeStatus)
                {
                    throw new System.IO.IOException("Handshake protocol error");
                }
                byte[] tmpbuf = new byte[buf.Length - 1];
                ibuf.readN(tmpbuf);
                char[] tmpChar;
                tmpChar = new char[tmpbuf.Length];
                tmpbuf.CopyTo(tmpChar, 0);
                System.String status = new System.String(tmpChar);

                if (status.CompareTo("ok") != 0)
                {
                    throw new System.IO.IOException("Peer replied with status '" + status + "' instead of 'ok'");
                }
            }
            catch (Erlang.DecodeException)
            {
                throw new System.IO.IOException("Handshake failed - not enough data");
            }
            catch (System.Net.Sockets.SocketException e)
            {
                throw new System.IO.IOException("Peer dropped connection: " + e.ToString());
            }

            if (traceLevel >= OtpTrace.Type.handshakeThreshold)
            {
                OtpTrace.TraceEvent("<- " + "HANDSHAKE recvStatus (ok)" + " local=" + self);
            }
        }

    }
}