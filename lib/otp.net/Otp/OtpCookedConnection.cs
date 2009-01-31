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
namespace Otp
{
	using System;

	/*
	* <p> Maintains a connection between a C# process and a remote
	* Erlang, C# or C node. The object maintains connection state and
	* allows data to be sent to and received from the peer. </p>
	*
	* <p> Once a connection is established between the local node and a
	* remote node, the connection object can be used to send and receive
	* messages between the nodes. </p>
	*
	* <p> The various receive methods are all blocking and will return
	* only when a valid message has been received or an exception is
	* raised. </p>
	*
	* <p> If an exception occurs in any of the methods in this class, the
	* connection will be closed and must be reopened in order to resume
	* communication with the peer. </p>
	*
	* <p> The message delivery methods in this class deliver directly to
	* {@link OtpMbox mailboxes} in the {@link OtpNode OtpNode} class.
	* </p>
	* 
	* <p> It is not possible to create an instance of this class
	* directly. OtpCookedConnection objects are created as needed by the
	* underlying mailbox mechanism. </p>
	**/
	public class OtpCookedConnection:AbstractConnection
	{
		new protected internal OtpNode self;
		
		/*The connection needs to know which local pids have links that
		* pass through here, so that they can be notified in case of
		* connection failure
		*/
		protected Links links = null;
        protected System.Collections.Hashtable monitors = null;
		
		/*
		* Accept an incoming connection from a remote node. Used by {@link
		* OtpSelf#accept() OtpSelf.accept()} to create a connection
		* based on data received when handshaking with the peer node, when
		* the remote node is the connection intitiator.
		*
		* @exception C#.io.IOException if it was not possible to connect to the peer.
		*
		* @exception OtpAuthException if handshake resulted in an authentication error
		*/
		// package scope
		internal OtpCookedConnection(OtpNode self, System.Net.Sockets.TcpClient s):base(self, s)
		{
			this.self = self;
			this.links = new Links(25);
            this.monitors = new System.Collections.Hashtable(49, (float)0.95);

			thread = new System.Threading.Thread(new System.Threading.ThreadStart(Start));
			thread.IsBackground = true;
			thread.Name = "cooked connection "+self.node()+" | "+s.ToString();
			thread.Start();
		}

		/*
		* Intiate and open a connection to a remote node.
		*
		* @exception C#.io.IOException if it was not possible to connect to the peer.
		*
		* @exception OtpAuthException if handshake resulted in an authentication error.
		*/
		// package scope
		internal OtpCookedConnection(OtpNode self, OtpPeer other):base(self, other)
		{
			this.self = self;
			this.links = new Links(25);
            this.monitors = new System.Collections.Hashtable(49, (float)0.95);

			thread = new System.Threading.Thread(new System.Threading.ThreadStart(Start));
			thread.IsBackground = true;
			thread.Name = "cooked connection2 "+self.node()+"->"+other.node();
			thread.Start();
		}

		// pass the error to the node
		public override void  deliver(System.Exception e)
		{
			self.deliverError(this, e);
			return ;
		}

        public new OtpPeer peer
        {
            get { return base.peer; }
        }
		
		/*
		* pass the message to the node for final delivery. Note that the
		* connection itself needs to know about links (in case of connection
		* failure), so we snoop for link/unlink too here.
		*/
		public override void  deliver(OtpMsg msg)
		{
			bool delivered = self.deliver(msg);

			switch (msg.type())
			{
				case OtpMsg.Tag.linkTag:
					if (delivered)
					{
						links.addLink(msg.getRecipientPid(), msg.getSenderPid());
					}
					else
						try
						{
							// no such pid - send exit to sender
							base.sendExit(msg.getRecipientPid(), msg.getSenderPid(), "noproc");
						}
						catch (System.IO.IOException)
						{
						}
					break;

                case OtpMsg.Tag.monitorPTag:
                    if (delivered)
                        monitors[msg.getSenderPid()] = msg.getMsg();
                    else
                        try
                        {
                            base.sendExit(msg.getRecipientPid(), msg.getSenderPid(), "noproc");
                        }
                        catch (System.IO.IOException)
                        {
                        }
                    break;

                case OtpMsg.Tag.demonitorPTag:
                case OtpMsg.Tag.monitorPexitTag:
                    monitors.Remove(msg.getSenderPid());
                    break;

                case OtpMsg.Tag.unlinkTag:
                case OtpMsg.Tag.exitTag:
					links.removeLink(msg.getRecipientPid(), msg.getSenderPid());
					break;

                case OtpMsg.Tag.exit2Tag:
					break;
			}
			return ;
		}


		/*
		* send to pid
		*/
		internal virtual void  send(Erlang.Pid from, Erlang.Pid dest, Erlang.Object msg)
		{
			// encode and send the message
			sendBuf(from, dest, new OtpOutputStream(msg));
		}

		/*
		* send to remote name
		* dest is recipient's registered name, the nodename is implied by
		* the choice of connection.
		*/
		internal virtual void  send(Erlang.Pid from, System.String dest, Erlang.Object msg)
		{
			// encode and send the message
			sendBuf(from, dest, new OtpOutputStream(msg));
		}

		public override void  close()
		{
			base.close();
			breakLinks();
		}
		
		~OtpCookedConnection()
		{
			this.close();
		}
		
		/*
		* this one called by dying/killed process
		*/
		internal virtual void  exit(Erlang.Pid from, Erlang.Pid to, System.String reason)
		{
			try
			{
				base.sendExit(from, to, reason);
			}
			catch (System.Exception)
			{
			}
		}
		
		/*
		* this one called explicitely by user code => use exit2
		*/
		internal virtual void  exit2(Erlang.Pid from, Erlang.Pid to, System.String reason)
		{
			try
			{
				base.sendExit2(from, to, reason);
			}
			catch (System.Exception)
			{
			}
		}
		
		/*
		* snoop for outgoing links and update own table
		*/
		internal virtual void  link(Erlang.Pid from, Erlang.Pid to)
		{
			lock(this)
			{
				try
				{
					base.sendLink(from, to);
					links.addLink(from, to);
				}
				catch (System.IO.IOException)
				{
					throw new Erlang.Exit("noproc", to);
				}
			}
		}
		
		/*
		* snoop for outgoing unlinks and update own table
		*/
		internal virtual void  unlink(Erlang.Pid from, Erlang.Pid to)
		{
			lock(this)
			{
				links.removeLink(from, to);
				try
				{
					base.sendUnlink(from, to);
				}
				catch (System.IO.IOException)
				{
				}
			}
		}
		
		/*When the connection fails - send exit to all local
		* pids with links through this connection
		*/
		internal virtual void  breakLinks()
		{
			lock(this)
			{
				if (links != null)
				{
					Link[] l = links.clearLinks();
					
					if (l != null)
					{
						int len = (int) (l.Length);
						
    					for (int i = 0; i < len; i++)
						{
							// send exit "from" remote pids to local ones
							self.deliver(new OtpMsg(OtpMsg.Tag.exitTag, l[i].remote(), l[i].local(), "noconnection"));
						}
					}
				}
			}
		}
	}
}