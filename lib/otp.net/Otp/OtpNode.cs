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
	* <p> Represents a local OTP node. This class is used when you do not
	* wish to manage connections yourself - outgoing connections are
	* established as needed, and incoming connections accepted
	* automatically. This class supports the use of a mailbox API for
	* communication, while management of the underlying communication
	* mechanism is automatic and hidden from the application programmer.
	* </p>
	*
	* <p> Once an instance of this class has been created, obtain one or
	* more mailboxes in order to send or receive messages. The first
	* message sent to a given node will cause a connection to be set up
	* to that node. Any messages received will be delivered to the
	* appropriate mailboxes. </p>
	*
	* <p> To shut down the node, call {@link #close close()}. This will
	* prevent the node from accepting additional connections and it will
	* cause all existing connections to be closed. Any unread messages in
	* existing mailboxes can still be read, however no new messages will
	* be delivered to the mailboxes. </p>
	*
	* <p> Note that the use of this class requires that Epmd (Erlang Port
	* Mapper Daemon) is running on each cooperating host. This class does
	* not start Epmd automatically as Erlang does, you must start it
	* manually or through some other means. See the Erlang documentation
	* for more information about this. </p>
	**/
	public class OtpNode:OtpLocalNode
	{
		private bool initDone = false;
        private int listenPort;
		
		// thread to manage incoming connections
		private Acceptor acceptor = null;
		
		// keep track of all connections
		internal System.Collections.Hashtable connections = null;
		
		// keep track of all mailboxes
		internal Mailboxes mboxes = null;
		
		/*
		* <p> Create a node using the default cookie. The default
		* cookie is found by reading the first line of the .erlang.cookie
		* file in the user's home directory. The home directory is obtained
		* from the System property "user.home". </p>
		*
		* <p> If the file does not exist, an empty string is used. This
		* method makes no attempt to create the file. </p>
		*
		* @param node the name of this node.
		*
		* @exception IOException if communication could not be initialized.
		*
		**/
        public OtpNode(System.String node)
            : this(node, true, defaultCookie, 0, false)
        {
        }

        public OtpNode(System.String node, bool acceptConnections)
            : this(node, acceptConnections, defaultCookie, 0, false)
        {
        }

        public OtpNode(System.String node, bool acceptConnections, bool shortName)
            : this(node, acceptConnections, defaultCookie, 0, shortName)
        {
        }

        /*
        * Create a node.
        *
        * @param node the name of this node.
        *
        * @param cookie the authorization cookie that will be used by this
        * node when it communicates with other nodes.
        *
        * @exception IOException if communication could not be initialized.
        *
        **/
        public OtpNode(System.String node, bool acceptConnections, System.String cookie)
            : this(node, acceptConnections, cookie, 0, false)
        {
        }

        /*
        * Create a node.
        *
        * @param acceptConnections when true this node will register with epmd and 
        * start listening for connections.
        * 
        * @param node the name of this node.
        *
        * @param cookie the authorization cookie that will be used by this
        * node when it communicates with other nodes.
        *
        * @param shortName defines whether the node should use short or long names when 
        * referencing other nodes.
        *
        * @exception IOException if communication could not be initialized.
        *
        **/

        public OtpNode(bool acceptConnections, System.String node, System.String cookie, bool shortName)
            : this(node, acceptConnections, cookie, 0, shortName)
        {
        }
		
		/*
		* Create a node.
		*
		* @param node the name of this node.
		*
		* @param cookie the authorization cookie that will be used by this
		* node when it communicates with other nodes.
		*
		* @param port the port number you wish to use for incoming
		* connections. Specifying 0 lets the system choose an available
		* port.
		*
		* @exception IOException if communication could not be initialized.
		*
		**/
        public OtpNode(System.String node, bool acceptConnections, System.String cookie, int port, bool shortName)
            : base(node, cookie, shortName)
        {
            init(acceptConnections, port);
        }

		private void  init(bool acceptConnections, int port)
		{
			lock(this)
			{
				if (!initDone)
				{
					connections = new System.Collections.Hashtable(17, (float) 0.95);
					mboxes = new Mailboxes(this);
                    if (acceptConnections)
					    acceptor = new Acceptor(this, port);
                    listenPort = port;
					initDone = true;
				}
			}
		}

        /*
         * When a node instance is created is started with acceptConnections set to false
         * then no incoming connections are accepted.  This method allows to begin
         * accepting connections.
         **/
        public bool startConnectionAcceptor()
        {
            if (acceptor != null)
                return true;
            else if (!initDone)
                return false;

            lock (this)
            {
                if (acceptor != null)
                    acceptor = new Acceptor(this, listenPort);
            }
            return true;
        }

        /*
        * Close the node. Unpublish the node from Epmd (preventing new
        * connections) and close all existing connections.
        **/
		public virtual void close()
		{
			lock(this)
			{
                if (acceptor != null)
				    acceptor.quit();
				OtpCookedConnection conn;
				System.Collections.IDictionaryEnumerator it = connections.GetEnumerator();

				mboxes.clear();
				it.Reset();

				while (it.MoveNext())
				{
					conn = (OtpCookedConnection) it.Value;
					conn.close();
				}
				initDone = false;
			}
		}
		
		~OtpNode()
		{
			close();
		}

		/*
		* Create an unnamed {@link OtpMbox mailbox} that can be used to
		* send and receive messages with other, similar mailboxes and with
		* Erlang processes. Messages can be sent to this mailbox by using
		* its associated {@link OtpMbox#self pid}.
		*
		* @return a mailbox.
		**/
		public virtual OtpMbox createMbox()
		{
			return mboxes.create();
		}
		
		/*
		* Close the specified mailbox.
		*
		* @param the mailbox to close.
		*
		* <p> After this operation, the mailbox will no longer be able to
		* receive messages. Any delivered but as yet unretrieved messages
		* can still be retrieved however. </p>
		*
		* <p> If there are links from the mailbox to other {@link
		* OtpErlangPid pids}, they will be broken when this method is
		* called and exit signals will be sent. </p>
		*
		**/
		public virtual void  closeMbox(OtpMbox mbox)
		{
			if (mbox != null)
			{
				mboxes.remove(mbox);
				mbox.name = null;
				mbox.breakLinks("normal");
			}
		}
		
		/*
		* Create an named mailbox that can be used to send and receive
		* messages with other, similar mailboxes and with Erlang processes.
		* Messages can be sent to this mailbox by using its registered name
		* or the associated {@link OtpMbox#self pid}.
		*
		* @param name a name to register for this mailbox. The name must be
		* unique within this OtpNode.
		*
		* @return a mailbox, or null if the name was already in use.
		*
		**/
		public virtual OtpMbox createMbox(System.String name)
		{
			return mboxes.create(name);
		}
		
		
		/*
		* <p> Register or remove a name for the given mailbox. Registering
		* a name for a mailbox enables others to send messages without
		* knowing the {@link OtpErlangPid pid} of the mailbox. A mailbox
		* can have at most one name; if the mailbox already had a name,
		* calling this method will supercede that name. </p>
		*
		* @param name the name to register for the mailbox. Specify null to
		* unregister the existing name from this mailbox.
		*
		* @param mbox the mailbox to associate with the name.
		*
		* @return true if the name was available, or false otherwise.
		**/
		public virtual bool registerName(System.String name, OtpMbox mbox)
		{
			return mboxes.register(name, mbox);
		}
		
		/*
		* Get a list of all known registered names on this node.
		*
		* @return an array of Strings, containins all known registered
		* names on this node.
		**/
		
		public virtual System.String[] getNames()
		{
			return mboxes.names();
		}

		/*
		* Determine the {@link Pid pid} corresponding to a
		* registered name on this node.
		*
		* @return the {@link Pid pid} corresponding to the
		* registered name, or null if the name is not known on this node.
		**/
		public virtual Erlang.Pid whereis(System.String name)
		{
			OtpMbox m = mboxes.get(name);
			if (m != null)
				return m.self();
			return null;
		}
		
		/*
		* <p> Determine if another node is alive. This method has the side
		* effect of setting up a connection to the remote node (if
		* possible). Only a single outgoing message is sent; the timeout is
		* how long to wait for a response. </p>
		*
		* <p> Only a single attempt is made to connect to the remote node,
		* so for example it is not possible to specify an extremely long
		* timeout and expect to be notified when the node eventually comes
		* up. If you wish to wait for a remote node to be started, the
		* following construction may be useful: </p>
		*
		<pre>
		// ping every 2 seconds until positive response
		while (!me.ping(him,2000));
		</pre>
		*
		* @param node the name of the node to ping.
		*
		* @param timeout the time, in milliseconds, to wait for response
		* before returning false.
		*
		* @return true if the node was alive and the correct ping response
		* was returned. false if the correct response was not returned on
		* time.
		**/
		/*internal info about the message formats...
		*
		* the request:
		*  -> REG_SEND {6,#Pid<bingo@aule.1.0>,'',net_kernel}
		*  {'$gen_call',{#Pid<bingo@aule.1.0>,#Ref<bingo@aule.2>},{is_auth,bingo@aule}}
		*
		* the reply:
		*  <- SEND {2,'',#Pid<bingo@aule.1.0>}
		*  {#Ref<bingo@aule.2>,yes}
		*/
		public virtual bool ping(System.String node, long timeout)
		{
			if (node.Equals(this._node))
				return true;

			OtpMbox mbox = null;
			try
			{
				mbox = createMbox();
				mbox.send("net_kernel", node, getPingTuple(mbox));
				Erlang.Object reply = mbox.receive(timeout);
				
				Erlang.Tuple t = (Erlang.Tuple) reply;
				Erlang.Atom a = (Erlang.Atom) (t.elementAt(1));
				return "yes".Equals(a.atomValue());
			}
			catch (System.Exception)
			{
			}
			finally
			{
				closeMbox(mbox);
			}
			return false;
		}
		
		/*create the outgoing ping message */
		private Erlang.Tuple getPingTuple(OtpMbox mbox)
		{
			Erlang.Object[] ping = new Erlang.Object[3];
			Erlang.Object[] pid = new Erlang.Object[2];
			Erlang.Object[] _node = new Erlang.Object[2];
			
			pid[0] = mbox.self();
			pid[1] = createRef();
			
			_node[0] = new Erlang.Atom("is_auth");
			_node[1] = new Erlang.Atom(node());
			
			ping[0] = new Erlang.Atom("$gen_call");
			ping[1] = new Erlang.Tuple(pid);
			ping[2] = new Erlang.Tuple(_node);
			
			return new Erlang.Tuple(ping);
		}
		
		/*
		* this method simulates net_kernel only for
		* the purpose of replying to pings.
		*/
		private bool netKernel(OtpMsg m)
		{
			OtpMbox mbox = null;
			try
			{
				Erlang.Tuple t = (Erlang.Tuple) (m.getMsg());
				Erlang.Tuple req = (Erlang.Tuple) t.elementAt(1); // actual request
				
				Erlang.Pid pid = (Erlang.Pid) req.elementAt(0); // originating pid
				
				Erlang.Object[] pong = new Erlang.Object[2];
				pong[0] = req.elementAt(1); // his #Ref
				pong[1] = new Erlang.Atom("yes");
				
				mbox = createMbox();
				mbox.send(pid, new Erlang.Tuple(pong));
				return true;
			}
			catch (System.Exception)
			{
			}
			finally
			{
				closeMbox(mbox);
			}
			return false;
		}
		
		
		/*
		* OtpCookedConnection delivers messages here
		* return true if message was delivered successfully, or false otherwise.
		*/
		internal virtual bool deliver(OtpMsg m)
		{
			OtpMbox mbox = null;
			
			try
			{
                OtpMsg.Tag t = m.type();
				
				if (t == OtpMsg.Tag.regSendTag)
				{
					System.String name = m.getRecipientName();
					/*special case for netKernel requests */
					if (name.Equals("net_kernel"))
						return netKernel(m);
					else
						mbox = mboxes.get(name);
				}
				else
				{
					mbox = mboxes.get(m.getRecipientPid());
				}
				if (mbox == null)
					return false;
				mbox.deliver(m);
			}
			catch (System.Exception)
			{
				return false;
			}
			
			return true;
		}
		
		
		/*
		* OtpCookedConnection delivers errors here, we send them on to the
		* handler specified by the application
		*/
		internal virtual void  deliverError(OtpCookedConnection conn, System.Exception e)
		{
			removeConnection(conn);
			remoteStatus(conn.name, false, e);
		}
		
		
		/*
		* find or create a connection to the given node
		*/
		public virtual OtpCookedConnection connection(System.String node)
		{
			OtpPeer peer = null;
			OtpCookedConnection conn = null;
			
			lock(connections)
			{
				// first just try looking up the name as-is
				conn = (OtpCookedConnection) connections[node];
				
				if (conn == null)
				{
					// in case node had no '@' add localhost info and try again
					peer = new OtpPeer(node);
					conn = (OtpCookedConnection) connections[peer.node()];
					
					if (conn == null)
					{
						try
						{
							conn = new OtpCookedConnection(this, peer);
							addConnection(conn);
						}
						catch (System.Exception e)
						{
							/*false = outgoing */
							connAttempt(peer.node(), false, e);
						}
					}
				}
				return conn;
			}
		}
		
		private void  addConnection(OtpCookedConnection conn)
		{
			if ((conn != null) && (conn.name != null))
			{
				connections[conn.name] = conn;
				remoteStatus(conn.name, true, null);
			}
		}
		
		private void  removeConnection(OtpCookedConnection conn)
		{
			if ((conn != null) && (conn.name != null))
			{
				connections.Remove(conn.name);
			}
		}
		
		/*this class used to wrap the mailbox hashtables so we can use weak references */
		public class Mailboxes
		{
			private void  InitBlock(OtpNode enclosingInstance)
			{
				this.enclosingInstance = enclosingInstance;
			}
			private OtpNode enclosingInstance;
			internal System.Collections.Hashtable byPid = null; // mbox pids here
			private System.Collections.Hashtable byName = null; // mbox names here
			
			public Mailboxes(OtpNode enclosingInstance)
			{
				InitBlock(enclosingInstance);
				byPid = new System.Collections.Hashtable(17, (float) 0.95);
				byName = new System.Collections.Hashtable(17, (float) 0.95);
			}
			
			public virtual OtpMbox create(System.String name)
			{
				OtpMbox m = null;
				
				lock(byName)
				{
					if (get(name) != null)
						return null;
					Erlang.Pid pid = enclosingInstance.createPid();
					m = new OtpMbox(enclosingInstance, pid, name);
					byPid[pid] = new WeakReference(m);
					byName[name] = new WeakReference(m);
				}
				return m;
			}
			
			public virtual OtpMbox create()
			{
				Erlang.Pid pid = enclosingInstance.createPid();
				OtpMbox m = new OtpMbox(enclosingInstance, pid);
				byPid[pid] = new WeakReference(m);
				return m;
			}
			
			public virtual void  clear()
			{
				byPid.Clear();
				byName.Clear();
			}

			public virtual System.String[] names()
			{
				System.String[] allnames = null;

				lock(byName)
				{
					int n = byName.Count;
					System.Collections.IDictionaryEnumerator keys = byName.GetEnumerator();
					allnames = new System.String[n];

					int i = 0;
					keys.Reset();
					while (keys.MoveNext())
					{
						allnames[i++] = (System.String) (keys.Key);
					}
				}
				return allnames;
			}

			public virtual System.String[] pids()
			{
				System.String[] allpids = null;

				lock(byPid)
				{
					int n = byPid.Count;
					System.Collections.IDictionaryEnumerator keys = byPid.GetEnumerator();
					allpids = new System.String[n];

					int i = 0;
					keys.Reset();
					while (keys.MoveNext())
					{
						allpids[i++] = ((Erlang.Pid) (keys.Key)).ToString();
					}
				}
				return allpids;
			}

			public virtual bool register(System.String name, OtpMbox mbox)
			{
				if (name == null)
				{
					if (mbox.name != null)
					{
						byName.Remove(mbox.name);
						mbox.name = null;
					}
				}
				else
				{
					lock(byName)
					{
						if (get(name) != null)
							return false;
						byName[name] = new WeakReference(mbox);
						mbox.name = name;
					}
				}
				return true;
			}
			
			/*look up a mailbox based on its name. If the mailbox has gone out
			* of scope we also remove the reference from the hashtable so we
			* don't find it again.
			*/
			public virtual OtpMbox get(System.String name)
			{
				WeakReference wr = (WeakReference) byName[name];
				
				if (wr != null)
				{
					OtpMbox m = (OtpMbox) wr.Target;
					
					if (m != null)
						return m;
					byName.Remove(name);
				}
				return null;
			}
			
			/*look up a mailbox based on its pid. If the mailbox has gone out
			* of scope we also remove the reference from the hashtable so we
			* don't find it again.
			*/
			public virtual OtpMbox get(Erlang.Pid pid)
			{
				WeakReference wr = (WeakReference) byPid[pid];
				
				if (wr != null)
				{
					OtpMbox m = (OtpMbox) wr.Target;
					
					if (m != null)
						return m;
					byPid.Remove(pid);
				}
				return null;
			}
			
			public virtual void remove(OtpMbox mbox)
			{
				byPid.Remove(mbox._self);
				if (mbox.name != null)
					byName.Remove(mbox.name);
			}
		}
		
		
		/*
		* this thread simply listens for incoming connections
		*/
		public class Acceptor
		{
			private void  InitBlock(OtpNode a_node)
			{
				this.a_node = a_node;
			}
			private OtpNode a_node;
			private System.Net.Sockets.TcpListener a_sock;
			private int a_port;
			private bool a_done = false;
			private System.Threading.Thread thread;

			internal Acceptor(OtpNode a_node, int port)
			{
				InitBlock(a_node);

				a_sock = new System.Net.Sockets.TcpListener(System.Net.Dns.GetHostEntry("localhost").AddressList[0], port);
				a_sock.Start();
				this.a_port = ((System.Net.IPEndPoint)a_sock.LocalEndpoint).Port;
				a_node._port = this.a_port;

				publishPort();
				thread = new System.Threading.Thread(new System.Threading.ThreadStart(Start));
				thread.IsBackground = true;
				thread.Name = "acceptor "+a_node.node();
				thread.Start();
			}

			private bool publishPort()
			{
				if (a_node.getEpmd() != null)
					return false;
				// already published
				OtpEpmd.publishPort(a_node);
				return true;
			}

			private void  unPublishPort()
			{
				// unregister with epmd
				OtpEpmd.unPublishPort(a_node);

				// close the local descriptor (if we have one)
				closeSock(a_node.epmd);
				a_node.epmd = null;
			}

			public virtual void  quit()
			{
				unPublishPort();
				a_done = true;
				closeSock(a_sock);
				//thread.Interrupt();
			}

			private void  closeSock(System.Net.Sockets.TcpListener s)
			{
				try
				{
					if (s != null)
						s.Stop();
				}
				catch (System.Exception)
				{
				}
			}
			
			private void  closeSock(System.Net.Sockets.TcpClient s)
			{
				try
				{
					if (s != null)
						s.Close();
				}
				catch (System.Exception)
				{
				}
			}
			
			public virtual int port()
			{
				return this.a_port;
			}

			public void Start()
			{
				System.Net.Sockets.TcpClient newsock = null;
				OtpCookedConnection conn = null;

				a_node.localStatus(a_node._node, true, null);
				
				while (!a_done)
				{
					conn = null;
					
					try
					{
						newsock = a_sock.AcceptTcpClient();
					}
					catch (System.Exception e)
					{
						a_node.localStatus(a_node._node, false, e);
						goto accept_loop_brk;
					}
					
					try
					{
						lock(a_node.connections)
						{
							conn = new OtpCookedConnection(a_node, newsock);
							a_node.addConnection(conn);
						}
					}
					catch (OtpAuthException e)
					{
						if (conn != null && conn.name != null)
							a_node.connAttempt(conn.name, true, e);
						else
							a_node.connAttempt("unknown", true, e);
						closeSock(newsock);
					}
					catch (System.IO.IOException e)
					{
						if (conn != null && conn.name != null)
							a_node.connAttempt(conn.name, true, e);
						else
							a_node.connAttempt("unknown", true, e);
						closeSock(newsock);
					}
					catch (System.Exception e)
					{
						closeSock(newsock);
						closeSock(a_sock);
						a_node.localStatus(a_node._node, false, e);
						goto accept_loop_brk;
					}
				}
accept_loop_brk: ;
				 // while

				// if we have exited loop we must do this too
				unPublishPort();
			}
		}
	}
}