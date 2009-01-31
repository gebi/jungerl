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
	/*
	* <p> Provides a simple mechanism for exchanging messages with Erlang
	* processes or other instances of this class.</p>
	*
	* <p> Each mailbox is associated with a unique {@link OtpErlangPid
	* pid} that contains information necessary for delivery of messages.
	* When sending messages to named processes or mailboxes, the sender
	* pid is made available to the recipient of the message. When sending
	* messages to other mailboxes, the recipient can only respond if the
	* sender includes the pid as part of the message contents. The sender
	* can determine his own pid by calling {@link #self self()}. </p>
	*
	* <p> Mailboxes can be named, either at creation or later. Messages
	* can be sent to named mailboxes and named Erlang processes without
	* knowing the {@link OtpErlangPid pid} that identifies the mailbox.
	* This is neccessary in order to set up initial communication between
	* parts of an application. Each mailbox can have at most one name.
	* </p>
	*
	* <p> Since this class was intended for communication with Erlang,
	* all of the send methods take {@link OtpErlangObject
	* OtpErlangObject} arguments. However this class can also be used to
	* transmit arbitrary C# objects (as long as they implement one of
	* C#.io.Serializable or C#.io.Externalizable) by encapsulating
	* the object in a {@link OtpErlangBinary OtpErlangBinary}. </p>
	*
	* <p> Messages to remote nodes are externalized for transmission, and
	* as a result the recipient receives a <b>copy</b> of the original
	* C# object. To ensure consistent behaviour when messages are sent
	* between local mailboxes, such messages are cloned before delivery.
	* </p>
	*
	* <p> Additionally, mailboxes can be linked in much the same way as
	* Erlang processes. If a link is active when a mailbox is {@link
	* #close closed}, any linked Erlang processes or OtpMboxes will be
	* sent an exit signal. As well, exit signals will be (eventually)
	* sent if a mailbox goes out of scope and its {@link #finalize
	* finalize()} method called. However due to the nature of
	* finalization (i.e. C# makes no guarantees about when {@link
	* #finalize finalize()} will be called) it is recommended that you
	* always explicitly close mailboxes if you are using links instead of
	* relying on finalization to notify other parties in a timely manner.
	* </p>
	*
	* When retrieving messages from a mailbox that has received an exit
	* signal, an {@link OtpErlangExit OtpErlangExit} exception will be
	* raised. Note that the exception is queued in the mailbox along with
	* other messages, and will not be raised until it reaches the head of
	* the queue and is about to be retrieved. </p>
	*
	**/
	using System;

    public class OtpMbox
	{
		internal OtpNode home;
		internal Erlang.Pid _self;
		internal GenericQueue queue;
		internal System.String name;
		internal Links links;
        internal System.Collections.Hashtable monitors;

		// package constructor: called by OtpNode:createMbox(name)
		// to create a named mbox
		internal OtpMbox(OtpNode home, Erlang.Pid self, System.String name)
		{
			this._self = self;
			this.home = home;
			this.name = name;
			this.queue = new GenericQueue();
			this.links = new Links(10);
            this.monitors = new System.Collections.Hashtable(49, (float)0.95);
		}
		
		// package constructor: called by OtpNode:createMbox() 
		// to create an anonymous
		internal OtpMbox(OtpNode home, Erlang.Pid self):this(home, self, null)
		{
		}

        public void flush()
        {
            this.queue.flush();
        }

		/*
		* <p> Get the identifying {@link Pid pid} associated with
		* this mailbox.</p>
		*
		* <p> The {@link Pid pid} associated with this mailbox
		* uniquely identifies the mailbox and can be used to address the
		* mailbox. You can send the {@link Pid pid} to a remote
		* communicating part so that he can know where to send his
		* response. </p>
		*
		* @return the self pid for this mailbox.
		**/
		public virtual Erlang.Pid self()
		{
			return _self;
		}

		/*
		* <p> Register or remove a name for this mailbox. Registering a
		* name for a mailbox enables others to send messages without
		* knowing the {@link Pid pid} of the mailbox. A mailbox
		* can have at most one name; if the mailbox already had a name,
		* calling this method will supercede that name. </p>
		*
		* @param name the name to register for the mailbox. Specify null to
		* unregister the existing name from this mailbox.
		*
		* @return true if the name was available, or false otherwise.
		**/
		public virtual bool registerName(System.String name)
		{
			lock(this)
			{
				return home.registerName(name, this);
			}
		}

		/*
		* Get the registered name of this mailbox.
		*
		* @return the registered name of this mailbox, or null if the
		* mailbox had no registerd name.
		**/
		public virtual System.String getName()
		{
			return this.name;
		}

		/*
		* Block until a message arrives for this mailbox.
		*
		* @return an {@link Object Object} representing
		* the body of the next message waiting in this mailbox.
		*
		* @exception Exit if a linked {@link Pid pid} has
		* exited or has sent an exit signal to this mailbox.
		*
		**/
		public virtual Erlang.Object receive()
		{
			return receive(-1);
		}
		
		/*
		* Wait for a message to arrive for this mailbox.
		*
		* @param timeout the time, in milliseconds, to wait for a message
		* before returning null.
		*
		* @return an {@link Object Object} representing
		* the body of the next message waiting in this mailbox.
		*
		* @exception Exit if a linked {@link Pid pid} has
		* exited or has sent an exit signal to this mailbox.
		*
		**/
		public virtual Erlang.Object receive(long timeout)
		{
			try
			{
				OtpMsg m = receiveMsg(timeout);
				if (m != null)
					return m.getMsg();
			}
			catch (Erlang.Exit e)
			{
				throw e;
			}
			catch (System.Exception)
			{
			}
			return null;
		}
		
		/*
		* Block until a message arrives for this mailbox.
		*
		* @return a byte array representing the still-encoded body of the
		* next message waiting in this mailbox.
		*
		* @exception Exit if a linked {@link Pid pid} has
		* exited or has sent an exit signal to this mailbox.
		*
		**/
		public virtual OtpInputStream receiveBuf()
		{
            return receiveBuf(-1);
		}
		
		/*
		* Wait for a message to arrive for this mailbox.
		*
		* @param timeout the time, in milliseconds, to wait for a message
		* before returning null.
		*
		* @return a byte array representing the still-encoded body of the
		* next message waiting in this mailbox.
		*
		* @exception Exit if a linked {@link Pid pid} has
		* exited or has sent an exit signal to this mailbox.
		*
		* @exception InterruptedException if no message if the method
		* times out before a message becomes available.
		**/
		public virtual OtpInputStream receiveBuf(long timeout)
		{
            OtpMsg m = receiveMsg();
            return m == null ? null : m.getMsgBuf();
        }
		
		/*
		* Block until a message arrives for this mailbox.
		*
		* @return an {@link OtpMsg OtpMsg} containing the header
		* information as well as the body of the next message waiting in
		* this mailbox.
		*
		* @exception Exit if a linked {@link Pid pid} has
		* exited or has sent an exit signal to this mailbox.
		*
		**/
		public virtual OtpMsg receiveMsg()
		{
            return receiveMsg(-1);
		}
		
		/*
		* Wait for a message to arrive for this mailbox.
		*
		* @param timeout the time, in milliseconds, to wait for a message.
		*
		* @return an {@link OtpMsg OtpMsg} containing the header
		* information as well as the body of the next message waiting in
		* this mailbox.
		*
		* @exception Exit if a linked {@link Pid pid} has
		* exited or has sent an exit signal to this mailbox.
		*
		* @exception InterruptedException if no message if the method
		* times out before a message becomes available.
		**/
		public virtual OtpMsg receiveMsg(long timeout)
		{
			OtpMsg m;
            try {
                m = (OtpMsg) queue.get(timeout);
            } catch (System.Threading.ThreadInterruptedException) {
                m = null;
            }
			
			if (m == null)
				return null;
			
			switch (m.type())
			{
				case OtpMsg.Tag.exitTag:
                case OtpMsg.Tag.exit2Tag:
					try
					{
						Erlang.Object o = m.getMsg();
						throw new Erlang.Exit(o.ToString(), m.getSenderPid());
					}
					catch (Erlang.DecodeException)
					{
						throw new Erlang.Exit("unknown", m.getSenderPid());
					}
				default:
					return m;
			}
		}
		
		/*
		* Send a message to a remote {@link Pid pid}, representing
		* either another {@link OtpMbox mailbox} or an Erlang process.
		*
		* @param to the {@link Pid pid} identifying the intended
		* recipient of the message.
		*
		* @param msg the body of the message to send.
		*
		**/
		public void send(Erlang.Pid to, Erlang.Object msg)
		{
			try
			{
				System.String node = to.node();
				if (node.Equals(home.node()))
				{
					home.deliver(new OtpMsg(to, (Erlang.Object) (msg.clone())));
				}
				else
				{
					OtpCookedConnection conn = home.connection(node);
					if (conn == null)
						return ;
					conn.send(_self, to, msg);
				}
			}
			catch (System.Exception)
			{
			}
		}
		
		/*
		* Send a message to a named mailbox created from the same node as
		* this mailbox.
		*
		* @param name the registered name of recipient mailbox.
		*
		* @param msg the body of the message to send.
		*
		**/
		public void send(System.String name, Erlang.Object msg)
		{
			home.deliver(new OtpMsg(_self, name, (Erlang.Object) (msg.clone())));
		}
		
		/*
		* Send a message to a named mailbox created from another node.
		*
		* @param name the registered name of recipient mailbox.
		*
		* @param node the name of the remote node where the recipient
		* mailbox is registered.
		*
		* @param msg the body of the message to send.
		*
		**/
		public void send(System.String name, System.String node, Erlang.Object msg)
		{
			try
			{
				if (node.Equals(home.node()))
				{
					send(name, msg);
				}
				else
				{
					OtpCookedConnection conn = home.connection(node);
					if (conn == null)
						return ;
					conn.send(_self, name, msg);
				}
			}
			catch (System.Exception)
			{
			}
		}

        public Erlang.Object rpcCall(string node, string mod, string fun, Erlang.List args)
        {
            return this.rpcCall(node, mod, fun, args, -1);
        }

        public Erlang.Object rpcCall(string node, string mod, string fun, Erlang.List args, int timeout)
        {
            return this.rpcCall(node, new Erlang.Atom(mod), new Erlang.Atom(fun), args, -1);
        }

        public Erlang.Object rpcCall(string node, Erlang.Atom mod, Erlang.Atom fun, Erlang.List args)
        {
            return this.rpcCall(node, mod, fun, args, -1);
        }

        public Erlang.Object rpcCall(string node, Erlang.Atom mod, Erlang.Atom fun, Erlang.List args, int timeout)
        {
            sendRPC(node, mod, fun, args);
            return receiveRPC(timeout);
        }

        public void sendRPC(string node, string mod, string fun, Erlang.List args)
        {
            sendRPC(node, new Erlang.Atom(mod), new Erlang.Atom(fun), args);
        }

        public void sendRPC(string node, Erlang.Atom mod, Erlang.Atom fun, Erlang.List args)
        {
            sendRPC(node, mod, fun, args, _self /* new Erlang.Atom("user") */);
        }

        public void sendRPC(string node, string mod, string fun, Erlang.List args, Erlang.Pid ioServer)
        {
            sendRPC(node, new Erlang.Atom(mod), new Erlang.Atom(fun), args, ioServer);
        }

        public void sendRPC(string node, Erlang.Atom mod, Erlang.Atom fun, Erlang.List args, Erlang.Pid ioServer)
        {
            if (node.Equals(home.node()))
            {
                throw new System.ArgumentException("Cannot make rpc calls on local node!");
            }
            else
            {
                Erlang.Object msg = AbstractConnection.encodeRPC(_self, mod, fun, args, ioServer );

                OtpCookedConnection conn = home.connection(node);
                if (conn == null)
                    throw new System.Exception("Cannot establish connection to node " + node);
                conn.send(_self, "rex", msg);
            }
        }

        public Erlang.Object receiveRPC(int timeout)
        {
            Erlang.Object result = receive(timeout);

            if (result == null)
                return result;
            else
            {
                Erlang.Object rpcReply = AbstractConnection.decodeRPC(result);
                return rpcReply == null ? result : rpcReply;
            }
        }

        public string receiveIO(int timeout)
        {
            Erlang.Object result = receive(timeout);

            if (result == null)
                return null;
            else
            {
                return AbstractConnection.decodeIO(result);
            }
        }

        /*
        * <p> Send an exit signal to a remote {@link Pid pid}.
        * This method does not cause any links to be broken, except
        * indirectly if the remote {@link Pid pid} exits as a
        * result of this exit signal. </p>
        *
        * @param to the {@link Pid pid} to which the exit signal
        * should be sent.
        *
        * @param reason a string indicating the reason for the exit.
        **/
		// it's called exit, but it sends exit2 
		public virtual void  exit(Erlang.Pid to, System.String reason)
		{
			exit(2, to, reason);
		}
		
		// this function used internally when "process" dies
		// since Erlang discerns between exit and exit/2.
		private void  exit(int arity, Erlang.Pid to, System.String reason)
		{
			try
			{
				System.String node = to.node();
				if (node.Equals(home.node()))
				{
					home.deliver(new OtpMsg(OtpMsg.Tag.exitTag, _self, to, reason));
				}
				else
				{
					OtpCookedConnection conn = home.connection(node);
					if (conn == null)
						return ;
					switch (arity)
					{
						case 1: 
							conn.exit(_self, to, reason);
							break;
						
						case 2: 
							conn.exit2(_self, to, reason);
							break;
						
					}
				}
			}
			catch (System.Exception)
			{
			}
		}
		
		/*
		* <p> Link to a remote mailbox or Erlang process. Links are
		* idempotent, calling this method multiple times will not result in
		* more than one link being created. </p>
		*
		* <p> If the remote process subsequently exits or the mailbox is
		* closed, a subsequent attempt to retrieve a message through this
		* mailbox will cause an {@link Exit Exit}
		* exception to be raised. Similarly, if the sending mailbox is
		* closed, the linked mailbox or process will receive an exit
		* signal. </p>
		*
		* <p> If the remote process cannot be reached in order to set the
		* link, the exception is raised immediately. </p>
		*
		* @param to the {@link Pid pid} representing the object to
		* link to.
		*
		* @exception Exit if the {@link Pid pid} referred
		* to does not exist or could not be reached.
		*
		**/
		public virtual void  link(Erlang.Pid to)
		{
			try
			{
				System.String node = to.node();
				if (node.Equals(home.node()))
				{
                    if (!home.deliver(new OtpMsg(OtpMsg.Tag.linkTag, _self, to)))
					{
						throw new Erlang.Exit("noproc", to);
					}
				}
				else
				{
					OtpCookedConnection conn = home.connection(node);
					if (conn != null)
						conn.link(_self, to);
					else
						throw new Erlang.Exit("noproc", to);
				}
			}
			catch (Erlang.Exit e)
			{
				throw e;
			}
			catch (System.Exception)
			{
			}
			
			links.addLink(_self, to);
		}
		
		/*
		* <p> Remove a link to a remote mailbox or Erlang process. This
		* method removes a link created with {@link #link link()}.
		* Links are idempotent; calling this method once will remove all
		* links between this mailbox and the remote {@link Pid
		* pid}. </p>
		*
		* @param to the {@link Pid pid} representing the object to
		* unlink from.
		*
		**/
		public virtual void  unlink(Erlang.Pid to)
		{
			links.removeLink(_self, to);
			
			try
			{
				System.String node = to.node();
				if (node.Equals(home.node()))
				{
                    home.deliver(new OtpMsg(OtpMsg.Tag.unlinkTag, _self, to));
				}
				else
				{
					OtpCookedConnection conn = home.connection(node);
					if (conn != null)
						conn.unlink(_self, to);
				}
			}
			catch (System.Exception)
			{
			}
		}
		
		/*
		* <p> Create a connection to a remote node. </p>
		*
		* <p> Strictly speaking, this method is not necessary simply to set
		* up a connection, since connections are created automatically
		* first time a message is sent to a {@link Pid pid} on the
		* remote node. </p>
		*
		* <p> This method makes it possible to wait for a node to come up,
		* however, or check that a node is still alive. </p>
		*
		* <p> This method calls a method with the same name in {@link
		* OtpNode#ping Otpnode} but is provided here for convenience. </p>
		*
		* @param node the name of the node to ping.
		*
		* @param timeout the time, in milliseconds, before reporting
		* failure.
		**/
		public virtual bool ping(System.String node, long timeout)
		{
			return home.ping(node, timeout);
		}
		
		/*
		* <p> Get a list of all known registered names on the same {@link
		* OtpNode node} as this mailbox. </p>
		*
		* <p> This method calls a method with the same name in {@link
		* OtpNode#getNames Otpnode} but is provided here for convenience.
		* </p>
		*
		* @return an array of Strings containing all registered names on
		* this {@link OtpNode node}.
		**/
		public virtual System.String[] getNames()
		{
			return home.getNames();
		}
		
		/*
		* Determine the {@link Pid pid} corresponding to a
		* registered name on this {@link OtpNode node}.
		*
		* <p> This method calls a method with the same name in {@link
		* OtpNode#whereis Otpnode} but is provided here for convenience.
		* </p>
		*
		* @return the {@link Pid pid} corresponding to the
		* registered name, or null if the name is not known on this node.
		**/
		public virtual Erlang.Pid whereis(System.String name)
		{
			return home.whereis(name);
		}
		
		/*
		* Close this mailbox.
		*
		* <p> After this operation, the mailbox will no longer be able to
		* receive messages. Any delivered but as yet unretrieved messages
		* can still be retrieved however. </p>
		*
		* <p> If there are links from this mailbox to other {@link
		* Pid pids}, they will be broken when this method is
		* called and exit signals will be sent. </p>
		*
		**/
		public void close()
		{
			home.closeMbox(this);
		}
		
		~OtpMbox()
		{
			this.close();
			queue.close();
		}
		
		/*
		* Determine if two mailboxes are equal.
		*
		* @return true if both Objects are mailboxes with the same
		* identifying {@link Pid pids}.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is OtpMbox))
				return false;
			
			OtpMbox m = (OtpMbox) o;
			return m._self.Equals(this._self);
		}

		public override int GetHashCode()
		{
			return _self.GetHashCode();
		}

		/*
		* called by OtpNode to deliver message to this mailbox.
		*
		* About exit and exit2: both cause exception to be raised upon
		* receive(). However exit (not 2) causes any link to be removed as
		* well, while exit2 leaves any links intact.
		*/
		internal virtual void  deliver(OtpMsg m)
		{
			switch (m.type())
			{
                case OtpMsg.Tag.linkTag: 
					links.addLink(_self, m.getSenderPid());
					break;

                case OtpMsg.Tag.unlinkTag:
					links.removeLink(_self, m.getSenderPid());
					break;

                case OtpMsg.Tag.exitTag:
					links.removeLink(_self, m.getSenderPid());
					queue.put(m);
					break;

                case OtpMsg.Tag.monitorPTag:
                    monitors[m.getSenderPid()] = m.getMsg();
                    break;

                case OtpMsg.Tag.demonitorPTag:
                    monitors.Remove(m.getSenderPid());
                    break;

                case OtpMsg.Tag.monitorPexitTag:
                    queue.put(m);
                    break;

                case OtpMsg.Tag.exit2Tag:
                default:
					queue.put(m);
					break;
			}
		}
		
		// used to break all known links to this mbox
		internal virtual void  breakLinks(System.String reason)
		{
			Link[] l = links.clearLinks();
			
			if (l != null)
			{
				int len = (int) (l.Length);
				
				 for (int i = 0; i < len; i++)
				{
					exit(1, l[i].remote(), reason);
				}
			}
		}
	}
}