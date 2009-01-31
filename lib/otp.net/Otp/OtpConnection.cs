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
	* <p> Once a connection is established between the local node and a
	* remote node, the connection object can be used to send and receive
	* messages between the nodes and make rpc calls (assuming that the
	* remote node is a real Erlang node).
	*
	* <p> The various receive methods are all blocking and will return
	* only when a valid message has been received or an exception is
	* raised.
	*
	* <p> If an exception occurs in any of the methods in this class, the
	* connection will be closed and must be explicitely reopened in order
	* to resume communication with the peer.
	* 
	* <p> It is not possible to create an instance of this class
	* directly. OtpConnection objects are returned by {@link
	* OtpSelf#connect(OtpPeer) OtpSelf.connect()} and {@link
	* OtpSelf#accept() OtpSelf.accept()}.
	**/
	public class OtpConnection:AbstractConnection
	{
		protected internal OtpSelf _self;
		protected internal GenericQueue queue; // messages get delivered here
		
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
		internal OtpConnection(OtpSelf self, System.Net.Sockets.TcpClient s):base(self, s)
		{
			this._self = self;
			this.queue = new GenericQueue();

			System.Threading.Thread t = new System.Threading.Thread(new System.Threading.ThreadStart(Start));
			t.IsBackground = true;
			t.Name = "connection "+self.node()+" | "+s.ToString();
			t.Start();
		}


		/*
		* Intiate and open a connection to a remote node.
		*
		* @exception C#.io.IOException if it was not possible to connect to the peer.
		*
		* @exception OtpAuthException if handshake resulted in an authentication error.
		*/
		// package scope
		internal OtpConnection(OtpSelf self, OtpPeer other):base(self, other)
		{
			this._self = self;
			this.queue = new GenericQueue();

			System.Threading.Thread t = new System.Threading.Thread(new System.Threading.ThreadStart(Start));
			t.IsBackground = true;
			t.Name = "connection2 "+self.node()+" -> "+other.node();
			t.Start();
		}

		public override void  deliver(System.Exception e)
		{
			queue.put(e);
		}

		public override void  deliver(OtpMsg msg)
		{
			queue.put(msg);
		}

		/*
		* Get information about the node at the peer end of this
		* connection.
		* 
		* @return the {@link OtpPeer Node} representing the peer node.
		**/
		public virtual OtpPeer getPeer()
		{
			return peer;
		}
		
		/*
		* Get information about the node at the local end of this
		* connection.
		* 
		* @return the {@link OtpSelf Node} representing the local node.
		**/
		public virtual new OtpSelf self()
		{
			return _self;
		}
		
		/*
		* Return the number of messages currently waiting in the receive
		* queue for this connection.
		*/
		public virtual int msgCount()
		{
			return queue.getCount();
		}

        /*
        * Receive an RPC reply from the remote Erlang node. This
        * convenience function receives a message from the remote node, and
        * expects it to have the following format:
        * 
        * <pre>
        * {rex, Term}
        * </pre>
        *
        * @return the second element of the tuple if the received message
        * is a two-tuple, otherwise null. No further error checking is
        * performed.
        *
        * @exception C#.io.IOException if the connection is not active or
        * a communication error occurs.
        *
        * @exception Erlang.Exit if an exit signal is
        * received from a process on the peer node.
        *
        * @exception OtpAuthException if the remote node
        * sends a message containing an invalid cookie.
        **/
        public virtual Erlang.Object receiveRPC()
        {

            Erlang.Object msg = receive();
            Debug.WriteLine("receiveRPC: " + msg.ToString());

            return AbstractConnection.decodeRPC(msg);
        }

        /*
        * Receive a message from a remote process. This method blocks
        * until a valid message is received or an exception is raised.
        *
        * <p> If the remote node sends a message that cannot be decoded
        * properly, the connection is closed and the method throws an
        * exception.
        *
        * @return an object containing a single Erlang term.
        *
        * @exception C#.io.IOException if the connection is not active or
        * a communication error occurs.
        *
        * @exception Erlang.Exit if an exit signal is
        * received from a process on the peer node.
        *
        * @exception OtpAuthException if the remote node
        * sends a message containing an invalid cookie.
        **/
		public virtual Erlang.Object receive()
		{
            return receive(-1);
		}
		
		/*
		* Receive a message from a remote process. This method blocks at
		* most for the specified time, until a valid message is received or
		* an exception is raised.
		*
		* <p> If the remote node sends a message that cannot be decoded
		* properly, the connection is closed and the method throws an
		* exception.
		*
		* @param timeout the time in milliseconds that this operation will
		* block. Specify 0 to poll the queue.
		*
		* @return an object containing a single Erlang term.
		*
		* @exception C#.io.IOException if the connection is not active or
		* a communication error occurs.
		*
		* @exception Erlang.Exit if an exit signal is
		* received from a process on the peer node.
		*
		* @exception OtpAuthException if the remote node
		* sends a message containing an invalid cookie.
		*
		* @exception InterruptedException if no message if the method
		* times out before a message becomes available.
		**/
		public virtual Erlang.Object receive(long timeout)
		{
			try
			{
				OtpMsg msg = receiveMsg(timeout);
                return msg == null ? null : msg.getMsg();
			}
			catch (Erlang.DecodeException e)
			{
				close();
				throw new System.IO.IOException(e.Message);
			}
		}
		
		/*
		* Receive a raw (still encoded) message from a remote process.
		* This message blocks until a valid message is received or an
		* exception is raised.
		*
		* <p> If the remote node sends a message that cannot be decoded
		* properly, the connection is closed and the method throws an
		* exception.
		*
		* @return an object containing a raw (still encoded) Erlang term.
		*
		* @exception C#.io.IOException if the connection is not active or
		* a communication error occurs.
		*
		* @exception Erlang.Exit if an exit signal is received from a
		* process on the peer node, or if the connection is lost for any
		* reason.
		*
		* @exception OtpAuthException if the remote node
		* sends a message containing an invalid cookie.
		**/
		public virtual OtpInputStream receiveBuf()
		{
			return receiveBuf(-1);
		}
		
		
		/*
		* Receive a raw (still encoded) message from a remote process. This
		* message blocks at most for the specified time until a valid
		* message is received or an exception is raised.
		*
		* <p> If the remote node sends a message that cannot be decoded
		* properly, the connection is closed and the method throws an
		* exception.
		*
		* @param timeout the time in milliseconds that this operation will
		* block. Specify 0 to poll the queue.
		*
		* @return an object containing a raw (still encoded) Erlang term.
		*
		* @exception C#.io.IOException if the connection is not active or
		* a communication error occurs.
		*
		* @exception Erlang.Exit if an exit signal is received from a
		* process on the peer node, or if the connection is lost for any
		* reason.
		*
		* @exception OtpAuthException if the remote node
		* sends a message containing an invalid cookie.
		*
		* @exception InterruptedException if no message if the method
		* times out before a message becomes available.
		**/
		public virtual OtpInputStream receiveBuf(long timeout)
		{
            OtpMsg str = receiveMsg(timeout);
            return str != null ? str.getMsgBuf() : null;
        }
		
		/*
		* Receive a messge complete with sender and recipient information.
		*
		* @return an {@link OtpMsg OtpMsg} containing the header
		* information about the sender and recipient, as well as the actual
		* message contents.
		*
		* @exception C#.io.IOException if the connection is not active or
		* a communication error occurs.
		*
		* @exception Erlang.Exit if an exit signal is received from a
		* process on the peer node, or if the connection is lost for any
		* reason.
		*
		* @exception OtpAuthException if the remote node
		* sends a message containing an invalid cookie.
		**/
		public virtual OtpMsg receiveMsg()
		{
            return receiveMsg(-1);
        }
		
		
		/*
		* Receive a messge complete with sender and recipient information.
		* This method blocks at most for the specified time.
		*
		* @param timeout the time in milliseconds that this operation will
		* block. Specify 0 to poll the queue.
		*
		* @return an {@link OtpMsg OtpMsg} containing the header
		* information about the sender and recipient, as well as the actual
		* message contents.
		*
		* @exception C#.io.IOException if the connection is not active or
		* a communication error occurs.
		*
		* @exception Erlang.Exit if an exit signal is received from a
		* process on the peer node, or if the connection is lost for any
		* reason.
		* 
		* @exception OtpAuthException if the remote node
		* sends a message containing an invalid cookie.
		*
		* @exception InterruptedException if no message if the method
		* times out before a message becomes available.
		**/
		public virtual OtpMsg receiveMsg(long timeout)
		{
            System.Object o = timeout == -1 ? queue.get() : queue.get(timeout);
			
			if (o is OtpMsg)
			{
				return ((OtpMsg) o);
			}
			else if (o is System.IO.IOException)
			{
				throw (System.IO.IOException) o;
			}
			else if (o is Erlang.Exit)
			{
				throw (Erlang.Exit) o;
			}
			else if (o is OtpAuthException)
			{
				throw (OtpAuthException) o;
			}
			
			return null;
		}
		
		/*
		* Send a message to a process on a remote node.
		*
		* @param dest the Erlang PID of the remote process.
		* @param msg the message to send.
		
		* @exception C#.io.IOException if the connection is not active
		* or a communication error occurs.
		**/
		public virtual void  send(Erlang.Pid dest, Erlang.Object msg)
		{
			// encode and send the message
			base.sendBuf(this._self.pid(), dest, new OtpOutputStream(msg));
		}
		
		
		/*
		* Send a message to a named process on a remote node.
		*
		* @param dest the name of the remote process.
		* @param msg the message to send.
		*
		* @exception C#.io.IOException if the connection is not active or
		* a communication error occurs.
		**/
		public virtual void  send(System.String dest, Erlang.Object msg)
		{
			// encode and send the message
			base.sendBuf(this._self.pid(), dest, new OtpOutputStream(msg));
		}
		
		
		/*
		* Send a pre-encoded message to a named process on a remote node.
		*
		* @param dest the name of the remote process.
		* @param payload the encoded message to send.
		*
		* @exception C#.io.IOException if the connection is not active or
		* a communication error occurs.
		**/
		public virtual void  sendBuf(System.String dest, OtpOutputStream payload)
		{
			base.sendBuf(this._self.pid(), dest, payload);
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
		public virtual void  sendBuf(Erlang.Pid dest, OtpOutputStream payload)
		{
			base.sendBuf(this._self.pid(), dest, payload);
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
		* @param args an array of Erlang terms, to be used as arguments to the function.
		*
		* @exception C#.io.IOException if the connection is not active
		* or a communication error occurs.
		**/
		public virtual void  sendRPC(System.String mod, System.String fun, Erlang.Object[] args)
		{
			sendRPC(mod, fun, new Erlang.List(args));
		}

        public virtual void sendRPC(System.String mod, System.String fun, Erlang.List args)
        {
            sendRPC(_self.pid(), mod, fun, args);
        }

		/*
		* Create a link between the local node and the specified process on
		* the remote node. If the link is still active when the remote
		* process terminates, an exit signal will be sent to this
		* connection. Use {@link #unlink unlink()} to remove the link.
		*
		* @param dest the Erlang PID of the remote process.
		*
		* @exception C#.io.IOException if the connection is not active
		* or a communication error occurs.
		**/
		public virtual void  link(Erlang.Pid dest)
		{
			base.sendLink(this._self.pid(), dest);
		}
		
		
		/*
		* Remove a link between the local node and the specified process on
		* the remote node. This method deactivates links created with
		* {@link #link link()}.
		*
		* @param dest the Erlang PID of the remote process.
		*
		* @exception C#.io.IOException if the connection is not active or
		* a communication error occurs.
		**/
		public virtual void  unlink(Erlang.Pid dest)
		{
			base.sendUnlink(this._self.pid(), dest);
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
		public virtual void  exit(Erlang.Pid dest, System.String reason)
		{
			base.sendExit2(this._self.pid(), dest, reason);
		}
	}
}