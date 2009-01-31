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
 *  Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 *
 * Converted from Java to C# by Vlad Dumitrescu (vlad_Dumitrescu@hotmail.com)
 */
namespace Otp
{
	/*
	* This class represents local node types. It is used to group the
	* node types {@link OtpNode OtpNode} and {@link OtpSelf OtpSelf}.
	**/
	using System;

	public class OtpLocalNode:AbstractNode
	{
		private int serial = 0;
		private int pidCount = 1;
		private int portCount = 1;
		private int[] refId;
		
		protected internal int _port;
		protected internal System.Net.Sockets.TcpClient epmd;
        public static bool ignoreLocalEpmdConnectErrors = false;
        // handle status changes
        protected OtpNodeStatus handler;

        /*
        * Create a node with the given name and the default cookie.
        **/
        protected internal OtpLocalNode(System.String node)
            : base(node)
        {
            init();
        }

        /*
        * Create a node with the given name and cookie.
        **/
        protected internal OtpLocalNode(System.String node, System.String cookie)
            : base(node, cookie, false)
        {
            init();
        }

        protected internal OtpLocalNode(System.String node, System.String cookie, bool shortName)
            : base(node, cookie, shortName)
        {
            init();
        }

        private void init()
		{
			serial = 0;
			pidCount = 1;
			portCount = 1;
			refId = new int[3];
			refId[0] = 1;
			refId[1] = 0;
			refId[2] = 0;
            handler = new OtpNodeStatus();
		}

        /*
        * Register interest in certain system events. The {@link
        * OtpNodeStatus OtpNodeStatus} handler object contains callback
        * methods, that will be called when certain events occur. 
        *
        * @param handler the callback object to register. To clear the
        * handler, specify null as the handler to use.
        *
        **/
        public virtual void registerStatusHandler(OtpNodeStatus handler)
        {
            lock (this)
            {
                this.handler = handler;
            }
        }

        public void registerStatusHandler(OtpNodeStatus.ConnectionStatusDelegate callback)
        {
            this.handler.registerStatusHandler(callback);
        }

        /*use these wrappers to call handler functions */
        internal void remoteStatus(System.String node, bool up, System.Object info)
        {
            lock (this)
            {
                if (handler == null)
                    return;
                try
                {
                    handler.remoteStatus(node, up, info);
                }
                catch (System.Exception)
                {
                }
            }
        }

        internal void localStatus(System.String node, bool up, System.Object info)
        {
            lock (this)
            {
                if (handler == null)
                    return;
                try
                {
                    handler.localStatus(node, up, info);
                }
                catch (System.Exception)
                {
                }
            }
        }

        internal void connAttempt(System.String node, bool incoming, System.Object info)
        {
            lock (this)
            {
                if (handler == null)
                    return;
                try
                {
                    handler.connAttempt(node, incoming, info);
                }
                catch (System.Exception)
                {
                }
            }
        }

        internal void epmdFailedConnAttempt(System.String node, System.Object info)
        {
            lock (this)
            {
                if (handler == null)
                    return;
                try
                {
                    handler.epmdFailedConnAttempt(node, info);
                }
                catch (System.Exception)
                {
                }
            }
        }

        /*
        * Get the port number used by this node.
        *
        * @return the port number this server node is accepting
        * connections on.
        **/
		public virtual int port()
		{
			return this._port;
		}
		
		/*
		* Set the Epmd socket after publishing this nodes listen port to
		* Epmd.
		*
		* @param s The socket connecting this node to Epmd.
		**/
		protected internal virtual void  setEpmd(System.Net.Sockets.TcpClient s)
		{
			this.epmd = s;
		}
		
		/*
		* Get the Epmd socket.
		*
		* @return The socket connecting this node to Epmd.
		**/
		protected internal virtual System.Net.Sockets.TcpClient getEpmd()
		{
			return epmd;
		}
		
		/*
		* Create an Erlang {@link Pid pid}. Erlang pids are based
		* upon some node specific information; this method creates a pid
		* using the information in this node. Each call to this method
		* produces a unique pid.
		*
		* @return an Erlang pid.
		**/
		public virtual Erlang.Pid createPid()
		{
			lock(this)
			{
				Erlang.Pid p = new Erlang.Pid(_node, pidCount, serial, _creation);
				
				pidCount++;
				if (pidCount > 0x7fff)
				{
					pidCount = 0;
					
					serial++;
					if (serial > 0x07)
					{
						serial = 0;
					}
				}
				
				return p;
			}
		}
		
		/*
		* Create an Erlang {@link Port port}. Erlang ports are
		* based upon some node specific information; this method creates a
		* port using the information in this node. Each call to this method
		* produces a unique port. It may not be meaningful to create a port
		* in a non-Erlang environment, but this method is provided for
		* completeness.
		*
		* @return an Erlang port.
		**/
		public virtual Erlang.Port createPort()
		{
			lock(this)
			{
				Erlang.Port p = new Erlang.Port(_node, portCount, _creation);
				
				portCount++;
				if (portCount > 0x3ffff)
					portCount = 0;
				
				return p;
			}
		}
		
		
		/*
		* Create an Erlang {@link Ref reference}. Erlang
		* references are based upon some node specific information; this
		* method creates a reference using the information in this node.
		* Each call to this method produces a unique reference.
		*
		* @return an Erlang reference.
		**/
		public virtual Erlang.Ref createRef()
		{
			lock(this)
			{
				Erlang.Ref r = new Erlang.Ref(_node, refId, _creation);
				
				// increment ref ids (3 ints: 18 + 32 + 32 bits)
				refId[0]++;
				if (refId[0] > 0x3ffff)
				{
					refId[0] = 0;
					
					refId[1]++;
					if (refId[1] == 0)
					{
						refId[2]++;
					}
				}
				
				return r;
			}
		}
    }
}