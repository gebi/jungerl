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
	* Represents a remote OTP node. It acts only as a container for the
	* nodename and other node-specific information that is needed by the
	* {@link OtpConnection} class.
	**/
	public class OtpPeer:AbstractNode
	{
		internal int distChoose = 0; /*this is set by OtpConnection
		* and is the highest common
		* protocol version we both support
		*/
		
		internal OtpPeer():base()
		{
		}
		
		/*
		* Create a peer node.
		*
		* @param node the name of the node.
		**/
		public OtpPeer(System.String node):base(node)
		{
		}
		
		/*
		* Create a connection to a remote node.
		*
		* @param self the local node from which you wish to connect.
		*
		* @return a connection to the remote node.
		*
		* @exception java.net.UnknownHostException if the remote host could
		* not be found.
		
		* @exception java.io.IOException if it was not possible to connect
		* to the remote node.
		
		* @exception OtpAuthException if the connection was refused by the
		* remote node.
		*
		@deprecated Use the corresponding method in {@link OtpSelf} instead.
		**/
		public virtual OtpConnection connect(OtpSelf self)
		{
			return new OtpConnection(self, this);
		}
		
		// package 
		/*
		* Get the port number used by the remote node.
		*
		* @return the port number used by the remote node, or 0 if the node
		* was not registered with the port mapper.
		*
		* @exception java.io.IOException if the port mapper could not be contacted.
		**/
		internal virtual int port()
		{
			return OtpEpmd.lookupPort(this);
		}
	}
}