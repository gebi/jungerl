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
	* Represents a local OTP client or server node. It is used when you
	* want other nodes to be able to establish connections to this one.
	*
	* When you create an instance of this class, it will bind a socket to
	* a port so that incoming connections can be accepted. However the
	* port number will not be made available to other nodes wishing to
	* connect until you explicitely register with the port mapper daemon
	* by calling {@link #publishPort()}.
	* 
	* <p> When the C# node will be connecting to a remote Erlang, C#
	* or C node, it must first identify itself as a node by creating an
	* instance of this class, after which it may connect to the remote
	* node.
	* 
	* <p> Setting up a connection may be done as follows:
	*
	* 
	* <pre>
	* OtpServer self = new OtpServer("server","cookie"); // identify self
	* self.publishPort(); // make port information available
	* 
	* OtpConnection conn = self.accept(); // get incoming connection
	* </pre>
	*
	* @see OtpSelf
	*
	@deprecated the functionality of this class has been moved to {@link OtpSelf}.
	**/
	public class OtpServer:OtpSelf
	{
		/*Create an {@link OtpServer} from an existing {@link OtpSelf}.
		*
		* @param self an existing self node.
		*
		* @exception C#.io.IOException if a ServerSocket could not be created.
		*
		**/
		public OtpServer(OtpSelf self):base(self.node(), self.cookie())
		{
		}
		
		/*
		* Create an OtpServer, using a vacant port chosen by the operating
		* system. To determine what port was chosen, call the object's
		* {@link #port()} method.
		*
		* @param node the name of the node.
		*
		* @param cookie the authorization cookie that will be used by this
		* node when accepts connections from remote nodes.
		*
		* @exception C#.io.IOException if a ServerSocket could not be created.
		*
		**/
		public OtpServer(System.String node, System.String cookie):base(node, cookie)
		{
		}
		
		/*
		* Create an OtpServer, using the specified port number.
		*
		* @param node a name for this node, as above.
		*
		* @param cookie the authorization cookie that will be used by this
		* node when accepts connections from remote nodes.
		*
		* @param port the port number to bind the socket to.
		*
		* @exception C#.io.IOException if a ServerSocket could not be
		* created or if the chosen port number was not available.
		*
		**/
		public OtpServer(System.String node, System.String cookie, int port):base(node, cookie, port)
		{
		}
	}
}