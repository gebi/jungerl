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
namespace Otp.Erlang
{
	/*
	* Exception raised when a communication channel is broken. This can
	* be caused for a number of reasons, for example:
	
	<ul>
	<li> an error in communication has occurred
	<li> a remote process has sent an exit signal
	<li> a linked process has exited
	</ul>
	
	* @see OtpConnection
	**/
	using System;
	
	public class Exit:ErlangException
	{
		internal Pid pid = null;
		
		/*
		* Create an Exit exception with the given reason.
		*
		* @param reason the reason this exit signal has been sent.
		**/
		public Exit(System.String reason):base(reason)
		{
		}
		
		/*
		* Create an Exit exception with the given reason and sender pid.
		*
		* @param reason the reason this exit signal has been sent.
		*
		* @param pid the pid that sent this exit.
		**/
		public Exit(System.String reason, Pid pid):base(reason)
		{
			this.pid = pid;
		}
		
		/*
		* Get the reason associated with this exit signal.
		**/
		public virtual System.String reason()
		{
			return Message;
		}
		
		/*
		* Get the pid that sent this exit.
		**/
		public virtual Pid getPid()
		{
			return this.pid;
		}
	}
}