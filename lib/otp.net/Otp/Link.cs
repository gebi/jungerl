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
	// package scope
	using System;

	public class Link
	{
		private Erlang.Pid _local;
		private Erlang.Pid _remote;
		
		public Link(Erlang.Pid local, Erlang.Pid remote)
		{
			this._local = local;
			this._remote = remote;
		}
		
		public virtual Erlang.Pid local()
		{
			return _local;
		}
		
		public virtual Erlang.Pid remote()
		{
			return _remote;
		}
		
		public virtual bool contains(Erlang.Pid pid)
		{
			return (this._local.Equals(pid) || this._remote.Equals(pid));
		}
		
		public virtual bool equals(Erlang.Pid local, Erlang.Pid remote)
		{
			return ((this._local.Equals(local) && this._remote.Equals(remote)) ||
					(this._local.Equals(remote) && this._remote.Equals(local)));
		}
	}
}