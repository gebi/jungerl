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
	using System;
	
	/*
	* Provides a C# representation of Erlang PIDs. PIDs represent
	* Erlang processes and consist of a nodename and a number of
	* integers.
	**/
	[Serializable]
    public class Pid:Erlang.Object
	{
		private System.String _node;
		private int _id;
		private int _serial;
		private int _creation;
		
		/*
		* Create a unique Erlang PID belonging to the local node.
		*
		* @param self the local node.
		*
		@deprecated use OtpLocalNode:createPid() instead
		**/
		public Pid(OtpLocalNode self)
		{
			Pid p = self.createPid();
			
			this._id = p._id;
			this._serial = p._serial;
			this._creation = p._creation;
			this._node = p._node;
		}
		
		/*
		* Create an Erlang PID from a stream containing a PID encoded in
		* Erlang external format.
		*
		* @param buf the stream containing the encoded PID.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang PID.
		**/
		public Pid(OtpInputStream buf)
		{
			Pid p = buf.read_pid();
			
			this._node = p.node();
			this._id = p.id();
			this._serial = p.serial();
			this._creation = p.creation();
		}
		
		/*
		* Create an Erlang pid from its components.
		*
		* @param node the nodename.
		*
		* @param id an arbitrary number. Only the low order 15 bits will
		* be used.
		*
		* @param serial another arbitrary number. Only the low order 3 bits
		* will be used.
		*
		* @param creation yet another arbitrary number. Only the low order
		* 2 bits will be used.
		**/
		public Pid(System.String node, int id, int serial, int creation)
		{
			this._node = node;
			this._id = id & 0x7fff; // 15 bits
			this._serial = serial & 0x07; // 3 bits
			this._creation = creation & 0x03; // 2 bits
		}
		
		/*
		* Get the serial number from the PID.
		*
		* @return the serial number from the PID.
		**/
		public virtual int serial()
		{
			return _serial;
		}
		
		/*
		* Get the id number from the PID.
		*
		* @return the id number from the PID.
		**/
		public virtual int id()
		{
			return _id;
		}
		
		/*
		* Get the creation number from the PID.
		*
		* @return the creation number from the PID.
		**/
		public virtual int creation()
		{
			return _creation;
		}
		
		/*
		* Get the node name from the PID.
		*
		* @return the node name from the PID.
		**/
		public virtual System.String node()
		{
			return _node;
		}
		
		/*
		* Get the string representation of the PID. Erlang PIDs are printed
		* as #Pid&lt;node.id.serial&gt;
		*
		* @return the string representation of the PID.
		**/
		public override System.String ToString()
		{
			return "#Pid<" + _node.ToString() + "." + _id + "." + _serial + ">";
		}
		
		/*
		* Convert this PID to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded PID should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			buf.write_pid(_node, _id, _serial, _creation);
		}
		
		/*
		* Return the hashCode for this Pid.
		*
		* @return the hashCode for this Pid.
		**/
		public virtual int hashCode()
		{
			return _id;
		}
		
		/*
		* Determine if two PIDs are equal. PIDs are equal if their
		* components are equal.
		*
		* @param port the other PID to compare to.
		*
		* @return true if the PIDs are equal, false otherwise.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is Pid))
				return false;
			
			Pid pid = (Pid) o;
			
			return ((this._creation == pid._creation) && (this._serial == pid._serial) && (this._id == pid._id) && (_node.CompareTo(pid._node) == 0));
		}
 
		public override int GetHashCode()
		{
			return 1;
		}
	}
}