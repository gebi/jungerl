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
	* Provides a C# representation of Erlang ports. 
	**/
	[Serializable]
    public class Port:Erlang.Object
	{
		private System.String _node;
		private int _id;
		private int _creation;
		
		/*
		* Create a unique Erlang port belonging to the local node. Since it isn't 
		* meaninful to do so, this constructor is private...
		*
		* @param self the local node.
		*
		@deprecated use OtpLocalNode:createPort() instead
		*/
		private Port(OtpSelf self)
		{
			Port p = self.createPort();
			
			this._id = p._id;
			this._creation = p._creation;
			this._node = p._node;
		}
		
		/*
		* Create an Erlang port from a stream containing a port encoded in
		* Erlang external format.
		*
		* @param buf the stream containing the encoded port.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang port.
		**/
		public Port(OtpInputStream buf)
		{
			Port p = buf.read_port();
			
			this._node = p.node();
			this._id = p.id();
			this._creation = p.creation();
		}
		
		/*
		* Create an Erlang port from its components.
		*
		* @param node the nodename.
		*
		* @param id an arbitrary number. Only the low order 18 bits will
		* be used.
		*
		* @param creation another arbitrary number. Only the low order
		* 2 bits will be used.
		**/
		public Port(System.String node, int id, int creation)
		{
			this._node = node;
			this._id = id & 0x3ffff; // 18 bits
			this._creation = creation & 0x03; // 2 bits
		}
		
		/*
		* Get the id number from the port.
		*
		*  @return the id number from the port.
		**/
		public virtual int id()
		{
			return _id;
		}
		
		/*
		* Get the creation number from the port.
		*
		*  @return the creation number from the port.
		**/
		public virtual int creation()
		{
			return _creation;
		}
		
		/*
		* Get the node name from the port.
		*
		*  @return the node name from the port.
		**/
		public virtual System.String node()
		{
			return _node;
		}
		
		/*
		* Get the string representation of the port. Erlang ports are printed
		* as #Port&lt;node.id&gt;.
		*
		* @return the string representation of the port.
		**/
		public override System.String ToString()
		{
			return "#Port<" + _node + "." + _id + ">";
		}
		
		/*
		* Convert this port to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded port should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			buf.write_port(_node, _id, _creation);
		}
		
		/*
		* Determine if two ports are equal. Ports are equal if their
		* components are equal.
		*
		* @param o the other port to compare to.
		*
		* @return true if the ports are equal, false otherwise.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is Port))
				return false;
			
			Port port = (Port) o;
			
			return ((this._creation == port._creation) && (this._id == port._id) && (_node.CompareTo(port._node) == 0));
		}

		public override int GetHashCode()
		{
			return 1;
		}
	}
}
