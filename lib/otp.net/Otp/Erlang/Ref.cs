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
	* Provides a C# representation of Erlang refs. There are two
	* styles of Erlang refs, old style (one id value) and new style
	* (array of id values). This class manages both types.
	**/
	[Serializable]
    public class Ref:Erlang.Object
	{
		private System.String _node;
		private int _creation;
		
		// old style refs have one 18-bit id
		// r6 "new" refs have array of ids, first one is only 18 bits however
		private int[] _ids = null;
		
		/*
		* Create a unique Erlang ref belonging to the local node. 
		*
		* @param self the local node.
		*
		@deprecated use OtpLocalNode:createRef() instead
		**/
		public Ref(OtpLocalNode self)
		{
			Ref r = self.createRef();
			
			this._ids = r._ids;
			this._creation = r._creation;
			this._node = r._node;
		}
		
		/*
		* Create an Erlang ref from a stream containing a ref encoded in
		* Erlang external format.
		*
		* @param buf the stream containing the encoded ref.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang ref.
		**/
		public Ref(OtpInputStream buf)
		{
			Ref r = buf.read_ref();
			
			this._node = r.node();
			this._creation = r.creation();
			
			this._ids = r.ids();
		}
		
		/*
		* Create an old style Erlang ref from its components.
		*
		* @param node the nodename.
		*
		* @param id an arbitrary number. Only the low order 18 bits will
		* be used.
		*
		* @param creation another arbitrary number. Only the low order
		* 2 bits will be used.
		**/
		public Ref(System.String node, int id, int creation)
		{
			this._node = node;
			this._ids = new int[1];
			this._ids[0] = id & 0x3ffff; // 18 bits
			this._creation = creation & 0x03; // 2 bits
		}
		
		/*
		* Create a new style Erlang ref from its components.
		*
		* @param node the nodename.
		*
		* @param ids an array of arbitrary numbers. Only the low order 18
		* bits of the first number will be used. If the array contains only
		* one number, an old style ref will be written instead. At most
		* three numbers will be read from the array.
		*
		* @param creation another arbitrary number. Only the low order
		* 2 bits will be used.
		**/
		public Ref(System.String node, int[] ids, int creation)
		{
			this._node = node;
			this._creation = creation & 0x03; // 2 bits
			
			// use at most 82 bits (18 + 32 + 32)
			int len = (int) (ids.Length);
			this._ids = new int[3];
			this._ids[0] = 0;
			this._ids[1] = 0;
			this._ids[2] = 0;
			
			if (len > 3)
				len = 3;
			Array.Copy(ids, 0, this._ids, 0, len);
			this._ids[0] &= 0x3ffff; // only 18 significant bits in first number
		}
		
		/*
		* Get the id number from the ref. Old style refs have only one id
		* number. If this is a new style ref, the first id number is returned.
		*
		* @return the id number from the ref.
		**/
		public virtual int id()
		{
			return _ids[0];
		}
		
		/*
		* Get the array of id numbers from the ref. If this is an old style
		* ref, the array is of length 1. If this is a new style ref, the
		* array has length 3.
		*
		* @return the array of id numbers from the ref.
		**/
		public virtual int[] ids()
		{
			return _ids;
		}
		
		/*
		* Determine whether this is a new style ref.
		*
		* @return true if this ref is a new style ref, false otherwise.
		**/
		public virtual bool isNewRef()
		{
			return (_ids.Length > 1);
		}
		
		/*
		* Get the creation number from the ref.
		*
		*  @return the creation number from the ref.
		**/
		public virtual int creation()
		{
			return _creation;
		}
		
		/*
		* Get the node name from the ref.
		*
		* @return the node name from the ref.
		**/
		public virtual System.String node()
		{
			return _node;
		}
		
		/*
		* Get the string representation of the ref. Erlang refs are printed
		* as #Ref&lt;node.id&gt;
		*
		* @return the string representation of the ref.
		**/
		public override System.String ToString()
		{
			System.String s = "#Ref<" + _node;
			
			 for (int i = 0; i < _ids.Length; i++)
			{
				s += "." + _ids[i];
			}
			
			s += ">";
			
			return s;
		}
		
		/*
		* Convert this ref to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded ref should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			buf.write_ref(_node, _ids, _creation);
		}
		
		/*
		* Determine if two refs are equal. Refs are equal if their
		* components are equal. New refs and old refs are considered equal
		* if the node, creation and first id numnber are equal.
		*
		* @param o the other ref to compare to.
		*
		* @return true if the refs are equal, false otherwise.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is Ref))
				return false;
			
			
			Ref ref_Renamed = (Ref) o;
			
			if (!(this._node.Equals(ref_Renamed.node()) && this._creation == ref_Renamed.creation()))
				return false;
			
			if (this.isNewRef() && ref_Renamed.isNewRef())
			{
				return (this._ids[0] == ref_Renamed._ids[0] && this._ids[1] == ref_Renamed._ids[1] && this._ids[2] == ref_Renamed._ids[2]);
			}
			return (this._ids[0] == ref_Renamed._ids[0]);
		}
		
 		public override int GetHashCode()
		{
			return 1;
		}

		public override System.Object clone()
		{
			Ref newRef = (Ref) (base.clone());
			newRef._ids = new int[_ids.Length];
			_ids.CopyTo(newRef._ids, 0);
			return newRef;
		}
	}
}