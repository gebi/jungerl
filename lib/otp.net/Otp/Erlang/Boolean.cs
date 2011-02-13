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
	* Provides a C# representation of Erlang booleans, which are
	* special cases of atoms with values 'true' and 'false'.
	**/
	[Serializable]
    public class Boolean : Erlang.Object
	{
        private bool value;
        internal static Erlang.Atom s_true  = new Erlang.Atom("true");
        internal static Erlang.Atom s_false = new Erlang.Atom("false");

		/*
		* Create a boolean from the given value
		*
		* @param t the boolean value to represent as an atom.
		**/
		public Boolean(bool t)
		{
            this.value = t;
		}
		
		/*
		* Create a boolean from a stream containing an atom encoded in
		* Erlang external format. The value of the boolean will be true if
		* the atom represented by the stream is "true" without regard to
		* case. For other atom values, the boolean will have the value
		* false.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang atom.
		**/
		public Boolean(OtpInputStream buf)
		{
			this.value = buf.read_boolean();
		}
		
        
        /*
        * The boolean value of this instance.
        *
        * @return the value of this atom expressed as a boolean value. If
        * the atom consists of the characters "true" (independent of case)
        * the value will be true. For any other values, the value will be
        * false.
        *
        **/
        public bool booleanValue()
        {
            return value;
        }

		/*The maximun allowed length of an atom, in characters */
		public const int maxAtomLength = 0xff; // one byte length
		
		/*
		* Get the printname of the atom represented by this object. The
		* difference between this method and {link #atomValue atomValue()}
		* is that the printname is quoted and escaped where necessary,
		* according to the Erlang rules for atom naming.
		*
		* @return the printname representation of this atom object.
		*
		* @see #atomValue
		**/
		public override System.String ToString()
		{
            return value ? "true" : "false";
		}
		
		/*
		* Determine if two atoms are equal.
		*
		* @param o the other object to compare to.
		*
		* @return true if the atoms are equal, false otherwise.
		**/
		public override bool Equals(System.Object o)
		{
            if (o is Erlang.Boolean)
                return value == (o as Erlang.Boolean).booleanValue();
            else if (o is Erlang.Atom)
                return value == ((o as Erlang.Atom) == s_true);
			else
				return false;
        }

		public override int GetHashCode()
		{
			return 1;
		}

		/*
		* Convert this boolean to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded atom should be
		* written.
		**/
		public override void encode(OtpOutputStream buf)
		{
			buf.write_boolean(this.value);
		}
    }
}