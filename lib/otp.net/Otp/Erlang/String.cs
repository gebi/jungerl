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
	* Provides a C# representation of Erlang strings. 
	**/
	[Serializable]
    public class String:Erlang.Object
	{
		private System.String str;
		
		/*
		* Create an Erlang string from the given string.
		**/
		public String(System.String str)
		{
			this.str = str;
		}
		
		/*
		* Create an Erlang string from a stream containing a string encoded in
		* Erlang external format.
		*
		* @param buf the stream containing the encoded string.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang string.
		**/
		public String(OtpInputStream buf)
		{
			str = buf.read_string();
		}
		
		/*
		* Get the actual string contained in this object.
		*
		* @return the raw string contained in this object, without regard
		* to Erlang quoting rules.
		* 
		* @see #toString
		**/
		public virtual System.String stringValue()
		{
			return str;
		}
		
		/*
		* Get the printable version of the string contained in this object.
		*
		* @return the string contained in this object, quoted.
		* 
		* @see #stringValue
		**/
		public override System.String ToString()
		{
			return "\"" + str + "\"";
		}
		
		/*
		* Convert this string to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded string should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			buf.write_string(str);
		}
		
		/*
		* Determine if two strings are equal. They are equal if they
		* represent the same sequence of characters. This method can be
		* used to compare Strings with each other and with
		* Strings.
		*
		* @param o the String or String to compare to.
		*
		* @return true if the strings consist of the same sequence of
		* characters, false otherwise. 
		**/
		public override bool Equals(System.Object o)
		{
			if (o is System.String)
			{
				return this.str.CompareTo((System.String) o) == 0;
			}
			else if (o is String)
			{
				return this.str.CompareTo(((String) o).str) == 0;
			}
			
			return false;
		}

		public override int GetHashCode()
		{
			return 1;
		}
	}
}