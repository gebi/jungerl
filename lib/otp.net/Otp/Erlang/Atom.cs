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
	* Provides a Java representation of Erlang atoms. Atoms can be
	* created from strings whose length is not more than {@link
	* #maxAtomLength maxAtomLength} characters.
	**/
	[Serializable]
	public class Atom:Erlang.Object
	{
		/*The maximun allowed length of an atom, in characters */
		public const int maxAtomLength = 0xff; // one byte length
		
		private System.String atom;
		
		
		/*
		* Create an atom from the given string.
		*
		* @param atom the string to create the atom from.
		* 
		* @exception java.lang.IllegalArgumentException if the string is
		* empty ("") or contains more than {@link #maxAtomLength
		* maxAtomLength} characters.
		**/
		public Atom(System.String atom)
		{
			if (atom == null || atom.Length < 1)
			{
				throw new System.ArgumentException("Atom must be non-empty");
			}
			
			if (atom.Length > maxAtomLength)
			{
				throw new System.ArgumentException("Atom may not exceed " + maxAtomLength + " characters");
			}
			this.atom = atom;
		}
		
		/*
		* Create an atom from a stream containing an atom encoded in Erlang
		* external format.
		*
		* @param buf the stream containing the encoded atom.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang atom.
		**/
		public Atom(OtpInputStream buf)
		{
			this.atom = buf.read_atom();
		}
		
		/*
		* Create an atom whose value is "true" or "false".
		**/
		public Atom(bool t)
		{
			this.atom = t.ToString();
		}
		
		/*
		* Get the actual string contained in this object.
		* 
		* @return the raw string contained in this object, without regard
		* to Erlang quoting rules.
		* 
		* @see #toString
		**/
		public virtual System.String atomValue()
		{
			return atom;
		}
		
		/*
		* The boolean value of this atom.
		*
		* @return the value of this atom expressed as a boolean value. If
		* the atom consists of the characters "true" (independent of case)
		* the value will be true. For any other values, the value will be
		* false.
		*
		**/
		public virtual bool booleanValue()
		{
			return System.Boolean.Parse(atomValue());
		}
		
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
			if (atomNeedsQuoting(atom))
			{
				return "'" + escapeSpecialChars(atom) + "'";
			}
			else
			{
				return atom;
			}
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
			
			if (!(o is Erlang.Atom))
				return false;
			
			Erlang.Atom atom = (Erlang.Atom) o;
			return this.atom.CompareTo(atom.atom) == 0;
		}

		public override int GetHashCode()
		{
			return 1;
		}

		/*
		* Convert this atom to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded atom should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			buf.write_atom(this.atom);
		}
		
		/*the following four predicates are helpers for the toString() method */
		private bool isErlangDigit(char c)
		{
			return (c >= '0' && c <= '9');
		}
		
		private bool isErlangUpper(char c)
		{
			return ((c >= 'A' && c <= 'Z') || (c == '_'));
		}
		
		private bool isErlangLower(char c)
		{
			return (c >= 'a' && c <= 'z');
		}
		
		private bool isErlangLetter(char c)
		{
			return (isErlangLower(c) || isErlangUpper(c));
		}
		
		// true if the atom should be displayed with quotation marks
		private bool atomNeedsQuoting(System.String s)
		{
			char c;
			
			if (s.Length == 0)
				return true;
			if (!isErlangLower(s[0]))
				return true;
			
			int len = s.Length;
			 for (int i = 1; i < len; i++)
			{
				c = s[i];
				
				if (!isErlangLetter(c) && !isErlangDigit(c) && c != '@')
					return true;
			}
			return false;
		}
		
		/*Get the atom string, with special characters escaped. Note that
		* this function currently does not consider any characters above
		* 127 to be printable.
		*/
		private System.String escapeSpecialChars(System.String s)
		{
			char c;
			System.Text.StringBuilder so = new System.Text.StringBuilder();
			
			int len = s.Length;
			 for (int i = 0; i < len; i++)
			{
				c = s[i];
				
				
				/*note that some of these escape sequences are unique to
				* Erlang, which is why the corresponding 'case' values use
				* octal. The resulting string is, of course, in Erlang format.
				*/
				
				switch (c)
				{
					case '\b': 
						so.Append("\\b");
						break;

					case (char) (127):
						so.Append("\\d");
						break;

					case (char) (27):
						so.Append("\\e");
						break;

					case '\f':
						so.Append("\\f");
						break;

					case '\n':
						so.Append("\\n");
						break;

					case '\r':
						so.Append("\\r");
						break;

					case '\t':
						so.Append("\\t");
						break;

					case (char) (11):
						so.Append("\\v");
						break;

					case '\\':
						so.Append("\\\\");
						break;

					case '\'':
						so.Append("\\'");
						break;

					case '\"':
						so.Append("\\\"");
						break;

					default:
						// some other character classes
						if (c < 23)
						{
							// control chars show as "\^@", "\^A" etc
							so.Append("\\^" + (char) (('A' - 1) + c));
						}
						else if (c > 126)
						{
							// 8-bit chars show as \345 \344 \366 etc
							so.Append("\\" + System.Convert.ToString(c, 8));
						}
						else
						{
							// character is printable without modification!
							so.Append(c);
						}
						break;

				}
			}
			return new System.String(so.ToString().ToCharArray());
		}
	}
}