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
	* Provides a C# representation of Erlang integral types. Erlang
	* does not distinguish between different integral types, however this
	* class and its subclasses {@link Byte}, {@link
	* Char}, {@link Int}, and {@link Short}
	* attempt to map the Erlang types onto the various C# integral
	* types. Two additional classes, {@link UInt} and {@link
	* UShort} are provided for Corba compatibility. See the
	* documentation for IC for more information.
	**/
	[Serializable]
    public class Long:Erlang.Object
	{
		private long val;
		
		/*
		* Create an Erlang integer from the given value.
		*
		* @param l the long value to use.
		**/
		public Long(long l)
		{
			this.val = l;
		}
		
		/*
		* Create an Erlang integer from a stream containing an integer
		* encoded in Erlang external format.
		*
		* @param buf the stream containing the encoded value.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang integer.
		**/
		public Long(OtpInputStream buf)
		{
			val = buf.read_long();
		}
		
		/*
		* Get this number as a long.
		*
		* @return the value of this number, as a long.
		**/
		public virtual long longValue()
		{
			return val;
		}
		
		/*
		* Get this number as an int.
		*
		* @return the value of this number, as an int.
		*
		* @exception RangeException if the value is too large to
		* be represented as an int.
		**/
		public virtual int intValue()
		{
			int i = (int) val;
			
			if (i != val)
			{
				throw new RangeException("Value too large for int: " + val);
			}
			
			return i;
		}
		
		/*
		* Get this number as a non-negative int.
		*
		* @return the value of this number, as an int.
		*
		* @exception RangeException if the value is too large to
		* be represented as an int, or if the value is negative.
		**/
		public virtual int uIntValue()
		{
			int i = (int) val;
			
			if (i != val)
			{
				throw new RangeException("Value too large for int: " + val);
			}
			else if (i < 0)
			{
				throw new RangeException("Value not positive: " + val);
			}
			
			return i;
		}
		
		/*
		* Get this number as a short.
		*
		* @return the value of this number, as a short.
		*
		* @exception RangeException if the value is too large to
		* be represented as a short.
		**/
		public virtual short shortValue()
		{
			short i = (short) val;
			
			if (i != val)
			{
				throw new RangeException("Value too large for short: " + val);
			}
			
			return i;
		}
		
		/*
		* Get this number as a non-negative short.
		*
		* @return the value of this number, as a short.
		*
		* @exception RangeException if the value is too large to
		* be represented as a short, or if the value is negative.
		**/
		public virtual short uShortValue()
		{
			short i = (short) val;
			
			if (i != val)
			{
				throw new RangeException("Value too large for short: " + val);
			}
			else if (i < 0)
			{
				throw new RangeException("Value not positive: " + val);
			}
			
			return i;
		}
		
		/*
		* Get this number as a char.
		*
		* @return the char value of this number.
		*
		* @exception RangeException if the value is too large to
		* be represented as a char.
		**/
		public virtual char charValue()
		{
			char i = (char) val;
			
			if (i != val)
			{
				throw new RangeException("Value too large for char: " + val);
			}
			
			return i;
		}
		
		
		/*
		* Get this number as a byte.
		*
		* @return the byte value of this number.
		*
		* @exception RangeException if the value is too large to
		* be represented as a byte.
		**/
		public virtual sbyte byteValue()
		{
			sbyte i = (sbyte) val;
			
			if (i != val)
			{
				throw new RangeException("Value too large for byte: " + val);
			}
			
			return i;
		}
		
		/*
		* Get the string representation of this number.
		*
		* @return the string representation of this number.
		**/
		public override System.String ToString()
		{
			return "" + val;
		}
		
		/*
		* Convert this number to the equivalent Erlang external
		* representation.
		*
		* @param buf an output stream to which the encoded number should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			buf.write_long(this.val);
		}
		
		/*
		* Determine if two numbers are equal. Numbers are equal if they
		* contain the same value.
		*
		* @param o the number to compare to.
		*
		* @return true if the numbers have the same value.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is Long))
				return false;
			
			Long l = (Long) o;
			return this.val == l.val;
		}
 
		public override int GetHashCode()
		{
			return 1;
		}
	}
}