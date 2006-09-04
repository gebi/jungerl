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
	* Provides a C# representation of Erlang floats and doubles. Erlang
	* defines only one floating point numeric type, however this class
	* and its subclass {@link Float} are used to provide
	* representations corresponding to the C# types Double and Float.
	**/
	[Serializable]
    public class Double:Erlang.Object
	{
		private double d;
		
		/*
		* Create an Erlang float from the given double value.
		**/
		public Double(double d)
		{
			this.d = d;
		}
		
		/*
		* Create an Erlang float from a stream containing a double encoded
		* in Erlang external format.
		*
		* @param buf the stream containing the encoded value.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang float.
		**/
		public Double(OtpInputStream buf)
		{
            this.d = buf.read_double();
            
            //System.Diagnostics.Debug.WriteLine("Double: " + this.d );
		}
		
		/*
		* Get the value, as a double.
		*
		* @return the value of this object, as a double.
		**/
		public virtual double doubleValue()
		{
			return d;
		}
		
		/*
		* Get the value, as a float.
		*
		* @return the value of this object, as a float.
		*
		* @exception RangeException if the value cannot be
		* represented as a float.
		**/
		public virtual float floatValue()
		{
			float f = (float) d;
			
			if (f != d)
			{
				throw new RangeException("Value too large for float: " + d);
			}
			
			return f;
		}
		
		/*
		* Get the string representation of this double.
		*
		* @return the string representation of this double.
		**/
		public override System.String ToString()
		{
			return d.ToString();
		}
		
		/*
		* Convert this double to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded value should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			buf.write_double(this.d);
		}
		
		/*
		* Determine if two floats are equal. Floats are equal if they
		* contain the same value.
		*
		* @param o the float to compare to.
		*
		* @return true if the floats have the same value.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is Double))
				return false;
			
			Double d = (Double) o;
			return this.d == d.d;
		}
 
		public override int GetHashCode()
		{
			return 1;
		}
	}
}