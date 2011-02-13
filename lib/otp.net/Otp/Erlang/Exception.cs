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
	* Exception raised when an attempt is made to create an Erlang term
	* by decoding a sequence of bytes that does not represent the type of
	* term that was requested.
	*
	* @see OtpInputStream
	**/
	using System;

	public class Exception : ErlangException
	{
		/*
		* Provides a detailed message.
		**/
		public Exception(System.String msg):base(msg)
		{
		}
	}

    public class EncodeException : ErlangException
    {
        /*
        * Provides a detailed message.
        **/
        public EncodeException(System.String msg)
            : base(msg)
        {
        }
    }

    public class CastException : ErlangException
    {
        public CastException() : base() { }
        public CastException(System.String msg) : base(msg) { }
    }

    /*
	* Exception raised when an attempt is made to create an Erlang term
	* with data that is out of range for the term in question.
	*
	* @see Byte
	* @see Char
	* @see Int
	* @see UInt
	* @see Short
	* @see UShort
	* @see Long
	**/
	public class RangeException : ErlangException
	{
		/*
		* Provides a detailed message.
		**/
		public RangeException(System.String msg):base(msg)
		{
		}
	}

    /// <summary>
    /// Exception raised when an attempt is made to reference an
    /// unbound variable.
    /// </summary>
    public class UnboundVarException : ErlangException
	{
        public UnboundVarException() : base() {}

		/*
		* Provides a detailed message.
		**/
        public UnboundVarException(System.String msg)
            : base(msg)
		{
		}
	}

    public class FormatException : ErlangException
    {
        public FormatException() : base() {}
        public FormatException(string msg) : base(msg) {}
    }
}