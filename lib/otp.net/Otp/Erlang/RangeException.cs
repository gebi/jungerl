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
	using System;
	public class RangeException:ErlangException
	{
		/*
		* Provides a detailed message.
		**/
		public RangeException(System.String msg):base(msg)
		{
		}
	}
}