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
    public class Boolean:Erlang.Atom
	{
		/*
		* Create a boolean from the given value
		*
		* @param t the boolean value to represent as an atom.
		**/
		public Boolean(bool t):base(t)
		{
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
		public Boolean(OtpInputStream buf):base(buf)
		{
		}
	}
}