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
* Copyright (c) 2011 Serge Aleynikov <saleyn@gmail.com>
*/
namespace Otp.Erlang
{
    using System;

    /*
    * Provides a C# representation of Erlang strings. 
    **/
    [Serializable]
    public class Var : Erlang.Object
    {
        private string m_var;

        /// <summary>
        /// Create an anonymous variable.
        /// </summary>
        public Var()
        {
            this.m_var = s_any;
        }

        /// <summary>
        /// Create an Erlang named variable
        /// </summary>
        /// <param name="name">Variable name</param>
        public Var(string name)
        {
            this.m_var = name;
        }

        private const string s_any = "_";

        public static string Any { get { return s_any; } }

        public bool isAny() { return m_var == s_any; }
        /*
        * Get the actual string contained in this object.
        *
        * @return the raw string contained in this object, without regard
        * to Erlang quoting rules.
        * 
        * @see #toString
        **/
        public string name()
        {
            return m_var;
        }

        /*
        * Get the printable version of the string contained in this object.
        *
        * @return the string contained in this object, quoted.
        * 
        * @see #stringValue
        **/
        public override string ToString()
        {
            return "\"" + m_var + "\"";
        }

        public override bool subst(ref Erlang.Object obj, Erlang.VarBind binding)
        {
            if (isAny() || binding == null || binding.Empty)
                throw new UnboundVarException();
            Erlang.Object term = binding[m_var];
            if (term == null)
                throw new UnboundVarException("Variable " + m_var + " not bound!");
            obj = term;
            return true;
        }

        public override bool match(Erlang.Object pattern, Erlang.VarBind binding)
        {
            if (binding == null)
                return false;
            Erlang.Object value = binding.find(m_var);
            if (value != null)
                return value.match(pattern, binding);
            Erlang.Object term = null;
            binding[m_var] = pattern.subst(ref term, binding) ? term : pattern;
            return true;
        }

        /*
        * Convert this string to the equivalent Erlang external representation.
        *
        * @param buf an output stream to which the encoded string should be
        * written.
        **/
        public override void encode(OtpOutputStream buf)
        {
            throw new EncodeException("Cannot encode vars!");
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
            return false;
        }

        public override int GetHashCode()
        {
            return 1;
        }
    }
}
