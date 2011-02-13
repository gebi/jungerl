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
using System.Collections.Generic;

namespace Otp.Erlang
{
    using System;

    /*
    * Provides a C# representation of Erlang strings. 
    **/
    public class VarBind
    {
        private Dictionary<string, Erlang.Object> m_dict;

        /// <summary>
        /// Create an variable binding dictionary.
        /// </summary>
        public VarBind()
        {
            this.m_dict = new Dictionary<string, Erlang.Object>(23);
        }

        public void bind(string name, Erlang.Object obj)
        {
            m_dict[name] = obj;
        }

        public Erlang.Object find(string name) 
        {
            Erlang.Object obj;
            return m_dict.TryGetValue(name, out obj) ? obj : null;
        }

        public T Cast<T>(string name) where T : Erlang.Object
        {
            Erlang.Object obj = m_dict[name];
            if (obj == null)
                throw new UnboundVarException(string.Format("Variable {0} not found!", name));
            return obj.Cast<T>();
        }

        public Erlang.Object this[string name]
        {
            get { return m_dict[name]; }
            set { m_dict[name] = value; }
        }

        public void merge(VarBind other)
        {
            foreach (KeyValuePair<string, Erlang.Object> kv in other.m_dict)
                m_dict[kv.Key] = kv.Value;
        }

        public void clear()
        {
            m_dict.Clear();
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
            System.Text.StringBuilder builder =
                new System.Text.StringBuilder();
            foreach (KeyValuePair<string, Erlang.Object> kv in m_dict)
                builder.AppendFormat("{0}{1}={2}", builder.Length == 0 ? string.Empty : ", ", kv.Key, kv.Value);
            return builder.ToString();
        }

        public int  Count { get { return m_dict.Count; } }
        public bool Empty { get { return m_dict.Count == 0; } }
    }
}
