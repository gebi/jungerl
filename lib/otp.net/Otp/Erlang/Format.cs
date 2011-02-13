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
    internal class Formatter
    {
        private enum State {
              ERL_OK                = 0
            , ERL_FMT_ERR           = -1
            , ERL_MAX_ENTRIES       = 255  /* Max entries in a tuple/list term */
            , ERL_MAX_NAME_LENGTH   = 255  /* Max length of variable names */
        };

        private static int skip_null_chars(char[] fmt, int pos) {
            for (char c = fmt[pos]; pos < fmt.Length && (c == ' ' || c == '\t' || c == '\n'); c = fmt[++pos]);
            return pos;
        }

        private static string pvariable(char[] fmt, ref int pos)
        {
            int start = pos;

            for (pos = skip_null_chars(fmt, pos); pos < fmt.Length; pos++)
            {
                char c = fmt[pos];
                if (char.IsLetterOrDigit(c) || (c == '_'))
                    continue;
                else
                    break;
            }
            int len = pos - start;
            return new string(fmt, start, len);
        }

        private static string patom(char[] fmt, ref int pos)
        {
            int start = pos;

            for (pos = skip_null_chars(fmt, pos); pos < fmt.Length; pos++)
            {
                char c = fmt[pos];
                if (char.IsLetterOrDigit(c) || (c == '_') || (c == '@'))
                    continue;
                else
                    break;
            }
            int len = pos - start;
            return new string(fmt, start, len);
        }

        private static string pdigit(char[] fmt, ref int pos)
        {
            int start = pos;
            bool dotp = false;

            for (pos = skip_null_chars(fmt, pos); pos < fmt.Length; pos++)
            {
                char c = fmt[pos];
                if (char.IsDigit(c) || c == '-')
                    continue;
                else if (!dotp && c == '.')
                {
                    dotp = true;
                    continue;
                }
                else
                    break;
            }
            int len = pos - start;
            return new string(fmt, start, len);
        }

        private static string pstring(char[] fmt, ref int pos)
        {
            int start = ++pos; // skip the first double quote

            for (; pos < fmt.Length; pos++)
            {
                char c = fmt[pos];
                if (c == '"') {
                    if (fmt[pos-1] == '\\')
                        continue;
                    else
                        break;
                }
            }
            int len = pos++ - start;
            return new string(fmt, start, len);
        }

        private static string pquotedAtom(char[] fmt, ref int pos)
        {
            int start = ++pos; // skip first quote

            for (pos = skip_null_chars(fmt, pos); pos < fmt.Length; pos++)
            {
                char c = fmt[pos];
                if (c == '\'')
                {
                    if (fmt[pos-1] == '\\')
                        continue;
                    else
                        break;
                }
            }
            int len = pos - start;
            return new string(fmt, start, len);
        }

        private static State ptuple(char[] fmt, ref int pos,
            ref System.Collections.Generic.List<Erlang.Object> items, ref int argc, params object[] args)
        {
            int start = pos;
            State rc = State.ERL_FMT_ERR;

            pos = skip_null_chars(fmt, pos);
            switch (fmt[pos++]) {
                case '}':
                    rc = State.ERL_OK;
                    break;

                case ',':
                    rc = ptuple(fmt, ref pos, ref items, ref argc, args);
                    break;

                default: {
                    --pos;
                    Erlang.Object obj = create(fmt, ref pos, ref argc, args);
                    items.Add(obj);
                    rc = ptuple(fmt, ref pos, ref items, ref argc, args);
                    break;
                }
            }

            return rc;
        }

        private static State plist(char[] fmt, ref int pos,
            ref System.Collections.Generic.List<Erlang.Object> items, ref int argc, params object[] args)
        {
            State rc = State.ERL_FMT_ERR;

            pos = skip_null_chars(fmt, pos);

            switch (fmt[pos++]) {
                case ']':
                    rc = State.ERL_OK;
                    break;

                case ',':
                    rc = plist(fmt, ref pos, ref items, ref argc, args);
                    break;

                case '|':
                    pos = skip_null_chars(fmt, pos);
                    if (char.IsUpper(fmt[pos]) || fmt[pos] == '_') {
                        string s = pvariable(fmt, ref pos);
                        items.Add(new Erlang.Var(s));
                        pos = skip_null_chars(fmt, pos);
                        if (fmt[pos] == ']')
                            rc = State.ERL_OK;
                        break;
                    }
                    break;

                default: {
                    --pos;
                    Erlang.Object obj = create(fmt, ref pos, ref argc, args);
                    items.Add(obj);
                    rc = plist(fmt, ref pos, ref items, ref argc, args);
                    break;
                }

            }

            return rc;
        }

        private static State pformat(char[] fmt, ref int pos,
            ref System.Collections.Generic.List<Erlang.Object> items, ref int argc, params object[] args)
        {
            pos = skip_null_chars(fmt, pos);
            if (fmt[pos++] != 'w')
                return State.ERL_FMT_ERR;
            object o = args[argc++];
            Type to = o.GetType();

            if (to == typeof(int))
                items.Add(new Erlang.Long((int)o));
            else if (to == typeof(long))
                items.Add(new Erlang.Long((long)o));
            else if (to == typeof(double))
                items.Add(new Erlang.Double((double)o));
            else if (to == typeof(string))
                items.Add(new Erlang.String((string)o));
            else if (to == typeof(bool))
                items.Add(new Erlang.Boolean((bool)o));
            else if (to == typeof(Erlang.Object))
                items.Add(o as Erlang.Object);
            else if (to == typeof(char))
                items.Add(new Erlang.Char((char)o));
            else
                throw new FormatException(
                    string.Format("Unknown type of argument #{0}: {1}", argc-1, to.ToString()));
            return State.ERL_OK;
        }

        internal static Erlang.Object create(
            char[] fmt, ref int pos, ref int argc, params object[] args)
        {
            var items = new System.Collections.Generic.List<Erlang.Object>();
            Erlang.Object result = null;
            pos = skip_null_chars(fmt, pos);

            switch (fmt[pos++]) {
                case '{':
                    if (State.ERL_OK != ptuple(fmt, ref pos, ref items, ref argc, args))
                        throw new FormatException(
                            string.Format("Error parsing tuple at pos {0}", pos));
                    result = new Erlang.Tuple(items.ToArray());
                    break;

                case '[':
                    if (fmt[pos] == ']') {
                        result = new Erlang.List();
                        break;
                    }
                    else if (State.ERL_OK == plist(fmt, ref pos, ref items, ref argc, args))
                    {
                        result = new Erlang.List(items.ToArray());
                        break;
                    }
                    throw new FormatException(
                        string.Format("Error parsing list at pos {0}", pos));

                case '$': /* char-value? */
                    result = new Erlang.Char(fmt[pos++]);
                    break;

                case '~':
                    if (State.ERL_OK != pformat(fmt, ref pos, ref items, ref argc, args))
                        throw new FormatException(
                            string.Format("Error parsing term at pos {0}", pos));
                    result = items[0];
                    break;

                default:
                    --pos;
                    char c = fmt[pos];
                    if (char.IsLower(c)) {         /* atom  ? */
                        string s = patom(fmt, ref pos);
                        result = new Erlang.Atom(s);
                    } else if (char.IsUpper(c) || c == '_') {
                        string s = pvariable(fmt, ref pos);
                        result = new Erlang.Var(s);
                    } else if (char.IsDigit(c) || c == '-') {    /* integer/float ? */
                        string s = pdigit(fmt, ref pos);
                        if (s.IndexOf('.') < 0)
                            result = new Erlang.Long(long.Parse(s));
                        else
                            result = new Erlang.Double(double.Parse(s));
                    } else if (c == '"') {      /* string ? */
                        string s = pstring(fmt, ref pos);
                        result = new Erlang.String(s);
                    } else if (c == '\'') {     /* quoted atom ? */
                        string s = pquotedAtom(fmt, ref pos);
                        result = new Erlang.Atom(s);
                    }
                    break;
            }

            return result;
        }
    }
}
