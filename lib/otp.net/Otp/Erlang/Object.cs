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
	* Base class of the Erlang data type classes. This class is used to
	* represent an arbitrary Erlang term.
	**/
	[Serializable]
    public abstract class Object : Amir_Harel.Cloning.BaseObject
	{
        public static Erlang.Object Format(
            string fmt, params object[] args)
        {
            int pos = 0, argc = 0;
            return Formatter.create(fmt.ToCharArray(), ref pos, ref argc, args);
        }

        public T Cast<T>() where T: Erlang.Object
        {
            if (!(this is T))
                throw new CastException();
            return (T)(this);
        }

		/*
		* Convert the object according to the rules of the Erlang external
		* format. This is mainly used for sending Erlang terms in messages,
		* however it can also be used for storing terms to disk.
		*
		* @param buf an output stream to which the encoded term should be
		* written.
		**/
		public abstract void  encode(OtpOutputStream buf);
		
		/*
		* Read binary data in the Erlang external format, and produce a
		* corresponding Erlang data type object. This method is normally
		* used when Erlang terms are received in messages, however it
		* can also be used for reading terms from disk.
		*
		* @param buf an input stream containing one or more encoded Erlang
		* terms.
		*
		* @return an object representing one of the Erlang data
		* types.
		*
		* @exception DecodeException if the stream does not
		* contain a valid representation of an Erlang term.
		**/
		public static Object decode(OtpInputStream buf)
		{
			return buf.read_any();
		}
		
        public virtual bool subst(ref Erlang.Object term, Erlang.VarBind binding)
        {
            return false;
        }

        public virtual bool match(Erlang.Object pattern, Erlang.VarBind binding)
        {
            return (pattern is Erlang.Var) ? pattern.match(this, binding) : this.Equals(pattern);
        }

		/*
		* Determine if two Erlang objects are equal. In general, Erlang
		* objects are equal if the components they consist of are equal.
		*
		* @param o the object to compare to.
		*
		* @return true if the objects are identical.
		**/
		//public abstract bool Equals(System.Object o);

		public virtual System.Object clone()
		{
			try
			{
				return base.Clone();
			}
			catch (System.Exception e)
			{
				/*cannot happen */
				throw new System.ApplicationException(e.ToString());
			}
		}

        public long     longValue()     { return this.Cast<Long>().longValue(); }
        public int      intValue()      { return this.Cast<Long>().intValue(); }
        public short    shortValue()    { return this.Cast<Long>().shortValue(); }
        public double   doubleValue()   { return this.Cast<Double>().doubleValue(); }
        public string   atomValue()     { return this.Cast<Atom>().atomValue(); }
        public string   stringValue()   { return this.Cast<String>().stringValue(); }
        public char     charValue()     { return this.Cast<Char>().charValue(); }
        public bool     boolValue()     { return this.Cast<Boolean>().booleanValue(); }
        public byte[]   binaryValue()   { return this.Cast<Binary>().binaryValue(); }
        public Pid      pidValue()      { return this.Cast<Pid>().pidValue(); }
        public Port     portValue()     { return this.Cast<Port>().portValue(); }
        public Ref      refValue()      { return this.Cast<Ref>().refValue(); }
        public Tuple    tupleValue()    { return this.Cast<Tuple>().tupleValue(); }
        public List     listValue()     { return this.Cast<List>().listValue(); }
	}
}