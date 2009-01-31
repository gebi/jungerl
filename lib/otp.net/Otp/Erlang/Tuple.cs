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
	* Provides a C# representation of Erlang tuples. Tuples are created
	* from one or more arbitrary Erlang terms. 
	*
	* <p> The arity of the tuple is the number of elements it contains.
	* Elements are indexed from 0 to (arity-1) and can be retrieved
	* individually by using the appropriate index.
	**/
	[Serializable]
    public class Tuple:Erlang.Object
	{
		private Object[] elems = null;
		
		/*
		* Create a unary tuple containing the given element.
		*
		* @param elem the element to create the tuple from.
		*
		* @exception C#.lang.IllegalArgumentException if the array is
		* empty (null).
		*
		public Tuple(Object elem)
		{
			if (elem == null)
			{
				throw new System.ArgumentException("Tuple element cannot be null");
			}
			this.elems = new Object[1];
			elems[0] = elem;
		}
		*/
		/*
		* Create a tuple from an array of terms.
		*
		* @param elems the array of terms to create the tuple from.
		*
		* @exception C#.lang.IllegalArgumentException if the array is
		* empty (null) or contains null elements.
		*/
		public Tuple(Object[] elems):this(elems, 0, elems.Length)
		{
		}

		/*
		* Create a tuple from an array of terms.
		*
		* @param elems the array of terms to create the tuple from.
		* @param start the offset of the first term to insert.
		* @param count the number of terms to insert.
		*
		* @exception C#.lang.IllegalArgumentException if the array is
		* empty (null) or contains null elements.
		*/
		public Tuple(Object[] elems, int start, int count)
		{
			if ((elems == null) || (count < 1))
			{
				throw new System.ArgumentException("Cannot make an empty tuple");
			}
			else
			{
				this.elems = new Object[count];
				 for (int i = 0; i < count; i++)
				{
					if (elems[start + i] != null)
					{
						this.elems[i] = elems[start + i];
					}
					else
					{
						throw new System.ArgumentException("Tuple element cannot be null (element" + (start + i) + ")");
					}
				}
			}
		}

        /*
		* Create a tuple from an array of terms.
		*
		* @param elems the array of terms to create the tuple from.
		* @param start the offset of the first term to insert.
		* @param count the number of terms to insert.
		*
		* @exception C#.lang.IllegalArgumentException if the array is
		* empty (null) or contains null elements.
		**/
		public Tuple(params System.Object[] elems)
		{
			if (elems == null)
				elems = new Object[] {};
			else
			{
				this.elems = new Object[elems.Length];
                for (int i = 0; i < elems.Length; i++)
				{
					if (elems[i] == null)
						throw new System.ArgumentException("Tuple element cannot be null (element" + i + ")");
					else
                    {
                        System.Object o = elems[i];
                        if (o is int) this.elems[i] = new Int((int)o);
                        else if (o is string) this.elems[i] = new String((string)o);
                        else if (o is float) this.elems[i] = new Double((float)o);
                        else if (o is double) this.elems[i] = new Double((double)o);
                        else if (o is Erlang.Object) this.elems[i] = (Erlang.Object)o;
                        //else if (o is BigInteger) this.elems[i] = (BigInteger)o;
                        else if (o is uint) this.elems[i] = new UInt((int)o);
                        else if (o is short) this.elems[i] = new Short((short)o);
                        else if (o is ushort) this.elems[i] = new UShort((short)o);
                        else
                            throw new System.ArgumentException("Unknown type of element[" + i + "]: " + o.GetType().ToString());
                    }
                }
			}
		}
		/*
        * Create a tuple from a stream containing an tuple encoded in Erlang
		* external format.
		*
		* @param buf the stream containing the encoded tuple.
		* 
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang tuple.
		**/
		public Tuple(OtpInputStream buf)
		{
			int arity = buf.read_tuple_head();
			
			if (arity > 0)
			{
				this.elems = new Object[arity];
				
				 for (int i = 0; i < arity; i++)
				{
					elems[i] = buf.read_any();
				}
			}
		}
		
		/*
		* Get the arity of the tuple.
		*
		* @return the number of elements contained in the tuple.
		**/
		public virtual int arity()
		{
			return elems.Length;
		}
		
		/*
		* Get the specified element from the tuple.
		*
		* @param i the index of the requested element. Tuple elements are
		* numbered as array elements, starting at 0.
		*
		* @return the requested element, of null if i is not a valid
		* element index.
		**/
		public virtual Object elementAt(int i)
		{
			if ((i >= arity()) || (i < 0))
				return null;
			return elems[i];
		}
		
		/*
		* Get all the elements from the tuple as an array.
		*
		* @return an array containing all of the tuple's elements. 
		**/
		public virtual Object[] elements()
		{
			Object[] res = new Object[arity()];
			Array.Copy(this.elems, 0, res, 0, res.Length);
			return res;
		}
		
		public Object this[int index]
        {
            get { return this.elems[index]; }
            set { this.elems[index] = value; }
        }

		/*
		* Get the string representation of the tuple.
		*
		* @return the string representation of the tuple.
		**/
		public override System.String ToString()
		{
			int i;
			System.Text.StringBuilder s = new System.Text.StringBuilder();
			int arity = (int) (elems.Length);
			
			s.Append("{");
			
			 for (i = 0; i < arity; i++)
			{
				if (i > 0)
					s.Append(",");
				s.Append(elems[i].ToString());
			}
			
			s.Append("}");
			
			return s.ToString();
		}
		
		/*
		* Convert this tuple to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded tuple should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			int arity = (int) (elems.Length);
			
			buf.write_tuple_head(arity);
			
			 for (int i = 0; i < arity; i++)
			{
				buf.write_any(elems[i]);
			}
		}
		
		
		/*
		* Determine if two tuples are equal. Tuples are equal if they have
		* the same arity and all of the elements are equal.
		*
		* @param o the tuple to compare to.
		*
		* @return true if the tuples have the same arity and all the
		* elements are equal.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is Tuple))
				return false;
			
			Tuple t = (Tuple) o;
			int a = this.arity();
			
			if (a != t.arity())
				return false;
			
			 for (int i = 0; i < a; i++)
			{
				if (!this.elems[i].Equals(t.elems[i]))
					return false;
				// early exit
			}
			
			return true;
		}

		public override int GetHashCode()
		{
			return 1;
		}

		public override System.Object clone()
		{
			Tuple newTuple = (Tuple) (base.clone());
			newTuple.elems = new Object[elems.Length];
			elems.CopyTo(newTuple.elems, 0);
			return newTuple;
		}
	}
}