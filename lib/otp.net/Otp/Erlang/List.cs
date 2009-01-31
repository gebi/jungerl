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
	* Provides a C# representation of Erlang lists. Lists are created
	* from zero or more arbitrary Erlang terms.
	*
	* <p> The arity of the list is the number of elements it contains.
	**/
	[Serializable]
    public class List:Erlang.Object
	{
		private Object[] elems = null;
		
		// todo: use a linked structure to make a proper list with append,
		// car, cdr etc methods. The current representation is essensially the
		// same as for tuples except that empty lists are allowed.
		// private Object car = null;
		// private Object cdr = null;
		// int arity;
		
		/*
		* Create an empty list.
		**/
		public List()
		{
			this.elems = null; // empty list
		}
		
		/*
		* Create a list of characters.
		*
		* @param str the characters from which to create the list.
		*
		public List(System.String str)
		{
    		this.elems = new Object[] { new Erlang.String(str) };
		}
		
		* Create a list containing one element.
		*
		* @param elem the elememet to make the list from.
		*
		public List(Object elem)
		{
			this.elems = new Object[1];
			elems[0] = elem;
		}
        */		
        
		/*
        * Create a list from an array of arbitrary Erlang terms.
		*
		* @param elems the array of terms from which to create the list.
		* @param count the number of terms to insert.
		*/
        public List(Object[] elems): this(elems, 0, elems.Length)
        {
        }

		/*
        * Create a list from an array of arbitrary Erlang terms.
		*
		* @param elems the array of terms from which to create the list.
		* @param start the offset of the first term to insert.
		* @param count the number of terms to insert.
		*/
        public List(Object[] elems, int start, int count)
		{
			if ((elems != null) && (count > 0))
			{
				this.elems = new Object[count];
                Array.Copy(elems, 0, this.elems, start, count);
            }
		}

        /*
        * Create a list from an array of arbitrary Erlang terms.
        *
        * @param elems the array of terms from which to create the list.
        **/
        public List(params System.Object[] elems)
        {
            if ((elems != null) && (elems.Length > 0))
            {
                this.elems = new Object[elems.Length];

                for (int i=0; i < elems.Length; i++) 
                {
                    System.Object o = elems[i];
                    if (o is int) this.elems[i] = new Int((int)o);
                    else if (o is string) this.elems[i] = new String((string)o);
                    else if (o is float) this.elems[i] = new Double((float)o);
                    else if (o is double) this.elems[i] = new Double((double)o);
                    else if (o is Erlang.Object) this.elems[i] = (o as Erlang.Object);
                    //else if (o is BigInteger) this.elems[i] = (BigInteger)o;
                    else if (o is uint) this.elems[i] = new UInt((int)o);
                    else if (o is short) this.elems[i] = new Short((short)o);
                    else if (o is ushort) this.elems[i] = new UShort((short)o);
                    else
                        throw new System.ArgumentException("Unknown type of element[" + i + "]: " + o.GetType().ToString());
                }
            }
        }

        /*
        * Create a list from a stream containing an list encoded in Erlang
        * external format.
        *
        * @param buf the stream containing the encoded list.
        * 
        * @exception DecodeException if the buffer does not
        * contain a valid external representation of an Erlang list.
        **/
		public List(OtpInputStream buf)
		{
			this.elems = null;
			
			int arity = buf.read_list_head();
			
			if (arity > 0)
			{
				this.elems = new Object[arity];
				
				 for (int i = 0; i < arity; i++)
				{
					elems[i] = buf.read_any();
				}
				
				/*discard the terminating nil (empty list) */
				buf.read_nil();
			}
		}
		
		/*
		* Get the arity of the list.
		*
		* @return the number of elements contained in the list.
		**/
		public int arity()
		{
			if (elems == null)
				return 0;
			else
				return elems.Length;
		}
		
		/*
		* Get the specified element from the list.
		*
		* @param i the index of the requested element. List elements are
		* numbered as array elements, starting at 0.
		*
		* @return the requested element, of null if i is not a valid
		* element index.
		**/
		public Object elementAt(int i)
		{
			if ((i >= arity()) || (i < 0))
				return null;
			return elems[i];
		}
		
		/*
		* Get all the elements from the list as an array.
		*
		* @return an array containing all of the list's elements. 
		**/
		public Object[] elements()
		{
			if (arity() == 0)
				return null;
			else
			{
				Object[] res = new Object[arity()];
				Array.Copy(this.elems, 0, res, 0, res.Length);
				return res;
			}
		}

        public Object this[int index]
        {
            get { return elementAt(index); }
            set { this.elems[index] = value; }
        }

        public int Length
        {
            get { return this.elems.Length; }
        }

		/*
		* Get the string representation of the list.
		*
		* @return the string representation of the list.
		**/
		public override System.String ToString()
		{
			System.Text.StringBuilder s = new System.Text.StringBuilder();
			int _arity = arity();
			
			s.Append("[");
			
			 for (int i = 0; i < _arity; i++)
			{
				if (i > 0)
					s.Append(",");
				s.Append(elems[i].ToString());
			}
			
			s.Append("]");
			
			return s.ToString();
		}
		
		/*
		* Convert this list to the equivalent Erlang external
		* representation. Note that this method never encodes lists as
		* strings, even when it is possible to do so.
		*
		* @param buf An output stream to which the encoded list should be
		* written.
		*
		**/
		public override void  encode(OtpOutputStream buf)
		{
			int _arity = arity();
			
			if (_arity > 0)
			{
				buf.write_list_head(_arity);
				
				 for (int i = 0; i < _arity; i++)
				{
					buf.write_any(elems[i]);
				}
			}
			
			buf.write_nil();
		}
		
		/*
		* Determine if two lists are equal. Lists are equal if they have
		* the same arity and all of the elements are equal.
		*
		* @param o the list to compare to.
		*
		* @return true if the lists have the same arity and all the
		* elements are equal.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is List))
				return false;
			
			List l = (List) o;
			int a = this.arity();
			
			if (a != l.arity())
				return false;
			
			 for (int i = 0; i < a; i++)
			{
				if (!this.elems[i].Equals(l.elems[i]))
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
			List newList = (List) (base.clone());
			newList.elems = new Object[elems.Length];
			elems.CopyTo(newList.elems, 0);
			return newList;
		}
	}
}