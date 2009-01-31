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
namespace Otp
{
	using System;

	/*
	* Provides a stream for decoding Erlang terms from external format.
	*
	* <p> Note that this class is not synchronized, if you need
	* synchronization you must provide it yourself.
	**/
	public class OtpInputStream:System.IO.MemoryStream
	{
		/*
		* Create a stream from a buffer containing encoded Erlang terms.
		**/
		public OtpInputStream(byte[] buf):base(buf)
		{
		}
		
		/*
		* Create a stream from a buffer containing encoded
		* Erlang terms at the given offset and length.
		**/
		public OtpInputStream(byte[] buf, int offset, int length):base(buf, offset, length)
		{
		}
		
		/*
		* Get the current position in the stream.
		*
		* @return the current position in the stream.
		**/
		public virtual int getPos()
		{
			return (int) base.Position;
		}
		
		/*
		* Set the current position in the stream.
		*
		* @param pos the position to move to in the stream. If pos
		* indicates a position beyond the end of the stream, the position
		* is move to the end of the stream instead. If pos is negative, the
		* position is moved to the beginning of the stream instead.
		*
		* @return the previous position in the stream.
		**/
		public virtual int setPos(int pos)
		{
			int oldpos = (int) base.Position;
			
			if (pos > (int) base.Length)
				pos = (int) base.Length;
			else if (pos < 0)
				pos = 0;
			
			base.Position = (System.Int64) pos;
			
			return oldpos;
		}
		
		/*
		* Read an array of bytes from the stream. The method reads at most
		* buf.length bytes from the input stream.
		*
		* @return the number of bytes read.
		*
		* @exception OtpErlangDecodeException if the next byte cannot be
		* read.
		**/
		public virtual int readN(byte[] buf)
		{
			try
			{
				return base.Read(buf, 0, buf.Length);
			}
			catch (System.IO.IOException)
			{
				throw new Erlang.DecodeException("Cannot read from input stream");
			}
		}
		
		/*
		* Look ahead one position in the stream without consuming the byte
		* found there.
		*
		* @return the next byte in the stream, as an integer.
		*
		* @exception Erlang.DecodeException if the next byte cannot be
		* read.
		**/
		public virtual int peek()
		{
			int i;
			try
			{
				i = base.ReadByte();
				base.Seek(-1, System.IO.SeekOrigin.Current);
				if (i < 0)
					i += 256;
				
				return i;
			}
			catch (System.Exception)
			{
				throw new Erlang.DecodeException("Cannot read from input stream");
			}
		}
		
		/*
		* Read a one byte integer from the stream.
		*
		* @return the byte read, as an integer.
		* 
		* @exception Erlang.DecodeException if the next byte cannot be
		* read.
		**/
		public virtual int read1()
		{
			int i;
			i = base.ReadByte();
			
			if (i < 0)
			{
				throw new Erlang.DecodeException("Cannot read from input stream");
			}
			
			return i;
		}
		
		/*
		* Read a two byte big endian integer from the stream.
		*
		* @return the bytes read, converted from big endian to an integer.
		* 
		* @exception Erlang.DecodeException if the next byte cannot be
		* read.
		**/
		public virtual int read2BE()
		{
			byte[] b = new byte[2];
			try
			{
				base.Read(b, 0, b.Length);
			}
			catch (System.IO.IOException)
			{
				throw new Erlang.DecodeException("Cannot read from input stream");
			}
			return ((((int) b[0] << 8) & 0xff00) + (((int) b[1]) & 0xff));
		}
		
		/*
		* Read a four byte big endian integer from the stream.
		*
		* @return the bytes read, converted from big endian to an integer.
		* 
		* @exception Erlang.DecodeException if the next byte cannot be
		* read.
		**/
		public virtual int read4BE()
		{
			byte[] b = new byte[4];
			try
			{
				base.Read(b, 0, b.Length);
			}
			catch (System.IO.IOException)
			{
				throw new Erlang.DecodeException("Cannot read from input stream");
			}
			return (int)((((int) b[0] << 24) & 0xff000000) + (((int) b[1] << 16) & 0xff0000) + (((int) b[2] << 8) & 0xff00) + (((int) b[3]) & 0xff));
		}

        /*
        * Read an eight byte big endian integer from the stream.
        *
        * @return the bytes read, converted from big endian to an integer.
        * 
        * @exception Erlang.DecodeException if the next byte cannot be
        * read.
        **/
        public System.UInt64 read8BE()
        {
            byte[] b = new byte[8];
            try
            {
                base.Read(b, 0, b.Length);
            }
            catch (System.IO.IOException)
            {
                throw new Erlang.DecodeException("Cannot read from input stream");
            }
            System.UInt64 i1 = (System.UInt64)((((int)b[0] << 24) & 0xff000000) + (((int)b[1] << 16) & 0xff0000) + (((int)b[2] << 8) & 0xff00) + (((int)b[3]) & 0xff));
            System.UInt64 i2 = (i1 << 32) & 0xffffffff00000000
                             + (System.UInt64)((((int)b[4] << 24) & 0xff000000) + (((int)b[5] << 16) & 0xff0000) + (((int)b[6] << 8) & 0xff00) + (((int)b[7]) & 0xff));
            return i2;
        }

        /*
        * Read a two byte little endian integer from the stream.
        *
        * @return the bytes read, converted from little endian to an
        * integer.
        * 
        * @exception Erlang.DecodeException if the next byte cannot be
        * read.
        **/
		public virtual int read2LE()
		{
			byte[] b = new byte[2];
			try
			{
				base.Read(b, 0, b.Length);
			}
			catch (System.IO.IOException)
			{
				throw new Erlang.DecodeException("Cannot read from input stream");
			}
			return ((((int) b[1] << 8) & 0xff00) + (((int) b[0]) & 0xff));
		}
		
		/*
		* Read a four byte little endian integer from the stream.
		*
		* @return the bytes read, converted from little endian to an
		* integer.
		* 
		* @exception Erlang.DecodeException if the next byte cannot be
		* read.
		**/
		public virtual int read4LE()
		{
			byte[] b = new byte[4];
			try
			{
				base.Read(b, 0, b.Length);
			}
			catch (System.IO.IOException)
			{
				throw new Erlang.DecodeException("Cannot read from input stream");
			}
			return (int)((((int) b[3] << 24) & 0xff000000) + (((int) b[2] << 16) & 0xff0000) + (((int) b[1] << 8) & 0xff00) + (((int) b[0]) & 0xff));
		}
		
		/*
		* Read an Erlang atom from the stream and interpret the value as a
		* boolean.
		*
		* @return true if the atom at the current position in the stream
		* contains the value 'true' (ignoring case), false otherwise.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not an atom.
		**/
		public virtual bool read_boolean()
		{
			return System.Boolean.Parse(this.read_atom());
		}
		
		/*
		* Read an Erlang atom from the stream.
		*
		* @return a String containing the value of the atom.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not an atom.
		**/
		public virtual System.String read_atom()
		{
			int tag;
			int len;
			byte[] strbuf;
			System.String atom;
			
			tag = this.read1();
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			if (tag != OtpExternal.atomTag)
			{
				throw new Erlang.DecodeException("wrong tag encountered, expected " + OtpExternal.atomTag + ", got " + tag);
			}
			
			len = this.read2BE();
			
			strbuf = new byte[len];
			this.readN(strbuf);
			char[] tmpChar;
			tmpChar = new char[strbuf.Length];
			strbuf.CopyTo(tmpChar, 0);
			atom = new System.String(tmpChar);
			
			if (atom.Length > OtpExternal.maxAtomLength)
			{
				atom = atom.Substring(0, (OtpExternal.maxAtomLength) - (0));
			}
			
			return atom;
		}
		
		/*
		* Read an Erlang binary from the stream.
		*
		* @return a byte array containing the value of the binary.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not a binary.
		**/
		public virtual byte[] read_binary()
		{
			int tag;
			int len;
			byte[] bin;
			
			tag = this.read1();
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			if (tag != OtpExternal.binTag)
			{
				throw new Erlang.DecodeException("Wrong tag encountered, expected " + OtpExternal.binTag + ", got " + tag);
			}
			
			len = this.read4BE();
			
			bin = new byte[len];
			this.readN(bin);
			
			return bin;
		}
		
		/*
		* Read an Erlang float from the stream.
		*
		* @return the float value.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not a float.
		**/
        public virtual float read_float()
        {
            double d = this.read_double();
            float f = (float)d;
            if (System.Math.Abs(d - f) >= 1.0E-20)
                throw new Erlang.DecodeException("Value cannot be represented as float: " + d);
            return f;
        }

        /*
        * Read an Erlang float from the stream.
        *
        * @return the float value, as a double.
        * 
        * @exception Erlang.DecodeException if the next term in the
        * stream is not a float.
        *
        **/
        public virtual double read_double()
		{
			return getFloatOrDouble();
		}

        private double getFloatOrDouble()
        {

            // parse the stream
            int tag = this.read1();
            if (tag == OtpExternal.versionTag)
            {
                tag = this.read1();
            }

            byte[] strbuf;
            double parsedValue = 0.0;

            if (tag == OtpExternal.floatTag)
            {
                // get the string
                strbuf = new byte[31];
                this.readN(strbuf);

                char[] tmpChar = new char[strbuf.Length];
                strbuf.CopyTo(tmpChar, 0);
                System.String str = new System.String(tmpChar);
                //System.Diagnostics.Debug.WriteLine("getFloatOrDouble: str = " + str);

                try
                {
                    // Easier than the java version.
                    parsedValue = System.Double.Parse(str);
                    return parsedValue;
                }
                catch
                {
                    throw new Erlang.DecodeException("Error parsing float format: '" + str + "'");
                }
            }
            else if (tag == OtpExternal.newFloatTag)
            {
                byte[] data = new byte[8];
                this.readN(data);
                // IEEE 754 decoder
                if (BitConverter.IsLittleEndian)
                {
                    Array.Reverse(data);
                }
                return BitConverter.ToDouble(data, 0);
            }
            else
            {
                throw new Erlang.DecodeException("Wrong tag encountered, expected " + OtpExternal.floatTag + ", got " + tag);
            }
        }
		
		/*
		* Read one byte from the stream.
		*
		* @return the byte read.
		* 
		* @exception Erlang.DecodeException if the next byte cannot be
		* read.
		**/
		public virtual byte read_byte()
		{
			long l = this.read_long();
			byte i = (byte) l;
			
			if (l != i)
			{
				throw new Erlang.DecodeException("Value too large for byte: " + l);
			}
			
			return i;
		}
		
		/*
		* Read a character from the stream.
		*
		* @return the character value.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not an integer that can be represented as a char.
		**/
		public virtual char read_char()
		{
			long l = this.read_long();
			char i = (char) l;
			
			if (l != i)
			{
				throw new Erlang.DecodeException("Value too large for byte: " + l);
			}
			
			return i;
		}
		
		/*
		* Read an unsigned integer from the stream.
		*
		* @return the integer value.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream can not be represented as a positive integer.
		**/
		public virtual int read_uint()
		{
			long l = this.read_long();
			int i = (int) l;
			
			if (l != i)
			{
				throw new Erlang.DecodeException("Value too large for integer: " + l);
			}
			else if (l < 0)
			{
				throw new Erlang.DecodeException("Value not unsigned: " + l);
			}
			
			return i;
		}
		
		/*
		* Read an integer from the stream.
		*
		* @return the integer value.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream can not be represented as an integer.
		**/
		public virtual int read_int()
		{
			long l = this.read_long();
			int i = (int) l;
			
			if (l != i)
			{
				throw new Erlang.DecodeException("Value too large for byte: " + l);
			}
			
			return i;
		}
		
		/*
		* Read an unsigned short from the stream.
		*
		* @return the short value.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream can not be represented as a positive short.
		**/
		public virtual short read_ushort()
		{
			long l = this.read_long();
			short i = (short) l;
			
			if (l != i)
			{
				throw new Erlang.DecodeException("Value too large for byte: " + l);
			}
			else if (l < 0)
			{
				throw new Erlang.DecodeException("Value not unsigned: " + l);
			}
			
			return i;
		}
		
		/*
		* Read a short from the stream.
		*
		* @return the short value.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream can not be represented as a short.
		**/
		public virtual short read_short()
		{
			long l = this.read_long();
			short i = (short) l;
			
			if (l != i)
			{
				throw new Erlang.DecodeException("Value too large for byte: " + l);
			}
			
			return i;
		}
		
		/*
		* Read an unsigned long from the stream.
		*
		* @return the long value.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream can not be represented as a positive long.
		**/
		public virtual long read_ulong()
		{
			long l = this.read_long();
			
			if (l < 0)
			{
				throw new Erlang.DecodeException("Value not unsigned: " + l);
			}
			
			return l;
		}
		
		/*
		* Read a long from the stream.
		*
		* @return the long value.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream can not be represented as a long.
		**/
		public virtual long read_long()
		{
			int tag;
			int sign;
			int arity;
			long val;
			
			tag = this.read1();
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			switch (tag)
			{
				case OtpExternal.smallIntTag: 
					val = this.read1();
					break;
					
				
				
				case OtpExternal.intTag: 
					val = this.read4BE();
					break;
					
				
				
				case OtpExternal.smallBigTag: {
					arity = this.read1();
                    sign  = this.read1();

                    byte[] nb = new byte[arity];
                    if (arity != this.readN(nb))
                    {
                        throw new Erlang.DecodeException("Cannot read from input stream. Expected smallBigTag arity " + arity);
                    }
                    if (arity > 8)
                        throw new Erlang.DecodeException("Value too large for long type (arity=" + arity + ")");

                    val = 0;
                    for (int i = 0; i < arity; i++)
                    {
                         val |= (long)nb[i] << (i * 8);
                    }

					val = (sign == 0 ? val : -val); // should deal with overflow
					
					break;
				}
				
				
				case OtpExternal.largeBigTag: default: 
					throw new Erlang.DecodeException("Not valid integer tag: " + tag);
				
			}
			
			return val;
		}
		
		/*
		* Read a list header from the stream.
		*
		* @return the arity of the list.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not a list.
		**/
		public virtual int read_list_head()
		{
			int arity = 0;
			int tag = this.read1();
			
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			switch (tag)
			{
				case OtpExternal.nilTag: 
					arity = 0;
					break;
					
				
				
				case OtpExternal.stringTag: 
					arity = this.read2BE();
					break;
					
				
				
				case OtpExternal.listTag: 
					arity = this.read4BE();
					break;
					
				
				
				default: 
					throw new Erlang.DecodeException("Not valid list tag: " + tag);
				
			}
			
			return arity;
		}
		
		/*
		* Read a tuple header from the stream.
		*
		* @return the arity of the tuple.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not a tuple.
		**/
		public virtual int read_tuple_head()
		{
			int arity = 0;
			int tag = this.read1();
			
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			// decode the tuple header and get arity
			switch (tag)
			{
				case OtpExternal.smallTupleTag: 
					arity = this.read1();
					break;
					
				
				
				case OtpExternal.largeTupleTag: 
					arity = this.read4BE();
					break;
					
				
				
				default: 
					throw new Erlang.DecodeException("Not valid tuple tag: " + tag);
				
			}
			
			return arity;
		}
		
		/*
		* Read an empty list from the stream.
		*
		* @return zero (the arity of the list).
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not an empty list.
		**/
		public virtual int read_nil()
		{
			int arity = 0;
			int tag = this.read1();
			
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			switch (tag)
			{
				case OtpExternal.nilTag: 
					arity = 0;
					break;
					
				
				
				default: 
					throw new Erlang.DecodeException("Not valid nil tag: " + tag);
				
			}
			
			return arity;
		}
		
		/*
		* Read an Erlang PID from the stream.
		*
		* @return the value of the PID.
		* 
		* @exception Erlang.DecodeException if the next term in the
		* stream is not an Erlang PID.
		**/
		public virtual Erlang.Pid read_pid()
		{
			System.String node;
			int id;
			int serial;
			int creation;
			int tag;
			
			tag = this.read1();
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			if (tag != OtpExternal.pidTag)
			{
				throw new Erlang.DecodeException("Wrong tag encountered, expected " + OtpExternal.pidTag + ", got " + tag);
			}
			
			node = this.read_atom();
			id = this.read4BE() & 0x7fff; // 15 bits
			serial = this.read4BE() & 0x07; // 3 bits
			creation = this.read1() & 0x03; // 2 bits
			
			return new Erlang.Pid(node, id, serial, creation);
		}
		
		/*
		* Read an Erlang port from the stream.
		*
		* @return the value of the port.
		* 
		* @exception DecodeException if the next term in the
		* stream is not an Erlang port.
		**/
		public virtual Erlang.Port read_port()
		{
			System.String node;
			int id;
			int creation;
			int tag;
			
			tag = this.read1();
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			if (tag != OtpExternal.portTag)
			{
				throw new Erlang.DecodeException("Wrong tag encountered, expected " + OtpExternal.portTag + ", got " + tag);
			}
			
			node = this.read_atom();
			id = this.read4BE() & 0x3ffff; // 18 bits
			creation = this.read1() & 0x03; // 2 bits
			
			return new Erlang.Port(node, id, creation);
		}
		
		/*
		* Read an Erlang reference from the stream.
		*
		* @return the value of the reference
		* 
		* @exception DecodeException if the next term in the
		* stream is not an Erlang reference.
		**/
		public virtual Erlang.Ref read_ref()
		{
			System.String node;
			int id;
			int creation;
			int tag;
			
			tag = this.read1();
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			switch (tag)
			{
				case OtpExternal.refTag: 
					node = this.read_atom();
					id = this.read4BE() & 0x3ffff; // 18 bits
					creation = this.read1() & 0x03; // 2 bits
					return new Erlang.Ref(node, id, creation);
					
				
				
				case OtpExternal.newRefTag: 
					int arity = this.read2BE();
					node = this.read_atom();
					creation = this.read1() & 0x03; // 2 bits
					
					int[] ids = new int[arity];
					 for (int i = 0; i < arity; i++)
					{
						ids[i] = this.read4BE();
					}
					ids[0] &= 0x3ffff; // first id gets truncated to 18 bits
					return new Erlang.Ref(node, ids, creation);
					
				
				
				default: 
					throw new Erlang.DecodeException("Wrong tag encountered, expected ref, got " + tag);
				
			}
		}
		
		/*
		* Read a string from the stream.
		*
		* @return the value of the string.
		* 
		* @exception DecodeException if the next term in the
		* stream is not a string.
		**/
		public virtual System.String read_string()
		{
			int tag;
			int len;
			byte[] strbuf;
			char[] charbuf;
			
			tag = this.read1();
			if (tag == OtpExternal.versionTag)
			{
				tag = this.read1();
			}
			
			switch (tag)
			{
				
				case OtpExternal.stringTag: 
					char[] tmpChar;
					len = this.read2BE();
					strbuf = new byte[len];
					this.readN(strbuf);
					tmpChar = new char[strbuf.Length];
					strbuf.CopyTo(tmpChar, 0);
					return new System.String(tmpChar);

				case OtpExternal.nilTag:
					return "";

				case OtpExternal.listTag:
					// List when unicode +
					len = this.read4BE();
					charbuf = new char[len];

					 for (int i = 0; i < len; i++)
						charbuf[i] = this.read_char();

					this.read_nil();
					return new System.String(charbuf);

				default:
					throw new Erlang.DecodeException("Wrong tag encountered, expected " + OtpExternal.stringTag + " or " + OtpExternal.listTag + ", got " + tag);
				
			}
		}

		/*
		* Read an arbitrary Erlang term from the stream.
		*
		* @return the Erlang term.
		*
		* @exception DecodeException if the stream does not
		* contain a known Erlang type at the next position.
		**/
		public virtual Erlang.Object read_any()
		{
			// calls one of the above functions, depending on o
			int tag = this.peek();
			if (tag == OtpExternal.versionTag)
			{
				this.read1();
				tag = this.peek();
			}

            //System.Diagnostics.Debug.WriteLine("read_any: tag = " + tag);

			switch (tag)
			{
				case OtpExternal.smallIntTag: case OtpExternal.intTag: case OtpExternal.smallBigTag:
					return new Erlang.Long(this);

				case OtpExternal.atomTag:
					return new Erlang.Atom(this);

				case OtpExternal.floatTag:
                case OtpExternal.newFloatTag:
                    return new Erlang.Double(this);

				case OtpExternal.refTag: case OtpExternal.newRefTag:
					return new Erlang.Ref(this);

				case OtpExternal.portTag:
					return new Erlang.Port(this);

				case OtpExternal.pidTag:
					return new Erlang.Pid(this);

				case OtpExternal.stringTag:
					return new Erlang.String(this);

				case OtpExternal.listTag: case OtpExternal.nilTag:
					return new Erlang.List(this);

				case OtpExternal.smallTupleTag: case OtpExternal.largeTupleTag:
					return new Erlang.Tuple(this);

				case OtpExternal.binTag:
					return new Erlang.Binary(this);

				case OtpExternal.largeBigTag: default:
					throw new Erlang.DecodeException("Uknown data type: " + tag);
				
			}
		}
	}
}