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
	* Provides a C# representation of Erlang binaries. Anything that
	*  can be represented as a sequence of bytes can be made into an
	*  Erlang binary.
	**/
	[Serializable]
    public class Binary:Erlang.Object
	{
		// binary contents
		private byte[] bin;
		
		/*
		* Create a binary from a byte array
		*
		* @param bin the array of bytes from which to create the binary.
		**/
		public Binary(byte[] bin)
		{
			this.bin = new byte[bin.Length];
			Array.Copy(bin, 0, this.bin, 0, bin.Length);
		}
		
		/*
		* Create a binary from a stream containinf a binary encoded in
		* Erlang external format.
		*
		* @param buf the stream containing the encoded binary.
		*
		* @exception DecodeException if the buffer does not
		* contain a valid external representation of an Erlang binary.
		**/
		public Binary(OtpInputStream buf)
		{
			this.bin = buf.read_binary();
		}
		
		/*
		* Create a binary from an arbitrary C# Object. The object must
		* implement C#.io.Serializable or C#.io.Externalizable.
		*
		* @param o the object to serialize and create this binary from.
		**/
		public Binary(System.Object o)
		{
			try
			{
				this.bin = toByteArray(o);
			}
			catch (System.IO.IOException)
			{
				throw new System.ArgumentException("Object must implement Serializable");
			}
		}
		
		private static byte[] toByteArray(System.Object o)
		{
			
			if (o == null)
				return null;
			
			/*need to synchronize use of the shared baos */
			System.IO.MemoryStream baos = new System.IO.MemoryStream();
			System.IO.BinaryWriter oos = new System.IO.BinaryWriter(baos);
			
			SupportClass.Serialize(oos, o);
			oos.Flush();
			
			return baos.ToArray();
		}
		
		private static System.Object fromByteArray(byte[] buf)
		{
			if (buf == null)
				return null;

			try
			{
				System.IO.MemoryStream bais = new System.IO.MemoryStream(buf);
				System.IO.BinaryReader ois = new System.IO.BinaryReader(bais);
				return SupportClass.Deserialize(ois);
			}
			catch (System.Exception)
			{
			}

			return null;
		}
		
		/*
		* Get the byte array from a binary.
		* 
		* @return the byte array containing the bytes for this binary.
		**/
		public virtual byte[] binaryValue()
		{
			return bin;
		}
		
		/*
		* Get the size of the binary.
		* 
		* @return the number of bytes contained in the binary.
		**/
		public virtual int size()
		{
			return bin.Length;
		}
		
		/*
		* Get the C# Object from the binary. If the binary contains a
		* serialized C# object, then this method will recreate the
		* object.
		*
		* 
		* @return the C# Object represented by this binary, or null if
		* the binary does not represent a C# Object.
		**/
		public virtual System.Object getObject()
		{
			return fromByteArray(this.bin);
		}
		
		
		/*
		* Get the string representation of this binary object. A binary is
		* printed as #Bin&lt;N&gt;, where N is the number of bytes
		* contained in the object.
		*
		* @return the Erlang string representation of this binary.
		**/
		public override System.String ToString()
		{
			return "#Bin<" + bin.Length + ">";
		}
		
		/*
		* Convert this binary to the equivalent Erlang external representation.
		*
		* @param buf an output stream to which the encoded binary should be
		* written.
		**/
		public override void  encode(OtpOutputStream buf)
		{
			buf.write_binary(this.bin);
		}
		
		/*
		* Determine if two binaries are equal. Binaries are equal if they have
		* the same length and the array of bytes is identical.
		*
		* @param o the binary to compare to.
		*
		* @return true if the byte arrays contain the same bytes, false
		* otherwise.
		**/
		public override bool Equals(System.Object o)
		{
			if (!(o is Binary))
				return false;
			
			Binary bin = (Binary) o;
			int size = this.size();
			
			if (size != bin.size())
				return false;
			
			 for (int i = 0; i < size; i++)
			{
				if (this.bin[i] != bin.bin[i])
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
			Binary newBin = (Binary) (base.clone());
			newBin.bin = new byte[bin.Length];
			bin.CopyTo(newBin.bin, 0);
			return newBin;
		}
	}
}