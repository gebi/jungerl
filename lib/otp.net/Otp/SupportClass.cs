using System;
public class SupportClass
{
	public static long currentTimeMillis()
	{
		return(Convert.ToInt64(Math.Round(DateTime.UtcNow.Subtract(new DateTime(1970,1,1,0,0,0,0)).TotalMilliseconds)));
	}

	/*******************************/
	public static int URShift(int number, int bits)
	{
		if ( number >= 0)
			return number >> bits;
		else
			return (number >> bits) + (2 << ~bits);
	}

	public static int URShift(int number, long bits)
	{
		return URShift(number, (int)bits);
	}

	public static long URShift(long number, int bits)
	{
		if ( number >= 0)
			return number >> bits;
		else
			return (number >> bits) + (2L << ~bits);
	}

	public static long URShift(long number, long bits)
	{
		return URShift(number, (int)bits);
	}


	/*******************************/
	public static void Serialize(System.IO.BinaryWriter binaryWriter, System.Object obj)
	{
		System.Runtime.Serialization.Formatters.Binary.BinaryFormatter formatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter();
		formatter.Serialize(binaryWriter.BaseStream, obj);
	}

	/*******************************/
	public static System.Object Deserialize(System.IO.BinaryReader binaryReader)
	{
		System.Runtime.Serialization.Formatters.Binary.BinaryFormatter formatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter();
		return formatter.Deserialize(binaryReader.BaseStream);
	}

}
