using System;
using System.IO;

namespace BinLogReader
{
	/// <summary>
	/// binary log stuff yah
	/// </summary>
	public class BinLog
	{
		private static uint BINLOG_MAGIC = 0x414D424C
		private static short BINLOG_VERSION = 0x0100;

		public static BinLog FromFile(string filename)
		{
			if (!File.Exists(filename))
				return null;

			System.IO.FileStream stream = File.Open(filename, System.IO.FileMode.Open);
			if (stream == null)
				return null;
			BinaryReader br = new BinaryReader(stream);
			if (br == null)
				return null;

			try
			{
			} 
			catch 
			{
			} 
			finally
			{
				br.Close();
				stream.Close();
				GC.Collect();
			}
		}
	}
}
