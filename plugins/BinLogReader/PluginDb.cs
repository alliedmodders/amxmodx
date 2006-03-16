using System;
using System.Collections;
using System.IO;
using System.Text;

namespace BinLogReader
{
	/// <summary>
	/// Aggregates plugin information
	/// </summary>
	public class PluginDb
	{
		private static uint BINDB_MAGIC = 0x414D4244;
		private static short BINDB_VERSION = 0x0100;
		private ArrayList PluginList;

		public int Count
		{
			get
			{
				return PluginList.Count;
			}
		}

		public PluginDb(uint plugins)
		{
			PluginList = new ArrayList((int)plugins);
		}

		public Plugin GetPluginById(ushort id)
		{
			return GetPluginById((int)id);
		}

		public Plugin GetPluginById(int id)
		{
			if (id < 0 || id >= PluginList.Count)
				return null;

			return (Plugin)PluginList[id];
		}

		public static PluginDb FromFile(string filename)
		{
			if (!File.Exists(filename))
				return null;

			System.IO.FileStream stream = File.Open(filename, System.IO.FileMode.Open);
			if (stream == null)
				return null;
			BinaryReader br = new BinaryReader(stream);
			if (br == null)
				return null;

			PluginDb db;

			try
			{
				//check header
				uint magic = br.ReadUInt32();
				if (magic != BINDB_MAGIC)
					throw new Exception("Invalid magic DB number");
				//check version
				ushort vers = br.ReadUInt16();
				if (vers > BINDB_VERSION)
					throw new Exception("Unknown DB version");
				//read plugins
				uint plugins = br.ReadUInt32();
				db = new PluginDb(plugins);
				for (uint i=0; i<plugins; i++)
				{
					byte status = br.ReadByte();
					byte length = br.ReadByte();
					byte [] name = br.ReadBytes(length + 1);
					uint natives = br.ReadUInt32();
					uint publics = br.ReadUInt32();
					int id = db.CreatePlugin(
						Encoding.ASCII.GetString(name, 0, length), 
						(int)natives,
						(int)publics, 
						status,
						(int)i);
					Plugin pl = db.GetPluginById(id);
					for (uint j=0; j<natives; j++)
					{
						length = br.ReadByte();
						name = br.ReadBytes(length + 1);
						pl.AddNative(Encoding.ASCII.GetString(name, 0, length));
					}
					for (uint j=0; j<publics; j++)
					{
						length = br.ReadByte();
						name = br.ReadBytes(length + 1);
						pl.AddPublic(Encoding.ASCII.GetString(name, 0, length));
					}
				}
			} 
			catch 
			{
				db = null;
				throw new Exception("DB file is corrupt");
			} 
			finally 
			{
				br.Close();
				stream.Close();
				GC.Collect();
			}

			return db;
		}

		private int CreatePlugin(string file, int natives, int publics, byte status, int index)
		{
			Plugin pl = new Plugin(file, natives, publics, status, index);
			PluginList.Add(pl);
			return PluginList.Count - 1;
		}
	}
}
