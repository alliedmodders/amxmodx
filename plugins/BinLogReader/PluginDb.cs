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

		public static PluginDb FromFile(BinaryReader br)
		{
			//read plugins
			uint plugins = br.ReadUInt32();
			PluginDb db = new PluginDb(plugins);
			for (uint i=0; i<plugins; i++)
			{
				byte status = br.ReadByte();
				byte length = br.ReadByte();
				uint files = 0;
				byte [] name = br.ReadBytes(length + 1);
				if (status == 2)
				{
					files = br.ReadUInt32();
				}
				uint natives = br.ReadUInt32();
				uint publics = br.ReadUInt32();
				int id = db.CreatePlugin(
					Encoding.ASCII.GetString(name, 0, length), 
					(int)natives,
					(int)publics,
					(int)files,
					status,
					(int)i);
				Plugin pl = db.GetPluginById(id);
				for (uint j=0; j<files; j++)
				{
					length = br.ReadByte();
					name = br.ReadBytes(length + 1);
					pl.AddFile(Encoding.ASCII.GetString(name, 0, length));
				}
				if (files == 0)
				{
					pl.AddFile(pl.File);
				}
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

			return db;
		}

		private int CreatePlugin(string file, int natives, int publics, int files, byte status, int index)
		{
			Plugin pl = new Plugin(file, natives, publics, files, status, index);
			PluginList.Add(pl);
			return PluginList.Count - 1;
		}
	}
}
