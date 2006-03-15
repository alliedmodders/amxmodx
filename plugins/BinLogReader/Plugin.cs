using System;
using System.Collections;

namespace BinLogReader
{
	/// <summary>
	/// plugin data
	/// </summary>
	public class Plugin
	{
		private byte status;
		private string Filename;
		private ArrayList Natives;
		private ArrayList Publics;

		public Plugin(string name, int natives, int publics, byte _status)
		{
			Filename = name;
			Natives = new ArrayList(natives);
			Publics = new ArrayList(publics);
			status = _status;
		}

		public void AddNative(string name)
		{
			Natives.Add(name);
		}

		public void AddPublic(string pubname)
		{
			Publics.Add(pubname);
		}

		public string FindNative(int id)
		{
			if (id < 0 || id >= Natives.Count)
				return null;

			return (string)Natives[id];
		}

		public string FindPublic(int id)
		{
			if (id < 0 || id >= Publics.Count)
				return null;

			return (string)Publics[id];
		}

		public bool IsValid()
		{
			return status != 0;
		}
		
		public bool IsDebug()
		{
			return status == 2;
		}
	}
}
