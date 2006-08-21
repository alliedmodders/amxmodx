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
		private ArrayList Files;
		private string title;
		private string version;
		private int index;

		public string File
		{
			get
			{
				return Filename;
			}
		}

		public string Title
		{
			get
			{
				return title;
			}
			set
			{
				title = value;
			}
		}

		public string Version
		{
			get
			{
				return version;
			}
			set
			{
				version = value;
			}
		}

		public string Status
		{
			get
			{
				if (status == 0)
					return "Failed";
				else if (status == 1)
					return "Running";
				else if (status == 2)
					return "Debug";
				return "";
			}
		}

		public int Index
		{
			get
			{
				return index;
			}
		}

		public Plugin(string name, int natives, int publics, int files, byte _status, int _index)
		{
			Filename = name;
			Natives = new ArrayList(natives);
			Publics = new ArrayList(publics);
			Files = new ArrayList(files+1);
			status = _status;
			index = _index;
		}

		public void AddNative(string name)
		{
			Natives.Add(name);
		}

		public void AddPublic(string pubname)
		{
			Publics.Add(pubname);
		}

		public void AddFile(string filename)
		{
			Files.Add(filename);
		}

		public string FindNative(int id)
		{
			if (id < 0 || id >= Natives.Count)
			{
				return null;
			}

			return (string)Natives[id];
		}

		public string FindFile(int id)
		{
			if (id < 0 || id >= Files.Count)
			{
				return null;
			}

			return (string)Files[id];
		}

		public string FindPublic(int id)
		{
			if (id < 0 || id >= Publics.Count)
			{
				return null;
			}

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
