using System;
using System.IO;

namespace AMXXRelease
{
	//Reads in config file info
	public class Config
	{
		private string m_SourceTree;
		private string m_OutputPath;
		private string m_DevenvPath;
		private string m_PathToCompress;
		private string m_ReleaseName;
		private string m_MakeOpts;

		public Config()
		{
		}

		public string GetSourceTree()
		{
			return m_SourceTree;
		}

		public string OutputPath()
		{
			return m_OutputPath;
		}

		public string DevenvPath()
		{
			return m_DevenvPath;
		}

		public string CompressPath()
		{
			return m_PathToCompress;
		}

		public string GetReleaseName()
		{
			return m_ReleaseName;
		}

		public string MakeOpts()
		{
			return m_MakeOpts;
		}

		public bool ReadFromFile(string file)
		{
			try
			{
				StreamReader sr = new StreamReader(file);
	
				string line;
				string delim = "\t \n\r\v";
				string splt = "=";
				while ( (line = sr.ReadLine()) != null )
				{
					line = line.Trim(delim.ToCharArray());
					if (line.Length < 1 || line[0] == ';')
						continue;
					string [] s = line.Split(splt.ToCharArray());
					string key, val="";
					if (s.GetLength(0) >= 1)
					{
						key = s[0];
						if (s.GetLength(0) >= 2)
						{
							for(int i=1; i<s.GetLength(0); i++)
								val += s[i];
						}
						key = key.Trim(delim.ToCharArray());
						val = val.Trim(delim.ToCharArray());
						if (key.CompareTo("compress")==0)
							m_PathToCompress = val;
						if (key.CompareTo("devenv")==0)
							m_DevenvPath = val;
						if (key.CompareTo("output")==0)
							m_OutputPath = val;
						if (key.CompareTo("source")==0)
							m_SourceTree = val;
						if (key.CompareTo("release")==0)
							m_ReleaseName = val;
						if (key.CompareTo("makeopts")==0)
							m_MakeOpts = val;
					}
				}

				return true;
			} catch {
				Console.WriteLine("Unable to read file: " + file);
			}

			return false;
		}
	}
}

