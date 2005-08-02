using System;

namespace AMXXRelease
{
	/// <summary>
	/// Summary description for Config.
	/// </summary>
	public class Config
	{
		public Config()
		{
		}

		public string GetSourceTree()
		{
			return "C:\\real\\code\\amxmodx";
		}

		public string OutputPath()
		{
			return "C:\\real\\done";
		}

		public string DevenvPath()
		{
			return "C:\\Program Files\\Microsoft Visual Studio .NET 2003\\Common7\\IDE\\devenv.com";
		}

		public string PathToZip()
		{
			return "C:\\Windows\\zip.exe";
		}
	}
}
