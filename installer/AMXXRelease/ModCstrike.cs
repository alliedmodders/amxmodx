using System;
using System.IO;

namespace AMXXRelease
{
	/// <summary>
	/// Summary description for ModCstrike.
	/// </summary>
	public class ModCstrike : AMod
	{
		public ModCstrike()
		{
			AddModules();
			AddPlugins();
		}

		public override sealed string GetName()
		{
			return "cstrike";
		}

		private void AddPlugins()
		{
			AddPlugin("miscstats");
			AddPlugin("stats_logging");
			AddPlugin("statsx");
			AddPlugin("restmenu");
			
			Plugin csstats = new Plugin("csstats");
			csstats.outdir = "data";
			m_Plugins.Add(csstats);
		}

		public override sealed bool CopyExtraFiles(string basedir, string source)
		{
			File.Copy(source + "\\dlls\\csx\\source\\WinCSX\\Release\\WinCSX.exe",
					  basedir + "\\data\\WinCSX.exe",
					  true);

			return true;
		}

		private void AddModules()
		{
			Module csx = new Module();
			csx.sourcedir = "dlls\\csx\\source";
			csx.projname = "csx_amxx";
			csx.bindir = "msvc";
			csx.vcproj = "csx";

			Module cstrike = new Module();
			cstrike.sourcedir = "cstrike";
			cstrike.projname = "cstrike_amxx";
			cstrike.vcproj = "cstrike";

			m_Modules.Add(csx);
			m_Modules.Add(cstrike);
		}
	}
}
