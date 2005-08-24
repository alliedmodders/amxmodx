using System;

namespace AMXXRelease
{
	//Day of Defeat
	public class ModDoD : AMod
	{
		public ModDoD()
		{
			AddModules();
			AddPlugins();
		}

		public override sealed string GetName()
		{
			return "dod";
		}

		private void AddPlugins()
		{
			AddPlugin("stats");
			AddPlugin("plmenu");
			AddPlugin("stats_logging");
			AddPlugin("statssounds");

			Plugin pl = AddPlugin("dodstats");
			pl.outdir = "data";
		}

		private void AddModules()
		{
			Module dodx = new Module();
			dodx.sourcedir = "dlls\\dod2\\dodx";
			dodx.projname = "dodx_amxx";
			dodx.bindir = "msvc";
			dodx.vcproj = "dodx";

			Module dodfun = new Module();
			dodfun.sourcedir = "dlls\\dod2\\dodfun";
			dodfun.projname = "dodfun_amxx";
			dodfun.bindir = "msvc";
			dodfun.vcproj = "dodfun";

			m_Modules.Add(dodx);
			m_Modules.Add(dodfun);
		}
	}
}

