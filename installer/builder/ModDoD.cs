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
			Module dodx = new Module("dodx");
			dodx.sourcedir = "dlls\\dod\\dodx";

			Module dodfun = new Module("dodfun");
			dodfun.sourcedir = "dlls\\dod\\dodfun";

			m_Modules.Add(dodx);
			m_Modules.Add(dodfun);
		}
	}
}

