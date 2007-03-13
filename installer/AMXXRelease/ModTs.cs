using System;

namespace AMXXRelease
{
	//The Specialists
	public class ModTs : AMod
	{
		public ModTs()
		{
			AddModules();
			AddPlugins();
		}

		public override sealed string GetName()
		{
			return "ts";
		}

		private void AddPlugins()
		{
			AddPlugin("stats");
			AddPlugin("stats_logging");
			AddPlugin("statssounds");
			Plugin pl = AddPlugin("tsstats");
			pl.outdir = "data";
		}

		private void AddModules()
		{
			Module tsx = new Module("tsx");
			tsx.sourcedir = "dlls\\ts\\tsx";

			Module tsfun = new Module("tsfun");
			tsfun.sourcedir = "dlls\\ts\\tsfun";

			m_Modules.Add(tsx);
			m_Modules.Add(tsfun);
		}
	}
}
