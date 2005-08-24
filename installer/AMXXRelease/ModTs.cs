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
			Module tsx = new Module();
			tsx.sourcedir = "dlls\\ts\\tsx";
			tsx.projname = "tsx_amxx";
			tsx.bindir = "msvc";
			tsx.vcproj = "tsx_amxx";

			Module tsfun = new Module();
			tsfun.sourcedir = "dlls\\ts\\tsfun";
			tsfun.projname = "tsfun_amxx";
			tsfun.vcproj = "tsfun";

			m_Modules.Add(tsx);
			m_Modules.Add(tsfun);
		}
	}
}
