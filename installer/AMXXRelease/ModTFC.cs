using System;

namespace AMXXRelease
{
	//Team Fortress Classic
	public class ModTFC : AMod
	{
		public ModTFC()
		{
			AddModules();
			AddPlugins();
		}

		public override sealed string GetName()
		{
			return "tfc";
		}

		private void AddPlugins()
		{
			AddPlugin("plmenu");
			AddPlugin("stats_logging");
			AddPlugin("statssounds");
			AddPlugin("stats");
			Plugin pl = AddPlugin("tfcstats");
			pl.outdir = "data";
		}

		private void AddModules()
		{
			Module tfcx = new Module();

			tfcx.sourcedir = "dlls\\tfc\\tfcx";
			tfcx.vcproj = "tfcx";
			tfcx.projname = "tfcx_amxx";
			tfcx.bindir = "msvc";

			m_Modules.Add(tfcx);
		}
	}
}
