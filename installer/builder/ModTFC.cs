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
			Module tfcx = new Module("tfcx");

			m_Modules.Add(tfcx);
		}
	}
}
