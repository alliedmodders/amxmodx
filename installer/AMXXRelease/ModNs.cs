using System;

namespace AMXXRelease
{
	//Natural Selection
	public class ModNs : AMod
	{
		public ModNs()
		{
			AddModules();
			AddPlugins();
		}

		public override sealed string GetName()
		{
			return "ns";
		}

		private void AddPlugins()
		{
			AddPlugin("mapchooser");
			AddPlugin("nextmap");
			AddPlugin("timeleft");
			AddPlugin("idlekicker");
			AddPlugin("nscommands");
			AddPlugin("unstuck");
			AddPlugin("plmenu");
		}

		private void AddModules()
		{
			Module ns = new Module("ns");

			m_Modules.Add(ns);
		}
	}
}
