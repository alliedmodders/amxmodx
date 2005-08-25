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
		}

		private void AddModules()
		{
			Module ns = new Module();

			ns.sourcedir = "dlls\\ns\\ns";
			ns.projname = "ns_amxx";
			ns.vcproj = "ns";

			m_Modules.Add(ns);
		}
	}
}
