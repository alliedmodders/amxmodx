using System;

namespace AMXXRelease
{
	/// <summary>
	/// Summary description for ModNs.
	/// </summary>
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
