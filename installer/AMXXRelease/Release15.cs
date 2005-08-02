using System;

namespace AMXXRelease
{
	/// <summary>
	/// Summary description for Release15.
	/// </summary>
	public class Release15 : ABuild
	{
		public Release15()
		{
			CoreMod core = new CoreMod();
			ModCstrike cstrike = new ModCstrike();
			ModDoD dod = new ModDoD();
			ModEsf esf = new ModEsf();
			ModNs ns = new ModNs();
			ModTFC tfc = new ModTFC();
			ModTs ts = new ModTs();

			m_Mods.Add(core);
			m_Mods.Add(cstrike);
			m_Mods.Add(dod);
			m_Mods.Add(esf);
			m_Mods.Add(ns);
			m_Mods.Add(tfc);
			m_Mods.Add(ts);
		}

		public override sealed string GetName()
		{
			return "amxmodx-1.5";
		}
	}
}
