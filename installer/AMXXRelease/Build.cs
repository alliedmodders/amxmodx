using System;
using System.Collections;

namespace AMXXRelease
{
	//Class that iterates the different pieces
	// to be completed over for the build
	public class Build
	{
		protected ArrayList m_Mods;

		public Build()
		{
			m_Mods = new ArrayList();

			CoreMod core = new CoreMod();
			ModCstrike cstrike = new 
ModCstrike();
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

		public virtual int GetMods()
		{
			return m_Mods.Count;
		}

		public virtual AMod GetMod(int i)
		{
			return (AMod)m_Mods[i];
		}
	}
}

