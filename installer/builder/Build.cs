using System;
using System.Collections;

namespace AMXXRelease
{
	//Class that iterates the different pieces
	// to be completed over for the build
	public class Build
	{
		protected ArrayList m_Mods;
		protected Config m_Cfg;

		public Build(Config cfg)
		{
			m_Mods = new ArrayList();
			m_Cfg = cfg;

			CoreMod core = new CoreMod();
			ModCstrike cstrike = new ModCstrike();
			ModDoD dod = new ModDoD();
			ModTFC tfc = new ModTFC();

			m_Mods.Add(core);
			m_Mods.Add(cstrike);
			m_Mods.Add(dod);
			m_Mods.Add(tfc);

			// These mods don't have OS X builds
			if (!Releaser.IsOSX)
			{
				ModEsf esf = new ModEsf();
				ModNs ns = new ModNs();
				ModTs ts = new ModTs();

				m_Mods.Add(esf);
				m_Mods.Add(ns);
				m_Mods.Add(ts);
			}
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

