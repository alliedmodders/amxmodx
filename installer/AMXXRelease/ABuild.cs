using System;
using System.Collections;

namespace AMXXRelease
{
	/// <summary>
	/// Summary description for ABuild.
	/// </summary>
	public abstract class ABuild
	{
		protected ArrayList m_Mods;

		public ABuild()
		{
			m_Mods = new ArrayList();
		}

		public abstract string GetName();

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
