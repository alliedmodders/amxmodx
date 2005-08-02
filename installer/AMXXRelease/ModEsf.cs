using System;

namespace AMXXRelease
{
	/// <summary>
	/// Summary description for ModEsf.
	/// </summary>
	public class ModEsf : AMod
	{
		public ModEsf()
		{
			AddModules();
		}

		public override sealed string GetName()
		{
			return "esf";
		}

		private void AddModules()
		{
			Module esfmod = new Module();
			esfmod.sourcedir = "dlls\\esforces\\esfmod";
			esfmod.vcproj = "esfmod";
			esfmod.projname = "esfmod_amxx";

			m_Modules.Add(esfmod);
		}
	}
}
