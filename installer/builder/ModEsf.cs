using System;

namespace AMXXRelease
{
	//Earth's Special Forces
	public class ModEsf : AMod
	{
		public ModEsf()
		{
			AddPlugins();
		}

		public override sealed string GetName()
		{
			return "esf";
		}

		private void AddPlugins()
		{
			AddPlugin("EvolutionX.Core");
		}
	}
}
