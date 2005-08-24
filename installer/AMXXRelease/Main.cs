using System;
using System.Diagnostics;
using System.IO;

namespace AMXXRelease
{
	//Entry point for application.
	//1. Reads config file
	//2. Instantiates correct build process (ABuilder)
	//3. Instantiates the build (Build)
	//4. Passes configuration and build to the Builder
	public class Releaser
	{
		private Config m_Cfg;

		[STAThread]
		static void Main(string[] args)
		{
			string file;
			if (args.GetLength(0) < 1)
				file = "release.info";
			else
				file = args[0];

			Releaser r = new Releaser();

			r.Release(file);
		}

		public void Release(string file)
		{
			m_Cfg = new Config();

			file = ABuilder.PropSlashes(file);
			if (!m_Cfg.ReadFromFile(file))
			{
				Console.WriteLine("Failed to read config, aborting!");
				return;
			}

			ABuilder builder = null;
			if ((int)System.Environment.OSVersion.Platform == 128)
			{
				builder = new LinuxBuilder();
			} else {
				builder = new Win32Builder();
			}

			Build build = new Build();

			builder.Build(m_Cfg, build);
		}
	}
}

