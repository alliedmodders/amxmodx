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
		public static bool IsWindows;

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

			if (!ValidateConfigPaths())
				return;

			ABuilder builder = null;
			if ((int)System.Environment.OSVersion.Platform == 128)
			{
				builder = new LinuxBuilder();
				Releaser.IsWindows = false;
			} else {
				builder = new Win32Builder();
				Releaser.IsWindows = true;
			}

			Build build = new Build(m_Cfg);

			builder.Build(m_Cfg, build);
		}

		private bool ValidateConfigPaths()
		{
			string source = ABuilder.PropSlashes(m_Cfg.GetSourceTree());

			if (!Directory.Exists(source))
			{
				Console.WriteLine("Failed to find source tree! Check 'source' option in config.");
				return false;
			} else {
				// Check subdirectories of source tree to make sure they contain necessary directories
				if (!Directory.Exists(ABuilder.PropSlashes(source + "\\amxmodx")) || 
					!Directory.Exists(ABuilder.PropSlashes(source + "\\configs")) ||
					!Directory.Exists(ABuilder.PropSlashes(source + "\\dlls"))    ||
					!Directory.Exists(ABuilder.PropSlashes(source + "\\plugins")))
				{
					Console.WriteLine("Source tree appears invalid! Check 'source' option in config.");
					return false;
				}
			}

			if ( !File.Exists( ABuilder.PropSlashes(m_Cfg.DevenvPath()) ) )
			{
				Console.WriteLine("Failed to find compilation program! Check 'devenv' option in config.");
				return false;
			}

			return true;
		}
	}
}

