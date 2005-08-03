using System;
using System.Diagnostics;
using System.IO;

namespace AMXXRelease
{
	/// <summary>
	/// Summary description for Class1.
	/// </summary>
	class Builder
	{
		public Config m_Cfg;
		private string m_AmxxPc;

		public Builder()
		{
			m_Cfg = new Config();

			m_AmxxPc = m_Cfg.GetSourceTree() + "\\plugins\\amxxpc.exe";
		}

		[STAThread]
		static void Main(string[] args)
		{
			Builder b = new Builder();
			Release15 r15 = new Release15();

			if (!b.Build(r15))
			{
				System.Console.WriteLine("Build failed: " + r15.GetName());
			} 
			else 
			{
				System.Console.WriteLine("Build succeeded!");
			}
            
		}

		public bool Build(ABuild build)
		{
			int num = build.GetMods();

			AMod mod;
			for (int i=0; i<num; i++)
			{
				mod = build.GetMod(i);
				if (!BuildMod(mod))
				{
					System.Console.WriteLine("Mod failed to build: " + mod.GetName());
					return false;
				}
				ZipDir(build.GetName() + "-" + mod.GetName() + ".zip", mod.GetName() + "\\*.*");
			}

			return true;
		}

		public bool BuildMod(AMod mod)
		{
			CopyConfigs(mod);
			if (!BuildModModules(mod))
				return false;
			if (!BuildModPlugins(mod))
				return false;
			
			string basedir = m_Cfg.OutputPath() + "\\" + mod.GetModPath();
			string sourcetree = m_Cfg.GetSourceTree();

			if (!mod.CopyExtraFiles(basedir, sourcetree))
				return false;
			
			return true;
		}

		public void ZipDir(string target, string dir)
		{
			ProcessStartInfo info = new ProcessStartInfo();

			info.FileName = m_Cfg.PathToZip();
			info.WorkingDirectory = m_Cfg.OutputPath();
			info.Arguments = "-r " + target + " " + dir;
			info.UseShellExecute = false;

			Process p = Process.Start(info);
			p.WaitForExit();
		}

		public void CopyConfigs(AMod mod)
		{
			string basedir = m_Cfg.OutputPath() + "\\" + mod.GetModPath() + "\\configs";

			if (!Directory.Exists(basedir))
				Directory.CreateDirectory(basedir);

			string srcdir = m_Cfg.GetSourceTree() + "\\configs";

			if (!Directory.Exists(srcdir))
				return;

			if (mod.GetPluginDir() != null)
				srcdir += "\\" + mod.GetBaseName();

			CopyNormal(srcdir, basedir);
		}

		public static void CopyNormal(string src, string dest)
		{
			string[] files = Directory.GetFiles(src);

			if (!Directory.Exists(dest))
				Directory.CreateDirectory(dest);

			for (int i=0; i<files.Length; i++)
			{
				File.Copy(files[i],
						  dest + "\\" + GetFileName(files[i]),
						  true);
			}
		}

		public bool BuildModPlugins(AMod mod)
		{
			int num = mod.GetPlugins();

			Plugin plugin;
			string binary, basedir;

			basedir = m_Cfg.OutputPath();
			basedir += "\\" + mod.GetModPath();

			string dir, file, target;
			for (int i=0; i<num; i++)
			{
				plugin = mod.GetPlugin(i);
				binary = BuildPlugin(mod, plugin);
				file = m_Cfg.GetSourceTree() + "\\plugins\\" + GetFileName(binary);
				if (!File.Exists(file))
				{
					System.Console.WriteLine("Plugin failed to compile: " + 
						mod.GetName() + "::" + plugin.name);
					return false;
				}
				dir = basedir + "\\" + plugin.outdir;
				if (!Directory.Exists(dir))
					Directory.CreateDirectory(dir);
				target = dir + "\\" + plugin.name + ".amxx";
				if (File.Exists(target))
					File.Delete(target);
				File.Move(file,
					target);
			}

			//Copy all files from the plugins dir to scripting
			
			string search_dir = m_Cfg.GetSourceTree() + "\\plugins";
			if (mod.GetPluginDir() != null)
				search_dir += "\\" + mod.GetPluginDir();

			string dest;
			if (Directory.Exists(search_dir))
			{
				string[] files = Directory.GetFiles(search_dir);
				if (!Directory.Exists(basedir + "\\scripting"))
					Directory.CreateDirectory(basedir + "\\scripting");
				for (int i=0; i<files.Length; i++)
				{
					dest = basedir + "\\scripting\\" + GetFileName(files[i]);
					File.Copy(files[i], dest, true);
				}
			}

			return true;
		}

		private static string GetFileName(string input)
		{
			for (int i=input.Length-1; i>=0; i--)
			{
				if (input[i] == '\\' && i != input.Length-1)
				{
					return input.Substring(i+1, input.Length-i-1);
				}
			}

			return input;
		}

		public bool BuildModModules(AMod mod)
		{
			int num = mod.GetModules();

			Module module;
			string binary, basedir;

			basedir = m_Cfg.OutputPath();
			basedir += "\\" + mod.GetModPath();

			string dir;
			for (int i=0; i<num; i++)
			{
				module = mod.GetModule(i);
				binary = BuildModule(module);

				if (binary == null)
				{
					System.Console.WriteLine("Module failed to compile: " + 
						mod.GetName() + "::" + module.projname + ".dll");	
					return false;
				}
				dir = basedir + "\\" + module.outdir;
				if (!Directory.Exists(dir))
					Directory.CreateDirectory(dir);
				File.Copy(binary,
					dir + "\\" + module.projname + ".dll",
					true);
			}

			return true;
		}

		public string BuildPlugin(AMod mod, Plugin pl)
		{
			string modoffs = mod.GetPluginDir();
			string pldir;

			if (modoffs != null)
				pldir = modoffs + "\\";
			else
				pldir = "";

			AmxxPc(pldir + pl.source, pl.options);

            string outfile = pldir + pl.name + ".amxx";

			return outfile;
		}

		public void AmxxPc(string inpath, string args)
		{
			ProcessStartInfo info = new ProcessStartInfo();

			info.WorkingDirectory = m_Cfg.GetSourceTree() + "\\plugins";
			info.FileName = (string)m_AmxxPc.Clone();
			info.Arguments = inpath + ".sma";
			if (args != null)
				info.Arguments += " " + args;
			info.UseShellExecute = false;

			Process p = Process.Start(info);
			p.WaitForExit();
		}

		public string BuildModule(Module module)
		{
			ProcessStartInfo info = new ProcessStartInfo();

			string dir = m_Cfg.GetSourceTree() + "\\" + module.sourcedir;
			if (module.bindir != null)
				dir += "\\" + module.bindir;
			string file = dir;
			if (module.bindir == null)
				file += "\\" + module.bindir;
			file += "\\" + module.build + "\\" + module.projname + ".dll";

			if (File.Exists(file))
				File.Delete(file);

			info.WorkingDirectory = dir;
			info.FileName = m_Cfg.DevenvPath();
			info.Arguments = "/build " + module.build + " " + module.vcproj + ".vcproj";
			info.UseShellExecute = false;

			Process p = Process.Start(info);
			p.WaitForExit();

			if (!File.Exists(file))
				return null;

			return file;
		}
	}
}
