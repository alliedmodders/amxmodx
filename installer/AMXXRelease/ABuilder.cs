using System;
using System.Diagnostics;
using System.IO;

namespace AMXXRelease
{
	//This class specifies the process that builds a release.
	//It also implements the functions that are unlikely to change from mod to mod.
	public abstract class ABuilder
	{
		protected Config m_Cfg;

		public virtual void OnBuild()
		{
		}

		public virtual void CreateDir(string dir)
		{
			Directory.CreateDirectory(dir);
		}

		public virtual bool Build(Config cfg, Build build)
		{
			m_Cfg = cfg;

			OnBuild();
			
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
				if (m_Cfg.CompressPath() != null)
				{
					CompressDir(
						PropSlashes(m_Cfg.OutputPath() + "\\" + m_Cfg.GetReleaseName() + "-" + mod.GetName()), 
						PropSlashes(m_Cfg.OutputPath() + "\\" + mod.GetName())
						);
				}
			}

			return true;
		}

		public virtual bool BuildMod(AMod mod)
		{
			CopyConfigs(mod);
			if (!BuildModModules(mod))
				return false;
			if (!BuildModPlugins(mod))
				return false;
			
			string basedir = PropSlashes(m_Cfg.OutputPath() + "\\" + mod.GetModPath());
			string sourcetree = m_Cfg.GetSourceTree();

			if (!mod.CopyExtraFiles(this, basedir, sourcetree))
				return false;
			
			return true;
		}

		public virtual void CopyConfigs(AMod mod)
		{
			string basedir = PropSlashes(m_Cfg.OutputPath() + "\\" + mod.GetModPath() + "\\configs");

			if (!Directory.Exists(basedir))
				CreateDir(basedir);

			string srcdir = PropSlashes(m_Cfg.GetSourceTree() + "\\configs");

			if (!Directory.Exists(srcdir))
				return;

			if (mod.GetPluginDir() != null)
				srcdir += "\\" + mod.GetBaseName();

			srcdir = PropSlashes(srcdir);

			CopyNormal(this, srcdir, basedir);
		}

		public static void CopyNormal(ABuilder ab, string src, string dest)
		{
			string[] files = Directory.GetFiles(src);

			if (!Directory.Exists(dest))
				ab.CreateDir(dest);

			for (int i=0; i<files.Length; i++)
			{
				File.Copy(files[i],
						  PropSlashes(dest + "\\" + GetFileName(files[i])),
						  true);
			}
		}

		public virtual bool BuildModPlugins(AMod mod)
		{
			int num = mod.GetPlugins();

			Plugin plugin;
			string binary, basedir;

			basedir = m_Cfg.OutputPath();
			basedir += "\\" + mod.GetModPath();
			basedir = PropSlashes(basedir);

			string dir, file, target;
			for (int i=0; i<num; i++)
			{
				plugin = mod.GetPlugin(i);
				binary = BuildPlugin(mod, plugin);
				file = PropSlashes(m_Cfg.GetSourceTree() + "\\plugins\\" + GetFileName(binary));
				if (!File.Exists(file))
				{
					System.Console.WriteLine("Plugin failed to compile: " + 
						mod.GetName() + "::" + plugin.name);
					return false;
				}
				dir = PropSlashes(basedir + "\\" + plugin.outdir);
				if (!Directory.Exists(dir))
					CreateDir(dir);
				target = PropSlashes(dir + "\\" + plugin.name + ".amxx");
				if (File.Exists(target))
					File.Delete(target);
				File.Move(file,
					target);
			}

			//Copy all files from the plugins dir to scripting
			
			string search_dir = m_Cfg.GetSourceTree() + "\\plugins";
			if (mod.GetPluginDir() != null)
				search_dir += "\\" + mod.GetPluginDir();
			search_dir = PropSlashes(search_dir);

			string dest;
			if (Directory.Exists(search_dir))
			{
				string[] files = Directory.GetFiles(search_dir);
				if (!Directory.Exists(PropSlashes(basedir + "\\scripting")))
					CreateDir(PropSlashes(basedir + "\\scripting"));
				for (int i=0; i<files.Length; i++)
				{
					dest = PropSlashes(basedir + "\\scripting\\" + GetFileName(files[i]));
					if (mod.ExcludeCopy(files[i]))
						continue;
					File.Copy(files[i], dest, true);
				}
			}

			return true;
		}

		public static string GetFileName(string input)
		{
			for (int i=input.Length-1; i>=0; i--)
			{
				if ((input[i] == '\\' || input[i] == '/') && i != input.Length-1)
				{
					return input.Substring(i+1, input.Length-i-1);
				}
			}

			return input;
		}

		public virtual bool BuildModModules(AMod mod)
		{
			int num = mod.GetModules();

			Module module;
			string binary, basedir;

			basedir = m_Cfg.OutputPath();
			basedir += "\\" + mod.GetModPath();
			basedir = PropSlashes(basedir);

			string dir;
			for (int i=0; i<num; i++)
			{
				module = mod.GetModule(i);
				binary = BuildModule(module);

				if (binary == null)
				{
					System.Console.WriteLine("Module failed to compile: " + 
						mod.GetName() + "::" + module.projname + GetLibExt());	
					return false;
				}
				dir = PropSlashes(basedir + "\\" + module.outdir);
				if (!Directory.Exists(dir))
					CreateDir(dir);
				File.Copy(binary,
					PropSlashes(dir + "\\" + module.projname + GetLibExt()),
					true);
			}

			return true;
		}

		public virtual string BuildPlugin(AMod mod, Plugin pl)
		{
			string modoffs = mod.GetPluginDir();
			string pldir;

			if (modoffs != null)
				pldir = modoffs + "\\";
			else
				pldir = "";

			AmxxPc(PropSlashes(pldir + pl.source), pl.options);

			string outfile = pldir + pl.name + ".amxx";

			return outfile;
		}

		public abstract void AmxxPc(string inpath, string args);

		public abstract string BuildModule(Module module);

		public abstract string GetLibExt();

		public abstract void CompressDir(string target, string dir);

		public static string PropSlashes(string path)
		{
			char sep;
			char alt;
			if (Releaser.IsWindows)
			{
				sep = '\\';
				alt = '/';
			} else {
				sep = '/';
				alt = '\\';
			}
			return path.Replace(alt,sep);
		}
	}
}

