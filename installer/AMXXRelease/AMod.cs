using System;
using System.Diagnostics;
using System.Collections;

namespace AMXXRelease
{
	public class Plugin
	{
		public string name;
		public string source;
		public string options;
		public string outdir;

		public Plugin(string Name)
		{
			name = (string)Name.Clone();
			source = (string)Name.Clone();
			outdir = "plugins";
		}
	}

	public class Module
	{
		public string sourcedir;
		public string projname;
		public string build;
		public string bindir;
		public string vcproj;
		public string outdir;

		public Module()
		{
			build = "Release";
			outdir = "modules";
		}
		
		public Module(string name)
		{
			build = "Release";
			outdir = "modules";
			sourcedir = "dlls\\" + name;
			projname = name + "_amxx";
			vcproj = name;
		}
	}

	public abstract class AMod
	{
		protected ArrayList m_Modules;
		protected ArrayList m_Plugins;

		public abstract string GetName();
		
		public virtual string GetBaseName()
		{
			return GetName();
		}

		public AMod()
		{
			m_Modules = new ArrayList();
			m_Plugins = new ArrayList();
		}

		public virtual bool CopyExtraFiles(string basedir, string sourcedir)
		{
			return true;
		}

		public virtual string GetPluginDir()
		{
			return GetName();
		}

		public virtual int GetModules()
		{
			return m_Modules.Count;
		}

		public virtual Module GetModule(int i)
		{
			return (Module)m_Modules[i];
		}

		public virtual int GetPlugins()
		{
			return m_Plugins.Count;
		}

		public virtual string GetModPath()
		{
			return GetName() + "\\addons\\amxmodx";
		}

		public virtual Plugin GetPlugin(int i)
		{
			return (Plugin)m_Plugins[i];
		}

		public virtual Plugin AddPlugin(string name)
		{
			Plugin pl = new Plugin(name);
			m_Plugins.Add(pl);
			return pl;
		}
	}


}
