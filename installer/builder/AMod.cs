using System;
using System.Diagnostics;
using System.Collections;

namespace AMXXRelease
{
	//Holds information about a plugin
	public class Plugin
	{
		public string name;		//Plugin output file name
		public string source;		//Source code file
		public string options;		//Compile-time options
		public string outdir;		//Output folder

		public Plugin(string Name)
		{
			name = (string)Name.Clone();
			source = (string)Name.Clone();
			outdir = "plugins";
		}
	}

	//Holds information necessary to compile a module/C++ program
	public class Module
	{
		public string sourcedir;	//Source directory
		public string projname;		//Output binary name (such as amxmodx_mm)
		public string build;		//Build configuration
		public string bindir;		//Binary directory
		public string vcproj;		//VCProj file name
		public string outdir;		//Output directory

		public Module()
		{
			build = "Release";
			outdir = "modules";
			bindir = "msvc10";
		}
		
		public Module(string name)
		{
			build = "Release";
			outdir = "modules";
			sourcedir = "dlls\\" + name;
			bindir = "msvc10";
			projname = name + "_amxx";
			vcproj = name;
		}
	}

	//Class that represents how a mod wants to be built.
	//It exports a list of functions, mods, and a few
	// tidbits of information.  It can also hook an extra
	// step for copying miscellanious files.
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

		//called when it's okay to build an extra dir structure
		// and copy files to it
		public virtual bool CopyExtraFiles(ABuilder ab, string basedir, string sourcedir)
		{
			return true;
		}

		//defines a copy prevention filter
		public virtual bool ExcludeCopy(string file)
		{
			return false;
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
