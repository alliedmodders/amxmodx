using System;
using System.IO;

namespace AMXXRelease
{
	//AMX Mod X core distribution
	public class CoreMod : AMod
	{
		public CoreMod()
		{
			AddModules();
			AddPlugins();
		}

		public override sealed string GetName()
		{
			return "base";
		}

		public override sealed string GetPluginDir()
		{
			return null;
		}

		public override sealed string GetBaseName()
		{
			return null;
		}

		//annoyingly complicated file exclusion filter
		public override sealed bool ExcludeCopy(string file)
		{
			if ( ((file.IndexOf(".so")!=-1) || (ABuilder.GetFileName(file).CompareTo("amxxpc")==0))
				&& (Releaser.IsWindows) )
				return true;
			if ( (file.IndexOf(".sh")!=-1) && Releaser.IsWindows )
				return true;
			if ( ((file.IndexOf(".exe")!=-1) || (file.IndexOf(".dll")!=-1))
				&& (!Releaser.IsWindows) )
				return true;
			if ( (file.IndexOf("dlsym")!=-1) && Releaser.IsWindows )
				return true;
			if ( ((ABuilder.GetFileName(file).CompareTo("sasm")) == 0) 
				  && Releaser.IsWindows )
				return true;

			return base.ExcludeCopy(file);
		}


		public override sealed bool CopyExtraFiles(ABuilder ab, string basedir, string source)
		{
			//Create directory structures
			string datadir = basedir + "\\data";

			if (!Directory.Exists(ABuilder.PropSlashes(datadir)))
				ab.CreateDir(ABuilder.PropSlashes(datadir));

			File.Copy(ABuilder.PropSlashes(source + "\\dlls\\geoip\\GeoIP.dat"),
					  ABuilder.PropSlashes(datadir + "\\GeoIP.dat"), 
					  true);

			ABuilder.CopyNormal(ab,
					ABuilder.PropSlashes(source + "\\plugins\\lang"), 
					ABuilder.PropSlashes(datadir + "\\lang")
					);

			if (!Directory.Exists(ABuilder.PropSlashes(basedir + "\\logs")))
				ab.CreateDir(ABuilder.PropSlashes(basedir + "\\logs"));

			ABuilder.CopyNormal(ab,
				ABuilder.PropSlashes(source + "\\plugins\\include"), 
				ABuilder.PropSlashes(basedir + "\\scripting\\include"));

			ABuilder.CopyNormal(ab,
				ABuilder.PropSlashes(source + "\\plugins\\include\\amxmod_compat"), 
				ABuilder.PropSlashes(basedir + "\\scripting\\include\\amxmod_compat"));

			ABuilder.CopyNormal(ab,
				ABuilder.PropSlashes(source + "\\plugins\\amxmod_compat"),
				ABuilder.PropSlashes(basedir + "\\scripting\\amxmod_compat"));

			ABuilder.CopyNormal(ab,
				ABuilder.PropSlashes(source + "\\plugins\\testsuite"),
				ABuilder.PropSlashes(basedir + "\\scripting\\testsuite"));

			return true;
		}

		private void AddPlugins()
		{
			AddPlugin("admin");

			Plugin admin_sql = new Plugin("admin_sql");
			admin_sql.source = "admin";
			admin_sql.options = "USING_SQL=1 -oadmin_sql.amx";
			m_Plugins.Add(admin_sql);

			Plugin bcompat = new Plugin("amxmod_compat");
			bcompat.source = "amxmod_compat/amxmod_compat";
			m_Plugins.Add(bcompat);

			AddPlugin("adminchat");
			AddPlugin("admincmd");
			AddPlugin("adminhelp");
			AddPlugin("adminslots");
			AddPlugin("adminvote");
			AddPlugin("antiflood");
			AddPlugin("imessage");
			AddPlugin("mapchooser");
			AddPlugin("mapsmenu");
			AddPlugin("menufront");
			AddPlugin("multilingual");
			AddPlugin("nextmap");
			AddPlugin("pausecfg");
			AddPlugin("plmenu");
			AddPlugin("scrollmsg");
			AddPlugin("statscfg");
			AddPlugin("telemenu");
			AddPlugin("timeleft");
			AddPlugin("cmdmenu");
		}

		private void AddModules()
		{
			Module core = new Module();

			core.bindir = "msvc";
			core.sourcedir = "amxmodx";
			core.vcproj = "amxmodx_mm";
			core.build = "JITRelease";
			core.projname = "amxmodx_mm";
			core.outdir = "dlls";

			Module mysqlx = new Module();
			mysqlx.projname = "mysql_amxx";
			mysqlx.sourcedir = "dlls\\" + "mysqlx";
			mysqlx.vcproj = "mysql2";

			Module sqlitex = new Module();
			sqlitex.projname = "sqlite_amxx";
			sqlitex.sourcedir = "dlls\\" + "sqlite";
			sqlitex.bindir = "msvc7";
			sqlitex.vcproj = "sqlite_amxx";

			Module engine = new Module();
			engine.sourcedir = "dlls\\engine";
			engine.projname = "engine_amxx";
			engine.vcproj = "engine";

			Module fun = new Module();
			fun.sourcedir = "dlls\\fun";
			fun.projname = "fun_amxx";
			fun.vcproj = "fun";

			Module geoip = new Module("geoip");
			Module fakemeta = new Module("fakemeta");
			Module sockets = new Module("sockets");
			Module regex = new Module("regex");
			Module nvault = new Module("nvault");

			m_Modules.Add(core);
			m_Modules.Add(mysqlx);
			m_Modules.Add(engine);
			m_Modules.Add(fun);
			m_Modules.Add(geoip);
			m_Modules.Add(fakemeta);
			m_Modules.Add(sockets);
			m_Modules.Add(regex);
			m_Modules.Add(nvault);
			m_Modules.Add(sqlitex);
		}
	}
}
