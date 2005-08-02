using System;
using System.IO;

namespace AMXXRelease
{
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

		public override sealed bool CopyExtraFiles(string basedir, string source)
		{
			//Create directory structures
			string datadir = basedir + "\\data";

			if (!Directory.Exists(datadir))
				Directory.CreateDirectory(datadir);

			File.Copy(source + "\\dlls\\geoip\\GeoIP.dat",
					  datadir + "\\GeoIP.dat", 
					  true);

			Builder.CopyNormal(source + "\\plugins\\lang", datadir + "\\lang");

			if (!Directory.Exists(basedir + "\\logs"))
				Directory.CreateDirectory(basedir + "\\logs");

			if (!Directory.Exists(basedir + "\\doc"))
				Directory.CreateDirectory(basedir + "\\doc");

			File.Copy(source + "\\doc\\amxmodx-doc.chm",
					  basedir + "\\doc\\amxmodx-doc.chm",
					  true);

			Builder.CopyNormal(source + "\\plugins\\include", basedir + "\\scripting\\include");

			return true;
		}

		private void AddPlugins()
		{
			AddPlugin("admin");

			Plugin admin_sql = new Plugin("admin_sql");
			admin_sql.source = "admin";
			admin_sql.options = "USING_SQL=1 -oadmin_sql.amx";
			m_Plugins.Add(admin_sql);

			AddPlugin("adminchat");
			AddPlugin("admincmd");
			AddPlugin("adminhelp");
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

			Module mysql = new Module();

			mysql.sourcedir = "mysql";
			mysql.projname = "mysql_amxx";
			mysql.bindir = "mysql_amxx";
			mysql.vcproj = "mysql_amxx";

			Module engine = new Module();
			engine.sourcedir = "engine";
			engine.projname = "engine_amxx";
			engine.vcproj = "engine";

			Module fun = new Module();
			fun.sourcedir = "fun";
			fun.projname = "fun_amxx";
			fun.vcproj = "fun";

			Module geoip = new Module("geoip");
			Module fakemeta = new Module("fakemeta");
			Module sockets = new Module("sockets");
			Module regex = new Module("regex");
			Module nvault = new Module("nvault");

			m_Modules.Add(core);
			m_Modules.Add(mysql);
			m_Modules.Add(engine);
			m_Modules.Add(fun);
			m_Modules.Add(geoip);
			m_Modules.Add(fakemeta);
			m_Modules.Add(sockets);
			m_Modules.Add(regex);
			m_Modules.Add(nvault);
		}
	}
}
