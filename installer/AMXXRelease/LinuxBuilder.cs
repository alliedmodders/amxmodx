using System;
using System.Diagnostics;
using System.IO;

namespace AMXXRelease
{
	//Build process for any Linux
	public class LinuxBuilder : ABuilder
	{
		private string m_AmxxPc;

		public override void OnBuild()
		{
			m_AmxxPc = PropSlashes(m_Cfg.GetSourceTree() + "/plugins/amxxpc");
		}

		public override void CompressDir(string target, string dir)
		{
			ProcessStartInfo info = new ProcessStartInfo();

			info.FileName = m_Cfg.CompressPath();
			info.WorkingDirectory = m_Cfg.OutputPath();
			info.Arguments = "zcvf " + target + " " + dir;
			info.UseShellExecute = false;

			Process p = Process.Start(info);
			p.WaitForExit();
		}

		public override void AmxxPc(string inpath, string args)
		{
			ProcessStartInfo info = new ProcessStartInfo();

			info.WorkingDirectory = PropSlashes(m_Cfg.GetSourceTree() + "\\plugins");
			info.FileName = (string)m_AmxxPc.Clone();
			info.Arguments = inpath + ".sma";
			if (args != null)
				info.Arguments += " " + args;
			info.UseShellExecute = false;

			Process p = Process.Start(info);
			p.WaitForExit();
		}

		public override string GetLibExt()
		{
			if (m_Cfg.MakeOpts().IndexOf("amd64") != -1)
				return "_amd64.so";
			else
				return "_i386.so";
		}

		public override string BuildModule(Module module)
		{
			ProcessStartInfo info = new ProcessStartInfo();

			string dir = m_Cfg.GetSourceTree() + "\\" + module.sourcedir;
			string file = dir;
			file += "\\" + "Release" + "\\" + module.projname + GetLibExt();
			file = PropSlashes(file);

			if (File.Exists(file))
				File.Delete(file);

			Console.WriteLine(PropSlashes(dir));
			info.WorkingDirectory = PropSlashes(dir);
			info.FileName = m_Cfg.DevenvPath();
			info.Arguments = "clean";
			info.UseShellExecute = false;

			Process p = Process.Start(info);
			p.WaitForExit();

			info.WorkingDirectory = PropSlashes(dir);
			info.FileName = m_Cfg.DevenvPath();
			info.Arguments =m_Cfg.MakeOpts();
			info.UseShellExecute = false;

			p = Process.Start(info);
			p.WaitForExit();

			if (!File.Exists(file))
			{
				Console.WriteLine("Output file failed: " + file);
				return null;
			}

			//Now we need to see if the DL handle is valid!
			string dlsym_dir = m_Cfg.GetSourceTree() + "\\plugins";
			string dlsym = dlsym_dir + "\\dlsym";
			if (m_Cfg.MakeOpts().IndexOf("amd64") != -1)
				dlsym += "64";
			dlsym = PropSlashes(dlsym);
			dlsym_dir = PropSlashes(dlsym_dir);
			info.WorkingDirectory = dlsym_dir;
			info.FileName = dlsym;
			info.Arguments = file;
			info.UseShellExecute = false;
			info.RedirectStandardOutput = true;

			p = Process.Start(info);
			p.WaitForExit();

			string output = p.StandardOutput.ReadToEnd();
			if (output.IndexOf("Handle:") == -1)
				return null;

			return file;
		}
	}
}

