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
			info.WorkingDirectory = dir;

			string [] files = Directory.GetFiles(dir);
			string file_list = "";
			for (int i=0; i<files.Length; i++)
				file_list += GetFileName(files[i]) + " ";
			files = Directory.GetDirectories(dir);
			for (int i=0; i<files.Length; i++)
				file_list += GetFileName(files[i]) + " ";

			ProcessStartInfo chmod = new ProcessStartInfo();
			chmod.FileName = "/bin/chmod";
			chmod.WorkingDirectory = dir;
			chmod.Arguments = "-R 755 " + file_list;
			chmod.UseShellExecute = false;
			Process c = Process.Start(chmod);
			c.WaitForExit();
			c.Close();


			info.Arguments = "zcvf \"" + target + "-linux.tar.gz\" " + file_list;
			info.UseShellExecute = false;

			Process p = Process.Start(info);
			p.WaitForExit();
			p.Close();
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
			p.Close();
		}

		public override string GetLibExt()
		{
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
			p.Close();

			info.WorkingDirectory = PropSlashes(dir);
			info.FileName = m_Cfg.DevenvPath();
			info.Arguments = m_Cfg.MakeOpts();
			info.UseShellExecute = false;

			p = Process.Start(info);
			p.WaitForExit();
			p.Close();

			if (!File.Exists(file))
			{
				Console.WriteLine("Output file failed: " + file);
				return null;
			}

			//Now we need to see if the DL handle is valid!
			string dlsym_dir = m_Cfg.GetSourceTree() + "\\plugins";
			string dlsym = dlsym_dir + "\\dlsym";
			dlsym = PropSlashes(dlsym);
			dlsym_dir = PropSlashes(dlsym_dir);
			info.WorkingDirectory = dlsym_dir;
			info.FileName = dlsym;
			info.Arguments = file;
			info.UseShellExecute = false;
			info.RedirectStandardOutput = true;

			p = Process.Start(info);
			string output = p.StandardOutput.ReadToEnd();
			p.WaitForExit();
			p.Close();

			if (output.IndexOf("Handle:") == -1)
				return null;

			return file;
		}
	}
}

