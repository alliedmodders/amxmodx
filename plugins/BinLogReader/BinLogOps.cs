using System;
using System.Diagnostics;
using System.Collections;

namespace BinLogReader
{
	public enum BinLogOp
	{
		BinLog_Invalid=0,
		BinLog_Start=1,
		BinLog_End,
		BinLog_NativeCall,	//<int16_t native id>
		BinLog_NativeError,
		BinLog_NativeRet,
		BinLog_CallPubFunc,	//<int16_t public id>
		BinLog_SetLine,		//<int16_t line no#>
		BinLog_Registered,	//<string title> <string version>
		BinLog_FormatString,
		BinLog_NativeParams,
		BinLog_GetString,
		BinLog_SetString,
	};

	public enum BinLogFlags
	{
		Show_RealTime = (1<<0),
		Show_GameTime = (1<<1),
		Show_PlugId = (1<<2),
		Show_PlugFile = (1<<3),
	};

	public abstract class BinLogEntry
	{
		protected float GameTime;
		protected long RealTime;
		protected Plugin pl;

		protected BinLogEntry(float gt, long rt, Plugin _pl)
		{
			GameTime = gt;
			RealTime = rt;
			pl = _pl;
		}

		public static bool HasFlag(BinLogFlags a, BinLogFlags b)
		{
			return ( (a & b) == b );
		}

		public static string PluginText(Plugin pl, BinLogFlags flags)
		{
			string plugintext = "";
			if (HasFlag(flags, BinLogFlags.Show_PlugId)
				&& HasFlag(flags, BinLogFlags.Show_PlugFile))
			{
				plugintext = "\"" + pl.File + "\"" + " (" + pl.Index + ")";
			} 
			else if (HasFlag(flags, BinLogFlags.Show_PlugId))
			{
				plugintext = pl.Index.ToString();
			} 
			else if (HasFlag(flags, BinLogFlags.Show_PlugFile))
			{
				plugintext = "\"" + pl.File + "\"";
			}
			return plugintext;
		}

		public static string BinLogString(BinLogEntry ble, BinLogFlags flags)
		{
			string logtext = "";
			if (HasFlag(flags, BinLogFlags.Show_RealTime))
				logtext += ble.realtime.ToString();
			if (HasFlag(flags, BinLogFlags.Show_GameTime))
			{
				if (logtext.Length > 0)
				{
					logtext += ", " + ble.gametime.ToString();
				}
				else
				{
					logtext += ble.gametime.ToString();
				}
			}
			logtext += ": ";
			logtext += ble.ToLogString(flags);

			return logtext;
		}

		public float gametime
		{
			[DebuggerStepThrough]
			get
			{
				return GameTime;
			}
		}

		public Plugin plugin
		{
			[DebuggerStepThrough]
			get
			{
				return pl;
			}
		}

		public long realtime
		{
			[DebuggerStepThrough]
			get
			{
				return RealTime;
			}
		}

		public abstract BinLogOp Op();
		public abstract string ToLogString(BinLogFlags flags);
	};

	public class BinLogSetLine : BinLogEntry
	{
		private int line;

		public int Line
		{
			get
			{
				return line;
			}
		}

		public BinLogSetLine(int _line, float gt, long rt, Plugin _pl)
			: base(gt, rt, _pl)
		{
			line = _line;
		}

		public override string ToLogString(BinLogFlags flags)
		{
			string plugtext = BinLogEntry.PluginText(plugin, flags);
			string logtext = "Plugin hit line " + Line + ".";

			return logtext;
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_SetLine;
		}
	}

	public class BinLogPublic : BinLogEntry
	{
		private int pubidx;

		public string Public
		{
			get
			{
				return plugin.FindPublic(pubidx);
			}
		}

		public BinLogPublic(int pi, float gt, long rt, Plugin _pl)
			: base(gt, rt, _pl)
		{
			pubidx = pi;
		}

		public override string ToLogString(BinLogFlags flags)
		{
			string plugtext = BinLogEntry.PluginText(plugin, flags);
			string logtext = "Plugin " + plugtext + " had public function ";
			logtext += "\"" + Public + "\" (" + pubidx + ") called.";

			return logtext;
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_CallPubFunc;
		}
	}

	public class BinLogSetString : BinLogEntry
	{
		private long address;
		private int maxlen;
		private string text;

		public BinLogSetString(long addr, int _maxlen, string fmt, float gt, long rt, Plugin _pl)
			: base(gt, rt, _pl)
		{
			address = addr;
			maxlen = _maxlen;
			text = fmt;
		}

		public override string ToLogString(BinLogFlags flags)
		{
			string plugtext = BinLogEntry.PluginText(plugin, flags);
			string logtext = "Setting string (addr " + address + ") (maxlen " + maxlen + ") from Plugin " + plugtext + ".  String:";
			logtext += "\n\t " + text;

			return logtext;
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_GetString;
		}
	}

	public class BinLogGetString : BinLogEntry
	{
		private long address;
		private string text;

		public BinLogGetString(long addr, string fmt, float gt, long rt, Plugin _pl)
			: base(gt, rt, _pl)
		{
			address = addr;
			text = fmt;
		}

		public override string ToLogString(BinLogFlags flags)
		{
			string plugtext = BinLogEntry.PluginText(plugin, flags);
			string logtext = "Retrieving string (addr " + address + ") from Plugin " + plugtext + ".  String:";
			logtext += "\n\t " + text;

			return logtext;
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_GetString;
		}
	}

	public class BinLogNativeRet : BinLogEntry
	{
		private long returnval;

		public BinLogNativeRet(long ret, float gt, long rt, Plugin _pl)
			: base(gt, rt, _pl)
		{
			returnval = ret;
		}

		public override string ToLogString(BinLogFlags flags)
		{
			return "Native returned: " + returnval;
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_NativeRet;
		}
	}

	public class BinLogNativeCall : BinLogEntry
	{
		private int nativeidx;
		private int numparams;

		public string Native
		{
			get
			{
				return plugin.FindNative(nativeidx);
			}
		}

		public BinLogNativeCall(int na, int nu, float gt, long rt, Plugin _pl)
			: base(gt, rt, _pl)
		{
			nativeidx = na;
			numparams = nu;
		}

		public override string ToLogString(BinLogFlags flags)
		{
			string plugtext = BinLogEntry.PluginText(plugin, flags);
			string logtext = "Plugin " + plugtext + " called native ";
			logtext += "\"" + Native + "\" (" + nativeidx + ")";
			logtext += " with " + numparams + " parameters.";

			return logtext;
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_NativeCall;
		}
	}

	public class BinLogSimple : BinLogEntry
	{
		private BinLogOp my_op;
		public BinLogSimple(BinLogOp op, float gt, long rt, Plugin _pl) : 
			base(gt, rt, _pl)
		{
			my_op = op;
		}

		public override string ToLogString(BinLogFlags flags)
		{
			switch (my_op)
			{
				case BinLogOp.BinLog_Start:
				{
					return "Binary log started.";
				}
				case BinLogOp.BinLog_End:
				{
					return "Binary log ended.";
				}
				case BinLogOp.BinLog_Invalid:
				{
					return "Binary log corrupt past this point.";
				}
			}

			return "";
		}

		public override BinLogOp Op()
		{
			return my_op;
		}
	}

	public class BinLogNativeParams : BinLogEntry
	{
		private ArrayList plist;

		public ArrayList ParamList
		{
			get
			{
				return plist;
			}
			set
			{
				plist = value;
			}
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_NativeParams;
		}

		public BinLogNativeParams(float gt, long rt, Plugin _pl)
			: base(gt, rt, _pl)
		{
		}

		public override string ToLogString(BinLogFlags flags)
		{
			string logtext = "Native parameters: (";
			if (plist != null)
			{
				for (int i=0; i<plist.Count; i++)
				{
					logtext += plist[i].ToString();
					if (i < plist.Count - 1)
						logtext += ", ";
				}
			}
			logtext += ")";

			return logtext;
		}
	}

	public class BinLogFmtString : BinLogEntry
	{
		private int parm;
		private int maxlen;
		private string text;

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_FormatString;
		}

		public BinLogFmtString(int pa, int ma, string str, float gt, long rt, Plugin _pl)
			: base(gt, rt, _pl)
		{
			parm = pa;
			maxlen = ma;
			text = str;
		}

		public override string ToLogString(BinLogFlags flags)
		{
			string plugintext = BinLogEntry.PluginText(pl, flags);
			string logtext = "Plugin " + plugintext + " formatted parameter " + parm;
			logtext += " (maxlen " + maxlen + "), result: \n\t" + text;

			return logtext;
		}
	}

	public class BinLogRegister : BinLogEntry
	{
		private string _title;
		private string _version;

		public string title
		{
			[DebuggerStepThrough]
			get
			{
				return _title;
			}
			[DebuggerStepThrough]
			set
			{
				_title = value;
			}
		}

		public string version
		{
			[DebuggerStepThrough]
			get
			{
				return _version;
			}
			[DebuggerStepThrough]
			set
			{
				_version = value;
			}
		}

		public override string ToLogString(BinLogFlags flags)
		{
			string plugintext = BinLogEntry.PluginText(pl, flags);
			string logtext = "Plugin " + plugintext + " registered as ";
			logtext += "(\"" + _title + "\", \"" + _version + "\")";

			return logtext;
		}

		public BinLogRegister(float gt, long rt, Plugin _pl) : 
			base(gt, rt, _pl)
		{
			
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_Registered;
		}
	}
}
