using System;
using System.Diagnostics;
using System.Collections;
using System.Text;

namespace BinLogReader
{
	public enum BinLogOp
	{
		BinLog_Invalid=0,
		BinLog_Start=1,
		BinLog_End,
		BinLog_NativeCall,	//<int32 native id> <int32_t num_params> <int32_t filename id>
		BinLog_NativeError, //<int32 errornum> <str[int16] string>
		BinLog_NativeRet,	//<cell value>
		BinLog_CallPubFunc,	//<int32 public id> <int32_t filename id>
		BinLog_SetLine,		//<int32 line no#> <int32_t filename id>
		BinLog_Registered,	//<string title> <string version>
		BinLog_FormatString, //<int32 param#> <int32 maxlen> <str[int16] string>
		BinLog_NativeParams, //<int32 num> <cell ...>
		BinLog_GetString,	//<cell addr> <string[int16]>
		BinLog_SetString,	//<cell addr> <int maxlen> <string[int16]>
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

		public static void PluginText(StringBuilder sb, Plugin pl, BinLogFlags flags)
		{
			if (HasFlag(flags, BinLogFlags.Show_PlugId)
				&& HasFlag(flags, BinLogFlags.Show_PlugFile))
			{
				sb.Append("\"");
				sb.Append(pl.File);
				sb.Append("\"");
				sb.Append(" (");
				sb.Append(pl.Index);
				sb.Append(")");
			} 
			else if (HasFlag(flags, BinLogFlags.Show_PlugId))
			{
				sb.Append(pl.Index);
			} 
			else if (HasFlag(flags, BinLogFlags.Show_PlugFile))
			{
				sb.Append("\"");
				sb.Append(pl.File);
				sb.Append("\"");
			}
		}

		public static void PluginText(StringBuilder sb, Plugin pl, BinLogFlags flags, int fileid)
		{
			if (HasFlag(flags, BinLogFlags.Show_PlugId)
				&& HasFlag(flags, BinLogFlags.Show_PlugFile))
			{
				sb.Append("\"");
				sb.Append(pl.File);
				if (pl.IsDebug())
				{
					sb.Append(", ");
					sb.Append(pl.FindFile(fileid));
				}
				sb.Append("\"");
				sb.Append(" (");
				sb.Append(pl.Index);
				sb.Append(")");
			} 
			else if (HasFlag(flags, BinLogFlags.Show_PlugId))
			{
				sb.Append(pl.Index);
			} 
			else if (HasFlag(flags, BinLogFlags.Show_PlugFile))
			{
				sb.Append("\"");
				sb.Append(pl.File);
				if (pl.IsDebug())
				{
					sb.Append(", ");
					sb.Append(pl.FindFile(fileid));
				}
				sb.Append("\"");
			}
		}

		public static void BinLogString(StringBuilder sb, BinLogEntry ble, BinLogFlags flags)
		{
			bool realtime = false;
			if (HasFlag(flags, BinLogFlags.Show_RealTime))
			{
				sb.Append(ble.realtime.ToString());
				realtime = true;
			}
			if (HasFlag(flags, BinLogFlags.Show_GameTime))
			{
				if (realtime)
				{
					sb.Append(", ");
					sb.Append(ble.gametime.ToString());
				}
				else
				{
					sb.Append(ble.gametime.ToString());
				}
			}
			sb.Append(": ");
			ble.ToLogString(sb, flags);
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
		public abstract void ToLogString(StringBuilder sb, BinLogFlags flags);
	};

	public class BinLogSetLine : BinLogEntry
	{
		private int line;
		private int fileid;

		public int Line
		{
			get
			{
				return line;
			}
		}

		public BinLogSetLine(int _line, float gt, long rt, Plugin _pl, int file)
			: base(gt, rt, _pl)
		{
			line = _line;
			fileid = file;
		}

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Plugin ");
			BinLogEntry.PluginText(sb, plugin, flags, fileid);
			sb.Append(" hit line ");
			sb.Append(Line);
			sb.Append(".");
		}

		public override BinLogOp Op()
		{
			return BinLogOp.BinLog_SetLine;
		}
	}

	public class BinLogPublic : BinLogEntry
	{
		private int pubidx;
		private int fileid;

		public string Public
		{
			get
			{
				return plugin.FindPublic(pubidx);
			}
		}

		public BinLogPublic(int pi, float gt, long rt, Plugin _pl, int _file)
			: base(gt, rt, _pl)
		{
			pubidx = pi;
			fileid = _file;
		}

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Plugin ");
			BinLogEntry.PluginText(sb, plugin, flags, fileid);
			sb.Append(" had public function \"");
			sb.Append(Public);
			sb.Append("\" (");
			sb.Append(pubidx);
			sb.Append(") called.");
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

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Setting string (addr ");
			sb.Append(address);
			sb.Append(") (maxlen ");
			sb.Append(maxlen);
			sb.Append(") from Plugin ");
			BinLogEntry.PluginText(sb, plugin, flags);
			sb.Append(".  String:");
			sb.Append("\n\t ");
			sb.Append(text);
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

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Retrieving string (addr ");
			sb.AppendFormat("0x{0:X}", address);
			sb.Append(") from Plugin ");
			BinLogEntry.PluginText(sb, plugin, flags);
			sb.Append(".  String:");
			sb.Append("\n\t ");
			sb.Append(text);
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

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Native returned: ");
			sb.Append(returnval);
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
		private int fileid;

		public string Native
		{
			get
			{
				return plugin.FindNative(nativeidx);
			}
		}

		public BinLogNativeCall(int na, int nu, float gt, long rt, Plugin _pl, int _file)
			: base(gt, rt, _pl)
		{
			nativeidx = na;
			numparams = nu;
			fileid = _file;
		}

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Plugin ");
			BinLogEntry.PluginText(sb, plugin, flags, fileid);
			sb.Append(" called native \"");
			sb.Append(Native); 
			sb.Append("\" (");
			sb.Append(nativeidx);
			sb.Append(") with ");
			sb.Append(numparams);
			sb.Append(" parameters.");
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

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			switch (my_op)
			{
				case BinLogOp.BinLog_Start:
				{
					sb.Append("Binary log started.");
					break;
				}
				case BinLogOp.BinLog_End:
				{
					sb.Append("Binary log ended.");
					break;
				}
				case BinLogOp.BinLog_Invalid:
				{
					sb.Append("Binary log corrupt past this point.");
					break;
				}
			}
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

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Native parameters: (");
			if (plist != null)
			{
				for (int i=0; i<plist.Count; i++)
				{
					sb.Append(plist[i].ToString());
					if (i < plist.Count - 1)
						sb.Append(", ");
				}
			}
			sb.Append(")");
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

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Plugin ");
			BinLogEntry.PluginText(sb, pl, flags);
			sb.Append(" formatted parameter ");
			sb.Append(parm);
			sb.Append(" (maxlen ");
			sb.Append(maxlen);
			sb.Append("), result: \n\t");
			sb.Append(text);
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

		public override void ToLogString(StringBuilder sb, BinLogFlags flags)
		{
			sb.Append("Plugin "); 
			BinLogEntry.PluginText(sb, pl, flags);
			sb.Append(" registered as (\"");
			sb.Append(_title);
			sb.Append("\", \"");
			sb.Append(_version);
			sb.Append("\")");
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
