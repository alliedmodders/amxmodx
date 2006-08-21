using System;
using System.IO;
using System.Text;
using System.Collections;

namespace BinLogReader
{
	/// <summary>
	/// binary log stuff yah
	/// </summary>
	public class BinLog
	{
		private static uint BINLOG_MAGIC = 0x414D424C;
		private static short BINLOG_VERSION = 0x0300;
		private static short BINLOG_MIN_VERSION = 0x0300;

		private ArrayList oplist;
		private PluginDb plugdb;

		public ArrayList OpList
		{
			get
			{
				return oplist;
			}
		}

		BinLog(int init_size)
		{
			oplist = new ArrayList(init_size);
		}

		public PluginDb GetPluginDB()
		{
			return plugdb;
		}

		public static BinLog FromFile(string filename)
		{
			if (!File.Exists(filename))
				return null;

			System.IO.FileStream stream = File.Open(filename, System.IO.FileMode.Open);
			if (stream == null)
				return null;
			BinaryReader br = new BinaryReader(stream);
			if (br == null)
				return null;

			BinLog bl = null;
			BinLogOp opcode = BinLogOp.BinLog_Invalid;
			long realtime = 0;
			float gametime = 0.0f;
			int plug_id = -1;
			Plugin pl = null;

			try
			{
				uint magic = br.ReadUInt32();
				if (magic != BINLOG_MAGIC)
					throw new Exception("Invalid magic log number");

				ushort version = br.ReadUInt16();
				if (version > BINLOG_VERSION || version < BINLOG_MIN_VERSION)
					throw new Exception("Unknown log version number");

				byte timesize = br.ReadByte();

				bool bits64 = (timesize == 8) ? true : false;

				FileInfo fi = new FileInfo(filename);
				//guestimate required size
				if (fi.Length > 500)
					bl = new BinLog( (int)((fi.Length - 500) / 6) );
				else 
					bl = new BinLog( (int)(fi.Length / 6) );

				bl.plugdb = PluginDb.FromFile(br);
				PluginDb db = bl.plugdb;

				if (db == null)
					throw new Exception("Plugin database read failure");

				do
				{
					opcode = (BinLogOp)br.ReadByte();
					gametime = br.ReadSingle();
					if (bits64)
						realtime = br.ReadInt64();
					else
						realtime = (long)br.ReadInt32();
					plug_id = br.ReadInt32();
					pl = db.GetPluginById(plug_id);
					switch (opcode)
					{
						case BinLogOp.BinLog_SetString:
						{
							long addr;
							if (bits64)
								addr = br.ReadInt64();
							else
								addr = (long)br.ReadInt32();
							int maxlen = br.ReadInt32();
							ushort len = br.ReadUInt16();
							byte [] str = br.ReadBytes(len+1);
							string text = Encoding.ASCII.GetString(str, 0, len);
							BinLogSetString bgs = 
								new BinLogSetString(addr, maxlen, text, gametime, realtime, pl);
							bl.OpList.Add(bgs);
							break;
						}
						case BinLogOp.BinLog_GetString:
						{
							long addr;
							if (bits64)
								addr = br.ReadInt64();
							else
								addr = (long)br.ReadInt32();
							ushort len = br.ReadUInt16();
							byte [] str = br.ReadBytes(len+1);
							string text = Encoding.ASCII.GetString(str, 0, len);
							BinLogGetString bgs = 
								new BinLogGetString(addr, text, gametime, realtime, pl);
							bl.OpList.Add(bgs);
							break;
						}
						case BinLogOp.BinLog_NativeParams:
						{
							ArrayList parms;
							if (bits64)
							{
								long num = br.ReadInt64();
								long p;
								Int64 i64;
								parms = new ArrayList((int)num);
								for (int i=0; i<(int)num; i++)
								{
									p = br.ReadInt64();
									i64 = new Int64();
									i64 = p;
									parms.Add(i64);
								}
							} 
							else 
							{
								int num = br.ReadInt32();
								int p;
								Int32 i32;
								parms = new ArrayList(num);
								for (int i=0; i<num; i++)
								{
									p = br.ReadInt32();
									i32 = new Int32();
									i32 = p;
									parms.Add(i32);
								}
							}
							BinLogNativeParams bnp = 
								new BinLogNativeParams(gametime, realtime, pl);
							bnp.ParamList = parms;
							bl.OpList.Add(bnp);
							break;
						}
						case BinLogOp.BinLog_FormatString:
						{
							int parm = br.ReadInt32();
							int max = br.ReadInt32();
							ushort len = br.ReadUInt16();
							byte [] str = br.ReadBytes(len + 1);
							string text = Encoding.ASCII.GetString(str, 0, len);
							BinLogFmtString bfs = 
								new BinLogFmtString(parm, max, text, gametime, realtime, pl);
							bl.OpList.Add(bfs);
							break;
						}
						case BinLogOp.BinLog_End:
						{
							BinLogSimple bs = 
								new BinLogSimple(BinLogOp.BinLog_End, gametime, realtime, pl);
							bl.OpList.Add(bs);
							break;
						}
						case BinLogOp.BinLog_SetLine:
						{
							int line = br.ReadInt32();
							int file = br.ReadInt32();
							BinLogSetLine bsl = 
								new BinLogSetLine(line, gametime, realtime, pl, file);
							bl.OpList.Add(bsl);
							break;
						}
						case BinLogOp.BinLog_CallPubFunc:
						{
							int pubidx = br.ReadInt32();
							int fileid = br.ReadInt32();
							BinLogPublic bp = 
								new BinLogPublic(pubidx,
									gametime,
									realtime,
									pl,
									fileid);
							bl.OpList.Add(bp);
							break;
						}
						case BinLogOp.BinLog_NativeRet:
						{
							long ret;
							if (bits64)
								ret = br.ReadInt64();
							else
								ret = (long)br.ReadUInt32();
							BinLogNativeRet bnr = 
								new BinLogNativeRet(ret, gametime, realtime, pl);
							bl.OpList.Add(bnr);
							break;
						}
						case BinLogOp.BinLog_NativeCall:
						{
							int native = br.ReadInt32();
							int parms = br.ReadInt32();
							int file = br.ReadInt32();
							BinLogNativeCall bn = 
								new BinLogNativeCall(native,
									parms,
									gametime,
									realtime,
									pl,
									file);
							bl.OpList.Add(bn);			
							break;
						}
						case BinLogOp.BinLog_Start:
						{
							BinLogSimple bs = 
								new BinLogSimple(opcode, gametime, realtime, null);
							bl.oplist.Add(bs);

							break;
						}
						case BinLogOp.BinLog_Registered:
						{
							byte length1 = br.ReadByte();
							byte [] title = br.ReadBytes(length1 + 1);
							byte length2 = br.ReadByte();
							byte [] vers = br.ReadBytes(length2 + 1);
							BinLogRegister be = 
								new BinLogRegister(gametime, realtime, pl);
							be.title = Encoding.ASCII.GetString(title, 0, length1);
							be.version = Encoding.ASCII.GetString(vers, 0, length2);
							bl.oplist.Add(be);
							pl.Title = be.title;
							pl.Version = be.version;
							
							break;
						}
						default:
						{
							BinLogSimple bs = new BinLogSimple(BinLogOp.BinLog_Invalid, gametime, realtime, pl);
							bl.oplist.Add(bs);
							opcode = BinLogOp.BinLog_End;
							break;
						}
					}
				} while (opcode != BinLogOp.BinLog_End);
				opcode =BinLogOp.BinLog_End;
			} 
			catch (Exception e)
			{
				if (bl != null && bl.plugdb != null)
				{
					BinLogSimple bs = new BinLogSimple(BinLogOp.BinLog_Invalid, gametime, realtime, pl);
					bl.oplist.Add(bs);
				} 
				else 
				{
					throw new Exception(e.Message);
				}
			}
			finally
			{
				br.Close();
				stream.Close();
				GC.Collect();
			}

			return bl;
		}
	}
}
