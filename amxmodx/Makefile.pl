#!/usr/bin/perl
#(C)2004 AMX Mod X Development Team
# by David "BAILOPAN" Anderson

# output will occur in bin.x.proc
# where x is debug or opt and proc is ix86 or amd64
# You must use this script from the amxmodx src dir

#options = 
# jit - use the JIT
# debug - enable gdb debugging
# amd64 - compile for AMD64 (impiles no jit)
# proc=ix86 - assumed not amd64
# clean - clean the specifications above

$PROJECT = "amxx_mm";
$sdk = "../hlsdk/SourceCode";
$mm = "../metamod/metamod";

@CPP_SOURCE_FILES = ("meta_api.cpp", "CFile.cpp", "CVault.cpp", "vault.cpp", "float.cpp", "file.cpp", "modules.cpp", "CMisc.cpp", "CTask.cpp", "string.cpp", "amxmodx.cpp", "CEvent.cpp", "CCmd.cpp", "CLogEvent.cpp", "srvcmd.cpp", "strptime.cpp", "amxcore.cpp", "amxtime.cpp", "power.cpp", "amxxlog.cpp", "fakemeta.cpp", "MMGR/MMGR.cpp", "amxxfile.cpp", "CLang.cpp", "md5.cpp", "emsg.cpp", "CForward.cpp", "CPlugin.cpp", "CModule.cpp", "CMenu.cpp", "util.cpp");

@C_SOURCE_FILES = ();
my %OPTIONS, %OPT;

$OPT{"debug"} = "-g -ggdb";
$OPT{"opt"} = "-O2 -ffast-math -funroll-loops -fomit-frame-pointer -s -DNDEBUG -Wall -Wno-unknown-pragmas -DOPT_TYPE=\"optimized\"";

$OPTIONS{"include"} = "-I$sdk -I. -I$mm -I$sdk/engine -I$sdk/common -I$sdk/pm_shared -I$sdk/dlls";

while ($cmd = shift)
{
	if ($cmd =~ /jit/)
	{
		if ($OPTIONS{"amd64"})
		{
			die "You cannot compile the JIT and AMD64 yet.\n";
		} else {
			$OPTIONS{"jit"} = 1;
		}
	} elsif ($cmd =~ /amd64/) {
		if ($OPTIONS{"jit"})
		{
			die "You cannot compile the JIT and AMD64 yet.\n";
		} else {
			$OPTIONS{"amd64"} = 1;
		}
	} elsif ($cmd =~ /debug/) {
		$OPTIONS{"debug"} = 1;
	} elsif ($cmd =~ /proc=i(\d)86/) {
		$proc = $1;
		if ($OPTIONS{"amd64"})
		{
			die "You cannot compile for i".$proc."86 and AMD64.\n";
		} else {
			$OPTIONS{"proc"} = "i".$proc."86";
		}
	} elsif ($cmd =~ /clean/) {
		$OPTIONS{"clean"} = 1;		
	}
}

$gcc = `gcc --version`;
if ($gcc =~ /2\.9/)
{
	`ln -s amx.cpp amx.c`;
	push(@C_SOURCE_FILES, "amx.c");
	$OPT{"opt"} .= " -malign-loops=2 -malign-jumps=2 -malign-functions=2";
} else {
	if ($OPTIONS{"amd64"})
	{
		`cp amx.cpp amx.c`;
		push(@C_SOURCE_FILES, "amx.c");
	} else {
		push(@CPP_SOURCE_FILES, "amx.cpp");
	}
	$OPT{"opt"} .= " -falign-loops=2 -falign-jumps=2 -falign-functions=2";
}

if ($OPTIONS{"debug"})
{
	$cflags = $OPT{"debug"};
} else {
	if (!$OPTIONS{"amd64"})
	{
		$proc = $OPTIONS{"proc"};
		if (!$proc)
		{
			$proc = 3;
		}
		$cflags = "-march=i".$proc."86 ".$OPT{"opt"};
	} else {
		$cflags = $OPT{"opt"};
	}
}

if ($OPTIONS{"amd64"})
{
	$cflags .= " -m64 -DSMALL_CELL_SIZE=64 -DHAVE_I64 $cflags";
}

if ($OPTIONS{"jit"})
{
	$cflags .= "-DJIT";
}

if ($OPTIONS{"debug"})
{
	$outdir = "bin.debug";
} else {
	$outdir = "bin.opt";
}

if ($OPTIONS{"amd64"})
{
	$outdir .= ".amd64";
	$bin = $PROJECT."_amd64.so";
} else {
	$proc = $OPTIONS{"proc"};
	if ($proc)
	{
		$outdir .= ".i".$proc."86";
		$bin = $PROJECT."_i".$proc."86.so";
	} else {
		$outdir .= ".i386";
		$bin = $PROJECT."_i386.so";
	}
}

if ($OPTIONS{"clean"})
{
	`rm $outdir/*.o`;
	`rm $outdir/MMGR/*.o`;
	`rm $outdir/minilzo/*.o`;
	`rm $outdir/$bin`;
	die("Project cleaned.\n");
}

#create the dirs
#build link list
my @LINK;
for ($i=0; $i<=$#CPP_SOURCE_FILES; $i++)
{
	$file = $CPP_SOURCE_FILES[$i];
	$file =~ s/\.cpp/\.o/;
	push(@LINK, $outdir."/".$file);
}
for ($i=0; $i<=$#C_SOURCE_FILES; $i++)
{
	$file = $C_SOURCE_FILES[$i];
	$file =~ s/\.c/\.o/;
	push(@LINK, $outdir."/".$file);
}
if ($OPTIONS{"jit"})
{
	push(@LINK, "JIT/jits.o");
}

if (!(-d $outdir))
{
	mkdir($outdir);
}
if (!(-d "$outdir/MMGR"))
{
	mkdir("$outdir/MMGR");
}
if (!(-d "$outdir/JIT"))
{
	mkdir("$outdir/JIT");
}
if (!(-d "$outdir/minilzo"))
{
	mkdir("$outdir/minilzo");
}

$inc = $OPTIONS{"include"};

for ($i=0; $i<=$#CPP_SOURCE_FILES; $i++)
{
	$file = $CPP_SOURCE_FILES[$i];
	$ofile = $file;
	$ofile =~ s/\.cpp/\.o/;
	$ofile = "$outdir/$ofile";
	$gcc = "gcc $cflags -Dstrcmpi=strcasecmp -fPIC $inc -c $file -o $ofile";
	if (-e $ofile)
	{
		$file_time = (stat($file))[9];
		$ofile_time = (stat($file))[9];
		if ($file_time > $ofile_time)
		{
			print "$gcc\n";
			`$gcc`;
		}
	} else {
		print "$gcc\n";
		`$gcc`;
	}
}

for ($i=0; $i<=$#CPP_SOURCE_FILES; $i++)
{
	$file = $C_SOURCE_FILES[$i];
	$ofile = $file;
	$ofile =~ s/\.c/\.o/;
	$ofile = "$outdir/$ofile";
	$gcc = "cc $cflags -Dstrcmpi=strcasecmp -fPIC $inc -c $file -o $ofile";
	if (-e $ofile)
	{
		$file_time = (stat($file))[9];
		$ofile_time = (stat($file))[9];
		if ($file_time > $ofile_time)
		{
			print "$gcc\n";
			`$gcc`;
		}
	} else {
		print "$gcc\n";
		`$gcc`;
	}
}

$gcc = "gcc $cflags -Lzlib/ -lz -shared -ldl -lm @LINK -o $outdir/$bin";
`$gcc`;
