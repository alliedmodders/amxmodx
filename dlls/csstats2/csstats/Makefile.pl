#!/usr/bin/perl
#(C)2004 AMX Mod X Development Team
# by David "BAILOPAN" Anderson

# output will occur in bin.x.proc
# where x is debug or opt and proc is ix86 or amd64
# You must use this script from the project src dir

#options = 
# debug - enable gdb debugging
# amd64 - compile for AMD64
# proc=ix86 - assumed not amd64
# clean - clean the specifications above

$PROJECT = "csstats_amxx";
$sdk = "../../../hlsdk/SourceCode";
$mm = "../../../metamod/metamod";
$gccf = "gcc";

@CPP_SOURCE_FILES = ("amxxmodule.cpp", "CMisc.cpp", "usermsg.cpp", "meta_api.cpp", "rank.cpp", "CRank.cpp");

@C_SOURCE_FILES = ();
my %OPTIONS, %OPT;

$OPT{"debug"} = "-g -ggdb";
$OPT{"opt"} = "-O2 -ffast-math -funroll-loops -fomit-frame-pointer -s -DNDEBUG -Wall -Wno-unknown-pragmas -DOPT_TYPE=\"optimized\"";

$OPTIONS{"include"} = "-I$sdk -I. -I$mm -I$sdk/engine -I$sdk/common -I$sdk/pm_shared -I$sdk/dlls";

while ($cmd = shift)
{
	if ($cmd =~ /amd64/) {
		$OPTIONS{"amd64"} = 1;
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

$gcc = `$gccf --version`;
if ($gcc =~ /2\.9/)
{
	$OPT{"opt"} .= " -malign-loops=2 -malign-jumps=2 -malign-functions=2";
} else {
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
	$cflags .= " -m64 -DHAVE_I64 -DSMALL_CELL_SIZE=64 $cflags";
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

unlink("$outdir/$bin");
if ($OPTIONS{"clean"})
{
	`rm $outdir/*.o`;
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

if (!(-d $outdir))
{
	mkdir($outdir);
}

$inc = $OPTIONS{"include"};

for ($i=0; $i<=$#CPP_SOURCE_FILES; $i++)
{
	$file = $CPP_SOURCE_FILES[$i];
	$ofile = $file;
	$ofile =~ s/\.cpp/\.o/;
	$ofile = "$outdir/$ofile";
	$gcc = "$gccf $cflags -Dstrcmpi=strcasecmp -fPIC $inc -c $file -o $ofile";
	if (-e $ofile)
	{
		$file_time = (stat($file))[9];
		$ofile_time = (stat($ofile))[9];
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
		$ofile_time = (stat($ofile))[9];
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

$gcc = "$gccf $cflags -shared -ldl -lm @LINK -o $outdir/$bin";
print "$gcc\n";
`$gcc`;
