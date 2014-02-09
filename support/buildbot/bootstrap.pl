#!/usr/bin/perl

use strict;
use Cwd;
use File::Basename;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

#Go to main source dir
chdir(Build::PathFormat('../../..'));

#Get the source path.
our ($root) = getcwd();

my $reconf = 0;

if (!(-f 'OUTPUT/.ambuild2/graph') || !(-f 'OUTPUT/.ambuild2/vars')) {
	rmtree('OUTPUT');
	mkdir('OUTPUT') or die("Failed to create output folder: $!\n");
	chdir('OUTPUT');
	my ($result, $argn);
	$argn = $#ARGV + 1;
	print "Attempting to reconfigure...\n";
	my $conf_args = '--enable-optimize --no-color';
	if ($argn > 0 && $^O !~ /MSWin/) {
		$result = `CC=$ARGV[0] CXX=$ARGV[0] python ../build/configure.py $conf_args`;
	} else {
		if ($^O =~ /MSWin/) {
			$result = `C:\\Python27\\Python.exe ..\\build\\configure.py $conf_args`;
		} else {
			$result = `CC=clang CXX=clang python ../build/configure.py $conf_args`;
		}
	}
	print "$result\n";
	if ($? != 0) {
		die("Could not configure: $!\n");
	}
}

sub IsNewer
{
	my ($file, $time) = (@_);

	my @s = stat($file);
	my $mtime = $s[9];
	return $mtime > $time;
}

exit(0);



