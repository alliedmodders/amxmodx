#!/usr/bin/perl
# vim: set ts=2 sw=2 tw=99 noet: 

use strict;
use Cwd;
use File::Basename;
use File::Path;

my ($myself, $path) = fileparse($0);
chdir($path);

use FindBin;
use lib $FindBin::Bin;
require 'helpers.pm';

#Go back above build dir
chdir(Build::PathFormat('../../..'));

#Get the source path.
our ($root) = getcwd();

my $reconf = 0;

if (!(-f 'OUTPUT/.ambuild2/graph') || !(-f 'OUTPUT/.ambuild2/vars')) {
	rmtree('OUTPUT');
	mkdir('OUTPUT') or die("Failed to create output folder: $!\n");
}
chdir('OUTPUT');
my ($result, $argn);
$argn = $#ARGV + 1;
print "Attempting to reconfigure...\n";
my $conf_args = '--enable-optimize --no-color --symbol-files';
if ($argn > 0 && $^O !~ /MSWin/) {
	$result = `CC=$ARGV[0] CXX=$ARGV[0] python3 ../build/configure.py $conf_args`;
} else {
	if ($^O =~ /MSWin/) {
		$result = `C:\\Python38\\Python.exe ..\\build\\configure.py $conf_args`;
	} else {
		$result = `CC=clang CXX=clang python3 ../build/configure.py $conf_args`;
	}
}
print "$result\n";
if ($? != 0) {
	die("Could not configure: $!\n");
}

sub IsNewer
{
	my ($file, $time) = (@_);

	my @s = stat($file);
	my $mtime = $s[9];
	return $mtime > $time;
}

exit(0);


