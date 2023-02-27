#!/usr/bin/perl

use File::Basename;

my ($myself, $path) = fileparse($0);
chdir($path);

use FindBin;
use lib $FindBin::Bin;
require 'helpers.pm';

chdir('../../../OUTPUT');

my $argn = $#ARGV + 1;
if ($argn > 0) {
	$ENV{CC} = $ARGV[0];
	$ENV{CXX} = $ARGV[0];
}

if ($^O !~ /MSWin/) {
	system("ambuild --no-color 2>&1");
} else {
	system("C:\\Python38\\scripts\\ambuild --no-color 2>&1");
}

if ($? != 0)
{
	die "Build failed: $!\n";
}
else
{
	exit(0);
}

