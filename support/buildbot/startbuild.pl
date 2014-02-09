#!/usr/bin/perl

use File::Basename;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

chdir('../../OUTPUT');

my $argn = $#ARGV + 1;
if ($argn > 0) {
	$ENV{CC} = $ARGV[0];
	$ENV{CXX} = $ARGV[0];
}

system("ambuild --no-color 2>&1");

if ($? != 0)
{
	die "Build failed: $!\n";
}
else
{
	exit(0);
}

