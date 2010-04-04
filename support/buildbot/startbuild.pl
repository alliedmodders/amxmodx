#!/usr/bin/perl

use File::Basename;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

chdir('../../installer/builder');

if ($^O eq "linux") {
	system("mono builder.exe");
} else {
	system("builder.exe");
}

if ($? != 0)
{
	die "Build failed: $!\n";
}
else
{
	exit(0);
}

