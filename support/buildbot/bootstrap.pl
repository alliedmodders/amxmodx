#!/usr/bin/perl

use strict;
use Cwd;
use File::Basename;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

#Go to main source dir
chdir(Build::PathFormat('../..'));

#Do the annoying revision bumping.
#Linux needs some help here.
if ($^O eq "linux")
{
	Build::Command("flip -u modules.versions");
	Build::Command("flip -u support/versionchanger.pl");
	Build::Command("chmod +x support/versionchanger.pl");
}
Build::Command(Build::PathFormat('support/versionchanger.pl') . ' --buildstring="-dev"');

open(DIRECTIONS, '>installer/builder/directions.info');
if ($^O eq "linux") {
	print DIRECTIONS "compress = /bin/tar\n";
} else {
	print DIRECTIONS "compress = C:\\WINDOWS\\zip.exe\n";
}
print DIRECTIONS "source = " . Cwd::abs_path('.') . "\n";
print DIRECTIONS "makeopts = \n";
print DIRECTIONS "output = " . Cwd::abs_path('../OUTPUT') . "\n";
if ($^O eq "linux") {
	print DIRECTIONS "devenv = /usr/bin/make\n";
} else {
	print DIRECTIONS "devenv = C:\\Program Files\\Microsoft Visual Studio 8\\Common7\\IDE\\devenv.com\n";
}
print DIRECTIONS "release = amxmodx-" . Build::ProductVersion('product.version') .
				 "-hg" . Build::HgRevNum('.') . "\n";
close(DIRECTIONS);
Build::Delete("" . Cwd::abs_path('../OUTPUT'));
Build::Command("mkdir \"" . Cwd::abs_path('../OUTPUT') . "\"");

