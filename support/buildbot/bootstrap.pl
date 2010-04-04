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

my $DEVENV = "C:\\Program Files\\Microsoft Visual Studio 8\\Common7\\IDE\\devenv.com";

#Build the amxmodx builder tool.
chdir('installer/builder');
if ($^O eq "linux") {
	Build::Command("make");
} else {
	Build::Command("\"$DEVENV\" /rebuild Debug builder.csproj");
}
if (!(-f 'builder.exe')) {
	die "Could not find build tool.\n";
}
chdir('../..');

#Output directions on how to build.
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
	print DIRECTIONS "devenv = $DEVENV\n";
}
print DIRECTIONS "release = amxmodx-" . Build::ProductVersion('product.version') .
				 "-hg" . Build::HgRevNum('.') . "\n";
close(DIRECTIONS);

#Clean the output path. Create dir twice for abs_path hack (Windows).
Build::Command("mkdir ../OUTPUT");
Build::Delete("" . Cwd::abs_path('../OUTPUT'));
Build::Command("mkdir ../OUTPUT");

