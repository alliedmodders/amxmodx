#!/usr/bin/perl

use strict;
use Cwd;
use File::Basename;
use Net::FTP;

my ($ftp_file, $ftp_host, $ftp_user, $ftp_pass, $ftp_path);

$ftp_file = shift;

open(FTP, $ftp_file) or die "Unable to read FTP config file $ftp_file: $!\n";
$ftp_host = <FTP>;
$ftp_user = <FTP>;
$ftp_pass = <FTP>;
$ftp_path = <FTP>;
close(FTP);

chomp $ftp_host;
chomp $ftp_user;
chomp $ftp_pass;
chomp $ftp_path;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

my ($version);
$version = Build::ProductVersion(Build::PathFormat('../../product.version'));
$version .= '-hg' . Build::HgRevNum('.');

#Switch to the output folder.
chdir(Build::PathFormat('../../../OUTPUT'));

my (@packages,@mac_exclude);
@packages = ('base', 'cstrike', 'dod', 'esf', 'ns', 'tfc', 'ts');
@mac_exclude = ('esf', 'ns', 'ts');

my ($major,$minor) = ($version =~ /^(\d+)\.(\d+)/);
$ftp_path .= "/$major.$minor";

my ($ftp);

$ftp = Net::FTP->new($ftp_host, Debug => 0, Passive => 0) 
    or die "Cannot connect to host $ftp_host: $@";

$ftp->login($ftp_user, $ftp_pass)
    or die "Cannot connect to host $ftp_host as $ftp_user: " . $ftp->message . "\n";

if ($ftp_path ne '')
{
    $ftp->cwd($ftp_path)
        or die "Cannot change to folder $ftp_path: " . $ftp->message . "\n";
}

$ftp->binary();
my ($i);
for ($i = 0; $i <= $#packages; $i++) {
	my ($filename);
	if ($^O eq "linux") {
		$filename = "amxmodx-$version-" . $packages[$i] . "-linux.tar.gz";
	} elsif ($^O eq "darwin") {
		next if ($packages[$i] ~~ @mac_exclude);
		$filename = "amxmodx-$version-" . $packages[$i] . "-mac.zip";
	} else {
		$filename = "amxmodx-$version-" . $packages[$i] . "-windows.zip";
	}
	print "Uploading $filename...\n";
	$ftp->put($filename)
		or die "Cannot drop file $filename ($ftp_path): " . $ftp->message . "\n";
}

$ftp->close();

print "Files sent to drop site -- build succeeded.\n";

exit(0);

