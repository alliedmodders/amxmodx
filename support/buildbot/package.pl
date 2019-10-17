#!/usr/bin/perl
# vim: set ts=4 sts=4 sw=4 tw=99 noet:

use strict;
use Cwd;
use File::Basename;
use File::stat;
use File::Temp qw/ tempfile :seekable/;
use Net::FTP;
use Archive::Tar;
use Time::localtime;

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
$version .= '-git' . Build::GitRevNum('.');

#Switch to the output folder.
chdir(Build::PathFormat('../../../OUTPUT'));

my $needNewGeoIP = 1;
if (-e '../GeoLite2-Country.tar.gz')
{
	my $stats = stat('../GeoLite2-Country.tar.gz');
	if ($stats->size != 0)
	{
		my $fileModifiedTime = $stats->mtime;
		my $fileModifiedMonth = localtime($fileModifiedTime)->mon;
		my $currentMonth = localtime->mon;
		my $thirtyOneDays = 60 * 60 * 24 * 31;

		# GeoIP file only updates once per month
		if ($currentMonth == $fileModifiedMonth || (time() - $fileModifiedTime) < $thirtyOneDays)
		{
			$needNewGeoIP = 0;
		}
	}
}

if ($needNewGeoIP)
{
    print "Downloading GeoLite2-Country.mmdb...\n";
    system('wget -q -O ../GeoLite2-Country.tar.gz https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz');
}
else
{
    print "Reusing existing GeoLite2-Country.mmdb\n";
}

my $geoIPfile = 'packages/base/addons/amxmodx/data/GeoLite2-Country.mmdb';
if (-e $geoIPfile) {
	unlink($geoIPfile);
}

my $next = Archive::Tar->iter('../GeoLite2-Country.tar.gz', 1, {filter => qr/\.mmdb$/});
while (my $file = $next->()) {
	$file->extract($geoIPfile) or warn "Extraction failed";
}

my (@packages,@mac_exclude);
@packages = ('base', 'cstrike', 'dod', 'esf', 'ns', 'tfc', 'ts');
@mac_exclude = ('esf', 'ns', 'ts');

my ($major,$minor) = ($version =~ /^(\d+)\.(\d+)/);

my ($i);
for ($i = 0; $i <= $#packages; $i++) {
	my ($filename);
	next if (($^O eq "darwin") && ($packages[$i] ~~ @mac_exclude));

	if ($^O eq "linux") {
		$filename = "amxmodx-$version-" . $packages[$i] . "-linux.tar.gz";
	} elsif ($^O eq "darwin") {
		$filename = "amxmodx-$version-" . $packages[$i] . "-mac.zip";
	} else {
		$filename = "amxmodx-$version-" . $packages[$i] . "-windows.zip";
	}
	unlink($filename);

	chdir(Build::PathFormat("packages/" . $packages[$i]));
	if ($^O eq "linux") {
		Build::Command("tar zcvf $filename addons");
	} else {
		Build::Command("zip -r $filename addons");
	}
	Build::Move($filename, Build::PathFormat('../../'));
	chdir(Build::PathFormat('../..'));
}

my ($ftp);
$ftp_path .= "/$major.$minor";

$ftp = Net::FTP->new($ftp_host, Debug => 0, Passive => 1) 
    or die "Cannot connect to host $ftp_host: $@";

$ftp->login($ftp_user, $ftp_pass)
    or die "Cannot connect to host $ftp_host as $ftp_user: " . $ftp->message . "\n";

if ($ftp_path ne '')
{
    $ftp->cwd($ftp_path)
	or die "Cannot change to folder $ftp_path: " . $ftp->message . "\n";
}

$ftp->binary();
for ($i = 0; $i <= $#packages; $i++) {
	my ($filename);
	my ($latest);
	if ($^O eq "linux") {
		$filename = "amxmodx-$version-" . $packages[$i] . "-linux.tar.gz";
		$latest = "amxmodx-latest-" . $packages[$i] . "-linux";
	} elsif ($^O eq "darwin") {
		next if ($packages[$i] ~~ @mac_exclude);
		$filename = "amxmodx-$version-" . $packages[$i] . "-mac.zip";
		$latest = "amxmodx-latest-" . $packages[$i] . "-mac";
	} else {
		$filename = "amxmodx-$version-" . $packages[$i] . "-windows.zip";
		$latest = "amxmodx-latest-" . $packages[$i] . "-windows";
	}

	my ($tmpfh, $tmpfile) = tempfile();
	print $tmpfh $filename;
	$tmpfh->seek( 0, SEEK_END );

	print "Uploading $filename...\n";
	$ftp->put($filename)
		or die "Cannot drop file $filename ($ftp_path): " . $ftp->message . "\n";

	print "Uploading $latest...\n";
	$ftp->put($tmpfile, $latest)
		or die "Cannot drop file $latest ($ftp_path): " . $ftp->message . "\n";
}

$ftp->close();

print "Files sent to drop site -- build succeeded.\n";

exit(0);