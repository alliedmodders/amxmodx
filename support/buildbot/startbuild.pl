#!/usr/bin/perl

use File::Basename;

our (@LIBRARIES);
my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

chdir('../../installer/builder');



