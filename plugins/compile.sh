#!/bin/bash

#AMX Mod X
#
# (c) 2002-2004, OLO
#  modified by the AMX Mod X Development Team

test -e compiled || mkdir compiled
ls *.sma | xargs -i ./sc \{\} -ocompiled/\{\} > temp.txt 
ls compiled/*.sma | xargs -i basename \{\} .sma | xargs -i mv compiled/\{\}.sma compiled/\{\}.amx
more temp.txt
rm temp.txt
