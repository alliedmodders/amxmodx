#!/bin/bash

# AMX Mod X
#
# by the AMX Mod X Development Team
#  originally developed by OLO
#
# This file is part of AMX Mod X.

test -e compiled || mkdir compiled
ls *.sma | xargs -i ./sc \{\} -ocompiled/\{\} > temp.txt 
ls compiled/*.sma | xargs -i basename \{\} .sma | xargs -i mv compiled/\{\}.sma compiled/\{\}.amx
more temp.txt
rm temp.txt