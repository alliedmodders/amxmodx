#!/bin/bash
test -e compiled || mkdir compiled
ls *.sma | xargs -i ./sc \{\} -ocompiled/\{\} > temp.txt 
ls compiled/*.sma | xargs -i basename \{\} .sma | xargs -i mv compiled/\{\}.sma compiled/\{\}.amx
more temp.txt
rm temp.txt
