rm meta_api.o
rm engine.so

#use VexD's optimizations

gcc -static -march=i386 -O6 -ffast-math -funroll-loops -fomit-frame-pointer -fexpensive-optimizations -fno-exceptions -fno-rtti -s -Wall -Wno-unknown-pragmas -DOPT_TYPE=\"optimized\"  -fPIC -I. -I../metamod/metamod -I../hlsdk/multiplayer/engine -I../hlsdk/multiplayer/common -I../hlsdk/multiplayer/pm_shared -I../hlsdk/multiplayer/dlls -I../hlsdk/multiplayer -I../amxmodx/ -c meta_api.cpp -o meta_api.o

gcc -static -march=i386 -O6 -ffast-math -funroll-loops -fomit-frame-pointer -fexpensive-optimizations -falign-loops=2 -falign-jumps=2 -falign-functions=2 -fno-exceptions -fno-rtti -s -Wall -Wno-unknown-pragmas -DOPT_TYPE=\"optimized\"  -shared -ldl -lm -lstdc++  meta_api.o -o engine.so
