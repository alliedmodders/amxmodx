rm meta_api.o
rm engine.so

#use VexD's optimizations

gcc -static -march=i586 -O6 -ffast-math -funroll-loops -fomit-frame-pointer -fexpensive-optimizations -fno-exceptions -fno-rtti -s -Wall -Wno-unknown-pragmas -DOPT_TYPE=\"optimized\"  -fPIC -I. -I../metamod -I../../multiplayer/engine -I../../multiplayer/common -I../../multiplayer/pm_shared -I../../multiplayer/dlls -I../../multiplayer -c meta_api.cpp -o meta_api.o

gcc -static -march=i586 -O6 -ffast-math -funroll-loops -fomit-frame-pointer -fexpensive-optimizations -falign-loops=2 -falign-jumps=2 -falign-functions=2 -fno-exceptions -fno-rtti -s -Wall -Wno-unknown-pragmas -DOPT_TYPE=\"optimized\"  -shared -ldl -lm -lstdc++  meta_api.o -o engine.so
