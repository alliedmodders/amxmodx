#include <vector>
#include "JBRandom.h"
#include "amxxmodule.h"
using namespace std;

enum SpaceContent {SPACE_EMPTY = 0, SPACE_WALL = 1};
typedef unsigned int UINT;

class CSpace  
{
	public:
		CSpace(UINT columnIn, UINT lineIn) {column = columnIn; line = lineIn;}
		UINT column, line;
};


// Globals
UINT COLUMNS, LINES;
AMX *g_amx;
