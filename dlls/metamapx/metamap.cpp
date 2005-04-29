#include "metamap.h"


bool& VisitedAccess(unsigned int col, unsigned int lin, bool* visited)
{
	return visited[lin * COLUMNS + col];
}

cell* MatrixWrite(UINT col, UINT lin, cell* theMatrix)
{
	return &(theMatrix[lin * COLUMNS + col]);
	//return &(crossword.at());
}

const cell* MatrixRead(UINT col, UINT lin, cell* theMatrix)
{
	return &(theMatrix[lin * COLUMNS + col]);
	//return &(crossword.at());
}

void AddSpace(int column, int line, vector<CSpace>& spaces)
{
	spaces.push_back(CSpace(column, line));
}

void EmptyMatrix(cell* theMatrix) {
	for (UINT i = 0; i < COLUMNS; i++) {
		for (UINT j = 0; j < LINES; j++) {
			*(MatrixWrite(i, j, theMatrix)) = SPACE_EMPTY;
		}
	}
}

bool BoundaryAndWallCheck(const UINT column, const UINT line, bool* CRvisited, cell* theMatrix)
{
	if (column >= 0 && column < COLUMNS && line >= 0 && line < LINES) {
		// That's boundaries, now walls, and visited!
		if (*MatrixRead(column, line, theMatrix) == SPACE_EMPTY) //if (theMatrix[column][line] == SPACE_EMPTY) {
			if (!(VisitedAccess(column, line, CRvisited)))//if (!CRvisited[column][line])
				return true;
			/*else {
				cout << "Visited" << column << ',' << line << '!';
			}*/
	}
	
	return false;
}

int Difference(int i, int j) {
	if (i > j)
		return i - j;
	else
		return j - i;
}

bool CheckReach(const UINT currentColumn, const UINT currentLine, const UINT targetColumn, const UINT targetLine, bool* CRvisited, cell* theMatrix)
{
	//cout << '>' << currentColumn << ',' << currentLine;
	if (currentColumn < 0 || currentColumn >= COLUMNS || currentLine < 0 || currentLine >= LINES
		||	targetColumn < 0 || targetColumn >= COLUMNS || targetLine < 0 || targetLine >= LINES) {
		MF_Log("Metamap: Out of range in CheckReach! currentColumn %d currentLine %d targetColumn %d targetLine %d");
		throw;
	}
	// This should happen when the spaces are directly next to each other.
	if (currentColumn == targetColumn && Difference(currentLine, targetLine) == 1
	||	currentLine == targetLine && Difference(currentColumn, targetColumn) == 1) {
		//cout << "Returns yes here because " << currentColumn << "," << currentLine << " and " << targetColumn << "," << targetLine << " are next to each other.";
		//cout << "=nextTo";
		return true;
	}
	else if (*MatrixRead(currentColumn, currentLine, theMatrix) == SPACE_WALL) //else if (theMatrix[currentColumn][currentLine] == SPACE_WALL)
		return false;

	(VisitedAccess(currentColumn, currentLine, CRvisited)) = true; //CRvisited[currentColumn][currentLine] = true;

	// Spaces are now at least two manhattan units from each other.
	// Try to go in the general direction of target (should be one or two possibilites here, depending on if we are on the same line/column or not)
	bool goLeft = false, goUp = false, goRight = false, goDown = false;

	if (targetColumn < currentColumn)
		goLeft = true;
	else if (targetColumn > currentColumn)
		goRight = true;
	if (targetLine < currentLine)
		goUp = true;
	else if (targetLine > currentLine)
		goDown = true;

	// Left
	int leftColumn = currentColumn - 1;
	int leftLine = currentLine;

	// Up
	int upColumn = currentColumn;
	int upLine = currentLine - 1;

	// Right
	int rightColumn = currentColumn + 1;
	int rightLine = currentLine;

	// Down
	int downColumn = currentColumn;
	int downLine = currentLine + 1;

	int nextColumn[4], nextLine[4];
	bool nextCheck[4];

	if (goLeft) {
		nextColumn[0] = leftColumn;
		nextLine[0] = leftLine;
		nextCheck[0] = BoundaryAndWallCheck(nextColumn[0], nextLine[0], CRvisited, theMatrix);
		if (goUp) {
			nextColumn[1] = upColumn;
			nextLine[1] = upLine;
			nextCheck[1] = BoundaryAndWallCheck(nextColumn[1], nextLine[1], CRvisited, theMatrix);

			nextColumn[2] = downColumn;
			nextLine[2] = downLine;
			nextCheck[2] = BoundaryAndWallCheck(nextColumn[2], nextLine[2], CRvisited, theMatrix);

			nextColumn[3] = rightColumn;
			nextLine[3] = rightLine;
			nextCheck[3] = BoundaryAndWallCheck(nextColumn[3], nextLine[3], CRvisited, theMatrix);
		}
		else {
			nextColumn[1] = downColumn;
			nextLine[1] = downLine;
			nextCheck[1] = BoundaryAndWallCheck(nextColumn[1], nextLine[1], CRvisited, theMatrix);

			nextColumn[2] = upColumn;
			nextLine[2] = upLine;
			nextCheck[2] = BoundaryAndWallCheck(nextColumn[2], nextLine[2], CRvisited, theMatrix);

			nextColumn[3] = rightColumn;
			nextLine[3] = rightLine;
			nextCheck[3] = BoundaryAndWallCheck(nextColumn[3], nextLine[3], CRvisited, theMatrix);
		}
	}
	else if (goRight) {
		nextColumn[0] = rightColumn;
		nextLine[0] = rightLine;
		nextCheck[0] = BoundaryAndWallCheck(nextColumn[0], nextLine[0], CRvisited, theMatrix);
		if (goUp) {
			nextColumn[1] = upColumn;
			nextLine[1] = upLine;
			nextCheck[1] = BoundaryAndWallCheck(nextColumn[1], nextLine[1], CRvisited, theMatrix);

			nextColumn[2] = downColumn;
			nextLine[2] = downLine;
			nextCheck[2] = BoundaryAndWallCheck(nextColumn[2], nextLine[2], CRvisited, theMatrix);

			nextColumn[3] = leftColumn;
			nextLine[3] = leftLine;
			nextCheck[3] = BoundaryAndWallCheck(nextColumn[3], nextLine[3], CRvisited, theMatrix);
		}
		else {
			nextColumn[1] = downColumn;
			nextLine[1] = downLine;
			nextCheck[1] = BoundaryAndWallCheck(nextColumn[1], nextLine[1], CRvisited, theMatrix);

			nextColumn[2] = upColumn;
			nextLine[2] = upLine;
			nextCheck[2] = BoundaryAndWallCheck(nextColumn[2], nextLine[2], CRvisited, theMatrix);

			nextColumn[3] = leftColumn;
			nextLine[3] = leftLine;
			nextCheck[3] = BoundaryAndWallCheck(nextColumn[3], nextLine[3], CRvisited, theMatrix);
		}
	}
	else {
		if (goUp) {
			nextColumn[0] = upColumn;
			nextLine[0] = upLine;
			nextCheck[0] = BoundaryAndWallCheck(nextColumn[0], nextLine[0], CRvisited, theMatrix);

			nextColumn[1] = leftColumn;
			nextLine[1] = leftLine;
			nextCheck[1] = BoundaryAndWallCheck(nextColumn[1], nextLine[1], CRvisited, theMatrix);

			nextColumn[2] = rightColumn;
			nextLine[2] = rightLine;
			nextCheck[2] = BoundaryAndWallCheck(nextColumn[2], nextLine[2], CRvisited, theMatrix);

			nextColumn[3] = downColumn;
			nextLine[3] = downLine;
			nextCheck[3] = BoundaryAndWallCheck(nextColumn[3], nextLine[3], CRvisited, theMatrix);
		}
		else {
			nextColumn[0] = downColumn;
			nextLine[0] = downLine;
			nextCheck[0] = BoundaryAndWallCheck(nextColumn[0], nextLine[0], CRvisited, theMatrix);

			nextColumn[1] = leftColumn;
			nextLine[1] = leftLine;
			nextCheck[1] = BoundaryAndWallCheck(nextColumn[1], nextLine[1], CRvisited, theMatrix);

			nextColumn[2] = rightColumn;
			nextLine[2] = rightLine;
			nextCheck[2] = BoundaryAndWallCheck(nextColumn[2], nextLine[2], CRvisited, theMatrix);

			nextColumn[3] = upColumn;
			nextLine[3] = upLine;
			nextCheck[3] = BoundaryAndWallCheck(nextColumn[3], nextLine[3], CRvisited, theMatrix);
		}
	}
	
	for (int i = 0; i < 4; i++) {
		if (nextCheck[i] && CheckReach(nextColumn[i], nextLine[i], targetColumn, targetLine, CRvisited, theMatrix)) {
			return true;
		}
	}

	return false;
}

bool CheckReaches(const UINT column, const UINT line, cell* theMatrix) {
	// can left, up, right and down still reach each other? if so, this move can be done.
	// Can left reach up, can up reach right, and can right reach down = good move.

	bool checkSpaces[4];
	int columns[4]; // These CANNOT be UINT! Evaluted >0 with expected possibility of going below 0!
	int lines[4]; // These CANNOT be UINT! Evaluted >0 with expected possibility of going below 0!
	// Left
	columns[0] = column - 1;
	lines[0] = line;
	// Boundary + wall check //checkSpaces[0] = columns[0] >= 0 ? (theMatrix[columns[0]][lines[0]] == SPACE_EMPTY ? true : false) : false; // Boundary + wall check
	checkSpaces[0] = columns[0] >= 0 ? (*MatrixRead(columns[0], lines[0], theMatrix) == SPACE_EMPTY ? true : false) : false;
	// Up
	columns[1] = column;
	lines[1] = line - 1;
	//checkSpaces[1] = lines[1] >= 0 ? (theMatrix[columns[1]][lines[1]] == SPACE_EMPTY ? true : false) : false; // Boundary + wall check
	checkSpaces[1] = lines[1] >= 0 ? (*MatrixRead(columns[1], lines[1], theMatrix) == SPACE_EMPTY ? true : false) : false;
	// Right
	columns[2] = column + 1;
	lines[2] = line;
	//checkSpaces[2] = columns[2] < COLUMNS ? (theMatrix[columns[2]][lines[2]] == SPACE_EMPTY ? true : false) : false; // Boundary + wall check
	checkSpaces[2] = columns[2] < (int)COLUMNS ? (*MatrixRead(columns[2], lines[2], theMatrix) == SPACE_EMPTY ? true : false) : false;
	// Down
	columns[3] = column;
	lines[3] = line + 1;
	//checkSpaces[3] = lines[3] < LINES ? (theMatrix[columns[3]][lines[3]] == SPACE_EMPTY ? true : false) : false; // Boundary + wall check
	checkSpaces[3] = lines[3] < (int)LINES ? (*MatrixRead(columns[3], lines[3], theMatrix) == SPACE_EMPTY ? true : false) : false;

	for (int j = 0, spacesToCheck = 0; j < 4; j++) {
		if (checkSpaces[j])
			spacesToCheck++;
	}

	if (spacesToCheck == 1) {
		// If only one space to check, the other are already used by walls or out of bounds, so don't bother checking anything more, just return true!
		return true;
	}
	else if (spacesToCheck == 0) {
		// If this is ever 0, we probably made an error earlier?
		//PrintMatrix(column, line);
		//cout << "Should check around: " << column << ',' << line << " is " << ((theMatrix[column][line] == SPACE_EMPTY) ? "empty" : "a wall") << endl;
		MF_Log("Metamap: Error - unreachable area may have been created earlier, quitting...");
		throw;
	}

	//cout << "Should find " << spacesToCheck << " connections." << endl;
	//int connectionsToFind
	bool reaches;
	int tested = 0;
	int connections = 0;
	//vector<bool> CRvisited; <-- burn in hell, STL! :-D
	//CRvisited.resize(COLUMNS * LINES);
	
	bool* visited = new bool[COLUMNS * LINES];
	//EmptyMatrix(theMatrix, visited);

	//bool CRvisited[COLUMNS][LINES];
	for (UINT i = 0; i < 4; i++) {
		if (!checkSpaces[i])
			continue;

		// We should be able to return true here, because if A can reach B, B can also reach A. :-)
		//if (connections == 1 && spacesToCheck == 2)
			//return true;
		reaches = false;
		tested = 0;
		for (UINT j = i + 1; tested < 4; j++) {
			if (j == 4)
				j = 0;
			tested++; // we have tested this direction
			if (i == j || !checkSpaces[j])
				continue;

			//cout << "Can " << columns[i] << ',' << lines[i] << " reach " << columns[j] << ',' << lines[j] << '?';
			for (UINT l = 0; l < LINES; l++) {
				for (UINT k = 0; k < COLUMNS; k++) {
					(VisitedAccess(k, l, visited)) = false;//CRvisited[l][k] = false;
				}
			}
			//MF_Log("i: %d, calls CheckReach(%d, %d, %d, %d, CRvisited, theMatrix)", i, columns[i], lines[i], columns[j], lines[j]);
			if (CheckReach(columns[i], lines[i], columns[j], lines[j], visited, theMatrix)) {
				//cout << " Yes, " << columns[i] << ',' << lines[i] << " reaches " << columns[j] << ',' << lines[j] << ", indexes " << i << " and " << j << '.' << endl;
				reaches = true;
				connections++;
				break; // break, don't check this space anymore, go on check with the rest.
			}
			else {
				// Really, if A cannot ever reach B, it does not matter if A can reach C! :-)
				delete [] visited;
				return false;
			}
			//cout << " No!" << endl;
		}

		// If we can't reach any of the other spaces, return false here.
		if (!reaches) {
			//cout << columns[i] << ',' << lines[i] << " can't reach any other space! Returning false here..." << endl;
			delete [] visited;
			return false;
		}
	}

	delete [] visited;
	return true;
}

bool PlaceWalls2(const UINT WALLSTOPLACE, cell* theMatrix)
{
	if (WALLSTOPLACE > COLUMNS * LINES) {
		MF_Log("Metamap: Too many walls!");
		return false;
	}

	UINT wallsPlaced = 0;
	// Find all empty spaces, add them to vector.
	vector<CSpace> emptySpaces;
	//int round = 0;
	while (wallsPlaced < WALLSTOPLACE) {
		//round++;
		//cout << "Starting new round, we should place " << wallsToPlace << " but have so far only placed " << wallsPlaced << " walls." << endl;
		//system("PAUSE");
		for (UINT i = 0, empties = 0; i < COLUMNS; i++) {
			for (UINT j = 0; j < LINES; j++) {
				if (*(MatrixRead(i, j, theMatrix)) == SPACE_EMPTY) { // if (theMatrix[i][j] == SPACE_EMPTY) {
					AddSpace(i, j, emptySpaces);
					empties++;
				}
			}
		}

#if defined _debug
		MF_Log("Added %d empty spaces... %d elements in emptySpaces", empties, emptySpaces.size());
#endif

		for (UINT column, line, element; wallsPlaced < WALLSTOPLACE && !emptySpaces.empty(); wallsPlaced++) {
			element = JBRandom::JBRandomize(0, emptySpaces.size() - 1);
			column = emptySpaces[element].column;
			line = emptySpaces[element].line;
			//MF_Log("element %d in column %d, line %d", element, column, line);

			*(MatrixWrite(column, line, theMatrix)) = SPACE_WALL;// theMatrix[column][line] = SPACE_WALL;
			//PrintMatrix(column, line);

			// Is it possible to place a wall here without blocking anything?

			if (!CheckReaches(column, line, theMatrix)) {
				//cout << "No!" << endl;
				*(MatrixWrite(column, line, theMatrix)) = SPACE_EMPTY; //theMatrix[column][line] = SPACE_EMPTY;
				wallsPlaced--;
				//system("PAUSE");
			}

			emptySpaces.erase(&emptySpaces[element]);
			//cout << emptySpaces.size() << endl;
			//system("PAUSE");
		}
		//cout << "One round ready..." << endl;
		//PrintMatrix();
		//system("PAUSE");
	}

	return true;
}

void CountWalls(cell* theMatrix) {
	int walls = 0, empties = 0, others = 0;
	for (UINT i = 0; i < LINES * COLUMNS; i++) {
		if (theMatrix[i] == SPACE_EMPTY)
			empties++;
		else if (theMatrix[i] == SPACE_WALL)
			walls++;
		else
			others++;

	}
	MF_Log("Walls: %d empties: %d others: %d", walls, empties, others);
}

static cell AMX_NATIVE_CALL metamap_getmap(AMX *amx, cell *params)  // native metamap_getmap(matrix[], columns, lines, walls); = 4 params
{
#if defined _DEBUG
	MF_Log("metamap_getmap start");
#endif
	g_amx = amx;
	// Get matrix
	cell* theMatrix = MF_GetAmxAddr(amx, params[1]);

	// Get rest of the parameters
	COLUMNS = params[2];
	LINES = params[3];
	const int WALLS = params[4];

	if (COLUMNS <= 0) {
		MF_Log("Too few columns! (%d)", COLUMNS);
		MF_RaiseAmxError(g_amx, AMX_ERR_NATIVE);
		return 0;
	}
	else if (LINES <= 0) {
		MF_Log("Too few lines! (%d)", LINES);
		MF_RaiseAmxError(g_amx, AMX_ERR_NATIVE);
		return 0;
	}
	else if (WALLS <= 0) {
		MF_Log("Too few walls! (%d)", WALLS);
		MF_RaiseAmxError(g_amx, AMX_ERR_NATIVE);
		return 0;
	}

	try {
		EmptyMatrix(theMatrix);

#if defined _DEBUG
		CountWalls(theMatrix);
#endif
		if (!PlaceWalls2(WALLS, theMatrix)) {
			MF_RaiseAmxError(g_amx, AMX_ERR_NATIVE);
			return 0;
		}
#if defined _DEBUG
		MF_Log("After placing walls:");
		CountWalls(theMatrix);
#endif
	}
	catch (...) {
		MF_Log("Metamap, main: Unhandled exception.");
		MF_RaiseAmxError(g_amx, AMX_ERR_NATIVE);
		return 0;
	}

#if defined _DEBUG
	MF_Log("metamap_getmap end");
#endif
	return 1;
}
/******************************************************************************************/
AMX_NATIVE_INFO metamap_Exports[] = {
	{"metamap_getmap",			metamap_getmap},
	  /////////////////// <--- 19 chars max
	{NULL,					NULL}
};

void OnAmxxAttach()
{
	MF_AddNatives(metamap_Exports);
}
