#include "ns.h"

void CSpawn::clear()
{
	while (spawnpointinfo)
	{
		spawnpointInfo* a = spawnpointinfo->next;
		delete spawnpointinfo;
		spawnpointinfo = a;
	}

}
void CSpawn::put(int type, vec3_t location)
{
	spawnpointinfo = new spawnpointInfo(type, location, spawnpointinfo);
}
float CSpawn::getnum(int type)
{
	float iTemp=0.0;
	spawnpointInfo *a = spawnpointinfo;
	while (a) 
	{
		if (a->type == type)
			iTemp+=1.0;
		a = a->next;
	}
	return iTemp;
}
vec3_t CSpawn::getpoint(int type, int num)
{ 
	int iTemp=0;
	spawnpointInfo *a = spawnpointinfo;
	while (a)
	{
		if (a->type == type)
		{
			iTemp++;
			if (iTemp == num)
			{
				return a->location;
			}
		}
		a=a->next;
	}
	return Vector(0.0,0.0,0.0);
}