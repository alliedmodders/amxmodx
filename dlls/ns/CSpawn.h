#ifndef CSPAWN_H
#define CSPAWN_H
class CSpawn
{
	struct spawnpointInfo 
	{
		int type; // 0 = Ready room, 1 = Marine, 2 = Alien
		vec3_t location;
		spawnpointInfo* next;
		spawnpointInfo(int type, vec3_t location, spawnpointInfo* next): type(type), location(location), next(next) {}
	} *spawnpointinfo;
public:
	CSpawn() { spawnpointinfo = 0; }
	~CSpawn() { clear(); }
	void clear();
	void put(int type, vec3_t location);
	float getnum(int type);
	vec3_t getpoint(int type, int num);
};
#endif

