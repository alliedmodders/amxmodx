#ifndef CPLAYER_H
#define CPLAYER_H
class CPlayer
{
public:
	void PreThink();
	void PreThink_Post();
	void PostThink_Post();
	void Spawn();
	void ChangeTeam();
	void ChangeClass(int newclass);
	void Connect();
	void Disconnect();
	void Reset();
	void Die();
	int GetClass();
	bool bot;
	// Basic engine stuff.
	edict_t *edict;
	entvars_t *pev;
	int	oldimpulse; // Store the previous impulse.
	int	index;

	bool connected;
	REAL fov;
	bool foved;
	// Custom model/body/skin
	bool custommodel;
	bool customskin;
	bool custombody;
	char model[128];
	int skin;
	int body;

	// Speed change
	int speedchange;
	int maxspeed;
	
	int iclass;
};
#endif

