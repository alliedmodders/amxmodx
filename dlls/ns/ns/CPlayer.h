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
	BOOL ClientCommand();
	int GetClass();
	bool bot;
	// Basic engine stuff.
	edict_t *edict;
	entvars_t *pev;
	int	oldimpulse; // Store the previous impulse.
	int	index;

	bool connected;

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
	struct
	{
		int		iFunctionIndex;
		bool	inmenu;
		float	time;
		int		keys;	
		char	text[512];
		int		chan;
		int		channel1;
		int		channel2;
	} menucmd;
	hudtextparms_t menuhudtext;
};
#endif