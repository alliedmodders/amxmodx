// CstrikePlayer.h: interface for the CCstrikePlayer class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(INCLUDED_CCSTRIKEPLAYER)
#define INCLUDED_CCSTRIKEPLAYER

class CCstrikePlayer  
{
public:
	CCstrikePlayer();

	bool GetModelled();
	bool SetModelled(bool modelledIn);
	const char* GetModel();
	void SetModel(const char* modelIn);

	bool GetInspectModel();
	void SetInspectModel(bool inspectModelIn);


private:
	bool inspectModel;
	bool modelled;
	char model[32];
};

#endif // !defined(INCLUDED_CCSTRIKEPLAYER)
