// CstrikePlayer.h: interface for the CCstrikePlayer class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(INCLUDED_CCSTRIKEPLAYER)
#define INCLUDED_CCSTRIKEPLAYER

class CCstrikePlayer  
{
public:
	CCstrikePlayer();

	/*bool GetOnline();
	bool SetOnline(bool onlineIn);*/
	bool GetModelled();
	bool SetModelled(bool modelledIn);
	//float GetTime();
	//void SetTime(float timeIn);
	const char* GetModel();
	const char* SetModel(const char* modelIn);
	bool GetInspectModel();
	void SetInspectModel(bool inspectModelIn);

private:
	bool online, inspectModel;
	bool modelled;
	char model[32];
	//float time;
};

#endif // !defined(INCLUDED_CCSTRIKEPLAYER)
