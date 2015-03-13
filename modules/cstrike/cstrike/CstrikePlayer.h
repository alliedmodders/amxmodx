// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Module
//

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
