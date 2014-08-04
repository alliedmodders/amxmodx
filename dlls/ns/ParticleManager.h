// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#ifndef PARTICLEMANAGER_H
#define PARTICLEMANAGER_H

typedef struct psystem_s
{
	String			 Name;
	int				 id;
	int				 IsStatic; // Set to 1 if the particle system is loaded from ns.ps

} ParticleSystem;

class ParticleManager
{
private:
	CVector<ParticleSystem *>		 Systems;
	int								 m_iFileLoaded;
	unsigned short					 m_iEventID;

public:
	ParticleManager()
	{
		m_iFileLoaded=0;
		m_iEventID=0;
		Systems.reserve(64);
	};

	// Remove all non-static particle systems
	inline void Prune()
	{
		if (Systems.size()==0)
		{
			return;
		}
		CVector<ParticleSystem *>::iterator		iter;
		while (1)
		{
			if (Systems.size()==0)
			{
				break;
			}
			iter=Systems.end();
			iter--;

			if ((*iter)->IsStatic)
			{
				break;
			}

			delete (*iter);
			Systems.pop_back();
		};
	};

	void ReadFile(void);

	inline int Add(const char *Start, int Static)
	{
		ParticleSystem *ps=new ParticleSystem;

		ps->id=Systems.size();
		ps->IsStatic=Static;
		ps->Name.assign(Start);

		Systems.push_back(ps);

		return Systems.size()-1;
	};
	inline void FireSystem(int id, float *Origin, float *Angles, int flags)
	{
		PLAYBACK_EVENT_FULL(flags,		/*flags*/
							NULL,		/*pInvoker*/
							m_iEventID, /*eventid*/
							0.0,		/*delay*/
							Origin,		/*origin*/
							Angles,		/*angles*/
							0.0,		/*fparam1*/
							0.0,		/*fparam2*/
							id,			/*iparam1 - particle system id*/
							0,			/*iparam2*/
							0,			/*bparam1*/
							0);			/*bparam2*/
	};
	inline void PrecacheEvent(const char *file)
	{
		if (strcmp(file,"events/Particle.sc")==0)
		{
			if (META_RESULT_STATUS >= MRES_OVERRIDE)
			{
				m_iEventID=META_RESULT_OVERRIDE_RET(unsigned short);
			}
			else
			{
				m_iEventID=META_RESULT_ORIG_RET(unsigned short);
			}
			//printf("EVENT=%d\n",m_iEventID);
		}
	};
	inline int Find(const char *Needle)
	{
		CVector<ParticleSystem *>::iterator iter=Systems.begin();
		CVector<ParticleSystem *>::iterator end=Systems.end();

		while (iter!=end)
		{
			if (strcmp(Needle,(*iter)->Name.c_str())==0)
			{
				return (*iter)->id;
			}
			++iter;
		}

		return -1;
	};

};

extern ParticleManager ParticleMan;

#endif // PARTICLEMANAGER_H
