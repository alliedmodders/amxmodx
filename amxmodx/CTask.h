/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#ifndef CTASK_H
#define CTASK_H

// *****************************************************
// class CTaskMngr
// *****************************************************

class CTaskMngr
{
public:
	
	class iterator;

	class CTask
	{

		friend class iterator;
		friend class CTaskMngr;

		CPluginMngr::CPlugin* plugin;
		int id;
		int func;
		int repeat;
		bool loop;
		bool afterstart;
		bool beforeend;
		float base_time;
		float exec_time;
		int param_len;
		cell* param;
		CTask* next;
		CTask* prev;
		inline void setToRemove() { exec_time = -1.0f; }
		inline void changeTime(float flNewTime) { exec_time = flNewTime; }
		inline bool isToReply() { return  (repeat-- > 0); }
		inline bool isRemoved() { return (exec_time == -1.0f);  }
		CTask( CPluginMngr::CPlugin* p, int f, int flags, int i, 
			float base,  float exec, int parlen , const cell* par, int r );
		~CTask() { if ( param_len ) delete[] param; }	
	
	public:

		inline int getParamLen() { return param_len; }
		inline int getTaskId() { return id; }
		inline int getFunction() { return func; }
		cell* getParam() { return param; }
		CPluginMngr::CPlugin* getPlugin() { return plugin; }
	};



private:  
	
	friend class iterator;
	CTask *head;
	CTask *tail;
	float* m_timer;
	float* m_timelimit;
	float* m_timeleft;
	CTask* getFirstValidTask(CTask* a);
	CTask* getNextTask(CTask* a);	
	CTask* findTask( int id , AMX* amx );
	void unlink(CTask* a);

public:
	
	CTaskMngr();
	~CTaskMngr();

	// Interface


	void registerTimers( float* timer , float* timelimit, float* timeleft );
	void registerTask( CPluginMngr::CPlugin* plugin, int func, int flags, int i, float base, float exec, int parlen , const cell* par, int repeat );
	inline int taskExists(  int id ,AMX* amx) { return findTask(id,amx ) ? 1 : 0; }
	int changeTask(int id, AMX *amx, float flNewTime);
	int removeTasks( int id , AMX* amx );
	void clear();

	class iterator {
		CTask* a;
		CTaskMngr* b;
	public:
		iterator(CTask*aa,CTaskMngr* bb) : a(aa), b(bb) {}
		iterator& operator++() {
			a = b->getNextTask( a );
			a = b->getFirstValidTask( a );
			return *this;
		}
		CTask& operator*() { return *a; }
		operator bool ( ) const { return a ? true : false; }
	};
	inline iterator begin() { return iterator(getFirstValidTask(head),this); }

};

#endif


