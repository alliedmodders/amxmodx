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

#include "amxmodx.h"
#include "CCmd.h"

// *****************************************************
// class CmdMngr
// *****************************************************

CmdMngr::CmdMngr()
{ 
	memset(sortedlists, 0, sizeof(sortedlists));
	srvcmdlist = 0;
	clcmdlist = 0;
	prefixHead = 0;
	buf_type = -1;
	buf_access = 0;
	buf_id = -1;
	buf_cmdid = -1;
	buf_cmdtype = -1;
	buf_cmdaccess = 0;

}

CmdMngr::Command::Command(CPluginMngr::CPlugin* pplugin, const char* pcmd, const char* pinfo, int pflags, 
							int pfunc, bool pviewable, CmdMngr* pparent) : commandline(pcmd), info(pinfo)
{
	char szCmd[64], szArg[64];
	*szCmd = 0; *szArg = 0;
	sscanf(pcmd, "%s %s", szCmd, szArg);
	command.assign(szCmd);
	argument.assign(szArg);
	plugin = pplugin;
	flags = pflags;
	cmdtype = 0;
	prefix = 0;
	function = pfunc;
	listable = pviewable;
	parent = pparent;
	id = --uniqueid;
}

CmdMngr::Command::~Command()
{
	++uniqueid;
}

CmdMngr::Command* CmdMngr::registerCommand(CPluginMngr::CPlugin* plugin, int func, char* cmd, char* info, int level, bool listable)
{
	Command* b = new Command(plugin, cmd, info, level, func, listable, this);
	if (b == 0) return 0;
	setCmdLink(&sortedlists[0], b);
	
	return b;
}

CmdMngr::Command* CmdMngr::getCmd(long int id, int type, int access)
{
	//if (id >= 1024 || id < 0) return (Command*)id;
	if (id < 0)
	{
		for (CmdMngr::iterator a = begin(type); a ; ++a)
		{
			if ((*a).id == id)
				return &(*a);
		}
		
		return 0;
	}

	if ((id < buf_cmdid) || (access != buf_cmdaccess) || (type != buf_cmdtype))
	{
		buf_cmdptr = begin(type);
		buf_cmdaccess = access;
		buf_cmdtype = type;
		buf_cmdid = id;
	} else {
		int a = id;
		id -= buf_cmdid;
		buf_cmdid = a;
	}

	while (buf_cmdptr)
	{
		if ((*buf_cmdptr).gotAccess(access) && (*buf_cmdptr).getPlugin()->isExecutable((*buf_cmdptr).getFunction()) && (*buf_cmdptr).isViewable())
		{
			if (id-- == 0) 
				return &(*buf_cmdptr);
		}
		++buf_cmdptr;
	}

	return 0;		
}

int CmdMngr::getCmdNum(int type, int access)
{

	buf_access = access;
	buf_type = type;
	buf_num = 0;

	CmdMngr::iterator a = begin(type);

	while (a)
	{
		if ((*a).gotAccess(access) && (*a).getPlugin()->isExecutable((*a).getFunction()) && (*a).isViewable())
			++buf_num;
		++a;
	}

	return buf_num;
}

void CmdMngr::setCmdLink(CmdLink** a, Command* c, bool sorted)
{
	CmdLink* np = new CmdLink(c);

	if (np == 0) return;

	if (sorted)
	{
		while (*a)
		{
			int i = strcmp(c->getCommand(), (*a)->cmd->getCommand());

			if ((i < 0) || ((i == 0) && (strcmp(c->getArgument(), (*a)->cmd->getArgument()) < 0)))
				break;
			
			a = &(*a)->next;
		}

		np->next = *a;
		*a = np;
	} else {
		while (*a) a = &(*a)->next;
		*a = np;
	}
}

void CmdMngr::clearCmdLink(CmdLink** phead, bool pclear)
{
	while (*phead)
	{
		CmdLink* pp = (*phead)->next;
		
		if (pclear) delete (*phead)->cmd;
		delete *phead;
		*phead = pp;
	}
}

void CmdMngr::Command::setCmdType(int a)
{
	switch (a)
	{
		case CMD_ConsoleCommand: cmdtype |= 3; break;
		case CMD_ClientCommand: cmdtype |= 1; break;
		case CMD_ServerCommand: cmdtype |= 2; break;
	}

	if (cmdtype & 1)	// ClientCommand
	{
		parent->setCmdLink(&parent->sortedlists[1], this);
		
		if (!parent->registerCmdPrefix(this))
			parent->setCmdLink(&parent->clcmdlist, this, false);
	}
	
	if (cmdtype & 2)	// ServerCommand
	{
		parent->setCmdLink(&parent->sortedlists[2], this);
		parent->setCmdLink(&parent->srvcmdlist, this, false);
	}
}

const char* CmdMngr::Command::getCmdType() const
{
	switch (cmdtype)
	{
		case 1:	return "client";
		case 2:	return "server";
		case 3:	return "console";
	}
	
	return "unknown";
}

bool CmdMngr::registerCmdPrefix(Command* cc)
{
	CmdPrefix** b = findPrefix(cc->getCommand());

	if (*b)
	{
		setCmdLink(&(*b)->list, cc, false);
		cc->prefix = (*b)->name.size();
		return true;
	}
	
	return false;
}

void CmdMngr::registerPrefix(const char* nn)
{
	if (*nn == 0) return;
	CmdPrefix** b = findPrefix(nn);
	
	if (*b) return;
	*b = new CmdPrefix(nn, this);
}

CmdMngr::CmdPrefix** CmdMngr::findPrefix(const char* nn)
{
	CmdPrefix** aa = &prefixHead;
	
	while (*aa)
	{
		if (!strncmp((*aa)->name.c_str(), nn, (*aa)->name.size()))
			break;
		aa = &(*aa)->next;
	}
	
	return aa;
}

void CmdMngr::clearPrefix()
{
	while (prefixHead)
	{
		CmdPrefix* a = prefixHead->next;
		delete prefixHead;
		prefixHead = a;
	}
}

void CmdMngr::clear()
{
	clearCmdLink(&sortedlists[0], true);
	clearCmdLink(&sortedlists[1]);
	clearCmdLink(&sortedlists[2]);
	clearCmdLink(&srvcmdlist);
	clearCmdLink(&clcmdlist);
	clearPrefix();
	clearBufforedInfo();
}

void CmdMngr::clearBufforedInfo()
{
	buf_type = -1; 
	buf_access = 0; 
	buf_id = -1; 
	buf_cmdid = -1;
	buf_cmdtype = -1;
	buf_cmdaccess = 0;
}

int CmdMngr::Command::uniqueid = 0;
