/* Ham Sandwich
 *   Copyright 2007-2014
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */
#ifndef HOOKLIST_T_H
#define HOOKLIST_T_H

typedef struct hook_s
{
	int isset;								// whether or not this hook is registered with hamdata
	int vtid;								// vtable index of this function
	const char *name;						// name used in the keys
	bool isvoid;							// whether or not the target trampoline uses voids
	bool needsretbuf;						// whether or not a pointer to a memory buffer is needed to store a return value
	int  paramcount;						// how many parameters are in the func
	void *targetfunc;						// the target hook
	int (*makefunc)(AMX *, const char*);	// function that creates forwards
	cell (*call)(AMX *, cell*);				// function to call the vcall
} hook_t;

extern hook_t hooklist[];

#endif
