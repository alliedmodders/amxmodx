#ifndef VTABLEMANAGER_H
#define VTABLEMANAGER_H

#include "Trampolines.h"

#include "hamsandwich.h"

#include "CVector.h"
#include "VTableEntries.h"

/* !!WARNING: HERE BE DRAGONS

                                                  .~))>>
                                                 .~)>>
                                               .~))))>>>
                                             .~))>>             ___
                                           .~))>>)))>>      .-~))>>
                                         .~)))))>>       .-~))>>)>
                                       .~)))>>))))>>  .-~)>>)>
                   )                 .~))>>))))>>  .-~)))))>>)>
                ( )@@*)             //)>))))))  .-~))))>>)>
              ).@(@@               //))>>))) .-~))>>)))))>>)>
            (( @.@).              //))))) .-~)>>)))))>>)>
          ))  )@@*.@@ )          //)>))) //))))))>>))))>>)>
       ((  ((@@@.@@             |/))))) //)))))>>)))>>)>
      )) @@*. )@@ )   (\_(\-\b  |))>)) //)))>>)))))))>>)>
    (( @@@(.@(@ .    _/`-`  ~|b |>))) //)>>)))))))>>)>
     )* @@@ )@*     (@)  (@) /\b|))) //))))))>>))))>>
   (( @. )@( @ .   _/  /    /  \b)) //))>>)))))>>>_._
    )@@ (@@*)@@.  (6///6)- / ^  \b)//))))))>>)))>>   ~~-.
 ( @jgs@@. @@@.*@_ VvvvvV//  ^  \b/)>>))))>>      _.     `bb
  ((@@ @@@*.(@@ . - | o |' \ (  ^   \b)))>>        .'       b`,
   ((@@).*@@ )@ )   \^^^/  ((   ^  ~)_        \  /           b `,
     (@@. (@@ ).     `-'   (((   ^    `\ \ \ \ \|             b  `.
       (*.@*              / ((((        \| | |  \       .       b `.
                         / / (((((  \    \ /  _.-~\     Y,      b  ;
                        / / / (((((( \    \.-~   _.`" _.-~`,    b  ;
                       /   /   `(((((()    )    (((((~      `,  b  ;
                     _/  _/      `"""/   /'                  ; b   ;
                 _.-~_.-~           /  /'                _.'~bb _.'
               ((((~~              / /'              _.'~bb.--~
                                  ((((          __.-~bb.-~
                                              .'  b .~~
                                              :bb ,' 
                                              ~~~~
*/


enum 
{
	HAM_UNSET = 0,
	HAM_IGNORED,
	HAM_HANDLED,
	HAM_OVERRIDE,
	HAM_SUPERCEDE
};

enum
{
	HAM_TYPE_UNKNOWN = 0,
	HAM_TYPE_CBASE,
	HAM_TYPE_ENTVAR,
	HAM_TYPE_EDICT,
	HAM_TYPE_INT,
	HAM_TYPE_FLOAT
};

enum
{
	HAM_ERROR_BOUNDS = -2,
	HAM_ERROR_TYPE = -1,
	HAM_ERROR_NONE = 0
};


typedef cell (*NATIVEFUNC)(AMX *, cell *);

class VTableManager
{
public:
#define VTINIT(Type) CVector<VTable##Type *>	Type##Entries
	VTINIT(Use);
	VTINIT(TakeDamage);
	VTINIT(Blocked);
	VTINIT(Killed);
	VTINIT(Respawn);
	VTINIT(Restart);
	VTINIT(AddPoints);
	VTINIT(AddPointsToTeam);
	VTINIT(AddPlayerItem);
	VTINIT(RemovePlayerItem);
	VTINIT(BloodColor);
	VTINIT(Classify);
	VTINIT(GetToggleState);
	VTINIT(IsAlive);
	VTINIT(IsBSPModel);
	VTINIT(IsInWorld);
	VTINIT(IsMoving);
	VTINIT(IsNetClient);
	VTINIT(IsPlayer);
	VTINIT(IsSneaking);
	VTINIT(ObjectCaps);
	VTINIT(Think);
	VTINIT(Touch);

#undef VTINIT
	static NATIVEFUNC					 RegisterNatives[HAM_END_DONT_USE_ME];
	static NATIVEFUNC					 RegisterIDNatives[HAM_END_DONT_USE_ME];
	static const char					*RegisterNames[HAM_END_DONT_USE_ME];

	static cell Register(AMX *amx, cell *params);
	static cell RegisterID(AMX *amx, cell *params);

	/* returns the original function */
	void *InsertIntoVTable(void **vtable, int index, void *trampoline);
	void  Cleanup(void);
};

void RegisterThisRegister(int index,NATIVEFUNC byname, NATIVEFUNC byid);
void RegisterThisRegisterName(int index, const char *name);
void RegisterRegisterNatives(void);

extern VTableManager VTMan;


#endif // VTABLEMANAGER_H
