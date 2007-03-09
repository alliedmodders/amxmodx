#ifndef VTABLEMANAGER_H
#define VTABLEMANAGER_H

#include "Trampolines.h"

#include "CVector.h"
#include "hooks.h"
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




class VTableManager
{
public:
	CVector<VTableUse *>			UseEntries;
	CVector<VTableTakeDamage *>		TakeDamageEntries;
	CVector<VTableBlocked *>		BlockedEntries;

	/* returns the original function */
	void *InsertIntoVTable(void **vtable, int index, void *trampoline);
	void  Cleanup(void);
};

extern VTableManager VTMan;

//#include "VTableEntries.h"

#endif // VTABLEMANAGER_H
