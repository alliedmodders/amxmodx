/* Vexd Utility backwards compatibility
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _Vexd_Utilities_included
  #endinput
#endif
#define _Vexd_Utilities_included

#include <engine>
#if defined AMXMOD_BCOMPAT
#if !defined _vexd_bcompat_included
#include <VexdUM>
#endif
#endif

stock Entvars_Get_Int(iIndex, iVariable)
	return entity_get_int(iIndex, iVariable);

stock Entvars_Set_Int(iIndex, iVariable, iNewValue)
	return entity_set_int(iIndex, iVariable, iNewValue);

stock Float:Entvars_Get_Float(iIndex, iVariable)
	return entity_get_float(iIndex, iVariable);

stock Entvars_Set_Float(iIndex, iVariable, Float:fNewValue)
	return entity_set_float(iIndex, iVariable, fNewValue);

stock Entvars_Get_Vector(iIndex, iVariable, Float:vRetVector[3])
	return entity_get_vector(iIndex, iVariable, vRetVector);

stock Entvars_Set_Vector(iIndex, iVariable, Float:vNewVector[3])
	return entity_set_vector(iIndex, iVariable, vNewVector);

stock Entvars_Get_Edict(iIndex, iVariable)
	return entity_get_edict(iIndex, iVariable);

stock Entvars_Set_Edict(iIndex, iVariable, iNewIndex)
	return entity_set_edict(iIndex, iVariable, iNewIndex);

stock Entvars_Get_String(iIndex, iVariable, szReturnValue[], iReturnLen)
	return entity_get_string(iIndex, iVariable, szReturnValue, iReturnLen);

stock Entvars_Set_String(iIndex, iVariable, szNewValue[])
	return entity_set_string(iIndex, iVariable, szNewValue);

stock Entvars_Get_Byte(iIndex, iVariable)
	return entity_get_byte(iIndex, iVariable);

stock Entvars_Set_Byte(iIndex, iVariable, iNewValue)
	return entity_set_byte(iIndex, iVariable, iNewValue);

stock CreateEntity(szClassname[])
	return create_entity(szClassname);

stock ENT_SetModel(iIndex, szModel[])
	return entity_set_model(iIndex, szModel);

stock ENT_SetOrigin(iIndex, Float:fNewOrigin[3])
	return entity_set_origin(iIndex, fNewOrigin);

stock FindEntity(iIndex, szValue[])
	return find_ent_by_class(iIndex, szValue);

stock RemoveEntity(iIndex)
	return remove_entity(iIndex);

stock TraceLn(iIgnoreEnt, Float:fStart[3], Float:fEnd[3], Float:vReturn[3])
	return trace_line(iIgnoreEnt, fStart, fEnd, vReturn);

stock TraceNormal(iIgnoreEnt, Float:fStart[3], Float:fEnd[3], Float:vReturn[3])
	return trace_normal(iIgnoreEnt, fStart, fEnd, vReturn);

stock VecToAngles(Float:fVector[3], Float:vReturn[3])
	return vector_to_angle(fVector, vReturn);

stock Float:VecLength(Float:vVector[3])
	return vector_length(vVector);

stock Float:VecDist(Float:vVector[3], Float:vVector2[3])
	return vector_distance(vVector, vVector2);

stock MessageBlock(iMessage, iMessageFlags)
	return set_msg_block(iMessage, iMessageFlags);

stock GetMessageBlock(iMessage)
	return get_msg_block(iMessage);

stock Float:HLTime()
	return halflife_time();

stock FakeTouch(iToucher, iTouched)
	return fake_touch(iToucher, iTouched);

stock AttachView(iIndex, iTargetIndex)
	return attach_view(iIndex, iTargetIndex);

stock SetView(iIndex, ViewType)
	return set_view(iIndex, ViewType);

stock SetSpeak(iIndex, iSpeakFlags)
	return set_speak(iIndex, iSpeakFlags);

forward vexd_pfntouch(pToucher, pTouched);

forward ServerFrame();
