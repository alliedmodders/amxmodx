/* Messaging functions (now part of Core)
 *
 * by the AMX Mod X Development Team
 *
 * This file is provided as is (no warranties).
 */

#if defined _coremsg_included
  #endinput
#endif
#define _coremsg_included

#include <message_const>

/* These functinos are used to generate client messages.
 * You may generate menu, smoke, shockwaves, thunderlights,
 * intermission and many many others messages.
 * See HL SDK for more examples. */
native message_begin(dest, msg_type, const origin[3] = {0,0,0}, player = 0);
native message_end();
native write_byte(x);
native write_char(x);
native write_short(x);
native write_long(x);
native write_entity(x);
native write_angle(x);
native write_coord(x);
native write_string(const x[]);

/* These are the same as above, except that the messages sent
 *  are also sent to all other plugins and Metamod plugins.
 * This means that if you send one of these messages, other plugins will
 *  be notified, which was previously impossible.  
 * BE CAREFUL! Using these incorrectly, or not for their intended purpose,
 *  could cause infinite recursion or something just as bad. 
 * NOTE! These natives are experimental.
 */
native emessage_begin(dest, msg_type, const origin[3] = {0,0,0}, player = 0);
native emessage_end();
native ewrite_byte(x);
native ewrite_char(x);
native ewrite_short(x);
native ewrite_long(x);
native ewrite_entity(x);
native ewrite_angle(x);
native ewrite_coord(x);
native ewrite_string(const x[]);

/* Sets/Gets what engine messages are blocked. */
native set_msg_block(iMessage, iMessageFlags);
native get_msg_block(iMessage);

/* Lets you directly hook a message in the engine!
 * You can overwrite the message before anything happens and either let the message continue
 * or fully block it. Here is how it works:
 * If you hook a message, the message is stored but not sent. You have the opportunity to
 * not only execute code, but to get/set the contents of the message, before you choose to 
 * either block it or let it go on its way. The hooked function will be passed a msg_id, msg_dest, and entity index. 
 * The return value can be passed to unregister_message() to stop the message from being hooked */
native register_message(iMsgId, const szFunction[]);

/* Unregisters a message hook previously created with register_message
 * You must pass the proper message id, and return value from the message to unregister the message successfully. */
native unregister_message(iMsgId, registeredmsg);


/* The get/set _msg commands will fail if used outside a hooked message scope.
 * They should never be used unless inside a registered message function.
 * There are eight different ways of sending a message, five are ints, two are floats, and one is string.
 * These are denoted by iArgType.  argn is the number
 * of the argument. Exceeding the bounds of 1 to get_msg_args() is a bad idea. 
 * As of AMX Mod X 1.5, the middle parameter of set_* no longer does anything.
 * You cannot change the message argument type (as this would crash the mod anyway)
 */

/* Gets number of arguments that were passed to this message */
native get_msg_args();

/* Gets the argument type of argument argn */
native get_msg_argtype(argn);

/* Gets the value of argn. */
native get_msg_arg_int(argn);
native Float:get_msg_arg_float(argn);
native get_msg_arg_string(argn, szReturn[], iLength);

/* sets the value of argn. */
native set_msg_arg_int(argn, argtype, iValue);
native set_msg_arg_float(argn, argtype, Float:fValue);
native set_msg_arg_string(argn, const szString[]);

/* Gets the origin of a message */
native get_msg_origin(const Float:_Origin[3]);
