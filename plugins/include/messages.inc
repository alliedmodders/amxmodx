// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Message Functions
//

#if defined _coremsg_included
  #endinput
#endif
#define _coremsg_included

#include <message_const>

/**
 * Marks the beginning of a client message.
 *
 * @note You may generate menus, smoke, shockwaves, thunderlights,
 *       intermission and many other messages.
 * @note For a list of HL game events, visit https://wiki.alliedmods.net/Half-Life_1_Game_Events
 * @note For a list of HL engine messages, visit https://wiki.alliedmods.net/Half-Life_1_Engine_Messages
 * @note You may also refer to the messages_const.inc file for examples.
 * @note Each message starts with a message_begin() or message_begin_f() function
 *       and ends with message_end(). The specific message arguments go in between
 *       these two by using the write_*() functions found in messages.inc.
 *
 * @param dest        Destination type (see MSG_* constants in messages_const.inc)
 * @param msg_type    Message id
 * @param origin      Message origin
 * @param player      Client index receiving the message or 0 for all clients
 *
 * @noreturn
 * @error             If an invalid message id is specified or an invalid number
 *                    of parameters is passed, an error will be thrown.
 */
native message_begin(dest, msg_type, const origin[3] = {0,0,0}, player = 0);

/**
 * Marks the beginning of a client message.
 *
 * @note You may generate menus, smoke, shockwaves, thunderlights,
 *       intermission and many other messages.
 * @note For a list of HL game events, visit https://wiki.alliedmods.net/Half-Life_1_Game_Events
 * @note For a list of HL engine messages, visit https://wiki.alliedmods.net/Half-Life_1_Engine_Messages
 * @note You may also refer to the messages_const.inc file for examples.
 * @note This function is the same as message_begin(), but the origin
 *       argument accepts only float values in this one.
 * @note Each message starts with a message_begin() or message_begin_f() function
 *       and ends with message_end(). The specific message arguments go in between
 *       these two by using the write_*() functions found in messages.inc.
 *
 * @param dest        Destination type (see MSG_* constants in messages_const.inc)
 * @param msg_type    Message id
 * @param origin      Message origin
 * @param player      Client index receiving the message or 0 for all clients
 *
 * @noreturn
 * @error             If an invalid message id is specified or an invalid number
 *                    of parameters is passed, an error will be thrown.
 */
native message_begin_f(dest, msg_type, const Float:origin[3] = {0.0,0.0,0.0}, player = 0);

/**
 * Ends a message that was started with message_begin() or message_begin_f().
 *
 * @note If the function is called without using message_begin() or
 *       message_begin_f() first, the server will crash immediately.
 *
 * @noreturn
 */
native message_end();

/**
 * Writes a single byte to a message.
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Byte to write
 *
 * @noreturn
 */
native write_byte(x);

/**
 * Writes a single character to a message.
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Character to write
 *
 * @noreturn
 */
native write_char(x);

/**
 * Writes a single number to a message (short).
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Number to write
 *
 * @noreturn
 */
native write_short(x);

/**
 * Writes a single number to a message (long).
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Number to write
 *
 * @noreturn
 */
native write_long(x);

/**
 * Writes an entity index to a message.
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Entity index to write
 *
 * @noreturn
 */
native write_entity(x);

/**
 * Writes an angle entry to a message.
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Angle to write
 *
 * @noreturn
 */
native write_angle(x);

/**
 * Writes an angle entry to a message using a float value.
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Angle to write
 *
 * @noreturn
 */
native write_angle_f(Float:x);

/**
 * Writes a coordinate entry to a message.
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Coordinate to write
 *
 * @noreturn
 */
native write_coord(x);

/**
 * Writes a coordinate entry to a message using a float value.
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Coordinate to write
 *
 * @noreturn
 */
native write_coord_f(Float:x);

/**
 * Writes a string to a message.
 *
 * @note This function should only be used in between a message_begin()
 *       or message_begin_f() and a message_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           String to write
 *
 * @noreturn
 */
native write_string(const x[]);

/**
 * Marks the beginning of a client message.
 *
 * @note You may generate menus, smoke, shockwaves, thunderlights,
 *       intermission and many other messages.
 * @note For a list of HL game events, visit https://wiki.alliedmods.net/Half-Life_1_Game_Events
 * @note For a list of HL engine messages, visit https://wiki.alliedmods.net/Half-Life_1_Engine_Messages
 * @note You may also refer to the messages_const.inc file for examples.
 * @note This function is the same as message_begin(), except that the messages
 *       sent with this one are also sent to all other AMXX and Metamod plugins.
 *       This means that if you send one of these messages, other plugins will
 *       be notified of that message, which was previously impossible.
 * @note BE CAREFUL! Using this incorrectly, or not for its intended purpose,
 *       could cause infinite recursion or something just as bad!
 * @note Each message starts with a emessage_begin() or emessage_begin_f() function
 *       and ends with emessage_end(). The specific message arguments go in between
 *       these two by using the ewrite_*() functions found in messages.inc.
 *
 * @param dest        Destination type (see MSG_* constants in messages_const.inc)
 * @param msg_type    Message id
 * @param origin      Message origin
 * @param player      Client index receiving the message or 0 for all clients
 *
 * @noreturn
 * @error             If an invalid message id is specified or an invalid number
 *                    of parameters is passed, an error will be thrown.
 */
native emessage_begin(dest, msg_type, const origin[3] = {0,0,0}, player = 0);

/**
 * Marks the beginning of a client message.
 *
 * @note You may generate menus, smoke, shockwaves, thunderlights,
 *       intermission and many other messages.
 * @note For a list of HL game events, visit https://wiki.alliedmods.net/Half-Life_1_Game_Events
 * @note For a list of HL engine messages, visit https://wiki.alliedmods.net/Half-Life_1_Engine_Messages
 * @note You may also refer to the messages_const.inc file for examples.
 * @note This function is the same as message_begin_f(), except that the messages
 *       sent with this one are also sent to all other AMXX and Metamod plugins.
 *       This means that if you send one of these messages, other plugins will
 *       be notified of that message, which was previously impossible.
 * @note BE CAREFUL! Using this incorrectly, or not for its intended purpose,
 *       could cause infinite recursion or something just as bad!
 * @note This function is the same as emessage_begin(), but the origin
 *       argument accepts only float values in this one.
 * @note Each message starts with a emessage_begin() or emessage_begin_f() function
 *       and ends with emessage_end(). The specific message arguments go in between
 *       these two by using the ewrite_*() functions found in messages.inc.
 *
 * @param dest        Destination type (see MSG_* constants in messages_const.inc)
 * @param msg_type    Message id
 * @param origin      Message origin
 * @param player      Client index receiving the message or 0 for all clients
 *
 * @noreturn
 * @error             If an invalid message id is specified or an invalid number
 *                    of parameters is passed, an error will be thrown.
 */
native emessage_begin_f(dest, msg_type, const Float:origin[3] = {0.0,0.0,0.0}, player = 0);

/**
 * Ends a message that was started with emessage_begin() or emessage_begin_f().
 *
 * @note If the function is called without using emessage_begin() or
 *       emessage_begin_f() first, the server will crash immediately.
 *
 * @noreturn
 */
native emessage_end();

/**
 * Writes a single byte to a message.
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Byte to write
 *
 * @noreturn
 */
native ewrite_byte(x);

/**
 * Writes a single character to a message.
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Character to write
 *
 * @noreturn
 */
native ewrite_char(x);

/**
 * Writes a single number to a message (short).
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Number to write
 *
 * @noreturn
 */
native ewrite_short(x);

/**
 * Writes a single number to a message (long).
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Number to write
 *
 * @noreturn
 */
native ewrite_long(x);

/**
 * Writes an entity index to a message.
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Entity index to write
 *
 * @noreturn
 */
native ewrite_entity(x);

/**
 * Writes an angle entry to a message.
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Angle to write
 *
 * @noreturn
 */
native ewrite_angle(x);

/**
 * Writes an angle entry to a message using a float value.
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Angle to write
 *
 * @noreturn
 */
native ewrite_angle_f(Float:x);

/**
 * Writes a coordinate entry to a message.
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Coordinate to write
 *
 * @noreturn
 */
native ewrite_coord(x);

/**
 * Writes a coordinate entry to a message using a float value.
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           Coordinate to write
 *
 * @noreturn
 */
native ewrite_coord_f(Float:x);

/**
 * Writes a string to a message.
 *
 * @note This function should only be used in between a emessage_begin()
 *       or emessage_begin_f() and a emessage_end() function. Trying to use
 *       it outside of these functions will crash the server immediately.
 *
 * @param x           String to write
 *
 * @noreturn
 */
native ewrite_string(const x[]);

/**
 * Sets whether or not an engine message will be blocked.
 *
 * @note For a list of message flags, have a look at the BLOCK_* constants
 *       in message_const.inc.
 *
 * @param iMessage       Message id
 * @param iMessageFlags  BLOCK_* constant
 *
 * @noreturn
 * @error                If an invalid message id is specified, an error
 *                       will be thrown.
 */
native set_msg_block(iMessage, iMessageFlags);

/**
 * Gets whether or not an engine message is blocked.
 *
 * @param iMessage       Message id
 *
 * @return               BLOCK_* constant
 * @error                If an invalid message id is specified, an error
 *                       will be thrown.
 */
native get_msg_block(iMessage);

/**
 * Lets you directly hook a message in the engine.
 *
 * @note The function is called in the following manner:
 *   msg_id          - Message id
 *   msg_dest        - Destination type (see MSG_* constants in messages_const.inc)
 *   msg_entity      - Entity receiving the message
 *
 * @note You can overwrite the message before anything happens by using the
 *       set_msg_arg_* functions and either let the message continue by
 *       returning PLUGIN_CONTINUE or fully block it with PLUGIN_HANDLED.
 * @note If you hook a message, the message is stored but not sent. You have
 *       the opportunity to not only execute code, but to get/set the contents
 *       of the message before you choose to either block it or let it go on
 *       its way.
 * @note The return value can be passed to unregister_message() in order to
 *       stop the message from being hooked.
 *
 * @param iMsgId         Message id
 * @param szFunction     Function that will be called
 *
 * @return               Id that can be passed to unregister_message() on
 *                       success, or 0 if an invalid message id is passed
 * @error                If the specified function can't be found, an
 *                       error will be thrown.
 */
native register_message(iMsgId, const szFunction[]);

/**
 * Unregisters a message hook previously created with register_message().
 *
 * @note You must pass the proper message id and return value from the
 *       message to unregister the message successfully.
 *
 * @param iMsgId         Message id
 * @param registeredmsg  Registered message id
 *
 * @return               Id that can again be passed to register_message() on
 *                       success, or 0 if an invalid message id is passed
 * @error                If an invalid registered message handle is passed, an
 *                       error will be thrown.
 */
native unregister_message(iMsgId, registeredmsg);

/**
 * Gets number of arguments that were passed to a message.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @return               Number of arguments
 */
native get_msg_args();

/**
 * Gets the argument type of a specified argument.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @param argn           Argument number
 *
 * @return               Argument type (see ARG_* constants in message_const.inc)
 */
native get_msg_argtype(argn);

/**
 * Gets the integer value of a specified argument.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @param argn           Argument number
 *
 * @return               Argument value as an integer
 * @error                If an invalid message argument is passed, an
 *                       error will be thrown.
 */
native get_msg_arg_int(argn);

/**
 * Gets the float value of a specified argument.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @param argn           Argument number
 *
 * @return               Argument value as a float
 * @error                If an invalid message argument is passed, an
 *                       error will be thrown.
 */
native Float:get_msg_arg_float(argn);

/**
 * Gets the string value from a specified argument.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @param argn           Argument number
 * @param szReturn       Buffer to store the value in
 * @param iLength        Maximum buffer length
 *
 * @return               String length
 * @error                If an invalid message argument is passed, an
 *                       error will be thrown.
 */
native get_msg_arg_string(argn, szReturn[], iLength);

/**
 * Sets the integer value of a specified argument.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @param argn           Argument number
 * @param argtype        Argument type (see ARG_* constants in message_const.inc)
 * @param iValue         Argument value
 *
 * @noreturn
 * @error                If an invalid message argument is passed, an
 *                       error will be thrown.
 */
native set_msg_arg_int(argn, argtype, iValue);

/**
 * Sets the float value of a specified argument.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @param argn           Argument number
 * @param argtype        Argument type (see ARG_* constants in message_const.inc)
 * @param fValue         Argument value
 *
 * @noreturn
 * @error                If an invalid message argument is passed, an
 *                       error will be thrown.
 */
native set_msg_arg_float(argn, argtype, Float:fValue);

/**
 * Sets the string value of a specified argument.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @param argn           Argument number
 * @param szString       Argument value
 *
 * @noreturn
 * @error                If an invalid message argument is passed, an
 *                       error will be thrown.
 */
native set_msg_arg_string(argn, const szString[]);

/**
 * Gets the origin of a message.
 *
 * @note This function will fail if used outside a hooked message scope, thus
 *       it should never be used unless inside a registered message function.
 *
 * @param _Origin        Array to store the origin in
 *
 * @noreturn
 * @error                If the function is used outside a message hook, an
 *                       error will be thrown.
 */
native get_msg_origin(const Float:_Origin[3]);
