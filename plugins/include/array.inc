/* (C)2004 David "BAILOPAN" Anderson
   Licensed under the GNU General Public License verion 2
   array.inc - Used for advanced array techniques
   */

/* This module uses some constructs capable of being memory intensive.
 * If you are making temporary keytables/lists, make sure you clear them!
 * For example:
 * new a = new_keytable()
 * //(code)
 * keytable_clear(a)
 * new b = new_floatlist()
 * //code
 * floatlist_delete(b)
 */

/* IMPORTANT NOTE! Bad Errors will return -1, Most errors return 0
 * HOWEVER, the first key/list you create will be zero, so do not do this!
 *   new a = new keytable()
 *   if (a) { ....
 * This will not work since the first keytable (and lists) are always id=0.
 * Also, errors on strings will fill the string with "dne" and/or return <=0
 */


/* Key Table functions
 * key tables are variables that hold a key and value pair, like this:
 * C++: std::map<key,val>; table[key] = value;
 * PHP: $table[$key] = $value;
 * Perl: my(%table);$table{$key}=$value;
 * For example, you can pair the value "ASDF" with the key "TEST_STRING"
 *  and then retrieve ASDF with TEST_STRING, like "vault".
 * (Internally stores as a vector of maps)
 */

//Returns a reference to a new keytable
native new_keytable();

//Deletes a keytable and unlinks the reference
native keytable_delete(keytab);


//Stores an integer value into a keytable
native store_int(keytab, key[], value);

//Stores a float value into a keytable
native store_float(keytab, key[], Float:value);

//Stores a string value into a keytable
native store_string(keytab, key[], value[]);


//Retrieves a keytable value as an integer
//Leave the key blank to retrieve the current iterator value
native getkey_int(keytab, key[]);

//Retrieves a keytable value as a float
//Leave the key blank to retrieve the current iterator value
native Float:getkey_float(keytab, key[]);

//Retrieves a keytable value as a string
//Leave the key blank to retrieve the current iterator value
native getkey_string(keytab, key[], ret[], len);


//Resets the internal iteration pointer of a keytable
native keytable_reset(keytab);

//Deletes all keys and values from a keytable
native keytable_clear(keytab);

//Advances the internal iteration pointer of a keytable
native keytable_next(keytab);

//Retrieves the current iteration pointer key of a keytable
native keytable_getkey(keytab, key[], len);

//Retrieves the current iteration pointer value of a keytable
native keytable_getval(keytab, val[], len);

//Retrieves the size of a keytable
native keytable_count(keytab)

/* List functions
 * Use for dynamic array allocation
 * Implemented as a vector of vectors.
 * There are stocks to abstract the type.
 */

enum {
	list_int = 1,
	list_float,
	list_string,
}

//Do not use this native - use the stocks
native new_list(type);

/* Functions to create lists */

//Creates a list of floats
stock new_float_list() {
	new_list(list_float)
}

//Creates a list of ints
stock new_int_list() {
	return new_list(list_int)
}

//Creates a list of strings
stock new_string_list() {
	return new_list(list_string)
}

/* Functions to store data in lists */

//Store a float into a list at member list_posn
native list_store_float(list_id, list_posn, Float:value);

//Push a float to the end of the list
native list_push_float(list_id, Float:value);

//Store an int into a list at member list_posn
native list_store_int(list_id, list_posn, value);

//Push an int to the end of the list
native list_push_int(list_id, value)

//Store a string into a list at member list_posn
native list_store_string(list_id, list_posn, value[])

//Push a string to the end of the list
native list_push_string(list_id, value[])


/* Functions to get data from lists */

//Do not use this native - use the stocks
native list_get(list_id, type, list_posn, retr[]="", retr_len=0);
native Float:list_getf(list_id, type, list_posn=-1)

//Returns the value of a float list at list_posn.  If list_posn is -1, the current 
// iterator position value is returned.
stock Float:list_get_float(list_id, list_posn=-1) {
	return list_getf(list_id, list_float, posn)
}

//Returns the value of an int list at list_posn.  If list_posn is -1, the current 
// iterator position value is returned.
stock list_get_int(list_id, list_posn=-1) {
	return list_get(list_id, list_int, list_posn)
}

//Returns the value of a string list at list_posn.  If list_posn is -1. the current
// iterator position value is returned.
stock list_get_string(list_id, list_posn=-1, retr[], retr_len) {
	return list_get(list_id, list_string, list_posn, retr, retr_len)
}


//Do not use this native - use stocks
native list_delete(list_id, type);

//List delete functions
stock list_delete_float(list_id) {
	return list_delete(list_id, list_float)
}

stock list_delete_int(list_id) {
	return list_delete(list_id, list_int)
}

stock list_delete_string(list_id) {
	return list_delete(list_id, list_string)
}


//Do not use this native - use stocks
native list_clear(list_id, list_type);

//Clear all data from a list
stock list_clear_float(list_id) {
	return list_clear(list_id, list_float)
}

stock list_clear_int(list_id) {
	return list_clear(list_id, list_int)
}

stock list_clear_string(list_id) {
	return list_clear(list_id, list_string)
}


//Do not use this native - use stocks
native list_reset(list_id, list_type);

//Reset the internal iterator pointer for a list
stock list_reset_float(list_id) {
	return list_reset(list_id, list_float)
}

stock list_reset_int(list_id) {
	return list_reset(list_id, list_int)
}

stock list_reset_string(list_id) {
	return list_reset(list_id, list_string)
}


//Do not use this native - use stocks
native list_next(list_id, list_type);

//Advance the internal iterator pointer
stock list_next_float(list_id) {
	return list_next(list_id, list_float)
}

stock list_next_int(list_id) {
	return list_next(list_id, list_int)
}

stock list_next_string(list_id) {
	return list_next(list_id, list_string)
}


//Do not use this native - use stocks
native list_size(list_id, list_type);

//Return the size of a list
stock list_size_float(list_id) {
	return list_size(list_id, list_float)
}

stock list_size_int(list_id) {
	return list_size(list_id, list_int)
}

stock list_size_string(list_id) {
	return list_size(list_id, list_string)
}

//Do not use this native - use stocks
native list_pop(list_id, list_type);

//Pop the last member off the list - returns 0 on failure
stock list_pop_float(list_id) {
	return list_pop(list_id, list_float)
}

stock list_pop_int(list_id) {
	return list_pop(list_id, list_int)
}

stock list_pop_string(list_id) {
	return list_pop(list_id, list_string)
}

//Check if an item is in a list
native in_list_string(list_id, const str[]);
native in_list_float(list_id, Float:num);
native in_list_int(list_id, num);