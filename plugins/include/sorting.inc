// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Sorting Functions
//

//
// All sort functions are based off the qsort() function from the
//  C standard library, which uses the Quick Sort algorithm.
// For more info, see: http://linux.wku.edu/~lamonml/algor/sort/sort.html
//

#if defined _sorting_included
  #endinput
#endif
#define _sorting_included

/**
 * Contains sorting orders.
 */
enum SortMethod
{
	Sort_Ascending = 0,
	Sort_Descending,
	Sort_Random,
};

/**
 * Data types for ADT Array Sorts
 */
enum SortType
{
	Sort_Integer = 0,
	Sort_Float,
 	Sort_String,
};
/**
 * Basic sorting functions below.
 */

native SortIntegers(array[], array_size, SortMethod:order = Sort_Ascending);

native SortFloats(Float:array[], array_size, SortMethod:order = Sort_Ascending);

native SortStrings(array[][], num_strings, SortMethod:order = Sort_Ascending);

/**
 * Custom sorting functions below.
 */

/** 
 * Sorts a custom 1D array.  You must pass in a comparison function.
 * The sorting algorithm then uses your comparison function to sort the data.
 * The function is called in the following manner:
 * 
 * public MySortFunc(elem1, elem2, const array[], const data[], data_size)
 *
 *  elem1, elem2	- Current element pair being compared
 *  array[]			- Array in its current mid-sorted state.
 *  data[]			- Extra data array you passed to the sort func.
 *  data_size		- Size of extra data you passed to the sort func.
 *
 * Your function should return:
 *  -1 if elem1 should go before elem2
 *   0 if elem1 and elem2 are equal
 *   1 if elem1 should go after elem2
 * Note that the parameters after elem2 are all optional and you do not need to specify them.
 */
native SortCustom1D(array[], array_size, const comparefunc[], data[]="", data_size=0);


/** 
 * Sorts a custom 2D array.
 * The sorting algorithm then uses your comparison function to sort the data.
 * The function is called in the following manner:
 *
 * public MySortFunc(const elem1[], const elem2[], const array[], data[], data_size)
 *
 *  elem1[], elem2[] - Current element array pairs being compared
 *  array[][]		 - Array in its currently being sorted state.
 *  data[]			 - Extra data array you passed to the sort func.
 *  data_size		 - Size of extra data you passed to the sort func.
 *
 * Your function should return:
 *  -1 if elem1[] should go before elem2[]
 *   0 if elem1[] and elem2 are equal[]
 *   1 if elem1[] should go after elem2[]
 * Note that the parameters after elem2[] are all optional and you do not need to specify them.
 */
native SortCustom2D(array[][], array_size, const comparefunc[], data[]="", data_size=0);

/**
 * Sort an ADT Array. Specify the type as Integer, Float, or String.
 *
 * @param array			Array Handle to sort
 * @param order			Sort order to use, same as other sorts.
 * @param type			Data type stored in the ADT Array
 * @noreturn
 */
native SortADTArray(Array:array, SortMethod:order, SortType:type);
