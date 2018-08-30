/* Core functions
*
* (c) Copyright 1998-2003, ITB CompuPhase
*
* This file is provided as is (no warranties).
*/

#if defined _core_included
	#endinput
#endif
#define _core_included

/**
 * Returns the free memory space available to the plugin.
 *
 * @note This is a debugging function that is not intended for general plugin
 *       use.
 *
 * @return  Free memory space in bytes
 */
native heapspace();

/**
 * Returns the function index of a public function declared in the plugin.
 *
 * @param name  Function name
 *
 * @return      Function index > 0 on success, -1 if function was not found
 * @error       If the function name is too long (longer than 63 characters)
 *              an error will be thrown.
 */
native funcidx(const name[]);

/**
 * Returns the number of arguments passed into the currently executed function.
 *
 * @return  Number of arguments passed
 */
native numargs();

/**
 * Retrieves an argument value passed into the currently executed function.
 *
 * @param arg       Argument index
 * @param index     Index to retrieve from the argument (for arrays and strings)
 *
 * @return          Argument value at given index
 */
native getarg(arg, index = 0);

/**
 * Sets the value of an argument passed into the currently executed function.
 *
 * @note This is not equal to assigning a new value to a by-reference argument.
 *
 * @param arg       Argument index
 * @param index     Index to set in the argument (for arrays and strings)
 */
native setarg(arg, index = 0, value);

/**
 * Converts a character to lowercase.
 *
 * @note This is not UTF8 or locale-safe.
 *
 * @param c     Character to convert
 *
 * @return      Converted character
 */
native tolower(c);

/**
 * Converts a character to uppercase.
 *
 * @note This is not UTF8 or locale-safe.
 *
 * @param c     Character to convert
 *
 * @return      Converted character
 */
native toupper(c);

/**
 * Swaps the bytes of a value (the lowest byte becomes the highest byte).
 *
 * @param c     Value to swap
 *
 * @return      Byte-swapped value
 */
native swapchars(c);

/**
 * Returns a random number between 0 and a specified upper bound.
 *
 * @param max   Exclusive upper bound
 *
 * @return      Random value
 */
native random(max);

/**
 * Returns the smaller of two provided values.
 *
 * @param value1    Value one
 * @param value2    Value two
 *
 * @return          Smaller of the two values
 */
native min(value1, value2);

/**
 * Returns the bigger of two provided values.
 *
 * @param value1    Value one
 * @param value2    Value two
 *
 * @return          Bigger of the two values
 */
native max(value1, value2);

/**
 * Limits a provided value between two specified bounds.
 *
 * @param value     Value to clamp
 * @param min       Lower bound
 * @param max       Upper bound
 *
 * @return          The value if it is between the lower and upper bound, min if
 *                  value is below, max if it is above the specified bounds.
 */
native clamp(value, min = cellmin, max = cellmax);

/**
 * Returns a value raised to a specified exponent.
 *
 * @param value     Value
 * @param exponent  Exponent to raise value to
 *
 * @return          Value to the power of exponent
 */
native power(value, exponent);

/**
 * Returns the approximated square root of a value.
 *
 * @note This uses a simple successice approximation algorithm (continuously
 *       dividing the value) and only deals with integers, this makes it very
 *       imprecise.
 *
 * @param value     Value
 *
 * @return          Square root of the value
 */
native sqroot(value);

/**
 * Retrieves the current time in hours, minutes and seconds.
 *
 * @param hour      Variable to store hours in
 * @param minute    Variable to store minutes in
 * @param second    Variable to store seconds in
 *
 * @return          Unix timestamp
 */
native time(&hour = 0, &minute = 0, &second = 0);

/**
 * Retrieves the current date in year, month and day.
 *
 * @param year      Variable to store year in
 * @param month     Variable to store month in
 * @param day       Variable to store day in
 *
 * @noreturn
 */
native date(&year = 0, &month = 0, &day = 0);

/**
 * Returns the elapsed CPU seconds.
 *
 * @note This is a debugging function that is not intended for general plugin
 *       use.
 * @note This uses the C clock() function internally and comes with all its
 *       drawbacks attached.
 *
 * @param granularity   Unused
 *
 * @return              Number of CPU seconds elapsed
 */
native tickcount(&granularity = 0);

/**
 * Returns the absolute value of a number.
 *
 * @param x     Integral value
 *
 * @return      Absolute value of x (x if it is greater than 0, -x otherwise)
 */
stock abs(x)
{
	return x > 0 ? x : -x;
}
