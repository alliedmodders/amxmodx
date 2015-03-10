// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

public plugin_init()
{
	register_plugin("Sort Test", "1.0", "BAILOPAN")
	
	register_srvcmd("test_sort_ints", "Command_TestSortInts")
	register_srvcmd("test_sort_floats", "Command_TestSortFloats")
	register_srvcmd("test_sort_strings", "Command_TestSortStrings")
	register_srvcmd("test_sort_1d", "Command_TestSort1D")
	register_srvcmd("test_sort_2d", "Command_TestSort2D")
	register_srvcmd("test_adtsort_ints", "Command_TestSortADTInts")
	register_srvcmd("test_adtsort_floats", "Command_TestSortADTFloats")
	register_srvcmd("test_adtsort_strings", "Command_TestSortADTStrings")
}

/*****************
 * INTEGER TESTS *
 *****************/
// Note that integer comparison is just int1-int2 (or a variation therein)

PrintIntegers(const array[], size)
{
	for (new i=0; i<size; i++)
	{
		server_print("array[%d] = %d", i, array[i])
	}
}

public Command_TestSortInts()
{
	new array[10] = {6, 7, 3, 2, 8, 5, 0, 1, 4, 9}
	
	server_print("Testing ascending sort:")
	SortIntegers(array, 10, Sort_Ascending)
	PrintIntegers(array, 10)
	
	server_print("Testing descending sort:")
	SortIntegers(array, 10, Sort_Descending)
	PrintIntegers(array, 10)
	
	server_print("Testing random sort:")
	SortIntegers(array, 10, Sort_Random)
	PrintIntegers(array, 10)
}

/**************************
 * Float comparison tests *
 **************************/

PrintFloats(const Float:array[], size)
{
	for (new i=0; i<size; i++)
	{
		server_print("array[%d] = %f", i, array[i])
	}
}

public Command_TestSortFloats()
{
	new Float:array[10] = {6.3, 7.6, 3.2, 2.1, 8.5, 5.2, 0.4, 1.7, 4.8, 8.2}
	
	server_print("Testing ascending sort:")
	SortFloats(array, 10, Sort_Ascending)
	PrintFloats(array, 10)
	
	server_print("Testing descending sort:")
	SortFloats(array, 10, Sort_Descending)
	PrintFloats(array, 10)
	
	server_print("Testing random sort:")
	SortFloats(array, 10, Sort_Random)
	PrintFloats(array, 10)
	
	return PLUGIN_HANDLED
}

public Custom1DSort(Float:elem1, Float:elem2)
{
	if (elem1 > elem2)
	{
		return -1;
	} else if (elem1 < elem2) {
		return 1;
	}
	
	return 0;
}

public Command_TestSort1D()
{
	new Float:array[10] = {6.3, 7.6, 3.2, 2.1, 8.5, 5.2, 0.4, 1.7, 4.8, 8.2}
	
	SortCustom1D(_:array, 10, "Custom1DSort")
	PrintFloats(array, 10)
	
	return PLUGIN_HANDLED
}

/***************************
 * String comparison tests *
 ***************************/

PrintStrings(const array[][], size)
{
	for (new i=0; i<size; i++)
	{
		server_print("array[%d] = %s", i, array[i])
	}
}

public Command_TestSortStrings()
{
	new array[][] = 
		{
			"faluco",
			"bailopan",
			"pm onoto",
			"damaged soul",
			"sniperbeamer",
			"sidluke",
			"johnny got his gun",
			"gabe newell",
			"hello",
			"WHAT?!"
		}
		
	server_print("Testing ascending sort:")
	SortStrings(array, 10, Sort_Ascending)
	PrintStrings(array, 10)
	
	server_print("Testing descending sort:")
	SortStrings(array, 10, Sort_Descending)
	PrintStrings(array, 10)
	
	server_print("Testing random sort:")
	SortStrings(array, 10, Sort_Random)
	PrintStrings(array, 10)
	
	return PLUGIN_HANDLED
}

public Custom2DSort(const elem1[], const elem2[])
{
	return strcmp(elem1, elem2)
}

public Command_TestSort2D()
{
	new array[][] = 
		{
			"faluco",
			"bailopan",
			"pm onoto",
			"damaged soul",
			"sniperbeamer",
			"sidluke",
			"johnny got his gun",
			"gabe newell",
			"hello",
			"WHAT?!"
		}
	
	SortCustom2D(array, 10, "Custom2DSort")
	PrintStrings(array, 10)
	
	return PLUGIN_HANDLED
}


/*******************
 * ADT ARRAY TESTS *
 *******************/
// Int and floats work the same as normal comparisions. Strings are direct
// comparisions with no hacky memory stuff like Pawn arrays.

PrintADTArrayIntegers(Array:array)
{
	new size = ArraySize(array);
	for (new i=0; i<size;i++)
	{
		server_print("array[%d] = %d", i, ArrayGetCell(array, i));	
	}
}

public Command_TestSortADTInts()
{
	new Array:array = ArrayCreate();
	ArrayPushCell(array, 6);
	ArrayPushCell(array, 7);
	ArrayPushCell(array, 3);
	ArrayPushCell(array, 2);
	ArrayPushCell(array, 8);
	ArrayPushCell(array, 5);
	ArrayPushCell(array, 0);
	ArrayPushCell(array, 1);
	ArrayPushCell(array, 4);
	ArrayPushCell(array, 9);
	
	server_print("Testing ascending sort:")
	SortADTArray(array, Sort_Ascending, Sort_Integer)
	PrintADTArrayIntegers(array)
	
	server_print("Testing descending sort:")
	SortADTArray(array, Sort_Descending, Sort_Integer)
	PrintADTArrayIntegers(array)
	
	server_print("Testing random sort:")
	SortADTArray(array, Sort_Random, Sort_Integer)
	PrintADTArrayIntegers(array)
	
	return PLUGIN_HANDLED
}

PrintADTArrayFloats(Array:array)
{
	new size = ArraySize(array);
	for (new i=0; i<size;i++)
	{
		server_print("array[%d] = %f", i, Float:ArrayGetCell(array, i));	
	}
}

public Command_TestSortADTFloats()
{
	new Array:array = ArrayCreate();
	ArrayPushCell(array, 6.0);
	ArrayPushCell(array, 7.0);
	ArrayPushCell(array, 3.0);
	ArrayPushCell(array, 2.0);
	ArrayPushCell(array, 8.0);
	ArrayPushCell(array, 5.0);
	ArrayPushCell(array, 0.0);
	ArrayPushCell(array, 1.0);
	ArrayPushCell(array, 4.0);
	ArrayPushCell(array, 9.0);
	
	server_print("Testing ascending sort:")
	SortADTArray(array, Sort_Ascending, Sort_Float)
	PrintADTArrayFloats(array)
	
	server_print("Testing descending sort:")
	SortADTArray(array, Sort_Descending, Sort_Float)
	PrintADTArrayFloats(array)
	
	server_print("Testing random sort:")
	SortADTArray(array, Sort_Random, Sort_Float)
	PrintADTArrayFloats(array)
	
	return PLUGIN_HANDLED
}

PrintADTArrayStrings(Array:array)
{
	new size = ArraySize(array);
	new buffer[64];
	for (new i=0; i<size;i++)
	{
		ArrayGetString(array, i, buffer, sizeof(buffer));
		server_print("array[%d] = %s", i, buffer);	
	}
}

public Command_TestSortADTStrings()
{
	new Array:array = ArrayCreate(64);
	ArrayPushString(array, "faluco");
	ArrayPushString(array, "bailopan");
	ArrayPushString(array, "pm onoto");
	ArrayPushString(array, "damaged soul");
	ArrayPushString(array, "sniperbeamer");
	ArrayPushString(array, "sidluke");
	ArrayPushString(array, "johnny got his gun");
	ArrayPushString(array, "gabe newell");
	ArrayPushString(array, "Hello pRED*");
	ArrayPushString(array, "WHAT?!");
	
	server_print("Testing ascending sort:")
	SortADTArray(array, Sort_Ascending, Sort_String)
	PrintADTArrayStrings(array)
	
	server_print("Testing descending sort:")
	SortADTArray(array, Sort_Descending, Sort_String)
	PrintADTArrayStrings(array)
	
	server_print("Testing random sort:")
	SortADTArray(array, Sort_Random, Sort_String)
	PrintADTArrayStrings(array)
	
	return PLUGIN_HANDLED
}
