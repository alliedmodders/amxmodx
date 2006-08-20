#include <amxmodx>

public plugin_init()
{
	register_plugin("Sort Test", "1.0", "BAILOPAN")
	
	register_srvcmd("test_sort_ints", "Command_TestSortInts")
	register_srvcmd("test_sort_floats", "Command_TestSortFloats")
	register_srvcmd("test_sort_strings", "Command_TestSortStrings")
	register_srvcmd("test_sort_1d", "Command_TestSort1D")
	register_srvcmd("test_sort_2d", "Command_TestSort2D")
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
