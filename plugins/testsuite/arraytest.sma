#include <amxmodx>


new __testnumber;
new errcount;
new __testfunc[32];
new __testfuncnum;

enum TestType
{
	TT_Equal = 0,
	TT_LessThan,
	TT_GreaterThan,
	TT_LessThanEqual,
	TT_GreaterThanEqual,
	TT_NotEqual
};

new TestWords[6][] = {
 "==",
 "<",
 ">",
 "<=",
 ">=",
 "!="
};



stock test(A,B=0,TestType:Type=TT_Equal)
{
	++__testnumber;
	
	new passed=0;
	
	switch (Type)
	{
		case TT_Equal: if (A==B) passed=1;
		case TT_LessThan: if (A<B) passed=1;
		case TT_GreaterThan: if (A>B) passed=1;
		case TT_LessThanEqual: if (A<=B) passed=1;
		case TT_GreaterThanEqual: if (A>=B) passed=1;
		case TT_NotEqual: if (A!=B) passed=1;
	}
	
	if (!passed)
	{
		log_amx("Failed test #%d (%d %s %d)",__testnumber,A,TestWords[_:Type],B);
		errcount++;
	}
}
stock starttests(const startfunc[])
{
	__testnumber=0;
	errcount=0;
	__testfuncnum=1;
	server_print("Starting tests...");
	formatex(__testfunc,sizeof(__testfunc)-1,"%s",startfunc);

	new func[32];
	formatex(func,sizeof(func)-1,"%s%d",__testfunc,__testfuncnum++);
	set_task(0.1,func);
}

stock showres()
{
	if (errcount==0)
	{
		new func[32];
		formatex(func,sizeof(func)-1,"%s%d",__testfunc,__testfuncnum++);
		if (get_func_id(func)==-1)
		{
			server_print("All tests ok!");
		}
		else
		{
			server_print("Test ok, moving on...");
			
			set_task(0.1,func);
		}
	}
	else
	{
		server_print("Test failed, aborting.");
	}
}



public plugin_init()
{
	register_srvcmd("arraytest","arraytest");
}

public arraytest()
{
	starttests("arraytest");
}
public arraytest1()
{
	server_print("Testing 1000 iterations of 1-cell arrays...");
	
	new Float:f;
	new Array:a=ArrayCreate(1);
	if (a == Invalid_Array)
	{
	}
	for (new i=0; i<1000; i++)
	{
		f=float(i);
		ArrayPushCell(a,f);
	}
	new Float:r;
	for (new i=0; i<1000; i++)
	{
		f=float(i);
		r=Float:ArrayGetCell(a, i);
		
		// This is normally bad for float "casting", but in this case it should be fine.
		test(_:f, _:r);
		
		
		// Reset with inversed values
		new g=_:f;
		g=~g;
		
		ArraySetCell(a, i, g);
		
		r=Float:ArrayGetCell(a,i);
		
		test(g, _:r);
		
	}
	
	ArrayDestroy(a);
	
	showres();
}
stock bool:checkarray(const a[], const b[], size)
{
	while (size--)
	{
		if (a[size]!=b[size])
		{
			return false;
		}
	}
	
	return true;
}
stock invarray(a[],size)
{
	while (size--)
	{
		a[size] = ~a[size];
	}
	
}
public arraytest2()
{
	server_print("Testing 1000 iterations of 40-cell arrays...");
	
	new Array:a=ArrayCreate(40);
	new buff[40];
	new buffb[40];
	for (new i=0; i<1000; i++)
	{
		arrayset(buff,i,sizeof(buff));
		
		ArrayPushArray(a, buff);
	}
	for (new i=0; i<1000; i++)
	{
		arrayset(buff, i, sizeof(buff));
		
		ArrayGetArray(a, i, buffb);
		
		test(_:checkarray(buff,buffb,sizeof(buff)),1);
		
		// Now overwrite the array with inversed value
		invarray(buff,sizeof(buff));
		
		ArraySetArray(a, i, buff);
		
		ArrayGetArray(a, i, buffb);
		
		test(_:checkarray(buff,buffb,sizeof(buff)),1);
	}
	
	ArrayDestroy(a);
	
	showres();
}
public arraytest3()
{
	server_print("Testing 1000 iterations of strings...");
	
	// The string is 10 long, the string we're trying to pass is 20 long.
	
	new Array:a=ArrayCreate(10);
	
	new buff[20]="1234567890abcdefghi";
	new buffb[20];
	
	for (new i=0; i<1000; i++)
	{
		ArrayPushString(a, buff);
	}
	for (new i=0; i<1000; i++)
	{
		ArrayGetString(a, i, buffb, sizeof(buffb)-1);
		
		test(strcmp(buffb,"123456789"),0);
		
		ArraySetString(a, i, "9876543210");
		
		ArrayGetString(a, i, buffb, sizeof(buffb)-1);
		
		test(strcmp(buffb,"987654321"),0);
		
		buffb[0]=0;
		
		formatex(buffb,sizeof(buffb)-1,"%S", ArrayGetStringHandle(a, i));
		
		test(strcmp(buffb, "987654321"),0);
	}
	
	ArrayDestroy(a);
	
	showres();
}

public sortcallback(Array:a, b, c)
{
	static stra[40];
	static strb[40];
	
	ArrayGetString(a, b, stra, sizeof(stra)-1);
	ArrayGetString(a, c, strb, sizeof(strb)-1);
	return strcmp(stra,strb);
}
public arraytest4()
{
	server_print("Testing sorting function...");
	
	new Array:a=ArrayCreate(40);
	
	ArrayPushString(a, "z");
	ArrayPushString(a, "yz");
	ArrayPushString(a, "xyz");
	ArrayPushString(a, "wxyz");
	ArrayPushString(a, "vwxyz");
	ArrayPushString(a, "uvwxyz");
	ArrayPushString(a, "tuvwxyz");
	ArrayPushString(a, "stuvwxyz");
	ArrayPushString(a, "rstuvwxyz");
	ArrayPushString(a, "qrstuvwxyz");
	ArrayPushString(a, "pqrstuvwxyz");
	ArrayPushString(a, "opqrstuvwxyz");
	ArrayPushString(a, "nopqrstuvwxyz");
	ArrayPushString(a, "mnopqrstuvwxyz");
	ArrayPushString(a, "lmnopqrstuvwxyz");
	ArrayPushString(a, "klmnopqrstuvwxyz");
	ArrayPushString(a, "jklmnopqrstuvwxyz");
	ArrayPushString(a, "ijklmnopqrstuvwxyz");
	ArrayPushString(a, "hijklmnopqrstuvwxyz");
	ArrayPushString(a, "ghijklmnopqrstuvwxyz");
	ArrayPushString(a, "fghijklmnopqrstuvwxyz");
	ArrayPushString(a, "efghijklmnopqrstuvwxyz");
	ArrayPushString(a, "defghijklmnopqrstuvwxyz");
	ArrayPushString(a, "cdefghijklmnopqrstuvwxyz");
	ArrayPushString(a, "bcdefghijklmnopqrstuvwxyz");
	ArrayPushString(a, "abcdefghijklmnopqrstuvwxyz");
	
	new OldSize=ArraySize(a);
	
	ArraySort(a, "sortcallback");
	
	test(ArraySize(a),OldSize);
	
	new buff[40];
	
	ArrayGetString(a,0,buff,sizeof(buff)-1);

	test(strcmp(buff,"abcdefghijklmnopqrstuvwxyz"),0);
	
	ArrayGetString(a,25,buff,sizeof(buff)-1);
	
	test(strcmp(buff,"z"),0);

	
	new start='a';
	
	for (new i=0;i<OldSize;i++)
	{
		ArrayGetString(a,i,buff,sizeof(buff)-1)
		
		test(buff[0],start++);
	}
	
	showres();
}
public arraytest5()
{
	server_print("Testing ArrayDeleteItem()...");
	new Array:a=ArrayCreate(1);
	
	new v;
	
	for (new i=0; i<1000; i++)
	{
		ArrayPushCell(a, i);
	}
	for (new i=ArraySize(a) - 1; i>=0 ; i--)
	{
		if (i % 2 == 0)
		{
			ArrayDeleteItem(a, i);
		}
	}
	test(ArraySize(a), 500);
	for (new i=0; i< 500; i++)
	{
		v=ArrayGetCell(a, i);
		
		// All items should be incrementing odd numbers
		test(((i + 1) * 2) - 1, v);

		// All remaining entries should be odd
		test((v & 1), 1);
	}
	ArrayDestroy(a);
	
	a=ArrayCreate(1);
	// Repeat the same test, but check even numbers
	for (new i=0; i<1000; i++)
	{
		ArrayPushCell(a, i);
	}
	for (new i=ArraySize(a) - 1; i>=0 ; i--)
	{
		if (i % 2 == 1)
		{
			ArrayDeleteItem(a, i);
		}
	}
	test(ArraySize(a), 500);
	for (new i=0; i< 500; i++)
	{
		v=ArrayGetCell(a, i);
		
		// All items should be incrementing even numbers
		test(((i + 1) * 2) - 2, v);

		// All remaining entries should be even
		test((v & 1), 0);
	}
	ArrayDestroy(a);
	
	showres();
}
public arraytest6()
{
	server_print("Testing ArrayInsertCellAfter()...");
	
	new Array:a=ArrayCreate(1);
	
	for (new i=0; i<10;i++)
	{
		ArrayPushCell(a, i);
		new item=ArraySize(a)-1;
		for (new j=0; j<10; j++)
		{
			ArrayInsertCellAfter(a, item + j, j);
		}
	}
	
	test(ArraySize(a), 110);
	
	new v;
	for (new i=0; i<110; i++)
	{
		v=ArrayGetCell(a, i);
		
		test(v, i / 10);
		for (new j=0; j<10; j++)
		{
			v=ArrayGetCell(a, ++i);
			test(v, j);
		}
	}
	
	
	ArrayDestroy(a);
	
	showres();
}
public arraytest7()
{
	server_print("Testing ArrayInsertCellBefore()...");
	
	new Array:a=ArrayCreate(1);
	
	for (new i=0; i<10;i++)
	{
		ArrayPushCell(a, i);
		new item=ArraySize(a)-1;
		for (new j=0; j<10; j++)
		{
			ArrayInsertCellBefore(a, item, j);
		}
	}
	
	test(ArraySize(a), 110);
	
	new v;
	for (new i=0; i<110; i++)
	{
		for (new j=9; j>=0; j--)
		{
			v=ArrayGetCell(a, i++);
			test(v, j);
		}
		
		v=ArrayGetCell(a, i);
		
		test(v, (i - 10) / 10);
	}
	
	
	ArrayDestroy(a);
	
	showres();
}
public arraytest8()
{
	server_print("Testing ArraySwap()...");
	new Array:a=ArrayCreate(1);
	
	for (new i=0; i<10; i++)
	{
		ArrayPushCell(a, i);
	}
	for (new i=0; i<5; i++)
	{
		ArraySwap(a, i, (10 - (i + 1)));
	}
	new v;
	for (new i=0; i<5; i++)
	{
		v=ArrayGetCell(a, i);
		
		test(v, (10 - (i + 1)));
	}
	
	ArrayDestroy(a);
	
	showres();
}
