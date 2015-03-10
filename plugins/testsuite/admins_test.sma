// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

new __testnumber;
new errcount;

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


public plugin_init()
{
	register_srvcmd("testadmins","testadmins");
}
public testadmins()
{

	new AuthData[44];
	new Password[32];
	new Access;
	new Flags;
	new id;
	
	__testnumber=0;
	errcount=0;
	

	test(admins_num(),0);
	
	admins_push("STEAM_0:1:23456","",read_flags("abcdefghijklmnopqrstu"),read_flags("ce"));
	
	test(admins_num(),1);
	
	admins_push("ABCDEFGHIJKLMNOP","abcdefghijklmnop",read_flags("z"),read_flags("a"));
	
	test(admins_num(),2);
	
	admins_push("ZYXWVUTSRQPONMLKJIHGFEDCBA","plop",read_flags("a"),read_flags("b"));
	
	test(admins_num(),3);
	
	id=0;
	
	admins_lookup(id,AdminProp_Auth,AuthData,sizeof(AuthData)-1); 
	admins_lookup(id,AdminProp_Password,Password,sizeof(Password)-1); 
	Access=admins_lookup(id,AdminProp_Access); 
	Flags=admins_lookup(id,AdminProp_Flags);
	
	test(strcmp(AuthData,"STEAM_0:1:23456"),0);
	test(strcmp(Password,""),0);
	test(Access,read_flags("abcdefghijklmnopqrstu"));
	test(Flags,read_flags("ce"));
	
	id++;
	
	admins_lookup(id,AdminProp_Auth,AuthData,sizeof(AuthData)-1); 
	admins_lookup(id,AdminProp_Password,Password,sizeof(Password)-1); 
	Access=admins_lookup(id,AdminProp_Access); 
	Flags=admins_lookup(id,AdminProp_Flags);

	test(strcmp(AuthData,"ABCDEFGHIJKLMNOP"),0);
	test(strcmp(Password,"abcdefghijklmnop"),0);
	test(Access,read_flags("z"));
	test(Flags,read_flags("a"));
	
	id++;
	
	admins_lookup(id,AdminProp_Auth,AuthData,sizeof(AuthData)-1); 
	admins_lookup(id,AdminProp_Password,Password,sizeof(Password)-1); 
	Access=admins_lookup(id,AdminProp_Access); 
	Flags=admins_lookup(id,AdminProp_Flags);
	
	test(strcmp(AuthData,"ZYXWVUTSRQPONMLKJIHGFEDCBA"),0);
	test(strcmp(Password,"plop"),0);
	test(Access,read_flags("a"));
	test(Flags,read_flags("b"));
	
	admins_flush();
	
	test(admins_num(),0);
	
	server_print("test complete, %d errors",errcount);
}
