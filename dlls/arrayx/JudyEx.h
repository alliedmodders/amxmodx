#ifndef _JUDYEX_INCLUDED
#define _JUDYEX_INCLUDED

#include <exception>

class JudyEx
{
private:
	char* ex_message;
	bool fatal;

public:

	JudyEx() { ex_message = NULL; fatal = true;  }
	JudyEx(char* set, bool isfatal) 
	{
		ex_message = new char[strlen(set) + 1]; 
		strcpy(ex_message, set); 

		fatal = isfatal; 
	}

	JudyEx(const JudyEx &copy)
	{
		ex_message = new char[strlen(copy.ex_message) + 1]; 
		strcpy(ex_message, copy.ex_message); 

		fatal = copy.fatal;
	}

	~JudyEx() { delete ex_message; }

	void Destroy() { delete ex_message;  }

	void SetIsFatal(bool isfatal) { fatal = isfatal; }
	bool IsFatal( void ) { return fatal; }

	char* SetErrorMessage(char* set)
	{
		ex_message = new char[strlen(set) + 1]; 
		strcpy(ex_message, set); 
	}

	char* ErrorMessage( void )
	{
		return ex_message;
	}
};

#endif