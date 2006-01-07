/****************************************************************************
*									element.h
* This class acts as sort of an interface for storing values in the judy 
* arrays. By storing an 'element' with error handling and type checking,
* crashes and errors are less likely. In addition, by storing the type of
* data it is also possible to save or load from a file.
****************************************************************************/

#if !defined(_CLASSDEF_ELEMENT_)
#define _CLASSDEF_ELEMENT_

const Vector* null_vec = new Vector();

enum {
	elem_type_none, //only used on init.
	elem_type_int,
	elem_type_real,
	elem_type_char,
	elem_type_vector
};

const char* elem_types[] =
{
	"-NO VALUE-",
	"INTEGER",
	"FLOAT",
	"STRING",
	"VECTOR"
};

class element
{
public:
	void set_int(int Value);
	void set_flo(REAL Value);
	void set_str(char* Value);
	void set_vec(Vector* Value);

	element	(int		Value)	{ set_int(Value); }
	element (REAL		Value)	{ set_flo(Value); }
	element (char*		Value)	{ set_str(Value); }
	element (Vector*	Value)	{ set_vec(Value); }
	element(void);

	REAL			get_flo(int &error);
	int				get_int(int &error);
	const char*		get_str(int &error);
	const Vector*	get_vec(int &error);

	int get_type(void);
	Pvoid_t get_ptr(void);

	virtual ~element();

	void delete_element(void) { clear(); element::~element(); }

	char* get_elem_as_string(void);

	//This is handy because it takes all the data needed and issues the error.
	void issue_type_error(AMX *amx, int Keytable, char* Index);
	void issue_type_error(AMX *amx, int Array, int Index);
private:
	Pvoid_t	element_ptr; //Contains a pointer to whatever data is actually stored.
	void clear(void);
	char element_type;
};

Pvoid_t element::get_ptr(void)
{
	return element_ptr;
}

void element::issue_type_error(AMX *amx, int Keytable, char* Index)
{
	MF_LogError(amx, AMX_ERR_NATIVE, 
		"Function attempted to read NON-%s value at key \"%s\" in keytable %d, actual value: %s",
		elem_types[element_type], Index, Keytable, get_elem_as_string()); 
}

void element::issue_type_error(AMX *amx, int Array, int Index)
{
	MF_LogError(amx, AMX_ERR_NATIVE, 
		"Function attempted to read NON-%s value at index %d in array %d, actual value: %s",
		elem_types[element_type], Index, Array, get_elem_as_string()); 
}

char* element::get_elem_as_string(void)
{
	char* value = "";
	Vector vector_val;
	int error; //Is not checked.
	switch (element_type)
	{
		case elem_type_int:
			sprintf(value, "%d", get_int(error));
			break;
		case elem_type_real:
			sprintf(value, "%f", get_int(error));
			break;
		case elem_type_char:
			sprintf(value, "\"%s\"", get_str(error));
			break;
		case elem_type_vector:
			vector_val = *get_vec(error);
			sprintf(value, "{%f,%f,%f}", vector_val.x, vector_val.y, vector_val.z);
			break;
		default:
			sprintf(value, "-NO VALUE-");
	}
	return value;
}

REAL element::get_flo(int &error)
{
	if (element_type == elem_type_real)
		return *reinterpret_cast<REAL*>(element_ptr);
	error = 1;
	return 0;
}

int element::get_int(int &error)
{
	if (element_type == elem_type_int)
		return reinterpret_cast<int>(element_ptr);
	error = 1;
	return 0;
}

const char* element::get_str(int &error)
{
	if (element_type == elem_type_char)
		return reinterpret_cast<const char*>(element_ptr);
	error = 1;
	return "";
}

const Vector* element::get_vec(int &error)
{
	if (element_type == elem_type_vector)
		return reinterpret_cast<const Vector*>(element_ptr);
	error = 1;
	return null_vec;
}

int element::get_type(void)
{
	return element_type;
}

element::element() { }
void element::set_int(int Value)
{
	clear();
	element_type = elem_type_int;
	element_ptr = reinterpret_cast<void*>(Value);
	//Don't need to make it a pointer to an int here.
}
void element::set_flo(REAL Value)
{
	clear();
	element_type = elem_type_real;
	element_ptr = new REAL(Value);
}
void element::set_str(char* Value)
{
	clear();
	element_type = elem_type_char;
	char *string_val = new char[strlen(Value)+1];
	strcpy(string_val,Value);
	element_ptr = reinterpret_cast<void*>(string_val);
}
void element::set_vec(Vector* Value)
{
	clear();
	element_type = elem_type_vector;
	element_ptr = reinterpret_cast<void*>(Value);
}

element::~element()
{
	//do nothing here or else data WILL be lost.
}

void element::clear()
{ 
	//This function intelligently creates a pointer x,
	//which will be of correct type and then deletes it.
	
	if (element_type == elem_type_real)
	{
		REAL *real_val = reinterpret_cast<REAL*>(element_ptr);
		delete real_val;
		//This is actually a pointer to the float/double.
	}
	else if (element_type == elem_type_char)
	{
		char *char_val = reinterpret_cast<char*>(element_ptr);
		delete char_val;
		//Again, cast a pointer.
	}
	else if (element_type == elem_type_vector)
	{
		Vector *vector_val = reinterpret_cast<Vector*>(element_ptr);
		delete vector_val;
		//And again.
	}
	element_ptr = NULL; //Null the address as well. (Used for ints too.)
}

#endif // !defined(_CLASSDEF_ELEMENT_)
