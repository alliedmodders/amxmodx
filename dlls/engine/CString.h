#ifndef _INCLUDE_CSTRING_H
#define _INCLUDE_CSTRING_H

//by David "BAILOPAN" Anderson
class CString
{
public:
	CString() { v = NULL; mSize = 0; }
	~CString() { if (v) delete [] v; }

	const char *c_str() { return v?v:""; }

	void append(const char *t)
	{
		Grow(strlen(v) + strlen(t));
		strcat(v, t);
	}

	void append(CString &d)
	{
		const char *t = d.c_str();
		Grow(strlen(v) + strlen(t));
		strcat(v, t);
	}

	void assign(const char *d)
	{
		if (!d)
		{
			Grow(1);
			strcpy(v, "");
			return;
		}
		Grow(strlen(d));
		if (v)
			strcpy(v, d);
	}

	void clear()
	{
		if (v)
			delete [] v;
		v = NULL;
		mSize = 0;
	}

	int compare (const char *d)
	{
		if (v) {
			if (d) {
				return strcmp(v, d);
			} else {
				return strlen(v);
			}
		} else {
			if (d) {
				return strlen(d);
			} else {
				return 0;
			}
		}
	}

	int size()
	{
		if (!v)
			return 0;
		return strlen(v);
	}

private:
	void Grow(int d)
	{
		if (d<1)
			return;
		if (d > mSize)
		{
			char *t = new char[d+1];
			if (v) {
				strcpy(t, v);
				t[strlen(v)] = 0;
				delete [] v;
			}
			v = t;
			mSize = d;
		}
	}

	char *v;
	int mSize;
};

#endif //_INCLUDE_CSTRING_H