#ifndef CCALLLIST_H
#define CCALLLIST_H

/*
 Very basic call list.
 */

class CCallList
{
public:
	struct CallList {
		int iFunctionIdx;
		int iCheckInt;
		CallList* next;
		CallList(int i, int check,CallList* n): iFunctionIdx(i), iCheckInt(check), next(n) {}
	} *head;

	int execute(int check,int id) {
		int ret = 0;
		CallList *a = head;
		while (a)
		{
			if (check == a->iCheckInt)
			{
				int temp = MF_ExecuteForward(a->iFunctionIdx,id);
				if (temp > ret)
					ret = temp;
			}
			a = a->next;
		}
		LOG_CONSOLE(PLID,"[IMPULSE] Returning: %i",ret);
		return ret;
	};
	void create(int function,int check) {
		head = new CallList(function, check , head );
	};
	void clear() {
		while (head)
		{
			CallList* a = head->next;
			delete head;
			head = a;
		}
	};

};

#endif