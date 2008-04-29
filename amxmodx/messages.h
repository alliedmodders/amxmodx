#ifndef _MSGS_INCLUDE_H
#define _MSGS_INCLUDE_H

#include <extdll.h>
#include <meta_api.h>
#include "amx.h"
#include "CVector.h"
#include "CString.h"
#include "sh_stack.h"

#define MAX_MESSAGES 255

#define MSGBLOCK_SET	0
#define MSGBLOCK_GET	1
#define BLOCK_NOT 0
#define BLOCK_ONCE 1
#define BLOCK_SET 2

class RegisteredMessage
{
private:
	CVector<int> m_Forwards;
	CStack<int> m_InExecution;
	bool m_Cleanup;

public:
	RegisteredMessage() : m_Cleanup(false) { } 
	~RegisteredMessage() { this->Clear(); }

	void AddHook(int fwd)
	{
		m_Forwards.push_back(fwd);
	}
	bool RemoveHook(int fwd)
	{
		// Don't erase a forward if we're in the middle of execution; this
		// could throw off the iterator that is going through the forwards
		// and executing them.  Instead, unregister the forward and set it
		// to -1 from within the vector.
		if (m_InExecution.size())
		{
			this->m_Cleanup = true;

			CVector<int>::iterator iter = m_Forwards.begin();
			CVector<int>::iterator end = m_Forwards.end();
			while (iter != end)
			{
				if (*iter == fwd)
				{
					if (*iter != -1)
					{
						unregisterSPForward(*iter);
					}
					*iter = -1;
					return true;
				}
				else
				{
					iter++;
				}
			}
		
		}
		else
		{
			CVector<int>::iterator iter = m_Forwards.begin();
			CVector<int>::iterator end = m_Forwards.end();
			while (iter != end)
			{
				if (*iter == fwd)
				{
					if (fwd != -1)
					{
						unregisterSPForward(fwd);

						m_Forwards.erase(iter);

						return true;
					}
					else
					{
						// -1 could be in here more than once
						m_Forwards.erase(iter);
					}
				}
				else
				{
					iter++;
				}
			}
		}

		return false;
	}

	void Clear()
	{
		while (m_InExecution.size())
		{
			m_InExecution.pop();
		}
		for (size_t i = 0; i < m_Forwards.size(); i++)
		{
			int fwd = m_Forwards[i];

			if (fwd != -1)
			{
				unregisterSPForward(m_Forwards[i]);
			}
		}

		m_Forwards.clear();
	}

	cell Execute(cell type, cell dest, cell entity)
	{
		m_InExecution.push(1);
		cell res = 0;
		cell thisres = 0;
		for (size_t i = 0; i < m_Forwards.size(); i++)
		{
			int fwd = m_Forwards[i];

			if (fwd != -1)
			{
				thisres = executeForwards(fwd, type, dest, entity);
				if (thisres > res)
				{
					res = thisres;
				}

			}
		}

		m_InExecution.pop();

		if (m_InExecution.size() == 0 && m_Cleanup)
		{
			this->RemoveHook(-1);
		}

		return res;
	}
	bool Hooked() const
	{
		return m_Forwards.size() != 0;
	}
};
enum msgtype
{
	arg_byte = 1,
	arg_char,
	arg_short,
	arg_long,
	arg_angle,
	arg_coord,
	arg_string,
	arg_entity,
};

struct msgparam
{
	msgtype type;
	union
	{
		REAL fData;
		int iData;
	} v;
	String szData;
};

class Message
{
public:
	Message();
	~Message();
	void AddParam(float data, msgtype type);
	void AddParam(int data, msgtype type);
	void AddParam(const char *data, msgtype type);
	void SetParam(size_t index, float data);
	void SetParam(size_t index, int data);
	void SetParam(size_t index, const char *data);
	const char *GetParamString(size_t index);
	float GetParamFloat(size_t index);
	bool Ready();
	void Init();
	int GetParamInt(size_t index);
	msgtype GetParamType(size_t index);
	void Reset();
	void Send();
	size_t Params();
private:
	msgparam *AdvPtr();
private:
	CVector<msgparam *> m_Params;
	size_t m_CurParam;
};

void C_MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
void C_WriteByte(int iValue);
void C_WriteChar(int iValue);
void C_WriteShort(int iValue);
void C_WriteLong(int iValue);
void C_WriteAngle(float flValue);
void C_WriteCoord(float flValue);
void C_WriteString(const char *sz);
void C_WriteEntity(int iValue);
void C_MessageEnd(void);

extern RegisteredMessage msgHooks[256];
extern int msgBlocks[256];

void ClearMessages();

#endif //_MSGS_INCLUDE_H

