#include <stdlib.h>
#include "journal.h"
#include "sdk/CVector.h"

struct j_info
{
	int id;
	Vault *vault;
};

Journal::Journal(const char *file)
{
	m_File.assign(file);
	m_Fp = NULL;
	m_LastId = 0;
}

#define rd(v,s) if (fread(&v, sizeof(s), 1, m_Fp) != 1) { \
	fclose(m_Fp); \
	m_Fp = NULL; \
	goto _error; }
#define rds(v,l) if (fread(v, sizeof(char), l, m_Fp) != l) { \
	fclose(m_Fp); \
	m_Fp = NULL; \
	goto _error; } else { \
	v[l] = '\0'; }

bool Journal::Replay(size_t &files, size_t &ops)
{
	m_Fp = fopen(m_File.c_str(), "rb");

	files = 0;
	ops = 0;

	if (!m_Fp)
		return false;

	//this must come before the first jump...
	CVector<j_info *> table;
	uint32_t magic;

	rd(magic, uint32_t);
	if (magic != JOURNAL_MAGIC)
		return false;

	j_info *j;
	uint8_t op, klen;
	uint16_t vlen;
	uint32_t id;
	size_t i;
	char *key=NULL, *val=NULL;
	while (!feof(m_Fp))
	{
		if (fread(&op, sizeof(uint8_t), 1, m_Fp) != 1)
		{
			if (feof(m_Fp))
				break;
			else
				goto _error;
		}
		switch (op)
		{
		case Journal_Nop:
			{
				break;
			}
		case Journal_Name:
			{
				rd(id, uint32_t);
				rd(klen, uint8_t);
				key = new char[klen+1];
				rds(key, klen);
				j = new j_info;
				j->id = id;
				j->vault = new Vault(key);
				j->vault->ReadFromFile();
				table.push_back(j);
				files++;
				delete [] key;
				key = NULL;
				break;
			}
		case Journal_Store:
			{
				//Stores key/val (id,time,klen,vlen,[],[])
				uint32_t stamp;
				rd(id, uint32_t);
				rd(stamp, uint32_t);
				rd(klen, uint8_t);
				rd(vlen, uint16_t);
				key = new char[klen+1];
				val = new char[vlen+1];
				rds(key, klen);
				rds(val, vlen);
				for (i=0; i<table.size(); i++)
				{
					if (table.at(i)->id == id)
					{
						table.at(i)->vault->Store(key, val, (time_t)stamp);
						break;
					}
				}
				delete [] key;
				delete [] val;
				key = NULL;
				val = NULL;
				break;
			}
		case Journal_Erase:
			{
				//Erases key (id,klen,[])
				rd(id, uint32_t);
				rd(klen, uint8_t);
				key = new char[klen+1];
				rds(key, klen);
				for (i=0; i<table.size(); i++)
				{
					if (table.at(i)->id == id)
					{
						table.at(i)->vault->EraseKey(key);
						break;
					}
				}
				delete [] key;
				key = NULL;
				break;
			}
		case Journal_Clear:
			{
				//Clears (id)
				rd(id, uint32_t);
				for (i=0; i<table.size(); i++)
				{
					if (table.at(i)->id == id)
					{
						table.at(i)->vault->Clear();
						break;
					}
				}
				break;
			}
		case Journal_Prune:
			{
				//Prunes (id,t1,t2,all)
				rd(id, uint32_t);
				uint32_t begin, end;
				uint8_t all;
				rd(begin, uint32_t);
				rd(end, uint32_t);
				rd(all, uint8_t);
				for (i=0; i<table.size(); i++)
				{
					if (table.at(i)->id == id)
					{
						table.at(i)->vault->Prune((time_t)begin, (time_t)end, all?true:false);
						break;
					}
				}
				break;
			}
		default:
			{
				goto _error;
			}
		} //end while
		ops++;
	}

	for (uint32_t i=0; i<table.size(); i++)
	{
		j = table.at(i);
		j->vault->WriteToFile();
		delete j->vault;
		delete j;
	}
    
	fclose(m_Fp);

	return true;
_error:
	for (uint32_t i=0; i<table.size(); i++)
	{
		j = table.at(i);
		j->vault->WriteToFile();
		delete j->vault;
		delete j;
	}
	if (key)
	{
		delete [] key;
		key = NULL;
	}
	if (val)
	{
		delete [] val;
		val = NULL;
	}
	return false;
}

void Journal::ClearJournal()
{
	m_Fp = fopen(m_File.c_str(), "wb");

	if (m_Fp)
	{
		fclose(m_Fp);
		m_Fp = NULL;
	}
}

bool Journal::StartJournal()
{
	m_Fp = fopen(m_File.c_str(), "wb");

	if (!m_Fp)
		return false;

	uint32_t magic = JOURNAL_MAGIC;

	fwrite(&magic, sizeof(uint32_t), 1, m_Fp);

	return true;
}

void Journal::EndJournal()
{
	if (m_Fp)
	{
		fclose(m_Fp);
		m_Fp = NULL;
	}
}

//Stores key/val (id,time,klen,vlen,[],[]
void Journal::Store(const char *name, const char *key, const char *val, time_t stamp)
{
	uint32_t time32 = (uint32_t)stamp;
	uint16_t vlen = (uint16_t)strlen(val);
	uint8_t klen = (uint8_t)strlen(key);

	BeginOp(name, Journal_Store);
		WriteInt(time32);
		WriteByte(klen);
		WriteShort(vlen);
		WriteString(key);
		WriteString(val);
	EndOp();
}

//Erases key (id,klen,[])
void Journal::Erase(const char *name, const char *key)
{
	uint8_t klen = (uint8_t)strlen(key);

	BeginOp(name, Journal_Erase);
		WriteByte(klen);
		WriteString(key);
	EndOp();
}

//Clears (id)
void Journal::Clear(const char *name)
{
	BeginOp(name, Journal_Clear);
	EndOp();
}

//Prunes (id,t1,t2,all)
void Journal::Prune(const char *name, time_t begin, time_t end, bool all)
{
	uint32_t begin32 = (uint32_t)begin;
	uint32_t end32 = (uint32_t)end;
	uint8_t all8 = (uint8_t)all;

	BeginOp(name, Journal_Prune);
		WriteInt(begin32);
		WriteInt(end32);
		WriteByte(all8);
	EndOp();
}

void Journal::BeginOp(const char *name, JournalOp jop)
{
	uint32_t id;

	if (!m_Names.KeyExists(name))
	{
		char name_buf[12];
		id = ++m_LastId;
		sprintf(name_buf, "%d", id);
		m_Names.Store(name, name_buf, false);
		WriteByte(Journal_Name);
		WriteInt(id);
		WriteByte(strlen(name));
		WriteString(name);
	} else {
		id = atoi(m_Names.Retrieve(name)->val.c_str());
	}

	WriteByte(jop);
	WriteInt(id);
}

void Journal::WriteByte(uint8_t num)
{
	fwrite(&num, sizeof(uint8_t), 1, m_Fp);
}

void Journal::WriteShort(uint16_t num)
{
	fwrite(&num, sizeof(uint16_t), 1, m_Fp);
}

void Journal::WriteInt(uint32_t num)
{
	fwrite(&num, sizeof(uint32_t), 1, m_Fp);
}

void Journal::WriteString(const char *str)
{
	fwrite(str, sizeof(char), strlen(str), m_Fp);
}

size_t Journal::EndOp()
{
	return 1;
}
