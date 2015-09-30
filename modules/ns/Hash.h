// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#ifndef _HASH_H_
#define _HASH_H_

#include <amtl/am-vector.h>
#include <amtl/am-string.h>


// Just a very simple hash map, no iteration or anything of the like (not needed)


inline int HashFunction(const ke::AString& name)
{
	static const int kHashNumTable[128] = 
	{
		0x1148FC3E, 0x0577C975, 0x3BED3AED, 0x62FBBD5F, 0x07DE2DA0, 0x6555C5E5, 0x24DB841A, 0x2AF3F568,
		0x01EA1B65, 0x46F7D976, 0x18172B99, 0x394B2A58, 0x1ED39AA8, 0x1214E706, 0x5DD47295, 0x53574932,
		0x2CE25D5C, 0x7A2E5BB4, 0x0F2F0153, 0x33888669, 0x729AC55F, 0x2A7BCA9E, 0x36C60816, 0x40F9A7E3,
		0x2A37DF30, 0x3D81BB17, 0x6450B311, 0x75FA2DC9, 0x2A2678A5, 0x4C5E3675, 0x743F4486, 0x3B6F74E3,
		0x51D5FFEA, 0x302C7F74, 0x1E6B3243, 0x59B42D8A, 0x15824559, 0x4346B65D, 0x04A822F2, 0x176C60BF,
		0x0A3E8FD3, 0x1CBF4E8B, 0x50B78B17, 0x29122A7B, 0x2ED43591, 0x2E8BFDAC, 0x7C6973AE, 0x5BB692EE,
		0x28BA5960, 0x0B987501, 0x0F3F1957, 0x1B551EBF, 0x36143F9F, 0x4605216D, 0x5C4EC6A2, 0x604C1ECF,
		0x0386DC84, 0x409F79B4, 0x56464C99, 0x2DAD5529, 0x0CFDB029, 0x4A85911F, 0x691CCA0D, 0x5ED3B013,
		0x7AB21093, 0x0787FC50, 0x3887DD9D, 0x103455ED, 0x4ACEB2AD, 0x3D30008F, 0x27A0B6AC, 0x550D4280,
		0x59EF4F1B, 0x785841C3, 0x7E1F6CFC, 0x08C384AC, 0x26E43F70, 0x7A88E0AA, 0x647A179A, 0x4F9E98D0,
		0x062155AB, 0x73B930F1, 0x6AF3B790, 0x3C35954B, 0x39BE525E, 0x47427E32, 0x1C81B41A, 0x3D452EE2,
		0x07E1F7E6, 0x72C800B3, 0x6AF2840C, 0x14DFA80F, 0x3D4D91D3, 0x540F4E19, 0x73B35822, 0x37FFA266,
		0x5B974A69, 0x2C3B35BF, 0x4833F853, 0x2665FD16, 0x696B364F, 0x6FD4AEFF, 0x7B733F96, 0x435A856A,
		0x682CF0C3, 0x7992AC92, 0x4C1E0A16, 0x0F113033, 0x741B8D3C, 0x309821B1, 0x5EAFC903, 0x7A3CE2E8,
		0x245152A2, 0x49A38093, 0x36727833, 0x5E0FA501, 0x10E5FEC6, 0x52F42C4D, 0x1B54D3E3, 0x18C7F6AC,
		0x45BC2D01, 0x064757EF, 0x2DA79EBC, 0x0309BED4, 0x5A56A608, 0x215AF6DE, 0x3B09613A, 0x35EDF071
	};
	size_t size = name.length();
	
	if (size == 0)
	{
		return 0;
	}
	
	int hasha = kHashNumTable[(*(name.chars() + (size - 1))) & 0x7F];
	int hashb = kHashNumTable[size % 128];

	
	unsigned char c = 0;
	for (size_t i = 0; i < size; i++)
	{
		c = (*(name.chars() + (size - 1))) & 0x7F;
		
		hasha = (hasha + hashb) ^ kHashNumTable[c];
		hashb = hasha + hashb + c + (hasha << 8) + (hashb & 0xFF);
	}
	
	
	return hasha;
}

/**
 * @param K		Key type.
 * @param D		Data type.
 * @param F		Hash function.
 * @param B		Bucket count.
 */



template <typename K = ke::AString, typename D = ke::AString, int (*F)(const K&) = HashFunction, int B = 1024>
class Hash
{
protected:
	ke::Vector<int> m_HashBuckets[B];
	ke::Vector<K> m_KeyBuckets[B];
	ke::Vector<D> m_DataBuckets[B];

	inline int GetBucket(int hash)
	{
		return (*reinterpret_cast<unsigned int*>(&hash)) % B;
	};


public:

	// prints bucket distribution
	inline void printbuckets() const
	{
		for (int i = 0; i < B; i++)
		{
			if (i % 32 == 0 && i != 0)
			{
				printf("\n");
			}
			printf("%d ", m_HashBuckets[i].size());
		}
		printf("\n");
	}
	inline void insert(const K& key, const D& value)
	{
		D* ptr;
		if ((ptr = this->find(key)) != NULL)
		{
			*ptr = value;
			return;
		}

		int hash = F(key);

		int bucketnum = GetBucket(hash);

		m_HashBuckets[bucketnum].append(hash);
		m_KeyBuckets[bucketnum].append(key);
		m_DataBuckets[bucketnum].append(value);

		return;

	}
	inline D& lookup_or_add(const K& key)
	{
		D* ptr;
		if ((ptr = this->find(key)) != NULL)
		{
			return *ptr;
		}

		int hash = F(key);

		int bucketnum = GetBucket(hash);

		m_HashBuckets[bucketnum].append(hash);
		m_KeyBuckets[bucketnum].append(key);
		m_DataBuckets[bucketnum].append(D());

		return m_DataBuckets[bucketnum].at(m_DataBuckets[bucketnum].size() - 1);

	}
	inline bool exists(const K& key)
	{
		return this->find(key) != NULL;
	}
	inline D* find(const K& key)
	{
		int hash = F(key);

		int bucketnum = GetBucket(hash);

		// TODO: Possibly make this binary search?
		//       insertion would be annoying, don't think it is worth it
		ke::Vector<int>* hashbucket = &m_HashBuckets[bucketnum];
		ke::Vector<K>* keybucket = &m_KeyBuckets[bucketnum];

		size_t bucketsize = hashbucket->length();

		for (size_t i = 0; i < bucketsize; i++)
		{
			if (hashbucket->at(i) == hash)
			{
				if (key.compare(keybucket->at(i)) == 0)
				{
					return &(m_DataBuckets[bucketnum].at(i));
				}
			}
		}

		return NULL;
	};
};

#endif
