// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <am-vector.h>

template <typename T>
class Handle
{
	private:

		ke::Vector<T*> m_handles;

	public:

		Handle() {}
		~Handle()
		{
			this->clear();
		}

		void clear()
		{
			for (size_t i = 0; i < m_handles.length(); ++i)
			{
				if (m_handles[i])
				{
					delete m_handles[i];
				}
			}

			m_handles.clear();
		}

		T *lookup(int handle)
		{
			--handle;

			if (handle < 0 || handle >= static_cast<int>(m_handles.length()))
			{
				return nullptr;
			}

			return m_handles[handle];
		}

		int create()
		{
			for (size_t i = 0; i < m_handles.length(); ++i)
			{
				if (!m_handles[i])
				{
					m_handles[i] = new T;

					return static_cast<int>(i) + 1;
				}
			}

			m_handles.append(new T);

			return m_handles.length();
		}

		bool destroy(int handle)
		{
			handle--;

			if (handle < 0 || handle >= static_cast<int>(m_handles.length()))
			{
				return false;
			}

			if (!m_handles[handle])
			{
				return false;
			}

			delete m_handles[handle];
			m_handles[handle] = nullptr;

			return true;
		}
};