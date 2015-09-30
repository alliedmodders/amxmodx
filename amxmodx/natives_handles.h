// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _NATIVES_NATIVES_HANDLES_H_
#define _NATIVES_NATIVES_HANDLES_H_

#include <amtl/am-vector.h>

// Note: All handles start at 1. 0 and below are invalid handles.
//       This way, a plugin that doesn't initialize a vector or
//       string will not be able to modify another plugin's data
//       on accident.

template <typename T>
class NativeHandle
{
	private:

		ke::Vector<T*> m_handles;

	public:

		NativeHandle() {}
		~NativeHandle()
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

		template <typename... Targs>
		int create(Targs... Fargs)
		{
			for (size_t i = 0; i < m_handles.length(); ++i)
			{
				if (!m_handles[i])
				{
					m_handles[i] = new T(Fargs...);

					return static_cast<int>(i) + 1;
				}
			}

			m_handles.append(new T(Fargs...));

			return m_handles.length();
		}

		int clone(T *data)
		{
			for (size_t i = 0; i < m_handles.length(); ++i)
			{
				if (!m_handles[i])
				{
					m_handles[i] = data;

					return static_cast<int>(i) + 1;
				}
			}

			m_handles.append(data);

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

#endif // _NATIVES_NATIVES_HANDLES_H_
