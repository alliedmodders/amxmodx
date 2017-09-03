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
#include <amtl/am-autoptr.h>

// Note: All handles start at 1. 0 and below are invalid handles.
//       This way, a plugin that doesn't initialize a vector or
//       string will not be able to modify another plugin's data
//       on accident.

template <typename T>
class NativeHandle
{
	private:

		ke::Vector<ke::AutoPtr<T>> m_handles;

	public:

		NativeHandle() {}
		~NativeHandle()
		{
			this->clear();
		}

		void clear()
		{
			m_handles.clear();
		}

		size_t size()
		{
			return m_handles.length();
		}

		T *lookup(size_t handle)
		{
			--handle;

			if (handle >= m_handles.length())
			{
				return nullptr;
			}

			return m_handles[handle].get();
		}

		template <typename... Targs>
		size_t create(Targs... Fargs)
		{
			for (size_t i = 0; i < m_handles.length(); ++i)
			{
				if (!m_handles[i])
				{
					m_handles[i] = ke::AutoPtr<T>(new T(Fargs...));

					return i + 1;
				}
			}

			m_handles.append(ke::AutoPtr<T>(new T(Fargs...)));

			return m_handles.length();
		}

		size_t clone(T *data)
		{
			for (size_t i = 0; i < m_handles.length(); ++i)
			{
				if (!m_handles[i])
				{
					m_handles[i] = ke::AutoPtr<T>(data);

					return i + 1;
				}
			}

			m_handles.append(ke::AutoPtr<T>(data));

			return m_handles.length();
		}

		bool destroy(size_t handle)
		{
			--handle;

			if (handle >= m_handles.length())
			{
				return false;
			}

			if (!m_handles[handle])
			{
				return false;
			}

			m_handles[handle] = nullptr;

			return true;
		}
};

enum ForwardState
{
	FSTATE_ACTIVE,
	FSTATE_STOP
};

#endif // _NATIVES_NATIVES_HANDLES_H_
