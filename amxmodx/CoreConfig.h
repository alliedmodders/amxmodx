// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _CORE_CONFIG_H_
#define _CORE_CONFIG_H_

#include "CPlugin.h"

class CoreConfig
{
	public:

		CoreConfig();
		~CoreConfig();

	public:

		void Clear();

		void ExecuteMainConfig();
		void ExecuteAutoConfigs();
		bool ExecuteAutoConfig(CPluginMngr::CPlugin *plugin, AutoConfig *config, bool can_create);
		void ExecuteMapConfig();

		void OnAmxxInitialized();
		void OnMapConfigTimer();

		void CheckLegacyBufferedCommand(char *command);
		void SetMapConfigTimer(float time);

	private:

		bool  m_ConfigsExecuted;          // Whether all configs have been executed
		bool  m_PendingForwardPush;       // Whether OnConfigsExecuted forward should be triggered to the next frame
		bool  m_LegacyMainConfigExecuted; // Whether the old admin.sma is used and amxx.cfg was executed from there
		bool  m_LegacyMapConfigsExecuted; // Whether the old admin.sma is used and per-map config was executed from there
		float m_legacyMapConfigNextTime;  // Sets the next time that per-map configs should be executed

		int m_ConfigsBufferedForward;
		int m_ConfigsExecutedForward;
};

extern CoreConfig CoreCfg;

#endif // _CORE_CONFIG_H_
