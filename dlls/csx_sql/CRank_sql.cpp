// The good stuff: http://dev.mysql.com/doc/mysql/en/mysql_query.html

#include "amxxmodule.h"
#include "CRank.h"
#include "rank.h"

#ifndef __linux__
#define WINDOWS_LEAN_AND_MEAN
#include <winsock.h>
#endif
#include <mysql/mysql.h>
#include <mysql/errmsg.h>

#include <time.h>
#define MYSQL_QUERY_IS_A_OKAY 0

cvar_t init_csx_sqlstats_host = {"csx_sqlstats_host", "127.0.0.1", FCVAR_SPONLY | FCVAR_PROTECTED};
cvar_t init_csx_sqlstats_username = {"csx_sqlstats_username", "", FCVAR_SPONLY | FCVAR_PROTECTED};
cvar_t init_csx_sqlstats_password = {"csx_sqlstats_password", "", FCVAR_SPONLY | FCVAR_PROTECTED};
cvar_t init_csx_sqlstats_db = {"csx_sqlstats_db", "amxmodx_stats_cs", FCVAR_SPONLY | FCVAR_PROTECTED};
cvar_t init_csx_sqlstats_table = {"csx_sqlstats_table", "cs", FCVAR_SPONLY | FCVAR_PROTECTED};
cvar_t init_csx_sqlstats_exportstats = {"csx_sqlstats_exportstats", "0", FCVAR_SPONLY | FCVAR_PROTECTED};
cvar_t *csx_sqlstats_host;
cvar_t *csx_sqlstats_username;
cvar_t *csx_sqlstats_password;
cvar_t *csx_sqlstats_db;
cvar_t *csx_sqlstats_table;
cvar_t *csx_sqlstats_exportstats;

void OnMetaAttach_sql() {
	CVAR_REGISTER(&init_csx_sqlstats_host);
	CVAR_REGISTER(&init_csx_sqlstats_username);
	CVAR_REGISTER(&init_csx_sqlstats_password);
	CVAR_REGISTER(&init_csx_sqlstats_db);
	CVAR_REGISTER(&init_csx_sqlstats_table);
	CVAR_REGISTER(&init_csx_sqlstats_exportstats);

	csx_sqlstats_host = CVAR_GET_POINTER(init_csx_sqlstats_host.name);
	csx_sqlstats_username = CVAR_GET_POINTER(init_csx_sqlstats_username.name);
	csx_sqlstats_password = CVAR_GET_POINTER(init_csx_sqlstats_password.name);
	csx_sqlstats_db = CVAR_GET_POINTER(init_csx_sqlstats_db.name);
	csx_sqlstats_table = CVAR_GET_POINTER(init_csx_sqlstats_table.name);
	csx_sqlstats_exportstats = CVAR_GET_POINTER(init_csx_sqlstats_exportstats.name);
}

int Error(MYSQL *mysql)
{
	if (mysql == NULL)
		return 0;

	return mysql_errno(mysql);
}

void RankSystem::saveRankSql()
{
	// Don't do anything if cvar says so.
	if (csx_sqlstats_exportstats->value == 0.0)
		return;

	MF_PrintSrvConsole("[CSX Sql] Exporting players' statistics to SQL db...");
	clock_t startTime = clock();

	MYSQL *mysql = NULL;
	mysql = mysql_init(NULL);


	/* Attempt to get a port */
	int port = 0;
	char *p = strchr(csx_sqlstats_host->string, ':');
	if (p)
		port = atoi(p+1);
	/*************************/	
	//MF_PrintSrvConsole("Host: %s (%d) Port: %d", host, strcspn(csx_sqlstats_host->string, ":"), port);

	int error = 0;
	if (!mysql_real_connect(mysql, csx_sqlstats_host->string, csx_sqlstats_username->string, csx_sqlstats_password->string, NULL, port, NULL, 0)) {
		error = Error(mysql);
		if (error) {
			MF_Log("DB Connection failed (%d): %s", error, mysql_error(mysql));
			mysql_close(mysql);
			return;
		}
	}

	if (mysql_select_db(mysql, csx_sqlstats_db->string) != 0) {
		error = Error(mysql);
		if (error) {
			MF_Log("DB Select DB failed (%d): %s", error, mysql_error(mysql));
			mysql_close(mysql);
			return;
		}
	}

	// Query
	char query[2048];

	snprintf(query, 2047, "CREATE TABLE IF NOT EXISTS `%s` (`timestamp` int(11) NOT NULL default '0', `stats_authid` varchar(100) NOT NULL default '', `stats_name` varchar(100) NOT NULL default '', `stats_tks` int(11) NOT NULL default '0', `stats_damage` int(11) NOT NULL default '0', `stats_deaths` int(11) NOT NULL default '0', `stats_frags` int(11) NOT NULL default '0', `stats_shots` int(11) NOT NULL default '0', `stats_hits` int(11) NOT NULL default '0', `stats_hs` int(11) NOT NULL default '0', `stats_defusions` int(11) NOT NULL default '0', `stats_defused` int(11) NOT NULL default '0', `stats_plants` int(11) NOT NULL default '0', `stats_explosions` int(11) NOT NULL default '0', `stats_bodyhits0` int(11) NOT NULL default '0', `stats_bodyhits1` int(11) NOT NULL default '0', `stats_bodyhits2` int(11) NOT NULL default '0', `stats_bodyhits3` int(11) NOT NULL default '0', `stats_bodyhits4` int(11) NOT NULL default '0', `stats_bodyhits5` int(11) NOT NULL default '0', `stats_bodyhits6` int(11) NOT NULL default '0', `stats_bodyhits7` int(11) NOT NULL default '0', `stats_bodyhits8` int(11) NOT NULL default '0', `stats_score` int(11) NOT NULL default '0') TYPE=MyISAM",
		csx_sqlstats_table->string);
	int queryResult = mysql_query(mysql, query);
	if (queryResult != MYSQL_QUERY_IS_A_OKAY)
	{
		error = Error(mysql);
		MF_Log("DB Query Create Table If Not Exists failed (%d): %s", error, mysql_error(mysql));
		mysql_close(mysql);
		return;
	}

	int exportedRecords = 0;

	RankSystem::iterator a = front();

	char *authid, *name;
	int tks, damage, deaths, kills, shots, hits, hs, defusions, defused, plants, explosions, *bodyHits, score;
	time_t now = time(NULL);

	while ( a )
	{
		if ( (*a).score != (1<<31) ) // score must be different than mincell
		{
			authid = (*a).unique;
			if (strcmp(authid, "BOT") == 0 || strcmp(authid, "STEAM_ID_PENDING") == 0) {
				--a;
				continue;
			}
			exportedRecords++;

			name = (*a).name;
			tks = (*a).tks;
			damage = (*a).damage;
			deaths = (*a).deaths;
			kills = (*a).kills;
			shots = (*a).shots;
			hits = (*a).hits;
			hs = (*a).hs;
			defusions = (*a).bDefusions;
			defused = (*a).bDefused;
			plants = (*a).bPlants;
			explosions = (*a).bExplosions;
			bodyHits = ((*a).bodyHits);
			score = (*a).score;

			_snprintf(query, 2047, "UPDATE `%s` SET `timestamp` = %d, `stats_name` = \"%s\", `stats_tks` = \"%d\", `stats_damage` = \"%d\", `stats_deaths` = \"%d\", `stats_frags` = \"%d\", `stats_shots` = \"%d\", `stats_hits` = \"%d\", `stats_hs` = \"%d\", `stats_defusions` = \"%d\", `stats_defused` = \"%d\", `stats_plants` = \"%d\", `stats_explosions` = \"%d\", `stats_bodyhits0` = \"%d\", `stats_bodyhits1` = \"%d\", `stats_bodyhits2` = \"%d\", `stats_bodyhits3` = \"%d\", `stats_bodyhits4` = \"%d\", `stats_bodyhits5` = \"%d\", `stats_bodyhits6` = \"%d\", `stats_bodyhits7` = \"%d\", `stats_bodyhits8` = \"%d\", `stats_score` = \"%d\" WHERE `stats_authid` = \"%s\" LIMIT 1",
								   csx_sqlstats_table->string,
								   now,
								   name,
								   tks,
								   damage,
								   deaths,
								   kills,
								   shots,
								   hits,
								   hs,
								   defusions,
								   defused,
								   plants,
								   explosions,
								   bodyHits[0],
								   bodyHits[1],
								   bodyHits[2],
								   bodyHits[3],
								   bodyHits[4],
								   bodyHits[5],
								   bodyHits[6],
								   bodyHits[7],
								   bodyHits[8],
								   score,
								   authid);
									//

			int queryResult = mysql_query(mysql, query);
			if (queryResult != MYSQL_QUERY_IS_A_OKAY)
			{
				error = Error(mysql);
				MF_Log("DB Query Update failed (%d): %s", error, mysql_error(mysql));
				mysql_close(mysql);
				return;
			}

			if (mysql_affected_rows(mysql) == 0) {
				// New player, do insert
				_snprintf(query, 2047, "INSERT INTO `%s` (`timestamp`, `stats_authid`, `stats_name`, `stats_tks`, `stats_damage`, `stats_deaths`, `stats_frags`, `stats_shots`, `stats_hits`, `stats_hs`, `stats_defusions`, `stats_defused`, `stats_plants`, `stats_explosions`, `stats_bodyhits0`, `stats_bodyhits1`, `stats_bodyhits2`, `stats_bodyhits3`, `stats_bodyhits4`, `stats_bodyhits5`, `stats_bodyhits6`, `stats_bodyhits7`, `stats_bodyhits8`, `stats_score`) VALUES (\"%d\", \"%s\", \"%s\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\")",
					csx_sqlstats_table->string,
					now,

					authid,
					name,

					tks,
					damage,
					deaths,
					kills,
					shots,

					hits,
					hs,
					defusions,
					defused,
					plants,

					explosions,
					
					bodyHits[0],
					bodyHits[1],
					bodyHits[2],
					bodyHits[3],
					bodyHits[4],
					bodyHits[5],
					bodyHits[6],
					bodyHits[7],
					bodyHits[8],

					score
					);

				int queryResult = mysql_query(mysql, query);
				if (queryResult != MYSQL_QUERY_IS_A_OKAY)
				{
					error = Error(mysql);
					MF_Log("DB Query Insert failed (%d): %s", error, mysql_error(mysql));
					mysql_close(mysql);
					return;
				}
			}
		}
		
		--a;
	}

	// Disconnect
	mysql_close(mysql);
	
	clock_t stopTime = clock();
	MF_PrintSrvConsole("...done! (exported %d records in %.2f seconds)\n", exportedRecords, (double)(stopTime - startTime) / (double)CLOCKS_PER_SEC);
}
