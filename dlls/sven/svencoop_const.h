// prevent double include
#ifndef __CONST_H__
#define __CONST_H__

#define PDATA_FRAGS 0x9
#define PDATA_DEATHS 0x21B0
#define PDATA_ALLY 0x28

#define SVEN_WEP_9MM 0
#define PDATA_9MM 0x216D
#define PDATA_9MM2 0x218D
const long sven_9mm[2] = {PDATA_9MM, PDATA_9MM2};

#define SVEN_WEP_SHOTGUN 1
#define PDATA_SHOTGUN 0x216E
#define PDATA_SHOTGUN2 0x218E
const long sven_shotgun[2] = {PDATA_9MM, PDATA_9MM2};

#define SVEN_WEP_RPG 2
#define PDATA_RPG 0x2195
#define PDATA_RPG2 0x21B5
const long sven_rpg[2] = {PDATA_9MM, PDATA_9MM2};

#define SVEN_WEP_RADIO 3
#define PDATA_RADIO 0x217A
#define PDATA_RADIO2 0x219A
const long sven_radio[2] = {PDATA_9MM, PDATA_9MM2};

#define SVEN_WEP_SNARK 4
#define PDATA_SNARK 0x217A
#define PDATA_SNARK2 0x219A
const long sven_snark[2] = {PDATA_9MM, PDATA_9MM2};

#endif