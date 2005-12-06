/* BrainBread Fun Module Stocks
 *
 * (c) 2005, XxAvalanchexX (converted to module by Rukia)
 *
 * This file is provided as is (no warranties).
 */

#if defined _brainbread_stocks_included
  #endinput
#endif
#define _brainbread_stocks_included

/* Used to create a spray */
stock bb_spray(type,origin[3]) {

	/* type: type of spray, see below */
	/* origin: origin of where spray comes from */

	/* TYPES:
	SPRAY_BSPRITZ	(1) = Blood spritz
	SPRAY_BGUSH	(2) = Blood gush
	SPRAY_SMOKEPUFF	(4) = Smoke puffs
	SPRAY_EXPLOSION	(5) = Firey explosion */

	message_begin(MSG_PVS,118,origin,0);
	write_byte(type);
	write_coord(origin[0]);
	write_coord(origin[1]);
	write_coord(origin[2]);
	write_coord(random_num(-1,1));
	write_coord(random_num(-1,1));
	write_coord(random_num(-1,1));
	write_coord(random_num(-1,1));
	write_coord(random_num(-1,1));
	write_coord(random_num(-1,1));
	message_end();

	return 1;
}

/* Used to create a spray, with more parameters */
stock bb_spray_adv(type,origin[3],size[3],dir[3]) {

	/* type: type of spray, see below */
	/* origin: origin of where spray comes from */
	/* size: size of spray, in XYZ format (I think), use a small number like -1 to 1 */
	/* dir: direction of spray, in XYZ format (I think), use a small number like -1 to 1 */

	/* TYPES:
	SPRAY_BSPRITZ	(1) = Blood spritz
	SPRAY_BGUSH	(2) = Blood gush
	SPRAY_SMOKEPUFF	(4) = Smoke puffs
	SPRAY_EXPLOSION	(5) = Firey explosion */

	message_begin(MSG_PVS,118,origin,0);
	write_byte(type);
	write_coord(origin[0]);
	write_coord(origin[1]);
	write_coord(origin[2]);
	write_coord(size[0]);
	write_coord(size[1]);
	write_coord(size[2]);
	write_coord(dir[0]);
	write_coord(dir[1]);
	write_coord(dir[2]);
	message_end();

	return 1;
}

/* Used to set dots on the radar */
stock bb_radar(id,dot_id,dot_origin[3],dot_status,dot_type) {

	/* dot_id: unique ID for this dot, use same ID to modify the same dot */
	/* dot_origin: the origin of where to place this dot */
	/* dot_status: 0 to remove the dot, 1 to add or modify the dot */
	/* dot_type: the type of dot, see below */

	/* dot_origin and dot_type need not be set accurately when removing the dot */

	/* TYPES:
	DOT_GREEN	(1) = Green Dot, used to mark teammates
	DOT_RED	(2) = Red Dot, used to mark enemies for zombies
	DOT_WHITE	(3) = White Dot, used to mark mission zones
	DOT_LTBLUE	(4) = Light Blue Dot, not used for BrainBread
	DOT_BLUE	(5) = Blue Dot, used to mark the BlackHawk
	DOT_ORANGE	(6) = Orange Dot, not used for BrainBread
	DOT_FLYELLOW	(7) = Flashing Yellow Dot, used to mark the Case or Fred
	DOT_FLGREEN	(8) = Flashing Green Dot, not used for BrainBread,
 	   it stops flashing and turns to white after 3 seconds */

	message_begin(MSG_ONE,93,{0,0,0},id);
	write_short(dot_id);
	write_byte(1);
	write_coord(dot_origin[0]);
	write_coord(dot_origin[1]);
	write_coord(dot_origin[2]);
	write_byte(1);
	message_end();

	message_begin(MSG_ONE,93,{0,0,0},id);
	write_short(dot_id);
	write_byte((dot_status > 0) ? 2 : 0);

	if(dot_status > 0) {
		write_byte(dot_type);
	}

	message_end();

	return 1;
}
