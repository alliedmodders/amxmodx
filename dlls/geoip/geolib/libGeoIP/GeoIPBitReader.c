/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* GeoIPBitReader.c
 *
 * Copyright (C) 2002 MaxMind.com.  All Rights Reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "GeoIPBitReader.h"

GeoIPBitReader * GeoIPBitReader_new(const char * filename) {
	GeoIPBitReader *gibr = (GeoIPBitReader *)malloc(sizeof(GeoIPBitReader));
	gibr->GeoIPBitFH = fopen(filename,"rb");
	if (gibr->GeoIPBitFH == NULL) {
		fprintf(stderr, "Error Opening file %s\n",filename);
		return NULL;
	}
	gibr->position = 8;
	return gibr;
}

unsigned long GeoIPBitReader_read(GeoIPBitReader * gibr, short int numBits) {
	unsigned long num = 0;
	int i, bit;
	int bytes_read;

	for (i = 0; i < numBits; ++i) {
		if (BITS_IN_BYTE == gibr->position) {
			/* read in next byte */
			bytes_read = fread(&gibr->bits, 1, 1, gibr->GeoIPBitFH);
			if (bytes_read == 0) {
				fprintf(stderr, "Warning: EOF reached\n");
				gibr->bits = 0;
			}
			gibr->position = 0;
		}
		bit = ((gibr->bits & (1 << (BITS_IN_BYTE - gibr->position - 1))) > 0) ? 1 : 0;
		bit <<= i;
		num += bit;
		++(gibr->position);
	}
	return num;
}
