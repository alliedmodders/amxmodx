/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* GeoIPCity.h
 *
 * Copyright (C) 2003 MaxMind LLC  All rights reserved.
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

#ifndef GEOIPCITY_H
#define GEOIPCITY_H

#include "GeoIP.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct GeoIPRecordTag {
	char *country_code;
	char *country_code3;
	char *country_name;
	char *region;
	char *city;
	char *postal_code;
	float latitude;
	float longitude;
	int dma_code;
	int area_code;
} GeoIPRecord;

GeoIPRecord * GeoIP_record_by_addr (GeoIP* gi, const char *addr);
GeoIPRecord * GeoIP_record_by_name (GeoIP* gi, const char *host);

int GeoIP_record_id_by_addr (GeoIP* gi, const char *addr);
int GeoIP_init_record_iter (GeoIP* gi);
/* returns 0 on success, 1 on failure */
int GeoIP_next_record (GeoIP* gi, GeoIPRecord **gir, int *record_iter);

void GeoIPRecord_delete (GeoIPRecord *gir);

#ifdef __cplusplus
}
#endif

#endif /* GEOIPCITY_H */
