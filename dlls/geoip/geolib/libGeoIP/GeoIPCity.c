/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* GeoIPCity.c
 *
 * Copyright (C) 2003 MaxMind LLC
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

#include <GeoIP.h>
#include <GeoIPCity.h>
#ifndef _WIN32
#include <netdb.h>
#include <netinet/in.h> /* For ntohl */
#else
#include <windows.h>
#include <winsock.h>
#endif
#include <sys/types.h> /* For uint32_t */
#ifdef HAVE_STDINT_H
#include <stdint.h>     /* For uint32_t */
#endif

const int FULL_RECORD_LENGTH = 50;

GeoIPRecord * _extract_record(GeoIP* gi, unsigned int seek_record, int *next_record_ptr) {
	int record_pointer;
	unsigned char *record_buf = NULL;
	unsigned char *begin_record_buf = NULL;
	GeoIPRecord * record;
	int str_length = 0;
	int j;
	double latitude = 0, longitude = 0;
	int dmaarea_combo = 0;
	int bytes_read = 0;
	if (seek_record == gi->databaseSegments[0])		
		return NULL;

	record = malloc(sizeof(GeoIPRecord));
	memset(record, 0, sizeof(GeoIPRecord));

	record_pointer = seek_record + (2 * gi->record_length - 1) * gi->databaseSegments[0];

	if (gi->cache == NULL) {
		fseek(gi->GeoIPDatabase, record_pointer, SEEK_SET);
		begin_record_buf = record_buf = malloc(sizeof(char) * FULL_RECORD_LENGTH);
		bytes_read = fread(record_buf, sizeof(char), FULL_RECORD_LENGTH, gi->GeoIPDatabase);
		if (bytes_read == 0) {
			/* eof or other error */
			return NULL;
		}
	} else {
		record_buf = gi->cache + (long)record_pointer;
	}

	/* get country */
	record->country_code	= (char *) GeoIP_country_code [record_buf[0]];
	record->country_code3 = (char *) GeoIP_country_code3[record_buf[0]];
	record->country_name	= (char *) GeoIP_country_name [record_buf[0]];
	record_buf++;

	/* get region */
	while (record_buf[str_length] != '\0')
		str_length++;
	if (str_length > 0) {
		record->region = malloc(str_length+1);
		strncpy(record->region, record_buf, str_length+1);
	}
	record_buf += str_length + 1;
	str_length = 0;

	/* get city */
	while (record_buf[str_length] != '\0')
		str_length++;
	if (str_length > 0) {
		record->city = malloc(str_length+1);
		strncpy(record->city, record_buf, str_length+1);
	}
	record_buf += (str_length + 1);
	str_length = 0;

	/* get postal code */
	while (record_buf[str_length] != '\0')
		str_length++;
	if (str_length > 0) {
		record->postal_code = malloc(str_length+1);
		strncpy(record->postal_code, record_buf, str_length+1);
	}
	record_buf += (str_length + 1);

	/* get latitude */
	for (j = 0; j < 3; ++j)
		latitude += (record_buf[j] << (j * 8));
	record->latitude = latitude/10000 - 180;
	record_buf += 3;

	/* get longitude */
	for (j = 0; j < 3; ++j)
		longitude += (record_buf[j] << (j * 8));
	record->longitude = longitude/10000 - 180;

	/* get area code and dma code for post April 2002 databases and for US locations */
	if (GEOIP_CITY_EDITION_REV1 == gi->databaseType) {
		if (!strcmp(record->country_code, "US")) {
			record_buf += 3;
			for (j = 0; j < 3; ++j)
				dmaarea_combo += (record_buf[j] << (j * 8));
			record->dma_code = dmaarea_combo/1000;
			record->area_code = dmaarea_combo % 1000;
		}
	}

	if (gi->cache == NULL)
		free(begin_record_buf);

	/* Used for GeoIP_next_record */
	if (next_record_ptr != NULL)
		*next_record_ptr = seek_record + record_buf - begin_record_buf + 3;

	return record;
}

GeoIPRecord * _get_record(GeoIP* gi, unsigned long ipnum) {
	unsigned int seek_record;

	if (gi->databaseType != GEOIP_CITY_EDITION_REV0 &&
			gi->databaseType != GEOIP_CITY_EDITION_REV1) {
		printf("Invalid database type %s, expected %s\n", GeoIPDBDescription[(int)gi->databaseType], GeoIPDBDescription[GEOIP_CITY_EDITION_REV1]);
		return 0;
	}

	seek_record = _seek_record(gi, ipnum);
	return _extract_record(gi, seek_record, NULL);
}

GeoIPRecord * GeoIP_record_by_addr (GeoIP* gi, const char *addr) {
	unsigned long ipnum;
	if (addr == NULL) {
		return 0;
	}
	ipnum = _addr_to_num(addr);
	return _get_record(gi, ipnum);
}

GeoIPRecord * GeoIP_record_by_name (GeoIP* gi, const char *name) {
	unsigned long ipnum;
	struct hostent * host;
	if (name == NULL) {
		return 0;
	}
	host = gethostbyname(name);
	if (host == NULL) {
		return 0;
	}
	ipnum = ntohl(*((unsigned int*)host->h_addr_list[0]));
	return _get_record(gi, ipnum);
}

int GeoIP_record_id_by_addr (GeoIP* gi, const char *addr) {
	unsigned long ipnum;
	if (gi->databaseType != GEOIP_CITY_EDITION_REV0 &&
			gi->databaseType != GEOIP_CITY_EDITION_REV1) {
		printf("Invalid database type %s, expected %s\n", GeoIPDBDescription[(int)gi->databaseType], GeoIPDBDescription[GEOIP_CITY_EDITION_REV1]);
		return 0;
	}
	if (addr == NULL) {
		return 0;
	}
	ipnum = _addr_to_num(addr);
	return _seek_record(gi, ipnum);
}

int GeoIP_init_record_iter (GeoIP* gi) {
	return gi->databaseSegments[0] + 1;
}

int GeoIP_next_record (GeoIP* gi, GeoIPRecord **gir, int *record_iter) {
	if (gi->cache != NULL) {
		printf("GeoIP_next_record not supported in memory cache mode\n");
		return 1;
	}
	*gir = _extract_record(gi, *record_iter, record_iter);
	return 0;
}

void GeoIPRecord_delete (GeoIPRecord *gir) {
	free(gir->region);
	free(gir->city);
	free(gir->postal_code);
	free(gir);
}
