/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* GeoIPBitReader.h
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

#ifndef GEOIPBITREADER_H
#define GEOIPBITREADER_H

#ifdef __cplusplus
extern "C" {
#endif

#include<stdio.h>
#include<stdlib.h>

#define BITS_IN_BYTE 8

typedef struct GeoIPBitReaderTag {
  FILE *GeoIPBitFH;
  char bits;
  short int position;
} GeoIPBitReader;

typedef enum {
  EOF_FLAG    = 0,
  NOOP_FLAG   = 1,
  VALUE_FLAG  = 2
} GeoIPBitReaderRecordCode;

GeoIPBitReader * GeoIPBitReader_new(const char * filename);
unsigned long GeoIPBitReader_read(GeoIPBitReader * gibr, short int numBits);

#ifdef __cplusplus
}
#endif

#endif // GEOIPBITREADER_H
