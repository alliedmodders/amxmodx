lib_LTLIBRARIES = libGeoIP.la libGeoIPUpdate.la libGeoIPBitReader.la

EXTRA_DIST = md5.h global.h

AM_CPPFLAGS = -DGEOIPDATADIR=\"$(pkgdatadir)\" -Wall

if WITH_CITY
libGeoIP_la_SOURCES = GeoIP.c GeoIPCity.c
include_HEADERS = GeoIP.h GeoIPCity.h GeoIPBitReader.h GeoIPUpdate.h
else
libGeoIP_la_SOURCES = GeoIP.c
include_HEADERS = GeoIP.h GeoIPBitReader.h GeoIPUpdate.h
endif

libGeoIPUpdate_la_SOURCES = GeoIPUpdate.c md5.c
libGeoIPBitReader_la_SOURCES = GeoIPBitReader.c

libGeoIP_la_LDFLAGS = -version-info @GEOIP_VERSION_INFO@

libGeoIPUpdate_la_LIBADD = -lz
