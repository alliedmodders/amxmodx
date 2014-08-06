#ifndef MAXMINDDB_H
#define MAXMINDDB_H

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200112L
#endif

#include "maxminddb_config.h"
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>

#ifdef _WIN32
#include "stdbool.h" /* Arksnine: Not supported by MSVC */
#include <BaseTsd.h> /* Arkshine: ssize_t replacement */
typedef SSIZE_T ssize_t;
#include <WinSock2.h>
#include <WS2tcpip.h>
typedef ADDRESS_FAMILY sa_family_t;
#else
#include <stdbool.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#endif

const char GeoIPCountryCode[][3] =
{
	"AP", "EU", "AD", "AE", "AF", "AG", "AI", "AL", "AM", "AN",
	"AO", "AQ", "AR", "AS", "AT", "AU", "AW", "AZ", "BA", "BB",
	"BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BM", "BN", "BO",
	"BR", "BS", "BT", "BV", "BW", "BY", "BZ", "CA", "CC", "CD",
	"CF", "CG", "CH", "CI", "CK", "CL", "CM", "CN", "CO", "CR",
	"CU", "CV", "CX", "CY", "CZ", "DE", "DJ", "DK", "DM", "DO",
	"DZ", "EC", "EE", "EG", "EH", "ER", "ES", "ET", "FI", "FJ",
	"FK", "FM", "FO", "FR", "FX", "GA", "GB", "GD", "GE", "GF",
	"GH", "GI", "GL", "GM", "GN", "GP", "GQ", "GR", "GS", "GT",
	"GU", "GW", "GY", "HK", "HM", "HN", "HR", "HT", "HU", "ID",
	"IE", "IL", "IN", "IO", "IQ", "IR", "IS", "IT", "JM", "JO",
	"JP", "KE", "KG", "KH", "KI", "KM", "KN", "KP", "KR", "KW",
	"KY", "KZ", "LA", "LB", "LC", "LI", "LK", "LR", "LS", "LT",
	"LU", "LV", "LY", "MA", "MC", "MD", "MG", "MH", "MK", "ML",
	"MM", "MN", "MO", "MP", "MQ", "MR", "MS", "MT", "MU", "MV",
	"MW", "MX", "MY", "MZ", "NA", "NC", "NE", "NF", "NG", "NI",
	"NL", "NO", "NP", "NR", "NU", "NZ", "OM", "PA", "PE", "PF",
	"PG", "PH", "PK", "PL", "PM", "PN", "PR", "PS", "PT", "PW",
	"PY", "QA", "RE", "RO", "RU", "RW", "SA", "SB", "SC", "SD",
	"SE", "SG", "SH", "SI", "SJ", "SK", "SL", "SM", "SN", "SO",
	"SR", "ST", "SV", "SY", "SZ", "TC", "TD", "TF", "TG", "TH",
	"TJ", "TK", "TM", "TN", "TO", "TL", "TR", "TT", "TV", "TW",
	"TZ", "UA", "UG", "UM", "US", "UY", "UZ", "VA", "VC", "VE",
	"VG", "VI", "VN", "VU", "WF", "WS", "YE", "YT", "RS", "ZA",
	"ZM", "ME", "ZW", "A1", "A2", "O1", "AX", "GG", "IM", "JE",
	"BL", "MF"
};

const char GeoIPCountryCode3[][4] =
{
	"AP", "EU", "AND", "ARE", "AFG", "ATG", "AIA", "ALB", "ARM", "ANT",
	"AGO", "AQ", "ARG", "ASM", "AUT", "AUS", "ABW", "AZE", "BIH", "BRB",
	"BGD", "BEL", "BFA", "BGR", "BHR", "BDI", "BEN", "BMU", "BRN", "BOL",
	"BRA", "BHS", "BTN", "BV", "BWA", "BLR", "BLZ", "CAN", "CC", "COD",
	"CAF", "COG", "CHE", "CIV", "COK", "CHL", "CMR", "CHN", "COL", "CRI",
	"CUB", "CPV", "CX", "CYP", "CZE", "DEU", "DJI", "DNK", "DMA", "DOM",
	"DZA", "ECU", "EST", "EGY", "ESH", "ERI", "ESP", "ETH", "FIN", "FJI",
	"FLK", "FSM", "FRO", "FRA", "FX", "GAB", "GBR", "GRD", "GEO", "GUF",
	"GHA", "GIB", "GRL", "GMB", "GIN", "GLP", "GNQ", "GRC", "GS", "GTM",
	"GUM", "GNB", "GUY", "HKG", "HM", "HND", "HRV", "HTI", "HUN", "IDN",
	"IRL", "ISR", "IND", "IO", "IRQ", "IRN", "ISL", "ITA", "JAM", "JOR",
	"JPN", "KEN", "KGZ", "KHM", "KIR", "COM", "KNA", "PRK", "KOR", "KWT",
	"CYM", "KAZ", "LAO", "LBN", "LCA", "LIE", "LKA", "LBR", "LSO", "LTU",
	"LUX", "LVA", "LBY", "MAR", "MCO", "MDA", "MDG", "MHL", "MKD", "MLI",
	"MMR", "MNG", "MAC", "MNP", "MTQ", "MRT", "MSR", "MLT", "MUS", "MDV",
	"MWI", "MEX", "MYS", "MOZ", "NAM", "NCL", "NER", "NFK", "NGA", "NIC",
	"NLD", "NOR", "NPL", "NRU", "NIU", "NZL", "OMN", "PAN", "PER", "PYF",
	"PNG", "PHL", "PAK", "POL", "SPM", "PCN", "PRI", "PSE", "PRT", "PLW",
	"PRY", "QAT", "REU", "ROU", "RUS", "RWA", "SAU", "SLB", "SYC", "SDN",
	"SWE", "SGP", "SHN", "SVN", "SJM", "SVK", "SLE", "SMR", "SEN", "SOM",
	"SUR", "STP", "SLV", "SYR", "SWZ", "TCA", "TCD", "TF", "TGO", "THA",
	"TJK", "TKL", "TKM", "TUN", "TON", "TLS", "TUR", "TTO", "TUV", "TWN",
	"TZA", "UKR", "UGA", "UM", "USA", "URY", "UZB", "VAT", "VCT", "VEN",
	"VGB", "VIR", "VNM", "VUT", "WLF", "WSM", "YEM", "YT", "SRB", "ZAF",
	"ZMB", "MNE", "ZWE", "A1", "A2", "O1", "ALA", "GGY", "IMN", "JEY",
	"BLM", "MAF"
};

#define MMDB_DATA_TYPE_EXTENDED (0)
#define MMDB_DATA_TYPE_POINTER (1)
#define MMDB_DATA_TYPE_UTF8_STRING (2)
#define MMDB_DATA_TYPE_DOUBLE (3)
#define MMDB_DATA_TYPE_BYTES (4)
#define MMDB_DATA_TYPE_UINT16 (5)
#define MMDB_DATA_TYPE_UINT32 (6)
#define MMDB_DATA_TYPE_MAP (7)
#define MMDB_DATA_TYPE_INT32 (8)
#define MMDB_DATA_TYPE_UINT64 (9)
#define MMDB_DATA_TYPE_UINT128 (10)
#define MMDB_DATA_TYPE_ARRAY (11)
#define MMDB_DATA_TYPE_CONTAINER (12)
#define MMDB_DATA_TYPE_END_MARKER (13)
#define MMDB_DATA_TYPE_BOOLEAN (14)
#define MMDB_DATA_TYPE_FLOAT (15)

/* GEOIPDB flags */
#define MMDB_MODE_MMAP (1)
#define MMDB_MODE_MASK (7)

/* GEOIPDB err codes */
#define MMDB_SUCCESS (0)
#define MMDB_FILE_OPEN_ERROR (1)
#define MMDB_CORRUPT_SEARCH_TREE_ERROR (2)
#define MMDB_INVALID_METADATA_ERROR (3)
#define MMDB_IO_ERROR (4)
#define MMDB_OUT_OF_MEMORY_ERROR (5)
#define MMDB_UNKNOWN_DATABASE_FORMAT_ERROR (6)
#define MMDB_INVALID_DATA_ERROR (7)
#define MMDB_INVALID_LOOKUP_PATH_ERROR (8)
#define MMDB_LOOKUP_PATH_DOES_NOT_MATCH_DATA_ERROR (9)
#define MMDB_INVALID_NODE_NUMBER_ERROR (10)
#define MMDB_IPV6_LOOKUP_IN_IPV4_DATABASE_ERROR (11)

#if !(MMDB_UINT128_IS_BYTE_ARRAY)
#if MMDB_UINT128_USING_MODE
typedef unsigned int mmdb_uint128_t __attribute__ ((__mode__(TI)));
#else
typedef unsigned __int128 mmdb_uint128_t;
#endif
#endif

/* This is a pointer into the data section for a given IP address lookup */
typedef struct MMDB_entry_s {
    struct MMDB_s *mmdb;
    uint32_t offset;
} MMDB_entry_s;

typedef struct MMDB_lookup_result_s {
    bool found_entry;
    MMDB_entry_s entry;
    uint16_t netmask;
} MMDB_lookup_result_s;

typedef struct MMDB_entry_data_s {
    bool has_data;
    union {
        uint32_t pointer;
        const char *utf8_string;
        double double_value;
        const uint8_t *bytes;
        uint16_t uint16;
        uint32_t uint32;
        int32_t int32;
        uint64_t uint64;
#if MMDB_UINT128_IS_BYTE_ARRAY
        uint8_t uint128[16];
#else
        mmdb_uint128_t uint128;
#endif
        bool boolean;
        float float_value;
    };
    /* This is a 0 if a given entry cannot be found. This can only happen
     * when a call to MMDB_(v)get_value() asks for hash keys or array
     * indices that don't exist. */
    uint32_t offset;
    /* This is the next entry in the data section, but it's really only
     * relevant for entries that part of a larger map or array
     * struct. There's no good reason for an end user to look at this
     * directly. */
    uint32_t offset_to_next;
    /* This is only valid for strings, utf8_strings or binary data */
    uint32_t data_size;
    /* This is an MMDB_DATA_TYPE_* constant */
    uint32_t type;
} MMDB_entry_data_s;

/* This is the return type when someone asks for all the entry data in a map or array */
typedef struct MMDB_entry_data_list_s {
    MMDB_entry_data_s entry_data;
    struct MMDB_entry_data_list_s *next;
} MMDB_entry_data_list_s;

typedef struct MMDB_description_s {
    const char *language;
    const char *description;
} MMDB_description_s;

typedef struct MMDB_metadata_s {
    uint32_t node_count;
    uint16_t record_size;
    uint16_t ip_version;
    const char *database_type;
    struct {
        size_t count;
        const char **names;
    } languages;
    uint16_t binary_format_major_version;
    uint16_t binary_format_minor_version;
    uint64_t build_epoch;
    struct {
        size_t count;
        MMDB_description_s **descriptions;
    } description;
} MMDB_metadata_s;

typedef struct MMDB_ipv4_start_node_s {
    uint16_t netmask;
    uint32_t node_value;
} MMDB_ipv4_start_node_s;

typedef struct MMDB_s {
    uint32_t flags;
    const char *filename;
    ssize_t file_size;
    const uint8_t *file_content;
    const uint8_t *data_section;
    uint32_t data_section_size;
    const uint8_t *metadata_section;
    uint32_t metadata_section_size;
    uint16_t full_record_byte_size;
    uint16_t depth;
    MMDB_ipv4_start_node_s ipv4_start_node;
    MMDB_metadata_s metadata;
} MMDB_s;

typedef struct MMDB_search_node_s {
    uint64_t left_record;
    uint64_t right_record;
} MMDB_search_node_s;

    /* *INDENT-OFF* */
    /* --prototypes automatically generated by dev-bin/regen-prototypes.pl - don't remove this comment */
    extern int MMDB_open(const char *const filename, uint32_t flags, MMDB_s *const mmdb);
    extern MMDB_lookup_result_s MMDB_lookup_string(MMDB_s *const mmdb,
                                                   const char *const ipstr,
                                                   int *const gai_error,
                                                   int *const mmdb_error);
    extern MMDB_lookup_result_s MMDB_lookup_sockaddr(
               MMDB_s *const mmdb,
               const struct sockaddr *const sockaddr,
               int *const mmdb_error);
    extern int MMDB_read_node(MMDB_s *const mmdb, uint32_t node_number,
                              MMDB_search_node_s *const node);
    extern int MMDB_get_value(MMDB_entry_s *const start,
                              MMDB_entry_data_s *const entry_data,
                              ...);
    extern int MMDB_vget_value(MMDB_entry_s *const start,
                               MMDB_entry_data_s *const entry_data,
                               va_list va_path);
    extern int MMDB_aget_value(MMDB_entry_s *const start,
                               MMDB_entry_data_s *const entry_data,
                               const char *const *const path);
    extern int MMDB_get_metadata_as_entry_data_list(
               MMDB_s *const mmdb, MMDB_entry_data_list_s **const entry_data_list);
    extern int MMDB_get_entry_data_list(
               MMDB_entry_s *start, MMDB_entry_data_list_s **const entry_data_list);
    extern void MMDB_free_entry_data_list(MMDB_entry_data_list_s *const entry_data_list);
    extern void MMDB_close(MMDB_s *const mmdb);
    extern const char *MMDB_lib_version(void);
    extern int MMDB_dump_entry_data_list(FILE *const stream,
                                         MMDB_entry_data_list_s *const entry_data_list,
                                         int indent);
    extern const char *MMDB_strerror(int error_code);
    /* --prototypes end - don't remove this comment-- */
    /* *INDENT-ON* */

#endif                          /* MAXMINDDB_H */
