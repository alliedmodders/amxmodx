// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <stdio.h>
#include <stdlib.h>
#include "../../amxmodx/sm_crc32.h"

int main(int argc, char **argv)
{
	if (argc < 2)
	{
		fprintf(stderr, "Usage: crc32 <file>\n");
		return 1;
	}

	const char *path = argv[1];
	FILE *fp = fopen(path, "rb");

	if (!fp)
	{
		fprintf(stderr, "Could not open file: %s\n", path);
		return 1;
	}

	if (fseek(fp, 0, SEEK_END) != 0)
	{
		fclose(fp);
		fprintf(stderr, "Failed to seek file: %s\n", path);
		return 1;
	}

	long sizeLong = ftell(fp);

	if (sizeLong <= 0 || fseek(fp, 0, SEEK_SET) != 0)
	{
		fclose(fp);
		fprintf(stderr, "Cannot checksum file \"%s\" (empty or unreadable).\n", path);
		return 1;
	}

	size_t size = static_cast<size_t>(sizeLong);
	void *buffer = malloc(size);

	if (!buffer)
	{
		fclose(fp);
		fprintf(stderr, "Unable to allocate %zu bytes of memory.\n", size);
		return 1;
	}

	size_t read = fread(buffer, 1, size, fp);
	fclose(fp);

	if (read != size)
	{
		free(buffer);
		fprintf(stderr, "Failed to read file: %s\n", path);
		return 1;
	}

	unsigned int crc32 = UTIL_CRC32(buffer, size);
	free(buffer);

	printf("%08X\n", crc32);
	return 0;
}
