/* by David "BAILOPAN" Anderson
 * No warranties of any kind
 * License: I hereby grant this work to the public domain and make no copyright claims.
 */
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <limits.h>

int main(int argc, char **argv)
{
	char *file=NULL;
	void *dl= NULL;
	FILE *fp = NULL;
	char path[PATH_MAX];
	if (argc != 2)
	{
		printf("Usage: dlsym <file>\n");
		exit(0);
	}
	file = argv[1];
	realpath(file, path);
	fp = fopen(path, "rb");
	if (!fp)
	{
		printf("File not found.\n");
		exit(0);
	}

	dl = dlopen(path, RTLD_NOW);
	
	if (dl)
	{
		printf("Shared module loaded.  Handle: %p\n", dl);
		dlclose(dl);
		dl = NULL;
	} else {
		printf("Shared module failed to load: %s\n", dlerror());
	}
	
	exit(0);
}

