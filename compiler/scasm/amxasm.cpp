/* AMX Assembler
 * Copyright (C)2004 David "BAILOPAN" Anderson
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id$
 */

#include "amxasm.h"

std::string filename;
std::string output_name;

int main(int argc, char **argv)
{
	Compiler Program;

	get_options(argc, argv, Program);

	if (filename.size() < 1)
	{
		print_version();
		print_options();
		exit(0);
	}

	Program.Load(filename);
	if (Program.Parse())
	{

		if (Program.Compile(output_name.size() ? output_name : filename))
			printf("Done.\n");
		else
			printf("Compile build failed.\n");
	} else {
		printf("Compile failed.\n");
	}

	exit(0);
}

void get_options(int argc, char **argv, Compiler &Prog)
{
	int i = 0;			/* index */
	char opt_flag = 0;	/* flag for option detection */
	char *option = 0;	/* option pointer */
	for (i=1; i<argc; i++)
	{
		if (argv[i][0] == '-')
		{
			opt_flag = 1;
			option = argv[i];
			switch (argv[i][1])
			{
			case 'o':
				{
					if (strlen(argv[i]) > 2)
					{
						output_name.assign(&(argv[i][2]));
					} else {
						opt_flag = 'o';
					}
					break;
				}
			case 'v':
				{
					opt_flag = 0;	/* no options expected */
					print_version();
					exit(0);
					break;
				} /* case */
			case 'd':
				{
					opt_flag = 0;
					Prog.SetDebug();
					break;
				}
			} /* switch */
		} else { /* - */
			if (!opt_flag)
			{
				filename.assign(argv[i]);
			} else {
				switch (opt_flag)
				{
				case 'o':
					{
						output_name.assign(argv[i]);
						break;
					}
				}
			}
		} /* if */
	}
}

void print_version()
{
	printf("Small/AMX Assembler 1.00\n");
	printf("(C)2004 David 'BAILOPAN' Anderson\n");
}

void print_options()
{
	printf("\nOptions:\n");
	printf("\t-d\t\t- Add debug opcodes (will double file size)\n");
	printf("\t-v\t\t- Output version and exit\n");
	printf("\t-o\t\t- Specify file to write\n");
	printf("\n");
}
