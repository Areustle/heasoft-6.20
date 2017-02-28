/* $Id: mkanlmodule.c,v 1.3 2005/02/23 07:00:28 ishisaki Exp $

	1995/04/04 M.Hirayama
		Get command line arguments

	1998/07/22 Y.ISHISAKI
		rewrite in C

	2005/02/17 Y.ISHISAKI	version 1.60
		stop using "ctype.h" for RedHat9 object compatibility,
			use CLstrupc() & CLstrdwc()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cli.h"

static char pname[] = "mkanlmodule";

extern void Write_AnlCmodule();
extern void Write_AnlFmodule();

static void
print_help()
{
	static char msg[] = "\
 USAGE: %s [-FORTRAN | -Fortran | -C] [-o output_file] module_name\n\
   -FORTRAN : Creates a template of ANL module written in FORTRAN. (default)\n\
         -C : Creates a template of ANL module written in C language.\n\
    -o file : Outputs a template into \"file\".\n\
            : Specify \"-\" to get a template into standard output. (default)\n";
	printf(msg, pname);
}

int
main(argc, argv)
  int argc;
  char **argv;
{
	static char filename[256] = "anlmodule.c";
	char answer[256], name[256], quest[256];
	char C_name[256], U_name[256], L_name[256];
	int i, yesno;
	int fortran = 1;
	int interactive = 1;

/* check arguments and ask/set file name etc. */
	if ( argc < 2 ) {
		interactive = 1;
/* --- ask user routines */
		*answer = '\0';
		CLtxtrd("Enter module name", answer, sizeof(answer));
		if ( *answer ) {
			strcpy(name, answer);
        } else {
			printf("No empty module created.\n");
			return 1;
        }
/* --- ask language to be used */
		CLlogrd("Do you write it in FORTRAN", &fortran);
		strcpy(filename, name);
		strcat(filename, fortran ? ".f" : ".c");
    } else {
		interactive = 0;
		strcpy(filename, "-");	/* default stdout in non-interactive mode */
		*name = '\0';
/* --- set file name etc. */
		for (i = 1; i < argc; i++) {
			if ( 0 == strcmp("-FORTRAN", argv[i]) ||
				 0 == strcmp("-Fortran", argv[i]) ) {
				fortran = 1;
            } else if ( 0 == strcmp("-C", argv[i]) ) {
				fortran = 0;
            } else if ( 0 == strcmp("-o", argv[i]) ) {
				i++;
				if ( i < argc ) {
					strcpy(filename, argv[i]);
                }
            } else if ( '-' == argv[i][0] ) {
				print_help();
				return 1;
            } else {
				strcpy(name, argv[i]);
            }
        }
		if ( '\0' == *name ) {
			print_help();
			return 1;
        }
    }
/* open output file */
 again:
	if ( interactive ) {
		CLtxtrd("Output file", filename, sizeof(filename));
		if ( 0 != strcmp("-", filename) ) {
			if ( 0 == access(filename, F_OK) ) {	/* already exits */
				yesno = 0;
				sprintf(quest, "'%s' already exists, delete", filename);
				CLlogrd(quest, &yesno);
				if ( yesno ) {
					unlink(filename);
                } else {
					yesno = 0;
					CLlogrd("Are you sure to exit", &yesno);
					if ( yesno ) {
						printf("Output file did not created.\n");
						return 1;
                    } else {
						goto again;
                    }
                }
            }
        }
    }
/* output an empty module */
	strcpy(C_name, name);
	strcpy(U_name, name);
	CLstrupc(U_name);
	strcpy(L_name, name);
	CLstrdwc(L_name);
	if ( fortran ) {
		Write_AnlFmodule(filename, name, U_name);
    } else {
		Write_AnlCmodule(filename, C_name, U_name, L_name);
    }
	if ( 0 != strcmp("-", filename) ) {
		printf("Empty module written in '%s'\n", filename);
    }
	return 0;
}
