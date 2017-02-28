#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cli.h"

void
push_args(int argc, char **argv)
{
	static char fn[] = "/tmp/anl.XXXXXX";	/* must contain "." for CLI bug */
	FILE *fp;
	int fd, i;

	strcpy(fn + sizeof(fn) - 7, "XXXXXX");	/* write back for previouse use */

	if ( 1 < argc ) {
		fd = mkstemp(fn);
		if ( -1 != fd ) {
			fp = fdopen(fd, "w");
			if ( NULL != fp ) {
				for (i = 1; i < argc; i++) {
					fprintf(fp, "%s\n", argv[i]);
				}
				fclose(fp);
				CLopnrd(fn);
			} else {
				close(fd);
            }
			unlink(fn);
		}
	}
}
