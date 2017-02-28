/* $Id: anl_main.c,v 1.7 2007/11/01 17:21:25 ishisaki Exp $

	2004/06/06 Y.ISHISAKI	version 1.50
		original anl_body(argc,argv) -> anl_main() and split out to anl_main.c

	2005/02/19 Y.ISHISAKI	version 1.60
		use mkstemp() instead of tempnam()
		call CLrhis() and CLwhis() to read/write "$HOME/.anl_history"

	2005/10/21 Y.ISHISAKI	version 1.61
		add free(hist_path) in anl_main()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cli.h"
#include "anl.h"
#include "anl_misc.h"

static void
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

int
anl_main(int argc, char **argv)
{
	static char hist_file[] = ".anl_history";
	int len, status, num_put, exit_status, hist_error;
	char *task_name, *task_version, *task_credits;
	char *home_path, *hist_path;

/* read history file */
	hist_path= NULL;
	home_path = getenv("HOME");
	if ( NULL != home_path ) {
		len = strlen(home_path);
		if ( len ) {
			hist_path = malloc(len + 1 + sizeof(hist_file));
			if ( NULL != hist_path ) {
				strcpy(hist_path, home_path);
				if ( '/' != hist_path[len-1] ) {
					hist_path[len] = '/';
					len++;
				}
				strcpy(hist_path+len, hist_file);
				/*printf("hist_path=%s\n", hist_path);*/
				CLrhis(hist_path, &hist_error);
			}
		}
	}

/* Get task name, version, credits */
	task_name = anl_task_name();
	task_version = anl_task_version();
	task_credits = anl_task_credits();

/* Print credits, if exists */
	if ( NULL != task_credits && '\0' != *task_credits ) {
		printf(task_credits, task_name, task_version);
	}

/* Push Arguments & call anl_body() */
	push_args(argc, argv);
	status = anl_body();
	anl_exit_status(&num_put, &exit_status);
	if ( num_put ) {
		status = exit_status;
	}

/* write back history file */
	if ( NULL != hist_path ) {
		CLwhis(hist_path, 100, &hist_error);
		free(hist_path);
	}

	return status;
}
