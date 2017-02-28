/* $Id: mkanlinit.c,v 1.11 2005/11/29 20:02:21 ishisaki Exp $
C
C File: mkanlinit.c
C Description: ANL main program generator
C Author: Y.Ishisaki
C

  2004/02/06 Y.ISHISAKI	version 1.40
	filename size changed to FILENAME_MAX
	add anl_task_name, anl_task_version, anl_task_credits
	remove anl_module_num(), anl_hbook_info(), anl_bnk_info(), anl_evs_info()
	put task_info, module_num, hbook/bnk/evs_info to eliminate global variables

  2004/06/06 Y.ISHISAKI	version 1.50
	call anl_main() instead of anl_body()

  2005/02/17 Y.ISHISAKI	version 1.60
	check module flag, as [flag-char]MODULE_NAME
	add 'int flag' in module_list[]
	stop using "ctype.h" for RedHat9 object compatibility, use CLstrdwc()
	add "*VERBOSE_LEVEL" & "*PROFILE_MODULE" commands
	use CLtitrd() instead of CLtxtrd(), call CLerok() at each line
	print VERBOSE_LEVEL, PROFILE_MODULE, TASK_NAME, VERSION, CREDITS

  2005/02/17 Y.ISHISAKI	version 1.70
	add "*CHATTER" commands
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cli.h"
#include "anl_def.h"

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024
#endif

static char *anlinit_template[3] = { "\
/*\n\
	Registration Routine for ANL\n\
	Created by mkanlinit in ANL %s\n\
*/\n\
\n\
static char task_name[] = \"%s\";\n\
static char task_version[] = \"%s\";\n\
static char task_credits[] = \"%s\";\n\
\n\
/* HBOOK */\n\
#define HBK_BUF_SIZE	%d\n\
#define HBK_SHARED_FLAG	%d\n\
#define HBK_SHARED_FILE	\"%s\"\n\
float pawc_[HBK_BUF_SIZE];\n\
\n\
/* BNK */\n\
#define BNK_BUF_SIZE	%d\n\
#define BNK_SHARED_FLAG	%d\n\
#define BNK_SHARED_FILE	\"%s\"\n\
\n\
/* EVS */\n\
#define EVS_SHARED_FLAG	%d\n\
#define EVS_SHARED_FILE	\"%s\"\n\
\n\
/* VERBOSE_LEVEL */\n\
#define DEFAULT_VERBOSE_LEVEL	%d\n\
\n\
/* PROFILE_MODULE */\n\
#define DEFAULT_PROFILE_FLAG	%d\n\
\n\
/* CHATTER */\n\
#define DEFAULT_CHATTER	%d\n\
\n\
typedef void (*FUNC)();\n\
\n\
", "\
static struct {\n\
	FUNC startup;\n\
	FUNC init;\n\
	FUNC com;\n\
	FUNC his;\n\
	FUNC bgnrun;\n\
	FUNC ana;\n\
	FUNC endrun;\n\
	FUNC exit;\n\
	char *name;\n\
	char *version;\n\
	int flag;\n\
} module_list[] = {\n\
	{\n\
", "\
		0,	/* startup */\n\
		0,	/* init */\n\
		0,	/* com */\n\
		0,	/* his */\n\
		0,	/* bgnrun */\n\
		0,	/* ana */\n\
		0,	/* endrun */\n\
		0,	/* exit */\n\
		0,	/* name */\n\
		0,	/* version */\n\
		0	/* flag */\n\
	}\n\
};\n\
\n\
int MAIN_;	/* work around SunOS 4.1.3 bug */\n\
\n\
extern void anl_put_task_name();\n\
extern void anl_put_task_version();\n\
extern void anl_put_task_credits();\n\
extern void anl_put_module_num();\n\
extern void anl_put_module_list();\n\
extern void anl_put_hbook_info();\n\
extern void anl_put_bnk_info();\n\
extern void anl_put_evs_info();\n\
extern void anl_put_verbose_level();\n\
extern void anl_put_profile_flag();\n\
extern void anl_put_msg_chatter();\n\
extern int anl_main();\n\
\n\
int\n\
main(argc, argv)\n\
  int argc;\n\
  char **argv;\n\
{\n\
	anl_put_task_name(task_name);\n\
	anl_put_task_version(task_version);\n\
	anl_put_task_credits(task_credits);\n\
	anl_put_module_num(sizeof(module_list) / sizeof(module_list[0]) - 1);\n\
	anl_put_module_list(module_list);\n\
	anl_put_hbook_info(HBK_BUF_SIZE, HBK_SHARED_FLAG, HBK_SHARED_FILE);\n\
	anl_put_bnk_info(BNK_BUF_SIZE, BNK_SHARED_FLAG, BNK_SHARED_FILE);\n\
	anl_put_evs_info(EVS_SHARED_FLAG, EVS_SHARED_FILE);\n\
	anl_put_verbose_level(DEFAULT_VERBOSE_LEVEL);\n\
	anl_put_profile_flag(DEFAULT_PROFILE_FLAG);\n\
	anl_put_msg_chatter(DEFAULT_CHATTER);\n\
\n\
	return anl_main(argc, argv);\n\
}\n\
" };

char *
make_entry_name(name, ext)
  char *name;
  char *ext;
{
	static char buf[256];
	int i, len;
	len = strlen(name);
	for (i = 0; i < len; i++) {
		buf[i] = name[i];
		if ( '/' == name[i] ) {
			if ( 0 == strcmp("startup", ext) &&
				 0 == strcmp("/ASCA_ANL", name+i) ) {
				return NULL;
			}
			if ( 0 == strcmp("version", ext) &&
				 0 == strcmp("/ASCA_ANL", name+i) ) {
				return NULL;
			}
			if ( 0 == strcmp("version", ext) &&
				 0 == strcmp("/FORTRAN", name+i) ) {
				return NULL;
			}
			if ( 0 == strcmp("/ASCA_ANL", name+i) ||
				 0 == strcmp("/FORTRAN", name+i) ) {
				buf[i] = '_';
				strcpy(buf+i+1, ext);
				strcat(buf, "_");
			}
			CLstrdwc(buf);
			return buf;
		}
	}
	sprintf(buf, "%s_%s", name, ext);
	return buf;
}

int
main(argc, argv)
  int argc;
  char *argv[];
{
#define NUMCMD	11
	static char *cmdtbl[NUMCMD] = {
		"TASK_NAME", "VERSION", "CREDITS",
		"HBOOK_BUF_SIZE", "BNK_BUF_SIZE",
		"SHARED_HBOOK", "SHARED_BNK", "SHARED_EVS",
		"VERBOSE_LEVEL", "PROFILE_MODULE", "CHATTER"
    };
#define NUMEXT	8
	static char *ext[NUMEXT] = {
		"startup", "init", "com", "his", "bgnrun", "ana", "endrun", "exit"
	};
	static char cmdans[32];	/* must be <= 32 due to the limit on CLI v1.90 */
	static char name[MAX_ANALYSIS][128];
	static int  flag[MAX_ANALYSIS];
	static char filename[FILENAME_MAX] = "anlinit.c";
	FILE *fp;
	int i, j, ichoice;
	int n = 0;
	int hsize = DEFAULT_HBOOK_BUF_SIZE;
	int bsize = DEFAULT_BNK_BUF_SIZE;
	int shared_hbook_flag = 0;
	int shared_bnk_flag = 0;
	int shared_evs_flag = 0;
	int verbose_level = -1;
	int profile_flag = 0;
	int chatter = 2;
	static char shared_hbook_file[FILENAME_MAX] = "hbook.share";
	static char shared_bnk_file[FILENAME_MAX] = "bnk.share";
	static char shared_evs_file[FILENAME_MAX] = "evs.share";
	static char task_name[1024];
	static char task_version[1024];
	static char task_credits[8192];
	static char task_credits_raw[8192];
/* ask user modules */
	for (;;) {
		int c;
		char quest[80];
		char *answer = name[n];
		sprintf(quest, "Enter module name or *command [%2d]", n+1);
		name[n][0] = '\0';
		CLtitrd(quest, name[n], sizeof(name[n]));
		c = *answer;
		if ( '\0' == c ) {
			break;
        } else if ( '*' == c ) {
			answer++;
			if ( *answer ) {
				int len = strlen(answer);
				CLugetrd(answer, len);
            }
			strcpy(cmdans, answer);
			CLkeyrd(-1, "Command", cmdans, cmdtbl, NUMCMD, &ichoice, sizeof(cmdans));
			if ( 0 == strcmp("TASK_NAME", cmdans) ) {
				CLtitrd(cmdans, task_name, sizeof(task_name));
			} else if ( 0 == strcmp("VERSION", cmdans) ) {
				CLtitrd(cmdans, task_version, sizeof(task_version));
			} else if ( 0 == strcmp("CREDITS", cmdans) ) {
				char *p = task_credits;
				for (;;) {
					int siz = sizeof(task_credits) - (p - task_credits) - 3;
					CLtitrd("CREDITS [End with \".\"]", p, siz);
					if ( 0 == strcmp(".", p) ) {
						*p = '\0';
						break;
					}
					strcat(task_credits_raw, p);
					strcat(task_credits_raw, "\n");
					strcat(p, "\\n\\\n");
					p += strlen(p);
				}
			} else if ( 0 == strcmp("HBOOK_BUF_SIZE", cmdans) ) {
				CLintrd(cmdans, &hsize);
            } else if ( 0 == strcmp("BNK_BUF_SIZE", cmdans) ) {
				CLintrd(cmdans, &bsize);
            } else if ( 0 == strcmp("SHARED_HBOOK", cmdans) ) {
				shared_hbook_flag = 1;
				CLtxtrd(cmdans, shared_hbook_file, sizeof(shared_hbook_file));
            } else if ( 0 == strcmp("SHARED_BNK", cmdans) ) {
				shared_bnk_flag = 1;
				CLtxtrd(cmdans, shared_bnk_file, sizeof(shared_bnk_file));
            } else if ( 0 == strcmp("SHARED_EVS", cmdans) ) {
				shared_evs_flag = 1;
				CLtxtrd(cmdans, shared_evs_file, sizeof(shared_evs_file));
            } else if ( 0 == strcmp("VERBOSE_LEVEL", cmdans) ) {
				CLintrd(cmdans, &verbose_level);
            } else if ( 0 == strcmp("PROFILE_MODULE", cmdans) ) {
				CLlogrd(cmdans, &profile_flag);
            } else if ( 0 == strcmp("CHATTER", cmdans) ) {
				CLintrd(cmdans, &chatter);
            }
		} else if ( c <= ' ' || NULL != strchr("#;%/!", c) ) {
			/* empty line or comment, ignore it */
		} else if ( ('A'<=c && c<='Z') || ('a'<=c && c<='z') || ('_'==c) ) {
			/* module name without flags */
			flag[n] = 0;
			n++;
		} else {
			/* module name with flags */
			flag[n] = c;
			for (i = 1; ' ' == name[n][i]; i++) {
				/* skip following spaces */
			}
			strcpy(name[n], name[n]+i);
			c = name[n][0];
			if ( ('A'<=c && c<='Z') || ('a'<=c && c<='z') || ('_'==c) ) {
				n++;
			}
        }

		CLerok();	/* discard the remaining characters in each line */
	}

/* display buffer size */
	printf("\n");
	printf("HBOOK_BUF_SIZE = %d\n", hsize);
	printf("BNK_BUF_SIZE   = %d\n", bsize);
	if ( shared_hbook_flag ) {
		printf("SHARED_HBOOK   = %s\n", shared_hbook_file);
	}
	if ( shared_bnk_flag ) {
		printf("SHARED_BNK     = %s\n", shared_bnk_file);
	}
	if ( shared_evs_flag ) {
		printf("SHARED_EVS     = %s\n", shared_evs_file);
	}
	printf("VERBOSE_LEVEL  = %d\n", verbose_level);
	printf("PROFILE_MODULE = %s\n", profile_flag ? "YES" : "NO");
	printf("CHATTER        = %d\n", chatter);

/* Print credits, if exists */
	if ( '\0' != *task_credits ) {
		printf("\n");
		printf(task_credits_raw, task_name, task_version);
	}

/* display linked modules */
	printf("\n");
	if ( 0 == n ) {
		printf("No modules are to be linked to ANL\n");
	} else {
		printf("Following modules are to be linked to ANL\n");
		for (i = 0; i < n; i++) {
			if ( 0 == flag[i] ) {
				printf("     [%2d] %s\n", i+1, name[i]);
			} else {
				printf("     [%2d] %c %s\n", i+1, flag[i], name[i]);
			}
		}
	}
	printf("\n");

/* open output file anlinit.c */
	for (;;) {
		CLtxtrd("Output file", filename, sizeof(filename));
		if ( 0 == access(filename, F_OK) ) {
			/* already exists */
			static char quest[80];
			int ans;
			sprintf(quest, "'%s' already exists, delete", filename);
			ans = 1;
			CLlogrd(quest, &ans);
			if ( ans ) {
				unlink(filename);
			} else {
				ans = 0;
				CLlogrd("Are you sure to exit", &ans);
				if ( ans ) {
					printf("Output file did not created.\n");
					return 1;
				}
				continue;
			}
		}
		break;
	}
	fp = fopen(filename, "w");
	if ( NULL == fp ) {
		printf("Can not open output file '%s'\n", filename);
		return 1;
    }
/* output anlinit.c */
	fprintf(fp, anlinit_template[0],
			ANL_VERSION, task_name, task_version, task_credits,
			hsize, shared_hbook_flag, shared_hbook_file,
			bsize, shared_bnk_flag, shared_bnk_file,
			shared_evs_flag, shared_evs_file,
			verbose_level, profile_flag, chatter);
	for (i = 0; i < n; i++) {
		char *entry_name;
		for (j = 0; j < NUMEXT; j++) {
			entry_name = make_entry_name(name[i], ext[j]);
			if ( entry_name ) {
				fprintf(fp, "extern void %s();\n", entry_name);
			}
        }
		entry_name = make_entry_name(name[i], "version");
		if ( entry_name ) {
			fprintf(fp, "extern char %s[];\n", entry_name);
		}
		fprintf(fp, "\n");
    }
	fprintf(fp, anlinit_template[1], hsize, bsize);
	for (i = 0; i < n; i++) {
		char *entry_name;
/* startup, init, com, his, bgnrun, ana, endrun, exit */
		for (j = 0; j < NUMEXT; j++) {
			entry_name = make_entry_name(name[i], ext[j]);
			if ( entry_name ) {
				fprintf(fp, "\t\t%s,\n", entry_name);
			} else {
				fprintf(fp, "\t\t(void*)0,\n");
			}
        }
/* name */
		fprintf(fp, "\t\t\"%s\",\n", name[i]);
/* version */
		entry_name = make_entry_name(name[i], "version");
		if ( entry_name ) {
			fprintf(fp, "\t\t%s,\n", entry_name);
		} else {
			fprintf(fp, "\t\t\"(unknown)\",\n");
		}
/* flag */
		fprintf(fp, "\t\t%d\n", flag[i]);
		fprintf(fp, "\t}, {\n");
    }
	fprintf(fp, anlinit_template[2]);
	fclose(fp);
/* exit from this program */
	printf("ANL program written in '%s'\n", filename);
	return 0;
}
