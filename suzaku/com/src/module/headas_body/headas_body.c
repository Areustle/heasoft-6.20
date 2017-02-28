/*
	headas_body.c

	1999-08-20	Y.ISHISAKI	version 1.0
		moved from local file to /usr/local/astroe/com/src/module/ftools_body/

	1999-10-26	Y.ISHISAKI	version 1.30 (skip number to match ANL version)
		modify anl_show_status() to end message of printf() by '\n'
		add irecv in anl_read_data()

	2003-09-30	Y.ISHISAKI	version 1.33
		modifications for HEADAS

	2004/02/06 Y.ISHISAKI	version 1.40
		refer anl_task_name, anl_task_version, anl_task_credits
		register taskname and version with set_toolname() & set_toolversion()
		anl_hbook/bnk/evs_info() functions changed,
		and hbk/bnk/evs_shared_file are now pointers to char (char *).

	2004/06/06 Y.ISHISAKI	version 1.50
		change functions names anl_**() -> hd_anl_**()
		change functions definitions static, except hd_anl_body()
		change function name of anl_body(argc, argv) -> anl_main(argc, argv)
		change function name of anl_body_sub() -> anl_body()

	2005/02/19 Y.ISHISAKI	version 1.60
		fix version string (1.41 -> 1.60)
		use mkstemp() instead of tempnam()
		fixed for anl_body() in aste_anl version 1.60
		call CLrhis() and CLwhis() to read/write "$HOME/.anl_history"

	2005/10/24 Y.ISHISAKI	version 1.61
		add free(hist_path) in anl_main()

	2005/11/30 Y.ISHISAKI	version 1.70
		use anl_msg_error(), anl_msg_warning(), anl_msg_always()
		allow longer module names in anl_show_profile()
		show chatter in anl_show_analysis()
		line oriented messaging with char msg[256] in anl_show_status()
		get "chatter" from parameter file and call anl_put_msg_chatter()
		only warning w/o anl_verbose,anl_profile,chatter,num_event,event_freq

	2006/05/27 Y.ISHISAKI	version 1.71
		return status in hd_anl_modify_param()
		quit unless _com() return ANL_OK, in hd_anl_body()

	2007/01/30 Y.ISHISAKI	version 1.72
		int -> double ievent, irecv in hd_anl_read_data()

	2007/02/02 Y.ISHISAKI	version 1.73
		add cast to int for nevent = (int)ievent in hd_anl_read_data()

	2007/04/30 Y.ISHISAKI	version 1.80
		return status in hd_anl_read_data()
		check ANL_ERROR for _bgnrun() in hd_anl_read_data()
		check status for _endrun() in hd_anl_read_data()
		check hd_anl_read_data() return status in hd_anl_body()

	2007/07/16 Y.ISHISAKI	version 1.81
		bug fix for ANL_ENDLOOP, int -> double num_event in anl_read_data()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "anl_def.h"
#include "anl_misc.h"
#include "pil.h"
#include "headas.h"

static char *hd_anl_version = "HEADAS converter 1.81 for ANL " ANL_VERSION;

struct analysis_status {
	int entry;		/* number of analysis call */
	int loop_entry;	/* number of loop return */
	int next;		/* number of ANL_OK or ANL_LOOP */
	int skip;		/* number of ANL_SKIP */
	int quit;		/* number of ANL_QUIT */
};

static int hbk_buf_size, hbk_shared_flag;
static int bnk_buf_size, bnk_shared_flag;
static int evs_shared_flag;
static char *hbk_shared_file, *bnk_shared_file, *evs_shared_file;

static void
hd_anl_define_analysis(int *num_chain_out, int *chain)
{
	int i, num_module, num_chain;

	num_module = anl_module_num();
	num_chain = 0;

	for (i = 0; i < num_module; i++) {
		if ( '-' != anl_routine_flag(i) ) {
			chain[num_chain] = i;
			num_chain++;
		}
	}

	*num_chain_out = num_chain;
}

static void
hd_anl_show_analysis(int num_chain, int *chain)
{
	int i, flag;

/*  Display the list of entries */
	anl_msg_always("\
\n\
     **************************\n\
     ***** Analysis chain *****\n\
     **************************\n\
\n\
      ANL %s\n\
\n", anl_version);
	if ( num_chain < 1 ) {
		anl_msg_always("\
Analysis chain NOT defined.\n");
	} else {
		for (i = 0; i < num_chain; i++) {
			flag = anl_routine_flag(chain[i]);
			if ( flag ) {
				anl_msg_always("\
     [%2d] %c %-22s", i+1, flag, anl_routine_name(chain[i]));
			} else {
				anl_msg_always("\
     [%2d] %-24s", i+1, anl_routine_name(chain[i]));
			}
			anl_msg_always("%s\n", anl_routine_version(chain[i]));
		}
    }
/* Display buffer size */
	anl_msg_always("\n");
	anl_msg_always("HBOOK_BUF_SIZE = %d\n", hbk_buf_size);
	anl_msg_always("BNK_BUF_SIZE   = %d\n", bnk_buf_size);
	if ( hbk_shared_flag ) {
		anl_msg_always("SHARED_HBOOK   = %s\n", hbk_shared_file);
	}
	if ( bnk_shared_flag ) {
		anl_msg_always("SHARED_BNK     = %s\n", bnk_shared_file);
	}
	if ( evs_shared_flag ) {
		anl_msg_always("SHARED_EVS     = %s\n", evs_shared_file);
	}
	anl_msg_always("VERBOSE_LEVEL  = %d\n", anl_verbose_level());
	anl_msg_always("PROFILE_MODULE = %s\n", anl_profile_flag() ? "YES" : "NO");
	anl_msg_always("CHATTER        = %d\n", anl_msg_chatter());
}

static int
hd_anl_modify_param(int num_chain, int *chain)
{
	int i, status;

	anl_profile_reset();
	for (i = 0; i < num_chain; i++) {
		anl_flush();
		status = 1;		/* for FTOOLS */
		anl_routine_com(chain[i], &status);
		anl_flush();
		if ( status ) {
			break;
		}
	}

	return status;
}

static int
hd_anl_read_data(int num_chain, int *chain, struct analysis_status *count, int inum_event, int event_freq)
{
#define MAXNEST	8
	double ievent, irecv, num_event;
	int nevent, ichain, eventid, status;
	int num_bgnrun, frq_count, evs_count, nest_level;
	int loop_return[MAXNEST];
	int quit_flag = 0;

/* Call begnrun */
	EvsClrAll();
	anl_profile_reset();
	for (ichain = num_bgnrun = 0; ichain < num_chain; ichain++) {
		status = ANL_OK;
		anl_flush();
		anl_routine_bgnrun(chain[ichain], &status);
		anl_flush();
		num_bgnrun = ichain + 1;
		if ( ANL_QUIT == status ) {
			goto skip;
        } else if ( ANL_ERROR == status ) {
			return status;
        }
    }
	num_event = (double)inum_event;
/* main loop */
	ievent = irecv = 0.0;
	frq_count = 0;
	nest_level = 0;
	anl_profile_reset();
	while ( -1 == inum_event ||
		    (ievent != num_event && irecv != - num_event) ) {
		ievent++;
		nevent = (int)ievent;
		if ( ANL_VERBOSE_EVENT & anl_verbose_level() ) {
			frq_count++;
			if ( 1 == frq_count ) {
				anl_msg_always("Event... %.0f (%.0f)\n", ievent, irecv);
			}
			if ( frq_count == event_freq ) {
				frq_count = 0;
			}
		}
		ichain = 0;
		do {
			evs_count = ANL_TRUE;
			while ( ichain < num_chain ) {
				status = ANL_OK;
				count[ichain].entry++;
				anl_routine_ana(chain[ichain], &nevent, &eventid, &status);
				if ( ANL_OK != status ) {
					if ( ANL_QUIT == status ) {
						status = ANL_ENDLOOP + ANL_DISCARD + ANL_NOCOUNT;
					} else if ( ANL_ERROR == status ) {
						return status;
					} else if ( ANL_SKIP == status ) {
						status = ANL_DISCARD;
					} else if ( ANL_LOOP == status ) {
						status = ANL_NEWROOT;
					}
					if ( status & ANL_NOCOUNT ) {
						evs_count = ANL_FALSE;
					}
					if ( status & ANL_ENDLOOP ) {
						nest_level = 0;
						inum_event = 0;
						num_event = ievent;
					}
					if ( status & ANL_DISCARD ) {
						if ( status & ANL_ENDLOOP ) {
							quit_flag++;
							count[ichain].quit++;
						} else {
							count[ichain].skip++;
						}
						ichain = num_chain;
						break;
					}
					if ( status & ANL_NEWROOT ) {
						loop_return[nest_level] = ichain;
						nest_level++;
					}
				}
				count[ichain].next++;
				ichain++;
			}
			if ( evs_count ) {
				EvsAcm();
			}
			if ( 0 < nest_level ) {
				nest_level--;
				ichain = loop_return[nest_level];
				count[ichain].loop_entry++;
			}
		} while ( ichain < num_chain );
		if ( ichain == num_chain ) {
			irecv++;
		}
    }
 skip:
	if ( 0 == quit_flag ) {
		count[num_chain].quit++;
	}
	anl_profile_reset();
	for (ichain = 0; ichain < num_bgnrun; ichain++) {
		status = ANL_OK;
		anl_flush();
		anl_routine_endrun(chain[ichain], &status);
		anl_flush();
		if ( status ) return status;
	}
	return 0;
}

/*
               PUT: 128
QUIT: 2         |
<--- [ 1] FITSREAD/ASCA_ANL    version 0.74
 		        | OK: 128/128
     [ 2] CIRCREG/ASCA_ANL     version 1.0     <------- LOOP:   0
                | OK: 28/128                   -------> SKIP: 100
     [ 3] GISSPEC/ASCA_ANL     version 4.0
QUIT: 1         | OK: 28/28
<--------      GET: 28
*/
static void
hd_anl_show_status(int num_chain, int *chain, struct analysis_status *count)
{
	int i;
	char msg[256];

	anl_msg_always("\
\n\
     **************************\n\
     ***** Analysis chain *****\n\
     **************************\n\
\n\
      ANL %s\n\
\n", anl_version);
	anl_msg_always("               PUT: %d\n", count[0].entry);
	if ( 0 < count[0].quit ) {
		anl_msg_always("QUIT: %-8d  |\n", count[0].quit);
	} else {
		anl_msg_always("                |\n");
	}
	for (i = 0; i < num_chain; i++) {
		if ( 0 < count[i].quit ) {
			strcpy(msg, "<--- ");
		} else {
			strcpy(msg, "     ");
		}
		sprintf(msg+5, "[%2d] %-28s ", i+1, anl_routine_name(chain[i]));
		if ( 0 < count[i].loop_entry ) {
			anl_msg_always("%s%-16s<------- LOOP: %d\n",
				   msg, anl_routine_version(chain[i]), count[i].loop_entry);
		} else {
			anl_msg_always("%s%s\n", msg, anl_routine_version(chain[i]));
		}
		if ( 0 < count[i+1].quit ) {
			sprintf(msg, "QUIT: %-10d|", count[i+1].quit);
		} else {
			strcpy(msg, "                |");
		}
		sprintf(msg + 17, " OK: %d/%d", count[i].next, count[i].entry);
		if ( 0 < count[i].skip ) {
			anl_msg_always("%-55s-------> SKIP: %d\n", msg, count[i].skip);
		} else {
			anl_msg_always("%s\n", msg);
		}
	}
	if ( 0 < count[num_chain].quit ) {
		strcpy(msg, "<--------- ");
	} else {
		strcpy(msg, "           ");
	}
	anl_msg_always("%s    GET: %d\n", msg, count[num_chain-1].next);
}

static void
hd_anl_reset_status(int num_chain, struct analysis_status *count)
{
	int i;

	for (i = 0; i < num_chain; i++) {
		count[i].entry = 0;
		count[i].loop_entry = 0;
		count[i].next = 0;
		count[i].skip = 0;
		count[i].quit = 0;
	}
	EvsRstAll();;
}

/*
                                  USER      SYSTEM         SUM       FRAC
                                      (s)         (s)         (s)      (%)
 [ 1] FITSREAD/ASCA_ANL        1234567.123 1234567.123 1234567.123   99.99
 [ 2] CIRCREG/ASCA_ANL         1234567.123 1234567.123 1234567.123   99.99
 [ 3] GISSPEC/ASCA_ANL         1234567.123 1234567.123 1234567.123   99.99
      (others)                 1234567.123 1234567.123 1234567.123   99.99
 --------------------------------------------------------------------------
      TOTAL                    1234567.123 1234567.123 1234567.123  100.00
*/
static void
hd_anl_show_profile(int num_chain, int *chain)
{
	static struct {
		double user, system;
	} others, total, prof[MAX_ANALYSIS+1];

	int i;
	double total_sum, sum, frac;

	anl_profile_module(-1, &total.user, &total.system);
	total_sum = total.user + total.system;
	others = total;

	for (i = 0; i < num_chain; i++) {
		anl_profile_module(chain[i], &prof[i].user, &prof[i].system);
		others.user -= prof[i].user;
		others.system -= prof[i].system;
	}

	if ( others.user < 0.0 ) {
		others.user = 0.0;
	}

	if ( others.system < 0.0 ) {
		others.system = 0.0;
	}

	anl_msg_always("\
\n\
     **************************\n\
     **** Analysis profile ****\n\
     **************************\n\
\n\
                                  USER      SYSTEM         SUM     FRAC\n\
                                      (s)         (s)         (s)      (%%)\n\
");

	for (i = 0; i < num_chain; i++) {
		sum = prof[i].user + prof[i].system;
		frac = ( 0.0 == total_sum ) ? 0.0 : ( 100.0 * sum / total_sum );
		anl_msg_always("\
 [%2d] %-24s%12.3f%12.3f%12.3f%8.2f\n",
			   i+1, anl_routine_name(chain[i]),
			   prof[i].user, prof[i].system, sum, frac);
	}
	sum = others.user + others.system;
	frac = ( 0.0 == total_sum ) ? 0.0 : ( 100.0 * sum / total_sum );
	anl_msg_always("\
      %-24s%12.3f%12.3f%12.3f%8.2f\n",
		   "(others)", others.user, others.system, sum, frac);
	anl_msg_always("\
 --------------------------------------------------------------------------\n\
          %-20s%12.3f%12.3f%12.3f%8.2f\n",
		   "TOTAL", total.user, total.system, total_sum, 100.0);
}

static int
hd_anl_body(void)
{
	static int chain[MAX_ANALYSIS];
	static struct analysis_status count[MAX_ANALYSIS+1];
	int i, status, verbose_level, profile_flag, chatter;
	int num_module, num_chain;
	int num_event, event_freq;

/* get ANL HEADAS parameters */

	verbose_level = -1;
	status = PILGetInt("anl_verbose", &verbose_level);
	if ( status ) {
		anl_msg_warning("\
ANL: WARNING: \"anl_verbose\" missing in parameters, anl_verbose=%d assumed\n",
			verbose_level);
	}
	anl_put_verbose_level(verbose_level);

	profile_flag = ANL_YES;
	status = PILGetBool("anl_profile", &profile_flag);
	if ( status ) {
		anl_msg_warning("\
ANL: WARNING: \"anl_profile\" missing in parameters, anl_profile=%s assumed\n",
			profile_flag ? "YES" : "NO");
	}
	anl_put_profile_flag(profile_flag);

	chatter = 2;
	status = PILGetInt("chatter", &chatter);
	if ( status ) {
		anl_msg_warning("\
ANL: WARNING: \"chatter\" missing in parameters, chatter=%d assumed\n",
			chatter);
	}
	anl_put_msg_chatter(chatter);

	num_event = -1;
	status = PILGetInt("num_event", &num_event);
	if ( status ) {
		anl_msg_warning("\
ANL: WARNING: \"num_event\" missing in parameters, num_event=%d assumed\n",
			num_event);
	}

	if ( ANL_VERBOSE_EVENT & anl_verbose_level() ) {
		event_freq = 1000;
		status = PILGetInt("event_freq", &event_freq);
		if ( status ) {
			anl_msg_warning("\
ANL: WARNING: \"event_freq\" missing in parameters, event_freq=%d assumed\n",
				event_freq);
		}
	}

/* print ANL version, when anl_verbose=0 */
	if ( 0 == anl_verbose_level() ) {
		anl_msg_always("\
Built on ANL %s\n", anl_version);
	}

/* Initialize library */

	hbk_shared_file = anl_hbook_info(&hbk_buf_size, &hbk_shared_flag);
	bnk_shared_file = anl_bnk_info(&bnk_buf_size, &bnk_shared_flag);
	evs_shared_file = anl_evs_info(&evs_shared_flag);

/* Com */

	CMinicom("ANL");

/* Initialize HBOOK, BNK, EVS for local memory */

/*	HLIMIT(hbk_buf_size);*/
	status = BnkIni(bnk_buf_size);
	if ( status ) {
		anl_msg_error("\
ANL:: BNK initialization failed.\n");
		return status;
	}
	status = EvsIz();
	if ( status ) {
		anl_msg_error("\
ANL:: EVS initialization failed.\n");
		return status;
	}

/* Call module startup routine */

	num_module = anl_module_num();
	status = 0;
	anl_profile_reset();
	for (i = 0; i < num_module; i++) {
		anl_routine_startup(i, &status);
		if ( status != ANL_OK ) {
			return status;
		}
    }

/* ANL talk menu */

	hd_anl_define_analysis(&num_chain, chain);
	status = hd_anl_modify_param(num_chain, chain);
	if ( status != ANL_OK ) {
		return status;
	}
	if ( ANL_VERBOSE_CHAIN & anl_verbose_level() ) {
		hd_anl_show_analysis(num_chain, chain);
	}

/* initialize analysis modules */

	hd_anl_reset_status(num_chain, count);
	anl_profile_reset();
	for (i = 0; i < num_chain; i++) {
		status = ANL_OK;
		anl_flush();
		anl_routine_init(chain[i], &status);
		anl_flush();
		if ( status ) return status;
    }

/* ANL analysis menu */

	status = hd_anl_read_data(num_chain, chain, count, num_event, event_freq);
	if ( status ) return status;

/* ANL exiting procedure */

	anl_profile_reset();
	for (i = 0; i < num_chain; i++) {
		anl_flush();
		anl_routine_exit(chain[i], &status);
		anl_flush();
		if ( status ) return status;
    }

/* print analysis status */

	if ( ANL_VERBOSE_CHAIN & anl_verbose_level() ) {
		hd_anl_show_status(num_chain, chain, count);
	}

	if ( ANL_VERBOSE_EVS & anl_verbose_level() ) {
		EvsOut();
	}

	if ( ANL_VERBOSE_BNK & anl_verbose_level() ) {
		BnkLst();
	}

	if ( anl_profile_flag() ) {
		hd_anl_show_profile(num_chain, chain);
	}

/* finish BNK, EVS */

	if ( bnk_shared_flag ) {
		BnkShmClose();
	} else {
		BnkEnd();
	}

	if ( evs_shared_flag ) {
		EvsShmClose();
	} else {
		EvsEnd();
	}

	return 0;
}

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

/* C/C++ compatibility. */
#ifdef __cplusplus
extern "C" {
#endif

  /****************************************************************************
   * Function declarations.                                                   *
   ****************************************************************************/
  int headas_init(int argc, char **argv);
  int headas_close(int errNum);
  /****************************************************************************/

/* C/C++ compatibility. */
#ifdef __cplusplus
}
#endif

/* Universal main function. */
int
anl_main(int argc, char *argv[])
{
	static char hist_file[] = ".anl_history";
	int len, status, num_put, exit_status, hist_error;
	char *task_name, *task_version, *task_credits;
	char mode[PIL_LINESIZE];
	char *home_path, *hist_path;

/* set ANL version string */
	anl_version = hd_anl_version;

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
		anl_msg_always(task_credits, task_name, task_version);
	}

	status = headas_init(argc, argv);
	if ( 0 == status ) {
/* Register taskname and version */
		if ( NULL != task_name && '\0' != *task_name ) {
			set_toolname(task_name);
			if ( NULL != task_version && '\0' != *task_version ) {
				set_toolversion(task_version);
			}
		}
		status = PILGetString("mode", mode);
		if ( 0 == status && 0 == strcmp(mode, "anl") ) {
/* ANL mode */
			while ( 1 < argc ) {
				if ( '@' == argv[1][0] ) break;
				argc--;
				argv++;
			}
			push_args(argc, argv);
			status = anl_body();
		} else {
/* HEADAS mode */
			status = hd_anl_body();
		}
		anl_exit_status(&num_put, &exit_status);
		if ( num_put ) {
			status = exit_status;
		}
	}
	status = headas_close(status);	/* if (0 != status), this returns status */

/* write back history file */
	if ( NULL != hist_path ) {
		CLwhis(hist_path, 100, &hist_error);
		/*printf("hist_path=%s, hist_err=%d\n", hist_path, hist_error);*/
		free(hist_path);
	}

	return status;
}
