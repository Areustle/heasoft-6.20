/* $Id: anl_body.c,v 1.22 2007/07/16 17:29:08 ishisaki Exp $
c **********************************************************************
c+++++ This is ASCA_ANL_BODY.FOR
c
c       ASCA Analysis Frame Work
c         By T.Takahashi/Y.Ishisaki
c         [Original Idea is taken from LBL TPC ANALYSIS]
c
c  history:
c    93/9/2 Y. Sekimoto
c        change position of ASCA_ANL_hist_save to ASCA_ANL_exit_prog.
c
c    93/11/7 M.Hirayama
c        modify ASCA_ANL_talk
c
c    93/11/8 M.Hirayama
c        create ASCA_ANL_show_analysis (which displays an analysis chain)
c        create ASCA_ANL_hist_clear (which resets histograms)
c        add close statment after a call to Hrend in ASCA_ANL_hist_save
c        modify ASCA_ANL_dialog (->Ignore routines NOT in the analysis chain)
c        modify ASCA_ANL_hist (->Ignore routines NOT in the analysis chain)
c        modify ASCA_ANL_hist (->Ignore routines which are called before)
c        remove 'HISTOGRAM' command from ASCA_ANL_ana
c        add 'CLEAR_HIST' command to ASCA_ANL_ana
c
c    93/11/13 H.Kubo
c        MAXHBUF 300000->600000
c    93/11/18 H.Kubo
c       dialog message changed
c    93/11/18 H.Kubo
c       position of ASCA_ANL_init_prog -> ASCA_ANL_talk
c       In ASCA_ANL_init_prog, Nprog->Listsp(0)
c    93/12/2  H.Kubo
c       BNK_ini -> BNKini
c       change system flag
c    93/12/5  H.Kubo
c       lun of HBK file is obtainted from ANL_getlun
c    93/12/26  M.Hirayama
c       modify ASCA_ANL_exit_prog (->Ignore routines NOT in the analysis chain)
c       set current_index in ASCA_ANL_init_prog
c       set current_index in ASCA_ANL_exit_prog
c    94/3/29  H.Kubo
c       put version 0.4 -> 0.5
c    94/06/26 M.Hirayama
c        MAXHBUF 600000->1200000
c    94/07/15 H.Kubo
c        MAXBUF 10000 -> 120000
c    94/09/23 Y.Ishsiaki
c       lun of HBK file is released by ANL_freelun
c       save histograms in subdirectories
c    94/11/07 Y.Ishisaki
c       include asca_anl_def.inc
c    94/11/13 Y.Ishisaki
c       user can define size of BNK and/or HBOOK in mkanlinit
c **********************************************************************

	1997/02/24	Y.ISHISAKI
		rewrite to C for ASTE_ANL

	1997/05/07	Y.ISHISAKI
		call BnkShmClose/End & EvsShmClose/End on exit
		call BnkShmOpen when share file start with '<'
		call EvsShmOpen when share file start with '<'

	1997/05/19	Y.ISHISAKI
		anl_read_data(): bug fix on num_bgnrun
		endrun of last module was not called before, pointed by K.Hayasida

	1998/04/13	Y.ISHISAKI
		*status = 0 before calling USER_com

	1998/07/22	Y.ISHISAKI
		check recv count

	1999/07/02	Y.ISHISAKI
		move calling startup after BNK/EVS/HBOOK init

	2003/09/29	Y.ISHISAKI
		size of hbk_shared_file, bnk_shared_file, evs_shared_file 256 -> 1024
		check shared_flags after anl_talk_menu() in anl_body()
		use HROPEN instead of HRFILE in anl_save_hist()

	2004/02/06 Y.ISHISAKI	version 1.40
		refer anl_task_name, anl_task_version, anl_task_credits
		anl_hbook/bnk/evs_info() functions changed,
		and hbk/bnk/evs_shared_file are now pointers to char (char *).
		remove several unused variables
		call CMinicom instead of INIT_COM_TALK
		call CMinauir instead of INQUIRE for MacOSX

	2004/06/06 Y.ISHISAKI	version 1.50
		change function name of anl_body_sub() -> anl_body()
		original anl_body(argc,argv) -> anl_main() and split out to anl_main.c
		bug fix on shared menu, using hbk/bnk/evs_filename_buf
		call anl_getlun/freelun instead of ANL_GETLUN/FREELUN
		call CMinauir/txtrd/intrd/logrd instead of INQUIRE/TXTRD/INTRD/LOGRD
		return 0 in anl_body() when manually quitting from anl_talk_menu()
		print ANL version on SHOW_ANALYSIS

	2004/07/06 Y.ISHISAKI	version 1.51
		static declaration of chpath & chopt for HCDIR() in anl_save_hist()

	2005/02/17 Y.ISHISAKI	version 1.60
		add help[] in anl_talk_menu(), anl_ana_menu()
		add VERBOSE_LEVEL menu in anl_talk_menu(), anl_ana_menu()
		refer anl_verbose_level() for EVENT/CHAIN/EVS/BNK messages
		add PROFILE_MODULE menu in anl_talk_menu()
		add anl_set_verbose_level(), anl_set_profile_flag(), anl_show_profile()
		call anl_profile_reset() in appropreate locations
		print module flags in anl_show_analysis(), change BOOKED -> [BOOKED]
		static declaration of functions except anl_body()
		change "char anl_version[] = .."  -> "char *anl_version = .."
		call anl_show_status(), after anl_routine_exit(), before BNK/EVS end
		remove anl_exit_proc()

	2005/11/30 Y.ISHISAKI	version 1.70
		use anl_msg_error(), anl_msg_warning(), anl_msg_always()
		add CHATTER menu in anl_talk_menu(), anl_ana_menu()
		add anl_set_chatter()
		allow longer module names in anl_show_profile()
		show chatter in anl_show_analysis()
		line oriented messaging with char msg[256] in anl_show_status()

	2007/02/02 Y.ISHISAKI	version 1.73
		int -> double ievent, irecv in anl_read_data()

	2007/04/30 Y.ISHISAKI	version 1.80
		return status in anl_read_data()
		check ANL_ERROR for _bgnrun() in anl_read_data()
		check status for _endrun() in anl_read_data()

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
#include "cfortran.h"
#include "hbook.h"

char *anl_version = ANL_VERSION;

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
static char *hbk_shared_file, hbk_filename_buf[256];
static char *bnk_shared_file, bnk_filename_buf[256];
static char *evs_shared_file, evs_filename_buf[256];

static void
anl_define_analysis(int *num_chain_p, int *chain)
{
	static char *table[MAX_ANALYSIS+2];
	static int ichoice[MAX_ANALYSIS+3];
	int i, num_chain, num_module;

/* Build the list of entries */
	num_module = anl_module_num();
	if ( MAX_ANALYSIS < num_module ) {
		anl_msg_warning("\
ANL:: Too MANY analysis modules (%d modules).\n\
ANL:: Trancating to %d.\n", num_module, MAX_ANALYSIS);
		num_module = MAX_ANALYSIS;
    }
	for (i = 0; i < num_module; i++) {
		table[i] = anl_routine_name(i);
    }
	table[num_module] = "NONE";
	table[num_module+1] = "FULL";
/* ask the question */
	CMinquir("ANL *** Define Analysis ***",
			num_module+2, table, table, -1, ichoice);
/* put the answer in chain[] */
	if ( 0 == ichoice[0] ) return;
	num_chain = 0;
	for (i = 0; i < ichoice[0]; i++) {
		int imodule = ichoice[i+1] - 1;
		char *answer = table[imodule];
		if ( 0 == strcmp("NONE", answer) ) {
			num_chain = 0;
			break;
        } else if ( 0 == strcmp("FULL", answer) ) {
			num_chain = num_module;
			for (i = 0; i < num_module; i++) {
				chain[i] = i;
            }
			break;
        } else {
			num_chain++;
			chain[i] = imodule;
        }
    }
	*num_chain_p = num_chain;
}

static void
anl_show_analysis(int num_chain, int *chain, int *book_flag)
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

			if ( book_flag[i] ) {
				anl_msg_always("\
%-16s [BOOKED]\n", anl_routine_version(chain[i]));
			} else {
				anl_msg_always("\
%s\n", anl_routine_version(chain[i]));
			}
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

static void
anl_modify_param(int num_chain, int *chain)
{
	static char *table[MAX_ANALYSIS+2];
	static int ichoice[MAX_ANALYSIS+3];
	int i, status;

/* Build the list of entries */
	for (i = 0; i < num_chain; i++) {
		table[i] = anl_routine_name(chain[i]);
    }
	table[num_chain] = "NONE";
	table[num_chain+1] = "FULL";
/* Ask the question */
	CMinquir("ANL *** Modify Parameter ***",
			num_chain+2, table, table, -1, ichoice);
	for (i = 0; i < ichoice[0]; i++) {
		int ichain = ichoice[i+1] - 1;
		char *answer = table[ichain];
         if ( 0 == strcmp("NONE", answer) ) {
			 break;
         } else if ( 0 == strcmp("FULL", answer) ) {
			 for (i = 0; i < num_chain; i++) {
				 anl_flush();
				 status = 0;	/* for ANL */
				 anl_routine_com(chain[i], &status);
				 anl_flush();
             }
			 break;
         } else {
			 anl_flush();
			 status = 0;		/* for ANL */
			 anl_routine_com(chain[ichain], &status);
			 anl_flush();
         }
    }
}

static void
hd_anl_modify_param(int num_chain, int *chain)
{
	int i, status;

	anl_profile_reset();
	for (i = 0; i < num_chain; i++) {
		anl_flush();
		status = 1;		/* for FTOOLS */
		anl_routine_com(chain[i], &status);
		anl_flush();
	}
}

static void
anl_book_histogram(int num_chain, int *chain, int *book_flag)
{
	static char *table[MAX_ANALYSIS+2];
	static int ichoice[MAX_ANALYSIS+3];
	int i;

/* Build the list of entries */
	for (i = 0; i < num_chain; i++) {
		table[i] = anl_routine_name(chain[i]);
    }
	table[num_chain] = "NONE";
	table[num_chain+1] = "FULL";
/* Ask the question */
	CMinquir("ANL *** Book Histogram ***",
			num_chain+2, table, table, -1, ichoice);
	for (i = 0; i < ichoice[0]; i++) {
		int ichain = ichoice[i+1] - 1;
		char *answer = table[ichain];
		if ( 0 == strcmp("NONE", answer) ) {
			 for (i = 0; i < num_chain; i++) {
				 book_flag[i] = ANL_FALSE;
             }
			 break;
         } else if ( 0 == strcmp("FULL", answer) ) {
			 for (i = 0; i < num_chain; i++) {
				 book_flag[i] = ANL_TRUE;
             }
			 break;
         } else {
			 book_flag[ichain] = ANL_TRUE;
         }
    }
}

static void
anl_share_memory(void)
{
#define NTABLE	5
	static char *table[NTABLE] = {
		"SHOW", "HBOOK", "BNK", "EVS", "EXIT"
	};
	static int ichoice[2];
	char *answer;

	for (;;) {
		/* Ask the question */
		CMinquir("ANL *** Share Memory ***", NTABLE, table, table, 1, ichoice);
#undef NTABLE
		answer = table[ichoice[1]-1];
		if ( 0 == strcmp("SHOW", answer) ) {
			anl_msg_always("\n");
			if ( hbk_shared_flag ) {
				anl_msg_always(" HBOOK : Shared ('%s')\n", hbk_shared_file);
			} else {
				anl_msg_always(" HBOOK : NOT Shared\n");
			}
			if ( bnk_shared_flag ) {
				anl_msg_always("  BNK  : Shared ('%s')\n", bnk_shared_file);
			} else {
				anl_msg_always("  BNK  : NOT Shared\n");
			}
			if ( evs_shared_flag ) {
				anl_msg_always("  EVS  : Shared ('%s')\n", evs_shared_file);
			} else {
				anl_msg_always("  EVS  : NOT Shared\n");
			}
		} else if ( 0 == strcmp("HBOOK", answer) ) {
			CLlogrd("Share HBOOK", &hbk_shared_flag);
			if ( hbk_shared_flag ) {
				CLtxtrd("Shared HBOOK file name",
						hbk_filename_buf, sizeof(hbk_filename_buf));
				hbk_shared_file = hbk_filename_buf;
				/*HLIMAP(hbk_buf_size, hbk_shared_file);*/
			} else {
				/*HLIMIT(hbk_buf_size);*/
			}
		} else if ( 0 == strcmp("BNK", answer) ) {
			CLlogrd("Share BNK", &bnk_shared_flag);
			if ( bnk_shared_flag ) {
				CLtxtrd("Shared BNK file name",
						bnk_filename_buf, sizeof(bnk_filename_buf));
				bnk_shared_file = bnk_filename_buf;
				/*BnkShmCreate(bnk_buf_size, bnk_shared_file);*/
			} else {
				/*BnkIni(bnk_buf_size);*/
			}
		} else if ( 0 == strcmp("EVS", answer) ) {
			CLlogrd("Share EVS", &evs_shared_flag);
			if ( evs_shared_flag ) {
				CLtxtrd("Shared EVS file name",
						evs_filename_buf, sizeof(evs_filename_buf));
				evs_shared_file = evs_filename_buf;
				/*EvsShmCreate(evs_shared_file);*/
			} else {
				/*EvsIz();*/
			}
		} else {
			break;
		}
	}
}

static void
anl_set_verbose_level(void)
{
	int verbose = anl_verbose_level();
	anl_msg_always("\
 Current VERBOSE_LEVEL = %d  (-1: Full, 0: Minimum)\n\
    Modifiers   +1: show event count up\n\
                +2: show analyais chain\n\
                +4: show EVS results\n\
                +8: show BNK list\n", verbose);
	CLintrd("VERBOSE_LEVEL", &verbose);
	anl_put_verbose_level(verbose);
}

static void
anl_set_profile_flag(void)
{
	int flag = anl_profile_flag();
	anl_msg_always("\
 Current PROFILE_MODULE = %s\n", flag ? "YES" : "NO");
	CLlogrd("Enable Profiling", &flag);
	anl_put_profile_flag(flag);
}

static void
anl_set_chatter(void)
{
	int chatter = anl_msg_chatter();
	anl_msg_always("\
 Current CHATTER [MESSAGE LEVEL] = %d  (0:min, 2:norm, 5:max)\n\
    MESSAGE LEVEL 0: only errors\n\
    MESSAGE LEVEL 1: + warnings\n\
    MESSAGE LEVEL 2: + information messages\n\
    MESSAGE LEVEL 3: + debug messages\n\
    MESSAGE LEVEL 4: + more debug messages\n\
    MESSAGE LEVEL 5: + maximum debug messages\n", chatter);
	CLintrd("CHATTER", &chatter);
	anl_put_msg_chatter(chatter);
}

static void
anl_talk_menu(int *num_chain_p, int *chain, int *book_flag, int *status)
{
#define NTABLE	11
	static char *table[NTABLE] = {
		"DEFINE_ANALYSIS",
		"SHOW_ANALYSIS",
		"MODIFY_PARAM",
		"BOOK_HISTOGRAM",
		"ANALYZE_DATA",
		"SHARE_MEMORY",
		"VERBOSE_LEVEL",
		"PROFILE_MODULE",
		"FTOOLS_PFILE",
		"CHATTER",
		"QUIT"
	};
	static char *help[NTABLE] = {
		"define analysis chain",
		"show analysis chain",
		"modify module parameters",
		"set/unset BOOK flag for each module",
		"enter the data analysis",
		"shared memory settings",
		"set ANL verbose level",
		"profile time consumption of each module",
		"read ftools parameter interface file",
		"set message level (0:min, 2:norm, 5:max)",
		"quit ANL"
	};
	char *answer;
	int ichoice[2];
	int num_chain = 0;

	for (;;) {
		CMinicom("ANL");
		CMinquir("ANL *** Talk Menu ***", NTABLE, table, help, 1, ichoice);
#undef NTABLE
		answer = table[ichoice[1]-1];
		if ( 0 == strcmp("DEFINE_ANALYSIS", answer) ) {
			CMinicom("DEF");
			anl_define_analysis(&num_chain, chain);
		} else if ( 0 == strcmp("SHOW_ANALYSIS", answer) ) {
			anl_show_analysis(num_chain, chain, book_flag);
		} else if ( 0 == strcmp("SHARE_MEMORY", answer) ) {
			CMinicom("SHM");
			anl_share_memory();
		} else if ( 0 == strcmp("VERBOSE_LEVEL", answer) ) {
			anl_set_verbose_level();
		} else if ( 0 == strcmp("PROFILE_MODULE", answer) ) {
			anl_set_profile_flag();
		} else if ( 0 == strcmp("CHATTER", answer) ) {
			anl_set_chatter();
		} else if ( 0 == strcmp("QUIT", answer) ) {
			*status = ANL_NG;
			return;
		} else if ( 0 == num_chain ) {
			anl_msg_error("\
\n\
ANL:: Analysis chain NOT defined.\n\
ANL:: Define analysis chain first.\n");
		} else if ( 0 == strcmp("MODIFY_PARAM", answer) ) {
			CMinicom("MOD");
			anl_modify_param(num_chain, chain);
		} else if ( 0 == strcmp("FTOOLS_PFILE", answer) ) {
			hd_anl_modify_param(num_chain, chain);
		} else if ( 0 == strcmp("BOOK_HISTOGRAM", answer) ) {
			CMinicom("BOK");
			anl_book_histogram(num_chain, chain, book_flag);
		} else if ( 0 == strcmp("ANALYZE_DATA", answer) ) {
			*status = ANL_OK;
			break;
		}
	}
	*num_chain_p = num_chain;
}

static int
anl_read_data(int num_chain, int *chain, struct analysis_status *count)
{
#define MAXNEST	8
	static int inum_event = -1;
	static int event_freq = 1000;
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
/* Ask event parameter */
	CLintrd("Number of events (-1=all,0=exit,-N:recv)", &inum_event);
	if ( 0 == inum_event ) {
		goto skip;
	}
	num_event = (double)inum_event;
	if ( ANL_VERBOSE_EVENT & anl_verbose_level() ) {
		CLintrd("Event number printout frequency", &event_freq);
	}
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

static void
anl_save_hist(void)
{
#define MAXDIR	32
	static char quest[256], fn[256] = "histfile.hbk";
	static char chpath[] = "//PAWC";
	static char chopt[] = " ";
	static char chopt2[] = "NP";
	static char chopt3[] = "T";
	static char chtop[] = "ROOT";
	static char dirs[MAXDIR][32];
	int i, istat, lrec, icycle, status, ndir;
	int lun, reply;

/* Ask file name */
	CLtxtrd("File Name:", fn, sizeof(fn));
	if ( 0 == access(fn, F_OK) ) {
		/* already exists */
		sprintf(quest, "'%s' already exists, delete", fn);
		reply = ANL_TRUE;
		CLlogrd(quest, &reply);
		if ( reply ) {
			unlink(fn);
		} else {
			return;
		}
	}
/* Save histogram */
	HCDIR(chpath, chopt);
	HRDIR(MAXDIR, dirs, ndir);
	if ( MAXDIR < ndir ) {
		anl_msg_error("\
ANL:: Too MANY subdirectories (ndir=%d).\n", ndir);
		ndir = MAXDIR;
    }
	anl_getlun(&lun);
	lrec = 1024;
	HROPEN(lun, chtop, fn, chopt2, lrec, istat);
	if ( istat ) {
		anl_msg_error("\
ANL:: Can not Open file in ANL save histogram.\n");
		return;
    }
	for (i = 0; i < ndir; i++) {
		char dirname[32];
		sprintf(dirname, "//ROOT/%s", dirs[i]);
		HMDIR(dirname, chopt);
    }
	HROUT(0, icycle, chopt3);	/* Write whole directory tree */
	HREND(chtop);
	CLclos(lun, &status);
	anl_freelun(lun);
}

static void
anl_clear_hist(void)
{
	static char chopt[] = " ";
	int i, hid;
	int num_clear = 0;

	CLintrd("How many histograms (0=all, -1=none)", &num_clear);
	if ( 0 == num_clear ) {
		anl_msg_always("ANL:: All histogram cleared.\n");
		HRESET(0, chopt);
    } else if ( num_clear < 0 ) {
		anl_msg_always("ANL:: No histogram cleared.\n");
    } else {
		hid = 0;
		for (i = 0; i < num_clear; i++) {
			CLintrd("Histogram ID", &hid);
			HRESET(hid, chopt);
			anl_msg_always("ANL:: Histogram cleared. (ID =%d)\n", hid);
        }
    }
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
anl_show_status(int num_chain, int *chain, struct analysis_status *count)
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
anl_reset_status(int num_chain, struct analysis_status *count)
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
anl_show_profile(int num_chain, int *chain)
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

static void
anl_ana_menu(int num_chain, int *chain, struct analysis_status *count)
{
#define NTABLE	8
	static char *table[NTABLE] = {
		"READ_DATA",
		"SAVE_HIST",
		"CLEAR_HIST",
		"SHOW_STATUS",
		"RESET_STATUS",
		"VERBOSE_LEVEL",
		"CHATTER",
		"EXIT"
	};
	static char *help[NTABLE] = {
		"read specified data",
		"save HBOOK histograms",
		"clear HBOOK histograms",
		"show ANL flow count, EVS, and BNK",
		"reset ANL flow count and EVS",
		"set ANL verbose level",
		"set message level (0:min, 2:norm, 5:max)",
		"exit from this menu"
	};
	char *answer;
	int ichoice[2], status;

	for (;;) {
		CMinquir("ANL *** Analysis Menu ***", NTABLE, table, help, 1, ichoice);
#undef NTABLE
		answer = table[ichoice[1]-1];
		if ( 0 == strcmp("READ_DATA", answer) ) {
			status = anl_read_data(num_chain, chain, count);
			if ( status ) {
				anl_msg_error("\
ANL:: READ_DATA failed (status=%d).\n", status);
			}
        } else if ( 0 == strcmp("SAVE_HIST", answer) ) {
			anl_save_hist();
        } else if ( 0 == strcmp("CLEAR_HIST", answer) ) {
			anl_clear_hist();
        } else if ( 0 == strcmp("SHOW_STATUS", answer) ) {
			anl_show_status(num_chain, chain, count);
			EvsOut();
			BnkLst();
			if ( anl_profile_flag() ) {
				anl_show_profile(num_chain, chain);
			}
        } else if ( 0 == strcmp("RESET_STATUS", answer) ) {
			anl_reset_status(num_chain, count);
		} else if ( 0 == strcmp("VERBOSE_LEVEL", answer) ) {
			anl_set_verbose_level();
		} else if ( 0 == strcmp("CHATTER", answer) ) {
			anl_set_chatter();
        } else if ( 0 == strcmp("EXIT", answer) ) {
			break;
        }
    }
}

int
anl_body(void)
{
	static int chain[MAX_ANALYSIS];
	static int book_flag[MAX_ANALYSIS];
	static struct analysis_status count[MAX_ANALYSIS+1];
	int i, status, booked;
	int num_module, num_chain;

/* Initialize library */

	hbk_shared_file = anl_hbook_info(&hbk_buf_size, &hbk_shared_flag);
	strncpy(hbk_filename_buf, hbk_shared_file, sizeof(hbk_filename_buf));
	bnk_shared_file = anl_bnk_info(&bnk_buf_size, &bnk_shared_flag);
	strncpy(bnk_filename_buf, bnk_shared_file, sizeof(bnk_filename_buf));
	evs_shared_file = anl_evs_info(&evs_shared_flag);
	strncpy(evs_filename_buf, evs_shared_file, sizeof(evs_filename_buf));

/* Com */

	CMinicom("ANL");

/* Initialize HBOOK, BNK, EVS for local memory */

	HLIMIT(hbk_buf_size);
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
		if ( status ) return status;
    }

/* ANL talk menu */

	anl_talk_menu(&num_chain, chain, book_flag, &status);
	if ( status ) {
		anl_msg_always("\
Quit from ANL...\n");
		return 0;
	}

/* check shared flags for HBOOK, BNK, EVS */

	if ( hbk_shared_flag ) {
		HLIMAP(hbk_buf_size, hbk_shared_file);
	}
	if ( bnk_shared_flag ) {
		if ( '<' == *bnk_shared_file ) {
			status = BnkShmOpen(bnk_shared_file+1);
			if ( status ) {
				anl_msg_error("\
BnkShmOpen: '%s' open error\n", bnk_shared_file+1);
			}
		} else if ( '>' == *bnk_shared_file ) {
			status= BnkShmCreate(bnk_buf_size, bnk_shared_file+1);
		} else {
			status = BnkShmCreate(bnk_buf_size, bnk_shared_file);
		}
		if ( status ) {
			anl_msg_error("\
ANL:: BNK initialization failed.\n");
			return status;
		}
	}
	if ( evs_shared_flag ) {
		if ( '<' == *evs_shared_file ) {
			status = EvsShmOpen(evs_shared_file+1);
			if ( status ) {
				anl_msg_error("\
EvsShmOpen: '%s' open error\n", evs_shared_file+1);
			}
		} else if ( '>' == *evs_shared_file ) {
			status = EvsShmCreate(evs_shared_file+1);
		} else {
			status = EvsShmCreate(evs_shared_file);
		}
		if ( status ) {
			anl_msg_error("\
ANL:: EVS initialization failed.\n");
			return status;
		}
	}

/* initialize analysis modules */

	if ( ANL_VERBOSE_CHAIN & anl_verbose_level() ) {
		anl_show_analysis(num_chain, chain, book_flag);
	}
	anl_reset_status(num_chain, count);
	anl_profile_reset();
	for (i = 0; i < num_chain; i++) {
		status = ANL_OK;
		anl_flush();
		anl_routine_init(chain[i], &status);
		anl_flush();
		if ( status ) return status;
		if ( book_flag[i] ) {
			anl_flush();
			anl_routine_his(chain[i], &status);
			anl_flush();
			if ( status ) return status;
        }
    }

/* ANL analysis menu */

	anl_ana_menu(num_chain, chain, count);

/* ANL exiting procedure */

	booked = ANL_FALSE;
	anl_profile_reset();
	for (i = 0; i < num_chain; i++) {
		anl_flush();
		anl_routine_exit(chain[i], &status);
		anl_flush();
		if ( status ) return status;
		if ( book_flag[i] ) {
			booked = ANL_TRUE;
        }
    }

	if ( booked ) {
		int reply = ANL_YES;
		CLlogrd("Save histogram", &reply);
		if ( reply ) {
			anl_save_hist();
        }
    }

/* print analysis status */

	if ( ANL_VERBOSE_CHAIN & anl_verbose_level() ) {
		anl_show_status(num_chain, chain, count);
	}

	if ( ANL_VERBOSE_EVS & anl_verbose_level() ) {
		EvsOut();
	}

	if ( ANL_VERBOSE_BNK & anl_verbose_level() ) {
		BnkLst();
	}

	if ( anl_profile_flag() ) {
		anl_show_profile(num_chain, chain);
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
