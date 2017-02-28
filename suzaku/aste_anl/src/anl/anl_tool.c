/* $Id: anl_tool.c,v 1.5 2005/02/23 06:59:49 ishisaki Exp $
c anl_tool.c
c     Includes following routines:
c        anl_put_module_num, anl_module_num, anl_current_module,
c        anl_put_verbose_level, anl_verbose_level, anl_put_module_list,
c        anl_routine_name, anl_routine_version, anl_routine_flag,
c        anl_routine_startup, anl_routine_init,
c        anl_routine_com, anl_routine_his,
c        anl_routine_bgnrun, anl_routine_ana,
c        anl_routine_endrun, anl_routine_exit,
c        anl_put_version, anl_getlun, anl_freelun
c        anl_put_profile_flag, anl_profile_flag,
c        anl_profile_reset, anl_profile_update, anl_profile_module

	2004/06/06 Y.ISHISAKI	version 1.50
		add anl_put_version, anl_getlun, anl_freelun

	2005/02/17 Y.ISHISAKI	version 1.60
		stop using "ctype.h" for RedHat9 object compatibility,
			use CLstrnicmp() instead
		add "int flag" in anl_module_list
		add anl_routine_flag(), anl_put_verbose_level(), anl_verbose_level()
		add anl_put_profile_flag(), anl_profile_flag(),
			anl_profile_update(), anl_profile_reset(), anl_profile_module()
		add anl_current_module()
		stop including "cfortran.h" for anl_getlun(), anl_freelun()
		anl_put_module_num(), anl_module_num() moved from "anl_task.c"

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "cli.h"
#include "anl_def.h"
#include "anl_misc.h"

typedef void (*FUNC)();

static struct {
	FUNC startup;
	FUNC init;
	FUNC com;
	FUNC his;
	FUNC bgnrun;
	FUNC ana;
	FUNC endrun;
	FUNC exit;
	char *name;
	char *version;
	int flag;		/* 0:normal, '-':hidden module for headas */
} *module_list = NULL;

static int num_module = 0;
static int anl_current = -1;	/* ANL current module number */
static int anl_verbose = -1;	/* ANL verbose level, -1:full, 0:minimum */
static int anl_profile = 0;		/* ANL profile flag */
static struct rusage last_rusage;

static struct anl_module_prof_struct {
	struct timeval utime;		/* user time used */
	struct timeval stime;		/* system time used */
} module_prof[MAX_ANALYSIS];

void
anl_put_module_num(int num)
{
	num_module = num;
}

int
anl_module_num(void)
{
	return num_module;
}

int
anl_current_module(void)
{
	return anl_current;
}

void
anl_put_verbose_level(int verbose)
{
	anl_verbose = verbose;
}

int
anl_verbose_level(void)
{
	return anl_verbose;
}

void
anl_put_module_list(void *p)
{
	module_list = p;
}

char *
anl_routine_name(int imodule)
{
	return module_list[imodule].name;
}

char *
anl_routine_version(int imodule)
{
	return module_list[imodule].version;
}

int
anl_routine_flag(int imodule)
{
	return module_list[imodule].flag;
}

#define ANL_ROUTINE(imodule, ext, args) \
	FUNC f = module_list[imodule].ext;\
	if ( f ) {\
		anl_current = imodule;\
		(*f) args;\
		anl_current = -1;\
		if ( anl_profile ) {\
			anl_profile_update(imodule);\
		}\
	}

void
anl_routine_startup(int imodule, int *status)
{
	ANL_ROUTINE(imodule, startup, (status))
}

void
anl_routine_init(int imodule, int *status)
{
	ANL_ROUTINE(imodule, init, (status))
}

void
anl_routine_com(int imodule, int *status)
{
	ANL_ROUTINE(imodule, com, (status))
}

void
anl_routine_his(int imodule, int *status)
{
	ANL_ROUTINE(imodule, his, (status))
}

void
anl_routine_bgnrun(int imodule, int *status)
{
	ANL_ROUTINE(imodule, bgnrun, (status))
}

void
anl_routine_ana(int imodule, int *nevent, int *eventid, int *status)
{
	ANL_ROUTINE(imodule, ana, (nevent, eventid, status))
}

void
anl_routine_endrun(int imodule, int *status)
{
	ANL_ROUTINE(imodule, endrun, (status))
}

void
anl_routine_exit(int imodule, int *status)
{
	ANL_ROUTINE(imodule, exit, (status))
}

void
anl_put_version_(char *name, char *version, int name_len, int version_len)
{
	int i;

	for (i = 0; i < num_module; i++) {
		char *module_name = module_list[i].name;
		if ( 0 == CLstrnicmp(name, module_name, name_len) ) {
			if ( '\0' == module_name[name_len] ||
				  '/' == module_name[name_len] ) {
				module_list[i].version = malloc(version_len+1);
				if ( module_list[i].version ) {
					strncpy(module_list[i].version, version, version_len);
					module_list[i].version[version_len] = '\0';
				} else {
					module_list[i].version = version;
				}
			}
		}
	}
}

void
anl_put_version(char *name, char *version)
{
	anl_put_version_(name, version, strlen(name), strlen(version));
}

void
anl_getlun(int *lun)
{
	extern void anl_getlun_(int *);
	anl_getlun_(lun);
}

void
anl_freelun(int lun)
{
	extern void anl_freelun_(int *);
	anl_freelun_(&lun);
}

void
anl_put_profile_flag(int flag)
{
	anl_profile = flag;
}

int
anl_profile_flag(void)
{
	return anl_profile;
}

void
anl_profile_reset(void)
{
	if ( anl_profile ) {
		getrusage(RUSAGE_SELF, &last_rusage);
	}
}

void
anl_profile_update(int imodule)
{
	struct rusage cur_rusage;
	struct timeval dt;
	struct anl_module_prof_struct *p = &module_prof[imodule];

/* get resource usage */
	getrusage(RUSAGE_SELF, &cur_rusage);

/* update user time usage */
	dt.tv_sec  = cur_rusage.ru_utime.tv_sec  - last_rusage.ru_utime.tv_sec;
	dt.tv_usec = cur_rusage.ru_utime.tv_usec - last_rusage.ru_utime.tv_usec;
	p->utime.tv_sec += dt.tv_sec;
	p->utime.tv_usec += dt.tv_usec;
	while ( p->utime.tv_usec < 0 ) {
		p->utime.tv_usec += 1000000;
		p->utime.tv_sec--;
	}
	while ( 1000000 < p->utime.tv_usec ) {
		p->utime.tv_usec -= 1000000;
		p->utime.tv_sec++;
	}

/* update system time usage */
	dt.tv_sec  = cur_rusage.ru_stime.tv_sec  - last_rusage.ru_stime.tv_sec;
	dt.tv_usec = cur_rusage.ru_stime.tv_usec - last_rusage.ru_stime.tv_usec;
	p->stime.tv_sec += dt.tv_sec;
	p->stime.tv_usec += dt.tv_usec;
	while ( p->stime.tv_usec < 0 ) {
		p->stime.tv_usec += 1000000;
		p->stime.tv_sec--;
	}
	while ( 1000000 < p->stime.tv_usec ) {
		p->stime.tv_usec -= 1000000;
		p->stime.tv_sec++;
	}

/* update last_rusage user & system time */
	last_rusage.ru_utime = cur_rusage.ru_utime;
	last_rusage.ru_stime = cur_rusage.ru_stime;
}

void
anl_profile_module(int imodule, double *utime, double *stime)
{
	if ( -1 == imodule ) {

/* calculate for all, including ANL itself */
		anl_profile_reset();

		*utime = last_rusage.ru_utime.tv_sec;
		*utime += 1.0e-6 * last_rusage.ru_utime.tv_usec;

		*stime = last_rusage.ru_stime.tv_sec;
		*stime += 1.0e-6 * last_rusage.ru_stime.tv_usec;

		return;

	} else if ( imodule < 0 || num_module <= imodule ) {

/* invalid module number */
		*utime = 0.0;
		*stime = 0.0;
		return;

	}

	*utime = module_prof[imodule].utime.tv_sec;
	*utime += 1.0e-6 * module_prof[imodule].utime.tv_usec;

	*stime = module_prof[imodule].stime.tv_sec;
	*stime += 1.0e-6 * module_prof[imodule].stime.tv_usec;
}
