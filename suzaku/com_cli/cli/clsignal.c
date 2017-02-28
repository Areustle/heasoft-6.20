/* $Id: clsignal.c,v 1.4 2005-02-20 10:02:50 ishisaki Exp $
  File: CLsignal.c
  Description: signal handler for CLI
  Author: Y. ISHISAKI
  Date: 05-Jun-1998

  History:
	05-Jun-1998 Y.ISHISAKI, moved from miscunix.f
*/

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#define NSIGTBL	32
#define True	(1==1)
#define False	(!True)

static struct {
	int flag;
	int stat;
	struct sigaction o_action;
} sigtbl[NSIGTBL];

void
clsigg_(signo, status)
  int *signo, *status;
{
	*status = ( 0 <= *signo && *signo < NSIGTBL ) ? sigtbl[*signo].stat : 0;
}

void
clsigp_(signo, status)
  int *signo, *status;
{
	if ( 0 <= *signo && *signo < NSIGTBL ) {
		sigtbl[*signo].stat = *status;
    }
}

int
clsigc_(signo)
  int *signo;
{
	int status;
	clsigg_(signo, &status);
	return status;
}

void
clsigs_(signo)
  int *signo;
{
	int status = True;
	clsigp_(signo, &status);
}

void
clsigr_(signo)
  int *signo;
{
	int status = False;
	clsigp_(signo, &status);
}

void
clsigh_(signo)
  int signo;
{
/*	printf("gaaa!!\n");*/
	clsigs_(&signo);
}

void
clsigf_(signo, handler)
  int *signo;
  void (*handler)();
{
	if ( 0 <= *signo && *signo < NSIGTBL ) {
		struct sigaction action;
		action.sa_handler = handler;
		sigemptyset(&action.sa_mask);
#ifdef SA_RESTART
		action.sa_flags = SA_RESTART;
#else
		action.sa_flags = 0;
#endif
		if ( 0 == sigaction(*signo, &action, &sigtbl[*signo].o_action) ) {
			sigtbl[*signo].flag = 1;
		}
	}
}

void
clsigi_(signo)
  int *signo;
{
	clsigf_(signo, clsigh_);
}

void
clsigx_(signo)
  int *signo;
{
	if ( 0 <= *signo && *signo < NSIGTBL && sigtbl[*signo].flag ) {
		struct sigaction action;
		sigaction(*signo, &sigtbl[*signo].o_action, &action);
		sigtbl[*signo].flag = 0;
	}
}
