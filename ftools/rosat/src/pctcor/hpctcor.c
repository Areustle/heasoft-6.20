/*
filename : hpctcor.c
purpose  : c wrapper for host pctcor task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define PCTCOR pctcor_
#endif
#ifdef vms
#define PCTCOR pctcor
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv);
PCTCOR();
CloseDefaultPF();
return(RETURN);
}
