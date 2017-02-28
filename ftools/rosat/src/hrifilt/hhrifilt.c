/*
filename : hhrifilt.c
purpose  : c wrapper for host hrifilt task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define HRIFILT hrifit_
#endif
#ifdef vms
#define HRIFILT hrifit
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
HRIFILT();
CloseDefaultPF();
return(RETURN);
}
