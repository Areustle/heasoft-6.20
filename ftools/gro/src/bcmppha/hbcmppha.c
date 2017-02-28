/*
filename : hbcmppha.c
purpose  : c wrapper for host cmppha task
author   : Rehana Yusaf
modified for bcmppha
*/
#include <stdio.h>
#ifdef unix
#define BCMPPHA bcmppha_
#endif
#ifdef vms
#define BCMPPHA bcmppha
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
BCMPPHA();
CloseDefaultPF();
return(RETURN);
}
