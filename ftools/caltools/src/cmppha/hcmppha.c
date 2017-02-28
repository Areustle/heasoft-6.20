/*
filename : hcmppha.c
purpose  : c wrapper for host cmppha task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define CMPPHA cmppha_
#endif
#ifdef vms
#define CMPPHA cmppha
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
CMPPHA();
CloseDefaultPF();
return(RETURN);
}
