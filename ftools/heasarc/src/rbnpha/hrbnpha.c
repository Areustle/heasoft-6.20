/*
filename : hrbnpha.c
purpose  : c wrapper for host rbnpha task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define RBNPHA rbnpha_
#endif
#ifdef vms
#define RBNPHA rbnpha
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
RBNPHA();
CloseDefaultPF();
return(RETURN);
}
