/*
filename : hpcecor.c
purpose  : c wrapper for host pcecor task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define PCECOR pcecor_
#endif
#ifdef vms
#define PCECOR pcecor
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
PCECOR();
CloseDefaultPF();
return(RETURN);
}
