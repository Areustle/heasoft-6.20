/*
filename : hpcfilt.c
purpose  : c wrapper for host pcfilt task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define PCFILT pcfilt_
#endif
#ifdef vms
#define PCFILT pcfilt
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
PCFILT();
CloseDefaultPF();
return(RETURN);
}
