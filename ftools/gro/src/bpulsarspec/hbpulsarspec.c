/*
filename : hbpulsarspec.c
purpose  : c wrapper for host cmppha task
author   : Rehana Yusaf
modified for bpulsarspec 
*/
#include <stdio.h>
#ifdef unix
#define BPULSARSPEC bpulsarspec
#endif
#ifdef vms
#define BPULSARSPEC bpulsarspec 
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
void BPULSARSPEC();

OpenDefaultPF(argc,argv);
BPULSARSPEC();
CloseDefaultPF();
return(RETURN);
}
