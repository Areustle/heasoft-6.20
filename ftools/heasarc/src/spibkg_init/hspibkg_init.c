/*
filename : hspibkg_init.c
purpose  : c wrapper for host spibkg_init task
author   : Rehana Yusaf
modified for spibkg_init 
*/
#include <stdio.h>
#ifdef unix
#define SPIBKG_INIT spibkg_init
#endif
#ifdef vms
#define SPIBKG_INIT spibkg_init 
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
void SPIBKG_INIT();
OpenDefaultPF(argc,argv);
SPIBKG_INIT();
CloseDefaultPF();
return(RETURN);
}
