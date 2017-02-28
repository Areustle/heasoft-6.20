/*
filename : hpcsasscor.c
purpose  : c wrapper for host pcsasscor task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define PCSASSCOR pcsasr_
#endif
#ifdef vms
#define PCSASSCOR pcsasr
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
PCSASSCOR();
CloseDefaultPF();
return(RETURN);
}
