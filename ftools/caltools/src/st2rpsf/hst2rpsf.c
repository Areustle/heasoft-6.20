/*
filename : hst2rpsf.c
purpose  : c wrapper for host st2rpsf task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define ST2RPSF st2rpf_
#endif
#ifdef vms
#define ST2RPSF st2rpf
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
ST2RPSF();
CloseDefaultPF();
return(RETURN);
}
