/*
filename : hgrppha.c
purpose  : c wrapper for host grppha task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define GRPPHA grppha_
#endif
#ifdef vms
#define GRPPHA grppha
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
GRPPHA();
CloseDefaultPF();
return(RETURN);
}
