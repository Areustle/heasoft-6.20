/*
filename : hbcont.c
purpose  : c wrapper for host cmppha task
author   : Rehana Yusaf
modified for bcont 
*/
#include <stdio.h>
#ifdef unix
#define BCONT bcont
#endif
#ifdef vms
#define BCONT bcont 
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
void BCONT();

OpenDefaultPF(argc,argv);
BCONT();
CloseDefaultPF();
return(RETURN);
}
