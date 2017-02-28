/*
filename : hbodfluxhis.c
purpose  : c wrapper for host cmppha task
author   : Rehana Yusaf
modified for bodfluxhis 
*/
#include <stdio.h>
#ifdef unix
#define BODFLUXHIS bodfluxhis
#endif
#ifdef vms
#define BODFLUXHIS bodfluxhis 
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
void BODFLUXHIS();

OpenDefaultPF(argc,argv);
BODFLUXHIS();
CloseDefaultPF();
return(RETURN);
}
