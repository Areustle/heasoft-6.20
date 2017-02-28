/*
filename : hgroview.c
purpose  : c wrapper for host groview task
author   : Rehana Yusaf
modified for groview 
*/
#include <stdio.h>
#ifdef unix
#define GROVIEW groview
#endif
#ifdef vms
#define GROVIEW groview 
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
void GROVIEW();

OpenDefaultPF(argc,argv);
GROVIEW();
CloseDefaultPF();
return(RETURN);
}
