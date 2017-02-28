/*
filename : hchkarf.c
purpose  :  c  wrapper for host ARFCHECK task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define CHKARF chkarf
#endif
#ifdef vms
#define CHKARF chkarf
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
void CHKARF();
OpenDefaultPF(argc, argv);
CHKARF();
CloseDefaultPF();
return(RETURN);
}
