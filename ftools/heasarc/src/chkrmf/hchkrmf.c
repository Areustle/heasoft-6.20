/*
filename : hchkrmf.c
purpose  :  c  wrapper for host RMFCHECK task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define CHKRMF chkrmf
#endif
#ifdef vms
#define CHKRMF chkrmf
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
void CHKRMF();
OpenDefaultPF(argc, argv);
CHKRMF();
CloseDefaultPF();
return(RETURN);
}
