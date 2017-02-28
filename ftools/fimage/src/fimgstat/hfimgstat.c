/*
filename : hfimgstat.c
purpose  :  c  wrapper for host FIMGSTAT task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define FIMGSTAT fimgstat
#endif
#ifdef vms
#define FIMGSTAT fimgstat
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
void FIMGSTAT();
OpenDefaultPF(argc, argv);
FIMGSTAT();
CloseDefaultPF();
return(RETURN);
}


