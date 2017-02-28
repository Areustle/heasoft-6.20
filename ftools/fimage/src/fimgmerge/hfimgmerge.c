/*
filename : hfimgmerge.c
purpose  :  c  wrapper for host FIMGMERGE task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define FIMGMERGE fimgmerge
#endif
#ifdef vms
#define FIMGMERGE fimgmerge
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
void FIMGMERGE();
OpenDefaultPF(argc, argv);
FIMGMERGE();
CloseDefaultPF();
return(RETURN);
}


