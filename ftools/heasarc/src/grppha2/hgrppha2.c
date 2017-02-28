/*
filename : grppha2.c
purpose  :  c  wrapper for host grppha2 task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define GRPPHA2 grppha2
#endif
#ifdef vms
#define GRPPHA2 grppha2
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "grppha2.par"

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
void GRPPHA2();
OpenDefaultPF(argc, argv, DEFAULT_PFILE);
GRPPHA2();
CloseDefaultPF();
return(RETURN);
}

