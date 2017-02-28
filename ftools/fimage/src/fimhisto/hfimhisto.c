/*
filename : hfimhisto.c
purpose  :  c  wrapper for host FIMHISTO task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define FIMHISTO fimhisto
#endif
#ifdef vms
#define FIMHISTO fimhisto
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
void FIMHISTO();
OpenDefaultPF(argc, argv);
FIMHISTO();
CloseDefaultPF();
return(RETURN);
}


