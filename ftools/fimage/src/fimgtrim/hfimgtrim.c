/*
filename : hfimgtrim.c
purpose  :  c  wrapper for host FIMGTRIM task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define FIMGTRIM fimgtrim
#endif
#ifdef vms
#define FIMGTRIM fimgtrim
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
void FIMGTRIM();
OpenDefaultPF(argc, argv);
FIMGTRIM();
CloseDefaultPF();
return(RETURN);
}


