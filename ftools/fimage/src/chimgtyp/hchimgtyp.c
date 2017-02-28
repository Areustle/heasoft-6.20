/*
filename : hchimgtyp.c
purpose  :  c  wrapper for host chimgtyp task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define CHIMGTYP chimgtyp
#endif
#ifdef vms
#define CHIMGTYP chimgtyp
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
void CHIMGTYP();
OpenDefaultPF(argc, argv);
CHIMGTYP();
CloseDefaultPF();
return(RETURN);
}


