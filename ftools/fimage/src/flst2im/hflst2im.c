/*
filename : hflst2im.c
purpose  :  c  wrapper for host FLST2IM task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define FLST2IM flst2im
#endif
#ifdef vms
#define FLST2IM flst2im
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
void FLST2IM();
OpenDefaultPF(argc, argv);
FLST2IM();
CloseDefaultPF();
return(RETURN);
}


