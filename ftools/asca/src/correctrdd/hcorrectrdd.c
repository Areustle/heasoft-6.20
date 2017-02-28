/*
filename : hcorrectrdd.c
purpose  :  c  wrapper for host CORRECTRDD task
author   : Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define CORRECTRDD correctrdd
#endif
#ifdef vms
#define CORRECTRDD correctrdd
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
void CORRECTRDD();
OpenDefaultPF(argc, argv);
CORRECTRDD();
CloseDefaultPF();
return(RETURN);
}


