/*
 filename: hcaldbflag.c
 purpose:  c - wrapper for host CALDBFLAG task
 author:   L. Breedon
*/

#include <stdio.h>

#ifdef unix
#define CALDBFLAG caldbg_
#endif
#ifdef vms
#define CALDBFLAG caldbg
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
 OpenDefaultPF(argc, argv);
 CALDBFLAG();
 CloseDefaultPF();
 return(RETURN);
}
