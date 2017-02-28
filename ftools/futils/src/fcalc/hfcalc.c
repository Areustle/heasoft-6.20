/*
 filename: hfcalc.c
 purpose:  c - wrapper for host FCALC task
 author:   Kent Blackburn
 date:     April 20, 1993
*/

#include <stdio.h>

#ifdef unix
#define FCALC fcalc_
#endif
#ifdef vms
#define FCALC fcalc 
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
 FCALC();
 CloseDefaultPF();
 return(RETURN);
}
