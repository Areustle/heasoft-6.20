/*
 filename: hfakelc.c
 purpose:  c - wrapper for host FAKELC task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define FAKELC fakelc_
#endif
#ifdef vms
#define FAKELC fakelc
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
 FAKELC();
 CloseDefaultPF();
 return(RETURN);
}
