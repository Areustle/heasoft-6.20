/*
 filename: hmkhratio.c
 purpose:  c - wrapper for host mkhratio task
*/

#include <stdio.h>

#ifdef unix
#define MKHRATIO mkhratio_
#endif
#ifdef vms
#define MKHRATIO mkhratio
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
 MKHRATIO();
 CloseDefaultPF();
 return(RETURN);
}
