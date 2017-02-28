/*
 filename: hxtederive.c
 purpose:  c - wrapper for host XTEDERIVE task
 author:   M. Tripicco
*/

#include <stdio.h>

#ifdef unix
#define XTEDERIVE xtedee_
#endif
#ifdef vms
#define XTEDERIVE xtedee
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
 XTEDERIVE();
 CloseDefaultPF();
 return(RETURN);
}
