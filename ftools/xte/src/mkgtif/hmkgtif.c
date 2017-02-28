/*
 filename: hmkgtif.c
 purpose:  c - wrapper for host MKGTIF task
*/

#include <stdio.h>

#ifdef unix
#define MKGTIF mkgtif_
#endif
#ifdef vms
#define MKGTIF mkgtif 
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
 MKGTIF();
 CloseDefaultPF();
 return(RETURN);
}
