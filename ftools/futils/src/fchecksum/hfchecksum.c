/*
 filename: hfchecksum.c
 purpose:  c - wrapper for host FCHECKSUM task
 author:   William Pence
*/

#include <stdio.h>

#ifdef unix
#define FCHECM fchecm_
#endif
#ifdef vms
#define FCHECM fchecm 
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
 FCHECM();
 CloseDefaultPF();
 return(RETURN);
}
