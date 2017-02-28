/*
 filename: hhkexpand.c
 purpose:  c - wrapper for host FHKEXPD task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define HKEXPAND hkexpd_
#endif
#ifdef vms
#define HKEXPAND hkexpd 
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
 HKEXPAND();
 CloseDefaultPF();
 return(RETURN);
}
