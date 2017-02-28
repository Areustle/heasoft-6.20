/*
 filename: hfdump.c
 purpose:  c - wrapper for host FDUMP task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FDUMP fdump_
#endif
#ifdef vms
#define FDUMP fdump 
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
 FDUMP();
 CloseDefaultPF();
 return(RETURN);
}
