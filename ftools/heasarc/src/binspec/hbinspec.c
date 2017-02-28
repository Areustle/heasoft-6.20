/*
 filename: hbinspec.c
 purpose:  c - wrapper for host BINSPEC task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define BINSPEC binspc_
#endif
#ifdef vms
#define BINSPEC binspc 
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
 BINSPEC();
 CloseDefaultPF();
 return(RETURN);
}
