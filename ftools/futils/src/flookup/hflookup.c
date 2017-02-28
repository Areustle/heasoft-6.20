/*
 filename: hflookup.c
 purpose:  c - wrapper for host FLOOKUP task
 author:   Emily Greene
*/

#include <stdio.h>

#ifdef unix
#define FLOOKUP flookp_
#endif
#ifdef vms
#define FLOOKUP flookp 
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
 FLOOKUP();
 CloseDefaultPF();
 return(RETURN);
}
