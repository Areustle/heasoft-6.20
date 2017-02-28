/*
 filename: hfstatistic.c
 purpose:  c - wrapper for host FSTAT task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FSTATISTIC fstatc_
#endif
#ifdef vms
#define FSTATISTIC fstatc 
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
 FSTATISTIC();
 CloseDefaultPF();
 return(RETURN);
}
