/*
 filename: hflorentz.c
 purpose:  c - wrapper for host Florentz task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FLORENTZ florez_
#endif
#ifdef vms
#define FLORENTZ florez 
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
 FLORENTZ();
 CloseDefaultPF();
 return(RETURN);
}
