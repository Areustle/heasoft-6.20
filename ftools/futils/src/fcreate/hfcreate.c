/*
 filename: hfcreate.c
 purpose:  c - wrapper for host FCREAT task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FCREATE fcreae_
#endif
#ifdef vms
#define FCREATE fcreae 
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
 FCREATE();
 CloseDefaultPF();
 return(RETURN);
}
