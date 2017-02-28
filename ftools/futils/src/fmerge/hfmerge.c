/*
 filename: hfmerge.c
 purpose:  c - wrapper for host FMERGE task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FMERGE fmerge_
#endif
#ifdef vms
#define FMERGE fmerge 
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
 FMERGE();
 CloseDefaultPF();
 return(RETURN);
}
