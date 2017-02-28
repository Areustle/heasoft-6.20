/*
 filename: hhxtarf.c
 purpose:  c - wrapper for host HXTARF task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define HXTARF hxtarf_
#endif
#ifdef vms
#define HXTARF hxtarf 
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
 HXTARF();
 CloseDefaultPF();
 return(RETURN);
}
