/*
 filename: hsssarf.c
 purpose:  c - wrapper for host SSSARF task
 author:   L. Breedon
*/

#include <stdio.h>

#ifdef unix
#define SSSARF sssarf_
#endif
#ifdef vms
#define SSSARF sssarf
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
 SSSARF();
 CloseDefaultPF();
 return(RETURN);
}
