/*
 filename: hsssbck.c
 purpose:  c - wrapper for host SSSBCK task
 author:   L. Breedon
*/

#include <stdio.h>

#ifdef unix
#define SSSBCK sssbck_
#endif
#ifdef vms
#define SSSBCK sssbck
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
 SSSBCK();
 CloseDefaultPF();
 return(RETURN);
}
