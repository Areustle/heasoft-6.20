/*
 filename: hlistdata.c
 purpose:  c - wrapper for host XRONOS LISTDATA task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define LISTDATA listda_
#endif
#ifdef vms
#define LISTDATA listda
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
 LISTDATA();
 CloseDefaultPF();
 return(RETURN);
}
