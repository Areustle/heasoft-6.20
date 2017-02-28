/*
 filename: hefold.c
 purpose:  c - wrapper for host XRONOS EFOLD task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define EFOLD efold_
#endif
#ifdef vms
#define EFOLD efold
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
 EFOLD();
 CloseDefaultPF();
 return(RETURN);
}
