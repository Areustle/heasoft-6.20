/*
 filename: hefsearch.c
 purpose:  c - wrapper for host XRONOS EFSEARCH task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define EFSEARCH efseah_
#endif
#ifdef vms
#define EFSEARCH efseah
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
 EFSEARCH();
 CloseDefaultPF();
 return(RETURN);
}
