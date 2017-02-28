/*
 filename: haddspec.c
 purpose:  c - wrapper for host ADDSPEC task
 author:   Ian M George
*/

#include <stdio.h>

#ifdef unix
#define addspec addspc_
#endif
#ifdef vms
#define addspec addspc
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
 addspec();
 CloseDefaultPF();
 return(RETURN);
}
