/*
 filename: hrsp2rmf.c
 purpose:  c - wrapper for host RSP2RMF task
 author:   Ian M George
*/

#include <stdio.h>

#ifdef unix
#define RSP2RMF rsp2rf_
#endif
#ifdef vms
#define RSP2RMF rsp2rf
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
 RSP2RMF();
 CloseDefaultPF();
 return(RETURN);
}
