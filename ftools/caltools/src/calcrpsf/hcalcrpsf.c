/*
 filename: hcalcrpsf.c
 purpose:  c - wrapper for host CALCRPSF task
 author:   Ian M George
*/

#include <stdio.h>

#ifdef unix
#define CALCRPSF calcrf_
#endif
#ifdef vms
#define CALCRPSF calcrf
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
 CALCRPSF();
 CloseDefaultPF();
 return(RETURN);
}
