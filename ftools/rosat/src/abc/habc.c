/*
 filename: habc.c
 purpose:  c - wrapper for host ABC task
 author:   Dr. Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define ABC abc_
#endif
#ifdef vms
#define ABC abc 
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
 ABC();
 CloseDefaultPF();
 return(RETURN);
}
