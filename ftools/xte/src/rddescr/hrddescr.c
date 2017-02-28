/*
 filename: hrddescr.c
 purpose:  c - wrapper for host RDDESCR task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define RDDESCR rddesr_
#endif
#ifdef vms
#define RDDESCR rddesr 
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
 RDDESCR();
 CloseDefaultPF();
 return(RETURN);
}
