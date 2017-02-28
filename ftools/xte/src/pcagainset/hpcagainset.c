/*
 filename: hpcagainset.c
 purpose:  c - wrapper for host RDDESCR task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define PCAGAINSET pcagat_
#endif
#ifdef vms
#define PCAGAINSET pcagat 
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
 PCAGAINSET();
 CloseDefaultPF();
 return(RETURN);
}
