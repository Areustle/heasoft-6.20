/*
 filename: hedsgcor.c
 purpose:  c - wrapper for host EDSGCOR task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define EDSGCOR edsgcr_
#endif
#ifdef vms
#define EDSGCOR edsgcr 
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
 EDSGCOR();
 CloseDefaultPF();
 return(RETURN);
}
