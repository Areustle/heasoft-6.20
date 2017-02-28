/*
 filename: hhkunexpand.c
 purpose:  c - wrapper for host fhkuexpd task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define HKUNEXPAND hkuned_
#endif
#ifdef vms
#define HKUNEXPAND hkuned 
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
 HKUNEXPAND();
 CloseDefaultPF();
 return(RETURN);
}
