/*
 filename: hburst.c
 purpose:  c - wrapper for host BURST task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define BURST burst_
#endif
#ifdef vms
#define BURST burst 
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
 BURST();
 CloseDefaultPF();
 return(RETURN);
}
