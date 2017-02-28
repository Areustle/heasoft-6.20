/*
 filename: hfindex.c
 purpose:  c - wrapper for host FINDEX task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FINDEX findex_
#endif
#ifdef vms
#define FINDEX findex 
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
 FINDEX();
 CloseDefaultPF();
 return(RETURN);
}
