/*
 filename: hfmemsort.c
 purpose:  c - wrapper for host Fmemsort task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FMEMSORT fmemst_
#endif
#ifdef vms
#define FMEMSORT fmemst 
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
 FMEMSORT();
 CloseDefaultPF();
 return(RETURN);
}
