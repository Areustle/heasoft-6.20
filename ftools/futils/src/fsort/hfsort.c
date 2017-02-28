/*
 filename: hfsort.c
 purpose:  c - wrapper for host Fsort task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FSORT fsort_
#endif
#ifdef vms
#define FSORT fsort 
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
 FSORT();
 CloseDefaultPF();
 return(RETURN);
}
