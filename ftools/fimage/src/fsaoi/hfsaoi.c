/*
 filename: hfsaoi.c
 purpose:  c - wrapper for host FSAOI task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FSAOI fsaoi_
#endif
#ifdef vms
#define FSAOI fsaoi 
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
 FSAOI();
 CloseDefaultPF();
 return(RETURN);
}
