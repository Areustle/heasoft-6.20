/*
 filename: hnh.c
 purpose:  c - wrapper for host nh task
*/

#include <stdio.h>

#ifdef unix
#define NH nh_
#endif
#ifdef vms
#define NH nh
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
 NH();
 CloseDefaultPF();
 return(RETURN);
}