/*
 filename: htime2sec.c
 purpose:  c - wrapper for host TIME2SEC task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define TIME2SEC time2c_
#endif
#ifdef vms
#define TIME2SEC time2c 
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
 TIME2SEC();
 CloseDefaultPF();
 return(RETURN);
}
