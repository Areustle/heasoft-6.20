/*
 filename: htimeskew.c
 purpose:  c - wrapper for host XRONOS AUTOCOR task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define TIMESKEW timesw_
#endif
#ifdef vms
#define TIMESKEW timesw
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
 TIMESKEW();
 CloseDefaultPF();
 return(RETURN);
}
