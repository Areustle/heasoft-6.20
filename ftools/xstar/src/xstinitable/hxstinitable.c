/*
 filename: hxstar2xspec.c
 purpose:  c - wrapper for host XSTAR2XSPEC task
 author/date:   James Peachey August 1996
*/

#include <stdio.h>

#define XSTINITABLE xstinitable
#define RETURN 0

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 XSTINITABLE();
 CloseDefaultPF();
 return(RETURN);
}
