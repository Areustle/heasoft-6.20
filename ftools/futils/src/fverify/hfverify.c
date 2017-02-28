/*
 filename: hfverify.c
 purpose:  c - wrapper for host fverify task
 author:   Ning Gan (1998 July)
*/

#include <stdio.h>

#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (int argc, char** argv)
{
 OpenDefaultPF(argc, argv);
 fverify();
 CloseDefaultPF();
 return(RETURN);
}