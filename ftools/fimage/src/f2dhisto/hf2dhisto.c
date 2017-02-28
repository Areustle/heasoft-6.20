/*
 filename: hf2dhisto.c
 purpose:  c - wrapper for host f2dhisto task
 author:   Ning Gan  2/11/98
*/

#include <stdio.h>

#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (int argc, char **argv)
{
 void f2dhisto();

 OpenDefaultPF(argc, argv);
 f2dhisto();
 CloseDefaultPF();
 return(RETURN);
}

