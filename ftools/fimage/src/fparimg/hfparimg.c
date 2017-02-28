/*
 filename: hfparimg.c
 purpose:  c - wrapper for host fparimg task
 author:  Alex Muslimov
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
 void fparimg();

 OpenDefaultPF(argc, argv);
 fparimg();
 CloseDefaultPF();
 return(RETURN);
}
