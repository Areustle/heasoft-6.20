/*
 filename: hmkfilter2.c
 purpose:  c - wrapper for host mkfilter2 task
*/

#include <stdio.h>

int MAIN_; /* work around SunOS 4.1.3 bug */

main (int argc, char **argv)
{
   int mkfilter2();
   int status;

   OpenDefaultPF(argc, argv);
   status = mkfilter2();
   CloseDefaultPF();

#ifdef unix
   return(status);
#endif
#ifdef vms
   return(1)
#endif
}
