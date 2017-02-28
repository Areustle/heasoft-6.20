/*
 filename: hfcopy
 purpose:  c - wrapper for host fcopy task
*/

#include <stdio.h>

int MAIN_; /* work around SunOS 4.1.3 bug */

main (int argc, char **argv)
{
   int fcopymain();
   int status;

   OpenDefaultPF(argc, argv);
   status = fcopymain();
   CloseDefaultPF();

#ifdef unix
   return(status);
#endif
#ifdef vms
   return(1)
#endif
}
