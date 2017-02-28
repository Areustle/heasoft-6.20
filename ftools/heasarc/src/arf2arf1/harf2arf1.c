/*
 filename: harf2arf1
 purpose:  c - wrapper for host arf2arf1 task
*/

#include <stdio.h>

int MAIN_; /* work around SunOS 4.1.3 bug */

main (int argc, char **argv)
{
   int arf2arf1main();
   int status;

   OpenDefaultPF(argc, argv);
   status = arf2arf1main();
   CloseDefaultPF();

#ifdef unix
   return(status);
#endif
#ifdef vms
   return(1)
#endif
}
