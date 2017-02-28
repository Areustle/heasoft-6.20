/*

filename    : hfdelete.c
purpose     : C - wrapper for host fdelete task
author/date : toliver / May, 1999

*/

#include <stdio.h>

int MAIN_; /* work around SunOS 4.1.3 bug */

main (int argc, char **argv)
{

   int fdelete ();

   int fdelete_status = 0;

   OpenDefaultPF (argc, argv);
   fdelete_status = fdelete ();
   CloseDefaultPF ();

   return (fdelete_status);

}
