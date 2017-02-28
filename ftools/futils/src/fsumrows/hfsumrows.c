/*

filename    : hfsumrows.c
purpose     : C - wrapper for host fsumrows task
author/date : toliver / April, 1999

*/

#include <stdio.h>

int MAIN_; /* work around SunOS 4.1.3 bug */

main (int argc, char **argv)
{

   int fsumrows ();

   int fsumrows_status = 0;

   OpenDefaultPF (argc, argv);
   fsumrows_status = fsumrows ();
   CloseDefaultPF ();

   return (fsumrows_status);

}
