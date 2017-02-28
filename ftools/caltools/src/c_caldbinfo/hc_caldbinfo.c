/*
 filename: hc_caldbinfo.c
 purpose:  c - wrapper for c_caldbinfo task
*/

#include <stdio.h>

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
  int c_calo();

  OpenDefaultPF(argc, argv);
  c_calo();
  CloseDefaultPF();
  return(RETURN);
}
