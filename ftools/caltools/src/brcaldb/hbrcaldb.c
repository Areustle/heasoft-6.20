/*
 filename: hbrcaldb.c
 purpose:  c - wrapper for brcaldb task
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
  int brcalb();

  OpenDefaultPF(argc, argv);
  brcalb();
  CloseDefaultPF();
  return(RETURN);
}
