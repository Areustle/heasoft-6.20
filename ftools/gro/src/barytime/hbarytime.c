/*
 filename: hbarytime.c
 purpose:  c - wrapper for BARYTIME task
 author:   S. Bansal
*/

#include <stdio.h>

#ifdef unix
#define BARYTIME Barytime
#endif
#ifdef vms
#define BARYTIME Barytime
#endif
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
  void BARYTIME();

  OpenDefaultPF(argc, argv);
  BARYTIME();
  CloseDefaultPF();
  return(RETURN);
}
