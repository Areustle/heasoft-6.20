/*
 filename: hdecodeevt.c
 purpose:  c - wrapper for host DECODEEVT task
 author:   Zhiyu Guo
*/

#include <stdio.h>

#ifdef unix
#define DECODEEVT decodeevt
#endif
#ifdef vms
#define DECODEEVT decodeevt
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
  void DECODEEVT();

  OpenDefaultPF(argc, argv);
  DECODEEVT();
  CloseDefaultPF();
  return(RETURN);
}
