/*
 filename: hsbmerge.c
 purpose:  c - wrapper for host SBMERGE task
 author:   Zhiyu Guo
*/

#include <stdio.h>

#ifdef unix
#define SBMERGE Sbmerge
#endif
#ifdef vms
#define SBMERGE Sbmerge
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
  void SBMERGE();

  OpenDefaultPF(argc, argv);
  SBMERGE();
  CloseDefaultPF();
  return(RETURN);
}
