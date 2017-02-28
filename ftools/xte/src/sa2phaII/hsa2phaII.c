/*
 filename: hsa2phaII.c
 purpose:  c - wrapper for host SA2PHAII task
 author:   Zhiyu Guo
*/

#include <stdio.h>

#ifdef unix
#define SA2PHAII sa2phaII
#endif
#ifdef vms
#define SA2PHAII sa2phaII
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
  void SA2PHAII();

  OpenDefaultPF(argc, argv);
  SA2PHAII();
  CloseDefaultPF();
  return(RETURN);
}
