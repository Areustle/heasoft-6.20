 /*
  filename: hlte.c
  purpose: c - wrapper for host LTE task
 */

#include <stdio.h>
#ifdef unix
#define LTE lte_
#endif
#ifdef vms
#define LTE lte
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
  OpenDefaultPF(argc, argv);
  LTE();
  CloseDefaultPF();
  return(RETURN);
}


