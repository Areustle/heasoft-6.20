 /*
  filename: hrate.c
  purpose: c - wrapper for host RATE task
 */

#include <stdio.h>
#ifdef unix
#define RATE rate_
#endif
#ifdef vms
#define RATE rate 
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
  RATE();
  CloseDefaultPF();
  return(RETURN);
}


