 /*
  filename: hcastpart.c
  purpose: c - wrapper for host CASTPART task
 */

#include <stdio.h>
#ifdef unix
#define CASTPART castpt_
#endif
#ifdef vms
#define CASTPART castpt
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
  CASTPART();
  CloseDefaultPF();
  return(RETURN);
}


