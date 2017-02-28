 /*
  filename: hratefit.c
  purpose: c - wrapper for host RATEFIT task
 */

#include <stdio.h>
#ifdef unix
#define RATEFIT rateft_
#endif
#ifdef vms
#define RATEFIT rateft 
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
  RATEFIT();
  CloseDefaultPF();
  return(RETURN);
}


