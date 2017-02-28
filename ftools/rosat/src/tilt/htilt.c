 /*
  filename: htilt.c
  purpose: c - wrapper for host TILT task
 */

#include <stdio.h>
#ifdef unix
#define TILT tilt_
#endif
#ifdef vms
#define TILT tilt 
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
  TILT();
  CloseDefaultPF();
  return(RETURN);
}


