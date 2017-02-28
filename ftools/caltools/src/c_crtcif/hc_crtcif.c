/*
 filename: hc_crtcif.c
 purpose:  c - wrapper for c_crtcif task
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
  int c_crtf();

  OpenDefaultPF(argc, argv);
  c_crtf();
  CloseDefaultPF();
  return(RETURN);
}
