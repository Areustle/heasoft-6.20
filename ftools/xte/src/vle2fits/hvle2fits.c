/*
 filename: hvle2fits.c
 purpose:  c - wrapper for host VLE2FITS task
*/

#include <stdio.h>

#ifdef unix
#define VLE2FITS Vle2fits
#endif
#ifdef vms
#define VLE2FITS Vle2fits
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
  void VLE2FITS();

  OpenDefaultPF(argc, argv);
  VLE2FITS();
  CloseDefaultPF();
  return(RETURN);
}
