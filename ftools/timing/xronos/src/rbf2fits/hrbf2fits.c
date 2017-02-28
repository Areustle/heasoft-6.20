/*
 filename: hrbf2fits.c
 purpose:  c - wrapper for host XRONOS RBF2FITS task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define RBF2FITS rbf2fs_
#endif
#ifdef vms
#define RBF2FITS rbf2fs
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
 RBF2FITS();
 CloseDefaultPF();
 return(RETURN);
}
