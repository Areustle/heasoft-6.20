/*
 filename: hfmosaicftool.c
 purpose:  c - wrapper for host FMOSAIC task
 author/date:   James Peachey August 1996 (used by Ilana Harrus Sept 1999)
*/

#include <stdio.h>

#ifdef unix
#define FMOSAIC fmosaic_
#endif
#ifdef vms
#define FMOSAIC fmosaic 
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
 FMOSAIC();
 CloseDefaultPF();
 return(RETURN);
}
