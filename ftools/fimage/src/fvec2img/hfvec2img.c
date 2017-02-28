/*
 filename: hfvec2img.c
 purpose:  c - wrapper for host FVEC2IMG task
 author:   Dr. Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define FVEC2IMG fvec2g_
#endif
#ifdef vms
#define FVEC2IMG fvec2g 
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
 FVEC2IMG();
 CloseDefaultPF();
 return(RETURN);
}
