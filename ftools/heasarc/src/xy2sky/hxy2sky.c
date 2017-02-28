/*
 filename: hxy2sky.c
 purpose:  c - wrapper for host XY2SKY task
 author:   Dr. Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define XY2SKY xy2sky_
#endif
#ifdef vms
#define XY2SKY xy2sky 
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
 XY2SKY();
 CloseDefaultPF();
 return(RETURN);
}
