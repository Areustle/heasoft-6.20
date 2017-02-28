/*
 filename: hsky2xy.c
 purpose:  c - wrapper for host SKY2XY task
 author:   Dr. Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define SKY2XY sky2xy_
#endif
#ifdef vms
#define SKY2XY sky2xy 
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
 SKY2XY();
 CloseDefaultPF();
 return(RETURN);
}
