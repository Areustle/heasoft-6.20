/*
 filename: hspecmat.c
 purpose:  c - wrapper for host SPECMAT task
 author/date:   Sandhia Bansal - 01/24/02
*/

#include <stdio.h>

#ifdef unix
#define SPECMAT specmat_
#endif
#ifdef vms
#define SPECMAT specmat 
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
 SPECMAT();
 CloseDefaultPF();
 return(RETURN);
}
