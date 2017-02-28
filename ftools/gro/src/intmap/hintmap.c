/*
 filename: hintmap.c
 purpose:  c - wrapper for host INTMAP task
 author/date:   Sandhia Bansal - 01/07/02
*/

#include <stdio.h>

#ifdef unix
#define INTMAP intmap_
#endif
#ifdef vms
#define INTMAP intmap 
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
 INTMAP();
 CloseDefaultPF();
 return(RETURN);
}
