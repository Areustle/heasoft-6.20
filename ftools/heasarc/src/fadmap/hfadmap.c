/*
 filename: hfadmap.c
 purpose:  c - wrapper for host FADMAP task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define FADMAP fadmap_
#endif
#ifdef vms
#define FADMAP fadmap 
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
 FADMAP();
 CloseDefaultPF();
 return(RETURN);
}
