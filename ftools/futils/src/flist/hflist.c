/*
 filename: hflist.c
 purpose:  c - wrapper for host FLIST task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define FLIST flist_
#endif
#ifdef vms
#define FLIST flist 
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
 FLIST();
 CloseDefaultPF();
 return(RETURN);
}
