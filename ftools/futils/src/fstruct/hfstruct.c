/*
 filename: hfstruct.c
 purpose:  c - wrapper for host FSTRUCT task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FSTRUCT fstrut_
#endif
#ifdef vms
#define FSTRUCT fstrut 
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
 FSTRUCT();
 CloseDefaultPF();
 return(RETURN);
}
