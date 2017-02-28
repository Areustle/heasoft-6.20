/*
 filename: hfkeyprint.c
 purpose:  c - wrapper for host FKEYPRINT task
 author:   James Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FKEYPRINT fkeypt_
#endif
#ifdef vms
#define FKEYPRINT fkeypt 
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "fkeyprint.par"

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv, DEFAULT_PFILE);
 FKEYPRINT();
 CloseDefaultPF();
 return(RETURN);
}
