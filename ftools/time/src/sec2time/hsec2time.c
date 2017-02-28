/*
 filename: hsec2time.c
 purpose:  c - wrapper for host SEC2TIME task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define SEC2TIME sec2te_
#endif
#ifdef vms
#define SEC2TIME sec2te 
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
 SEC2TIME();
 CloseDefaultPF();
 return(RETURN);
}
