/*
 filename: hsaextrct.c
 purpose:  c - wrapper for host SAEXTRCT task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define SAEXTRCT saextt_
#endif
#ifdef vms
#define SAEXTRCT saextt
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
 SAEXTRCT();
 CloseDefaultPF();
 return(RETURN);
}
