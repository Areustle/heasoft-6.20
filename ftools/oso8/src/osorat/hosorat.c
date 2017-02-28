/*
 filename: hosorat.c
 purpose:  c - wrapper for host osorat task
 author:   David Dawson
*/

#include <stdio.h>

#ifdef unix
#define OSORAT osorat_
#endif
#ifdef vms
#define OSORAT osorat 
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
 OSORAT();
 CloseDefaultPF();
 return(RETURN);
}
