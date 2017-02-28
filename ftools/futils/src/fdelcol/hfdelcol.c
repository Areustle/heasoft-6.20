/*
 filename: hfdelcol.c
 purpose:  c - wrapper for host FDELCOL task
*/

#include <stdio.h>


#ifdef unix
#define FDELCOL Fdelcol
#endif
#ifdef vms
#define FDELCOL Fdelcol
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
 void FDELCOL();

 OpenDefaultPF(argc, argv);
 FDELCOL();
 CloseDefaultPF();
 return(RETURN);
}
