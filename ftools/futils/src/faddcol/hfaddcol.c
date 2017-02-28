/*
 filename: hfaddcol.c
 purpose:  c - wrapper for host FADDCOL task
*/

#include <stdio.h>


#ifdef unix
#define FADDCOL faddcol
#endif
#ifdef vms
#define FADDCOL faddcol
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
 void FADDCOL();

 OpenDefaultPF(argc, argv);
 FADDCOL();
 CloseDefaultPF();
 return(RETURN);
}
