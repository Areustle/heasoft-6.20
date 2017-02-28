/*
 filename: hfim2lst.c
 purpose:  c - wrapper for host FIM2LST task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FIM2LST fim2lt_
#endif
#ifdef vms
#define FIM2LST fim2lt 
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
 FIM2LST();
 CloseDefaultPF();
 return(RETURN);
}
