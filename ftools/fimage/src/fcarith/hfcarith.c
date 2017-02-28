/*
 filename: hfcarith.c
 purpose:  c - wrapper for host FCARITH task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FCARITH fcarih_
#endif
#ifdef vms
#define FCARITH fcarih 
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
 FCARITH();
 CloseDefaultPF();
 return(RETURN);
}
