/*
 filename: hpcarf.c
 purpose:  c - wrapper for host gevt2rdf task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define GEVT2RDF gevt2f_
#endif
#ifdef vms
#define GEVT2RDF gevt2f
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
 GEVT2RDF();
 CloseDefaultPF();
 return(RETURN);
}
