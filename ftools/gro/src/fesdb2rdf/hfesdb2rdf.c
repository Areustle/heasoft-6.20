/*
 filename: hfesdb2rdf.c
 purpose:  c - wrapper for host FESDB2RDF task
 author:   Mark Cresitello-Dittmar
*/

#include <stdio.h>

#ifdef unix
#define FESDB2RDF fesdbf_
#endif
#ifdef vms
#define FESDB2RDF fesdbf 
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
 FESDB2RDF();
 CloseDefaultPF();
 return(RETURN);
}
