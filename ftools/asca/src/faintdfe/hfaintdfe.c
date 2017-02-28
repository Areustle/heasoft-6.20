/*
 filename: hfaintdfe.c
 purpose:  c - wrapper for host FAINTDFE task
*/

#define RETURN 0

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
  void faintdfe();

  OpenDefaultPF(argc, argv);
  faintdfe();
  CloseDefaultPF();
  return(RETURN);
}
