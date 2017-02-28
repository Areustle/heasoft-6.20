/** \file ape_test_main.c
    \brief Main function for unit test application, which executes the unit test top function.
    \author James Peachey, HEASARC/EUD/GSFC.
*/
#include "ape/ape_test.h"

int main(int argc, char ** argv) {
  return ape_test(argc, argv);
}

/*
 * $Log: ape_test_main.c,v $
 * Revision 1.2  2007/10/09 16:45:01  peachey
 * Use command line arguments in test binary to name the log file.
 * If no log file, do not redirect output for the convenience of debugging.
 *
 * Revision 1.1.1.1  2006/04/05 13:45:19  peachey
 * Initial import of All-purpose Parameter Environment (APE).
 *
*/
