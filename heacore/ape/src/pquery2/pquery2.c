/** \file pquery2.c
    \brief
    \author James Peachey, HEASARC/EUD/GSFC.
*/
#include "ape/ape_binary.h"
#include "ape/ape_error.h"

int main(int argc, char ** argv) {
  return eOK != ape_binary_pquery2(argc - 1, argv + 1) ? 1 : 0;
}

/*
 * $Log: pquery2.c,v $
 * Revision 1.3  2013/02/15 21:16:30  irby
 * Call ape_binary_pquery2 to get pquery2 behavior!
 *
 * Revision 1.2  2006/06/08 02:25:52  peachey
 * Rationalize pget, plist and pquery2 binaries to handle par files
 * the same way, or to use binary names and PFILES to find the parameter file(s).
 *
 * Revision 1.1  2006/05/22 17:37:22  peachey
 * Add pquery binary.
 *
 */
