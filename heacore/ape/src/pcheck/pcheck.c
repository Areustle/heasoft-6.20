/** \file pcheck.c
    \brief 
    \author James Peachey, HEASARC/EUD/GSFC.
*/
#include "ape/ape_binary.h"
#include "ape/ape_error.h"

int main(int argc, char ** argv) {
  return eOK != ape_binary_pcheck(argc - 1, argv + 1) ? 1 : 0;
}

/*
 * $Log: pcheck.c,v $
 * Revision 1.5  2006/08/22 20:36:09  peachey
 * Move body of pcheck binary to a function ape_binary_pcheck.
 *
 */
