/*  like.h  */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "xpi.h"

#define  FILESZ  80
#define  NPHASE  11



int jputenv_(char *string,int jstrlen);

int jsetenv_(char *string,  int *mkmk, int jstrlen);

void readinput(float *ranal, int *clobber, int *status);
