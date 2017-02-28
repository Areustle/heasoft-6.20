#ifndef PARAM_INCLUDED
#include <stdio.h>
#include "specdump.h"

#define PARAM_STRING_DIMEN 256
#define NGRADES 8

typedef struct {

char infile[ PARAM_STRING_DIMEN];
char extname[PARAM_STRING_DIMEN];
char tblfile[PARAM_STRING_DIMEN];
char outfile[PARAM_STRING_DIMEN];

FILE* fpout;

int split;

double binsec;

int zerodef;
int mincounts;

int use_grade[NGRADES];

double accuracy;

SPECDUMP* specdump;


} PARAM;

/*************************************************************************
**************************************************************************
* read the parfile into a new PARAM structure
*************************************************************************/
PARAM* readParam(void);

#define PARAM_INCLUDED
#endif  PARAM_INCLUDED
