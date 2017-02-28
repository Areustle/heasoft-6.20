/*
 filename: namepha.c
 purpose: creates name for output PHA file from FTOOL bod2pha
          beginning with user-supplied string phastr, with format
          XXXXXX_ladN.pha, where XXXXXX is value of string phastr,
          and N is BATSE Large Area Detector number
 author/date: Peter J.T. Leonard, August, 1998
*/

void namepha(char *phastr, char *chidet, char *phafil);

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "xpi.h"
 
void namepha(char *phastr, char *chidet, char *phafil)
{
    int  ind=0, status=0;

    char tmpstr[121];
    char *end_ptr;


    size_t tmpstrlen;
    size_t sz=sizeof(phastr);
 

    for (ind=0; phastr[ind]!='\0'; ++ind)
	phafil[ind] = phastr[ind];
    phafil[ind] = '\0';

    strcat(strcat(strcat(phafil, "_lad"), chidet), ".pha");
}

FCALLSCSUB3(namepha, NAMEPHA, namepha, PSTRING, PSTRING, PSTRING)
