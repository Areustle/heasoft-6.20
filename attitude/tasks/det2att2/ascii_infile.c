#include "infile.h"

#include <stdlib.h>

/****************************************************************************
*
****************************************************************************/
INFILE* openInfile(char* name) {

    INFILE* infile;
    
    /*************************
    * allocate the structure *
    **************************/
    infile = (INFILE*)malloc(sizeof(INFILE));
    if(infile==NULL) return NULL;

    /****************
    * open the file *
    ****************/
    infile->fp = fopen(name, "r");
    if(infile->fp==NULL) return NULL;
    
    return infile;


}

/***************************************************************************
*
***************************************************************************/
int readInfileValues(INFILE* infile, 
                     double* time, double* detx, double* dety) {

    return fscanf(infile->fp, "%lf %lf %lf", time, detx, dety) == 3;
    
}

/***************************************************************************
*
***************************************************************************/
void closeInfile(INFILE* infile) {
    fclose(infile->fp);
    
} /* end of readInfileValues method */

