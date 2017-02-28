#include <stdlib.h>
#include <string.h>
#include "param_wrappers.h"
#include "param.h"

#include "att_fatal.h"

/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void) {

    PARAM* param;
    char teldef[FILENAME_DIMEN];
    char points[FILENAME_DIMEN];
    
    char extname[FLEN_VALUE];
    char x0_colname[FLEN_VALUE];
    char y0_colname[FLEN_VALUE];
    char x1_colname[FLEN_VALUE];
    char y1_colname[FLEN_VALUE];
    char wgt_colname[FLEN_VALUE];
    /************************************
    * allocater space for the structure *
    ************************************/
    param=(PARAM*)malloc(sizeof(PARAM));


    /***********************
    * read the teldef file *
    ***********************/
    read_string_param("teldef" , teldef , FILENAME_DIMEN);
    param->teldef = readTelDef(teldef);
    if(param->teldef == NULL) {
        fprintf(stderr, "Could not read Teldef file %s\n", teldef);
        att_fatal(1);
    }

    /**********************
    * read the input data *
    **********************/
    read_string_param("pointsfile", points, FILENAME_DIMEN);
    read_string_param("pointsext", extname, FLEN_VALUE);
    read_string_param("x0_col", x0_colname, FLEN_VALUE);
    read_string_param("y0_col", y0_colname, FLEN_VALUE);
    read_string_param("x1_col", x1_colname, FLEN_VALUE);
    read_string_param("y1_col", y1_colname, FLEN_VALUE);
    read_string_param("weight_col", wgt_colname, FLEN_VALUE);

    param->points = readPoints(points, extname,
                               x0_colname, y0_colname,
                               x1_colname, y1_colname,
                               wgt_colname            );
    if(param->points == NULL) {
        fprintf(stderr, "Could not read points file %s\n", points);
        att_fatal(1);
    }
    
    /**********************************
    * read the convergence parameters *
    **********************************/
    param->tolerance = read_double_param("tolerance");
    param->max_iterations = read_int_param("iterations");

    /*******************
    * output file name *
    *******************/
    read_string_param("outfile", param->outfile, FLEN_FILENAME);
    
    param->decimals = read_int_param("decimals");


    param->history = read_boolean_param("history");

    return(param);
} /* end of readparam function */
