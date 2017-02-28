#include <stdlib.h>

#include "param_wrappers.h"
#include "param.h"

/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void) {

  PARAM* param;
  
  param = (PARAM*) calloc(1, sizeof(PARAM));
  
  read_string_param("infile"     , param->infile , FILENAME_DIMEN);
  read_string_param("outfile"    , param->outfile, FILENAME_DIMEN);
  read_string_param("teldeffile" , param->teldeffile , FILENAME_DIMEN);
  read_string_param("startsys"   , param->startsys , FLEN_VALUE);
  param->debug = read_boolean_param("debug");
  
  return(param);
}
