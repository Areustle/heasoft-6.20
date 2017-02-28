#include <stdlib.h>

#include "param.h"
#include "param_wrappers.h"
#include "fitsio.h"
#include "headas.h"

/*******************************************************************************
*
*******************************************************************************/
PARAM* readParam() {


PARAM* param;

param = (PARAM*)malloc(sizeof(PARAM));

read_string_param("command", param->command, COMMAND_PARAM_LENGTH);
read_string_param("outfile", param->outfile, FILENAME_LENGTH);

return param;

} /* end of readParam function */



int parstamp_path (const char * path)
{
   fitsfile * fptr = 0;
   int status = 0;

   fits_open_file(&fptr, path, READWRITE, &status);

   HDpar_stamp(fptr, 0, &status);
   fits_write_chksum(fptr, &status);

   if (fptr) {
      int tmp = 0;
      fits_close_file(fptr, &tmp);
   }

   fits_report_error(heaout, status);

   return status;
}

