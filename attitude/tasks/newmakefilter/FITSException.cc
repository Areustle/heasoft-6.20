#include "FITSException.hh"


/****************************************************************************
* constructor
****************************************************************************/
FITSException::FITSException(const string& context, fitsfile* fp, int status,
                             const char* file, int line)
              : Exception(assemble_error_message(context, fp, status), file, line) {
              
    this->status = status;
    this->fp = fp;

} // end of constructor


/****************************************************************************
* static method used by the constructor
****************************************************************************/
string FITSException::assemble_error_message(const string& context,
                                              fitsfile* fp, int status) {

    char number[16];
    
    string error_message="";

    /***********************************
    * report the context of this error *
    ***********************************/
    error_message += context + ":\n";
    
    /************************************
    * report information about the file *
    ************************************/
    if(fp != NULL) {
        int local_status=0;

        char filename[FLEN_FILENAME];
        fits_file_name(fp, filename, &local_status);
        if(! local_status) {
            error_message += "FITS file: ";
            error_message += filename;


            int hdu=0;
            fits_get_hdu_num(fp, &hdu);
            sprintf(number, "%d", hdu);
            error_message += " HDU: ";
            error_message += number;
            error_message += "\n";
        }
    }

    /**************************
    * report the status value *
    **************************/
    sprintf(number, "%d", status);

    error_message += "FITSIO status=";
    error_message += number;

    /***************************
    * ...and the status string *
    ***************************/
    char status_string[FLEN_STATUS];
    fits_get_errstatus(status, status_string);

    error_message += ": ";
    error_message += status_string;

    /********************************
    * now dump the FITS error stack *
    ********************************/
    char fits_error_message[FLEN_ERRMSG];
    while(fits_read_errmsg(fits_error_message) ) {
        error_message += "\n";
        error_message += fits_error_message;
    }

    /****************************************************************
    * clear the error stack so we don't report the same error twice *
    ****************************************************************/
    fits_clear_errmsg();

    return error_message;

} // end of get_fits_error_message method
