#include "PILException.hh"

#include "pil.h"

/****************************************************************************
* static method used by the constructor
****************************************************************************/
string PILException::assemble_error_message(const string& context, int status) {


    char number[16];

    string error_message="";

    /***********************************
    * report the context of this error *
    ***********************************/
    error_message += context + ":\n";

    /**************************
    * report the status value *
    **************************/
    sprintf(number, "%d", status);

    error_message += "PIL status=";
    error_message += number;
    

    /***************************
    * ...and the status string *
    ***************************/
    char status_string[FLEN_STATUS];
    fits_get_errstatus(status, status_string);

    error_message += ": ";
    error_message += PIL_err_handler(status);

    return error_message;

} // end of get_fits_error_message method
