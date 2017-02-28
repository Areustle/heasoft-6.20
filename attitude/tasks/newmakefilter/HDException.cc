#include "HDException.hh"

#include "headas_error.h"

/********************************************************************************
*
********************************************************************************/
string HDException::assemble_error_message(const string& message) {

    int status = HDerror_get();
    
    char number[32];
    sprintf(number, "%d", status);

    string error_message = "";
    error_message += "HEAdas error status=";
    error_message += number;
    error_message += "\n";
    error_message += message;
    
    return error_message;
    
} // end of assemble_error_message method

/********************************************************************************
*
********************************************************************************/
HDException::HDException(const string& message, const char* file, int line) 
            :Exception(assemble_error_message(message), file, line) {
            
    status = HDerror_get();

} // end of constructor

