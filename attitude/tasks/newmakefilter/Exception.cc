#include "Exception.hh"

#include <cstdio>



string Exception::assemble_error_message(const string& message, const char* file, int line) {

    string error_message = "";

    if(file != NULL ) {
        /***********************************************
        * add the file and line number if they are set *
        ***********************************************/    
        char number[16];
        sprintf(number, "%d", line);

        error_message += "The following Exception was thrown from";
        error_message += " file ";
        error_message +=  file;
        error_message += " line ";
        error_message += number;
        error_message += "\n";

    } // end if tagging file and line number

    error_message += message;

    return error_message;

} // end of assemble_error_message static method
