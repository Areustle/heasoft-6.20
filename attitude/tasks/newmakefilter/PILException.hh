#ifndef PIL_EXCEPTION_INCLUDED
#define PIL_EXCEPTION_INCLUDED


#include "Exception.hh"


extern "C" {
#include "fitsio.h"
}

/********************************************************************
* this exception indicates a generic problem with the configuration file.
********************************************************************/
class PILException : public Exception {

    int status;


    public:
        PILException(const string& context, int status,
                     const char* file, int line) 
          : Exception(assemble_error_message(context, status), file, line) {}

        virtual int       get_status() { return status; }

    private:
        static string assemble_error_message(const string& context, int status);

}; // end of IOException embedded class

#endif // PIL_EXCEPTION_INCLUDED
