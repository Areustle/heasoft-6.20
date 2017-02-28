#ifndef FITS_EXCEPTION_INCLUDED
#define FITS_EXCEPTION_INCLUDED


#include "Exception.hh"


extern "C" {
#include "fitsio.h"
}

/********************************************************************
* this exception indicates a generic problem with the configuration file.
********************************************************************/
class FITSException : public Exception {

    int status;
    fitsfile* fp;

    public:
        FITSException(const string& context, fitsfile* fp, int status,
                      const char* file, int line);
                      
        virtual fitsfile* get_fits()   { return fp; }
        virtual int       get_status() { return status; }

    private:
        static string assemble_error_message(const string& context,
                                              fitsfile* fp,int status);

}; // end of IOException embedded class

#endif // FITS_EXCEPTION_INCLUDED
