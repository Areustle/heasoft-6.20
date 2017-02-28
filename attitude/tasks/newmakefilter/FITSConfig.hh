#ifndef FITS_CONFIG_INCLUDED
#define FITS_CONFIG_INCLUDED

#include "config.hh"

#include <string>
#include "FITSException.hh"

extern "C" {
#include "fitsio.h"
}

class FITSConfig : public Config {

private:
    fitsfile* fp;
    int status;
    long row;
    long nrows;
    
    int incol_col;
    int file_col;
    int extension_col;
    int interp_col;
   // int calib_col;
    int outcol_col;
    int comment_col;
    
    char* incol;
    char* insuffix;
    char* extension;
    char* interp;
 //   char* calib;
    char* outcol;
    char* comment;
    


public:
    /**************
    * constructor *
    **************/
    FITSConfig(fitsfile* fp, FITSFileBroker* fits_broker, FilterFile* filter) throw(FITSException);
    
protected:
    virtual bool read_next() throw(Exception);
    virtual void close() throw(Exception);



private:
    /**********************************************************
    * this is kind of a cludge to get the fitsfile name out
    * of the fitsfile struct in the constructor
    **********************************************************/
    static char* temp_filename;

    static char* extract_filename(fitsfile* fp) {
        int status=0;
        fits_file_name(fp, temp_filename, &status);
        if(status) temp_filename[0] = '\0';
        return temp_filename;
    }


}; // end of FITSConfig class


#endif // FITS_CONFIG_INCLUDED
