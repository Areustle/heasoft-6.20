#include "FITSConfig.hh"

#include "headas.h"

char* FITSConfig::temp_filename = new char[FLEN_FILENAME];

/***************************************************************************
* constructor
***************************************************************************/
FITSConfig::FITSConfig(fitsfile* fp,
                       FITSFileBroker* fits_broker, FilterFile* filter) throw(FITSException)
            : Config(extract_filename(fp), fits_broker, filter) {

    this->fp = fp;
    status=0;
    row=1l;


    /*************************************
    * determine the total number of rows *
    *************************************/
    fits_get_num_rows(fp, &nrows, &status);


    /***********************
    * find all the columns *
    ***********************/
    fits_get_colnum(fp, CASEINSEN, "INCOL",     &incol_col,     &status);
    fits_get_colnum(fp, CASEINSEN, "FILE",      &file_col,      &status);
    fits_get_colnum(fp, CASEINSEN, "EXTENSION", &extension_col, &status);
    fits_get_colnum(fp, CASEINSEN, "INTERP",    &interp_col,    &status);
  //  fits_get_colnum(fp, CASEINSEN, "CALIB",     &calib_col,     &status);
    fits_get_colnum(fp, CASEINSEN, "OUTCOL",    &outcol_col,    &status);
    fits_get_colnum(fp, CASEINSEN, "COMMENT",   &comment_col,   &status);

    if(status) {
        throw FITSException("While finding columns in config file", fp, status, __FILE__, __LINE__);
    }



    /************************************************************************
    * get the size of each columns so we can allocate data buffers for them *
    ************************************************************************/
    int type;
    long repeat;
    long width;

    fits_get_coltype(fp, incol_col, &type, &repeat, &width, &status);
    incol = new char[width+1];

    fits_get_coltype(fp, file_col, &type, &repeat, &width, &status);
    insuffix = new char[width+1];
    
    fits_get_coltype(fp, extension_col, &type, &repeat, &width, &status);
    extension = new char[width+1];

    fits_get_coltype(fp, interp_col, &type, &repeat, &width, &status);
    interp = new char[width+1];
    
//     fits_get_coltype(fp, calib_col, &type, &repeat, &width, &status);
//     calib = new char[width+1];

    fits_get_coltype(fp, outcol_col, &type, &repeat, &width, &status);
    outcol = new char[width+1];

    fits_get_coltype(fp, comment_col, &type, &repeat, &width, &status);
    comment = new char[width+1];

    if(status) {
        throw FITSException("While getting string witdth in config file", fp, status, __FILE__, __LINE__);
    }


} // end of constructor


/**************************************************************************
* returns information about the next column listed in the config file
**************************************************************************/
bool FITSConfig::read_next()  throw(Exception) {

    if(status) return false;

    if(nrows> nrows) return false;
    
    headas_chat(3, "Reading config row %ld\n", row);

    /***********************
    * read the current row *
    ***********************/
    fits_read_col(fp,TSTRING,     incol_col, row, 1, 1, 0, incol,    0, &status);
    fits_read_col(fp,TSTRING,      file_col, row, 1, 1, 0, insuffix, 0, &status);
    fits_read_col(fp,TSTRING, extension_col, row, 1, 1, 0, extension,0, &status);
    fits_read_col(fp,TSTRING,    interp_col, row, 1, 1, 0, interp,   0, &status);
  //  fits_read_col(fp,TSTRING,     calib_col, row, 1, 1, 0, calib,    0, &status);
    fits_read_col(fp,TSTRING,    outcol_col, row, 1, 1, 0, outcol,   0, &status);
    fits_read_col(fp,TSTRING,   comment_col, row, 1, 1, 0, comment,  0, &status);
    ++row;
    
    if(status) {
        char number[32];
        sprintf(number, "%ld", row);
        string message = "While reading row ";
        message += number;
        message += " of config file";
        throw FITSException(message, fp, status, __FILE__, __LINE__);
    }
    
    /*********************************************************
    * calibration is not used, so we give it a default value *
    *********************************************************/
    string calib="%";

    /**************************************
    * record the information in this line *
    **************************************/
    add(incol,
        insuffix,
        extension,
        interp,
        calib,
        outcol,
        comment);
        
    /********************************************
    * return true to say that we are not at EOF *
    ********************************************/
    return true;

    
} // end of read_next method



/**************************************************************************
* returns information about the next column listed in the config file
**************************************************************************/
void FITSConfig::close() throw(Exception) {

    fits_close_file(fp, &status);

    if(status) {
        throw FITSException("While closing config file", fp, status, __FILE__, __LINE__);
    }
}
