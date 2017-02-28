#include "headas.h"

#include "hkfile.hh"

/************************************************************************
* constructor
*************************************************************************/
HKFile::HKFile(const string& filename) throw(FITSException) {

    /*******************
    * debugging output *
    *******************/
    headas_chat(3, "Opening %s\n", filename.c_str());

    /*********************
    * save the file name *
    *********************/
    this->filename = filename;

    /****************
    * open the file *
    ****************/
    int status = 0;
    fitspt=NULL;
    fits_open_file(&fitspt, filename.c_str(), READONLY, &status);
    if(status) {
        throw FITSException("While opening "+filename,
                            NULL, status, __FILE__, __LINE__ );
    }

    /***********************
    * find the time column *
    ***********************/
    if(fits_get_colnum(fitspt, CASEINSEN, "TIME", &timecol, &status)) {
        throw FITSException("While finding TIME column",
                            fitspt, status, __FILE__, __LINE__ );
    }


    /**************************************
    * get the number of rows in the table *
    **************************************/
    fits_get_num_rows (fitspt, &nrows, &status);
    if(status) {
        throw FITSException("While determining number of rows",
                            fitspt, status, __FILE__, __LINE__ );
    }



    /*************
    * read TZERO *
    *************/
    fits_read_key(fitspt, TDOUBLE, "TIMEZERO", &tzero, 0, &status);
    if(status == KEY_NO_EXIST) {
        status = 0;
        fits_clear_errmsg();
        tzero = 0.0;
    } else if(status) {
        throw FITSException("While reading TIMEZERO",
                            fitspt, status, __FILE__, __LINE__ );
    }

    /**************
    * find TSTART *
    **************/
    fits_read_key(fitspt, TDOUBLE, "TSTART", &tstart, 0, &status);
    if(status == KEY_NO_EXIST) {
        status = 0;
        fits_clear_errmsg();
        /*********************************************************
        * use the first TIME value if there is no TSTART keyword *
        *********************************************************/
        tstart = 0.0;
        if(nrows>0) {
            double null=1.0;
            int anynull=0;
            long row = 1l;
            do {
                fits_read_col(fitspt, TDOUBLE, timecol, row++, 1l, 1l,
                            &null, &tstart, &anynull, &status);
            } while(anynull && !status);
        }

    }
    
    /******************************
    * check for stray FITS errors *
    ******************************/
    if(status) {
        throw FITSException("While reading TSTART",
                            fitspt, status, __FILE__, __LINE__ );
    }
    
    /**************
    * find TSTOP *
    **************/
    fits_read_key(fitspt, TDOUBLE, "TSTOP", &tstop, 0, &status);
    if(status == KEY_NO_EXIST) {
        status = 0;
        fits_clear_errmsg();
        /*********************************************************
        * use the first TIME value if there is no TSTART keyword *
        *********************************************************/
        tstop = 0.0;
        if(nrows>0) {
            double null=1.0;
            int anynull=0;
            long row=nrows;
            do {
                fits_read_col(fitspt, TDOUBLE, timecol, row--, 1l, 1l,
                            &null, &tstop, &anynull, &status);

            } while(anynull && ! status);
        }

    }
    
    headas_chat(3, "File %s TSTART=%.14f TSTOP=%.14f\n", 
                filename.c_str(), tstart, tstop);
    
    /******************************
    * check for stray FITS errors *
    ******************************/
    if(status) {
        throw FITSException("While reading TSTOP",
                            fitspt, status, __FILE__, __LINE__ );
    }


} // end of constructor

/************************************************************************
* destructor - close the underlying fitsfile
************************************************************************/
HKFile::~HKFile()
{
    int status = 0;
    fits_close_file(fitspt, &status);    
}


