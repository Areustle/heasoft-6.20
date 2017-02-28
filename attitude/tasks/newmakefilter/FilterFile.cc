#include "FilterFile.hh"

#include "headas.h"
#include "headas_error.h"

#include "HDException.hh"

#include <cstring>
#include <iomanip>

/*******************************************************************************
*
*******************************************************************************/
FilterFile::FilterFile(const string& filename, bool clobber, const string& mission) throw(FITSException) {

    /*********************
    * save the file name *
    *********************/
    this->filename = filename;
    this->mission = mission;
    
    tzero=0.0;
    row=1l;

    /****************
    * open the file *
    ****************/
    int status=0;

    fits_create_file(&fp, filename.c_str(),&status);
    if(status == FILE_NOT_CREATED && clobber) {

        remove(filename.c_str());
        status = 0;
        fits_create_file(&fp, filename.c_str(), &status);
    }

    /*******************
    * check for errors *
    *******************/
    if(status) {
        throw FITSException("While opening "+filename, fp, status, __FILE__, __LINE__ );
    }

} // end of constructor

/*******************************************************************************
*
*******************************************************************************/
FilterFile::~FilterFile() {


} // end of destructor

/*****************************************************************************
*
*****************************************************************************/
void FilterFile::add(Interpolation* interp) {

// cout << "Adding interpolation to filter file"<<endl;

    /****************************************
    * do some accounting on the time values *
    ****************************************/
    HKFile* file = interp->get_file();
    
    if(get_ncolumns() == 0) {
        /*****************************
        * initialize the time values *
        *****************************/
        tzero  = file->get_tzero();
        tstart = file->get_tstart();
        tstop  = file->get_tstop();
    } else {
        /**********************
        * save extreme values *
        **********************/
        if(file->get_tzero()  < tzero  ) tzero  = file->get_tzero();
        if(file->get_tstart() < tstart ) tstart = file->get_tstart();
        if(file->get_tstop()  > tstop  ) tstop  = file->get_tstop();
    }

    /**********************
    * remember the column *
    **********************/
    columns.insert(columns.end(), interp);


} // end of add method

/****************************************************************************
*
****************************************************************************/
void FilterFile::write_header() throw(Exception) {

    headas_chat(4, "Writing FITS header\n");

    /***********************************************
    * get the total number of columns in the table *
    ***********************************************/
    int ncolumns = get_ncolumns() +1; // add one for the TIME column

    char** ttypes = new char*[ncolumns];
    char** tforms = new char*[ncolumns];
    char** tunits = new char*[ncolumns];

    /**************
    * time column *
    **************/
    ttypes[0] = "TIME";
    tforms[0] = "1D";
    tunits[0] = "s";
    
    /******************************
    * now the rest of the columns *
    ******************************/
    for(int i=1; i<ncolumns; ++i) {
        HKColumn* column = columns[i-1]->get_column();

        ttypes[i] = const_cast<char*>(column->get_out_name());
        tforms[i] = column->get_tform();
        tunits[i] = column->get_tunit();
        
//         cout << "ttype="<<ttypes[i]<<" tform="<<tforms[i]
//              <<" tunit="<<tunits[i]
//              <<endl;

    }

    /******************************
    * create the column extension *
    ******************************/
    int status=0;
    fits_create_tbl(fp,BINARY_TBL , 0l, ncolumns, ttypes,
                        tforms, tunits, "FILTER", &status);


    /***************************************************
    * now give the TTYPE keywords interesting comments *
    ***************************************************/
    fits_modify_comment(fp, "TTYPE1", "Time at which a value changed", &status);
    for(int i=2; i<=ncolumns; ++i) {
        HKColumn* column = columns[i-2]->get_column();

        char key[FLEN_KEYWORD];
        sprintf(key, "TTYPE%d", i);
        if (strlen(column->get_comment()) > 0 ) {
            fits_modify_comment(fp, key, column->get_comment(), &status);
        }
    }

    /*****************************************************
    * write the TNULL keywords
    *****************************************************/
    for(int i=2; i<= ncolumns; ++i) {
        HKColumn* column = columns[i-2]->get_column();
        char key[FLEN_KEYWORD];
        
        if(column->has_tnull_keyword()) {
            sprintf(key, "TNULL%d", i);

            long tnull = column->get_tnull();
            fits_write_key(fp, TLONG, key, &tnull, 0, &status);

        }

        sprintf(key, "TZERO%d", i);
        double z = column->get_tzero();
        if (column->get_coltype()== TULONG || column->get_coltype() == TDOUBLE  ||
	       column->get_coltype() == TFLOAT || 
               column->get_coltype() == TSBYTE || column->get_coltype() == TBYTE ||
               column->get_coltype() == TSHORT || column->get_coltype() == TLONG ||
               column->get_coltype() == TUSHORT ||
               column->get_coltype() == TINT || column->get_coltype()==TUINT) {
            fits_write_key(fp, TDOUBLE, key, &z, "data offset", &status); 
        }


        sprintf(key, "TSCAL%d", i);
        double s = column->get_tscale();
        if ( column->get_coltype()== TULONG ||  column->get_coltype() == TDOUBLE || 
	  	   column->get_coltype() == TFLOAT ||
                   column->get_coltype() == TSBYTE || column->get_coltype() == TBYTE ||
                   column->get_coltype() == TSHORT || column->get_coltype() == TLONG ||
                   column->get_coltype() == TUSHORT ||
                   column->get_coltype() == TINT || column->get_coltype()==TUINT) 
        {
            fits_write_key(fp, TDOUBLE, key, &s, "data scaling", &status); 
        }


        
        
    } // end of loop over columns



    /*******************
    * TIMEZERO keyword *
    *******************/
    fits_write_key(fp, TDOUBLE, "TIMEZERO", &tzero, 0, &status);
    
    /*******************
    * TELESCOP keyword *
    *******************/
    if(mission != "") {
        fits_write_key(fp, TSTRING, "TELESCOP", 
                       const_cast<char*>(mission.c_str()),
                       "Mission name", &status);
    }
    
    /****************************************************
    * PREFR and POSTFR - these tell maketime what to do *
    ****************************************************/
    fits_write_key_dbl(fp, "PREFR", 0.0, 1,
                       "For maketime - no time before a row", &status);

    fits_write_key_dbl(fp, "POSTFR", 1.0, 1,
                       "For maketime - all time after a row", &status);
                       
    /*************************
    * some other decorations *
    *************************/
    fits_write_date(fp, &status);
    HDpar_stamp(fp, 0, &status);
    if(HDerror_get()) {
        /*****************************
        * there was a non-FITS error *
        *****************************/
        throw HDException("While writing parameters to header", 
                          __FILE__, __LINE__);

    }

    /*******************
    * check for errors *
    *******************/
    if(status) {
        throw FITSException("While writing header ",
                            fp, status, __FILE__, __LINE__ );
    }


} // end of write_header method

/****************************************************************************
*
****************************************************************************/
bool FilterFile::write_row() throw(Exception) {

    /**********************************************************
    * find the earliest time from all the columns
    * For the first row we initialize the time to 
    * TSTART, since we want to force a write at TSTART
    * even if none of the input files have a row there.
    * The reason is that maketime clips rows at the 
    * first and last TIME value regardless of TSTART/TSTOP,
    * so we have to make sure that TSTART/TSTOP are the
    * same as the first and last TIMEs in the filter file.
    *********************************************************/
    double time=0.0;
    if(row==1) time = tstart;

    bool time_initialized=false;
    int ncolumns = get_ncolumns();
    for(int i=0; i<ncolumns; ++i) {
        Interpolation* interp = columns[i];
        
        if(interp->has_more_points() ) {
            /*******************************************
            * there are more time values to write here *
            *******************************************/
            double next_time = interp->next_time();

            if(!time_initialized || next_time < time )  {
                /************************
                * set the new best time *
                ************************/
                time = next_time;
                time_initialized=true;
            }
        }

    } // end of loop over columns


    /******************************************************
    * check if there's anything left to write.
    * We want to force a write at TSTOP for reasons
    * discussed above. Note it doesn't matter what values
    * we write in the last row, since maketime only uses
    * it to find the end time of the last GTI.
    ******************************************************/
    if(! time_initialized) {
//cout << "ran out of data tstop="<<setprecision(14)<<tstop<<endl;
        if(tstop > last_time ) time = tstop;
        else                   return false;
    }
    
    /*******************
    * debugging output *
    *******************/
    headas_chat(5, "Writing row %ld TIME=%.14g\n", row, time);
    

    /*****************
    * write the time *
    *****************/
    int status=0;
    double offset_time = time - tzero;
    fits_write_col(fp, TDOUBLE, 1, row, 1, 1, &offset_time, &status);
    if(status) {
        string message ="While writing TIME to row ";
        char number[32];
        sprintf(number, "%ld", row);
        message += number;

        throw FITSException(message, fp, status, __FILE__, __LINE__ );
    }

    /**************************
    * write the column values *
    **************************/
    for(int i=0; i<ncolumns; ++i) {
        Interpolation* interp = columns[i];

        /******************
        * write the value *
        ******************/
        interp->get_value(time).write(row, i+2, fp );

        /**********************************
        * read more input data if need be *
        **********************************/
        if(interp->has_more_points() && interp->next_time() == time ) {
            interp->advance();
        }

    } // end of loop over columns

    /********************************************************************
    * do time accounting 
    * although we get TSTART/TSTOP from the input files, these could 
    * be overridden if the time values in the input files are outside
    * the TSTART/TSTOP in those files. The shouldn't be, but they could.
    *******************************************************************/
    if(row==1) tstart = time;
    else       last_time  = time;
    
    /*************************
    * advance the row number *
    *************************/
    ++row;

   // cout << "done writing row"<<endl;
    
    return true;

} // end of write_row method

/***************************************************************************
*
***************************************************************************/
void FilterFile::close() throw(FITSException) {

    int status=0;

    fits_write_key(fp, TDOUBLE, "TSTART", &tstart, 0, &status);
    fits_write_key(fp, TDOUBLE, "TSTOP",  &last_time,  0, &status);
    
    fits_write_chksum(fp, &status);
    fits_close_file(fp, &status);
    
    if(status) {
        throw FITSException("While closing", fp, status, __FILE__, __LINE__ );
    }

} // end of close method
