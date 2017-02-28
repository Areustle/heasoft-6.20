#include "hkcolumn.hh"

#include "headas.h"

#include <cstring>

HKColumn::HKColumn(const string& name,  fitsfile* fp,
                   const string& calibration, const string& interpolation,
                   const char *xcom, const string& out_name ) throw(FITSException) { 

    /****************************
    * remember some things *
    ***********************/
    this->name = name;
    strncpy(this->comment, xcom, FLEN_COMMENT);
  
   // this->fitspt = xfitspt;

    this->calibration = calibration;
    this->interpolation = interpolation;
    this->out_name = out_name;

    /*********************************************
    * read column information from the FITS file *
    *********************************************/
    char key[FLEN_KEYWORD];
    int status = 0;

    /******************
    * find the column *
    ******************/
    if(fits_get_colnum(fp,CASEINSEN, const_cast<char*>(name.c_str()),
                       &colnum, &status)) {

        throw FITSException("While finding column "+name,
                            NULL, status, __FILE__, __LINE__ );

    }

    /********
    * TFORM *
    ********/
    sprintf(key, "TFORM%d",colnum);
    if(fits_read_key(fp, TSTRING, key, tform, NULL, &status)) {

        throw FITSException("While reading TFORM for column "+name,
                            NULL, status, __FILE__, __LINE__ );
    }


    /********
    * TUNIT *
    ********/
    sprintf(key, "TUNIT%d",colnum);
    fits_read_key(fp,TSTRING,key,tunit,0,&status);
    if(status == KEY_NO_EXIST) {
        /*******************
        * blank by default *
        *******************/
        tunit[0] = '\0';
        status = 0;
        fits_clear_errmsg();
    } else if(status) {
        /********
        * error *
        ********/
        throw FITSException("While reading TUNIT for column "+name,
                            fp, status, __FILE__, __LINE__ );
    }

    /****************************
    * TNULL for integer columns *
    ****************************/
    has_tnull=false;

    int type;
    fits_binary_tform(tform, &type, NULL, NULL, &status);
    coltype = type;
    if(type == TLONG || type == TINT || type == TSHORT || type == TBYTE ) {

        sprintf(key, "TNULL%d",colnum);
        fits_read_key(fp,TLONG,key,&tnull,0,&status);
        
        if(status==KEY_NO_EXIST) {
            /***********************************************************
            * there's no TNULL, so we are going to need to make one up *
            ***********************************************************/
            char filename[FLEN_FILENAME];
            fits_file_name(fp, filename, &status);
            headas_chat(1, "Warning: column %s in file %s has no TNULL keyword\n",
                        name.c_str(), filename);

            /****************************************************************
            * pick a TNULL value
            * Note the compiler complains if we say
            * -2147483648, probably because it first tries to represent the
            * positive number and then take the negative of it.
            * so that's whay the funny "-1". limits.h actually
            * does the same thing.
            ****************************************************************/
            if(type == TLONG ) tnull = -2147483647 -1; // 32 bit
            if(type == TSHORT) tnull = -32768;      // 16 bit
            if(type == TBYTE)  tnull = 255;         // 8 bit;
            headas_chat(1, "Will use %ld for TNULL\n\n", tnull);

            has_tnull=true;
            status = 0;
            fits_clear_errmsg();
        } else if(status) {
            /***********************
            * caught a stray error *
            ***********************/
            throw FITSException("While reading TNULL for column "+name,
                    fp, status, __FILE__, __LINE__ );
        } else {
            /***********************
            * we got a TNULL value *
            ***********************/
            has_tnull = true;
        }
    } // end if this coulumn could have TNULL


    /********
    * TZERO *
    ********/
    sprintf(key, "TZERO%d",colnum);
    fits_read_key(fp,TDOUBLE,key,&tzero,0,&status);
    if(status == KEY_NO_EXIST) {
        /******************
        * defaults to 0.0 *
        ******************/
        tzero = 0.0;
        status =0;
        fits_clear_errmsg();
    } else if(status) {
        /*******************
        * unexpected error *
        *******************/
        throw FITSException("While reading TZERO for column "+name,
                            fp, status, __FILE__, __LINE__ );
    }

    /********
    * TSCAL *
    ********/
    sprintf(key, "TSCAL%d",colnum);
    tscal = 1.0;
    fits_read_key(fp,TDOUBLE,key, &tscal,0,&status);
    if(status == KEY_NO_EXIST) {
        /******************
        * defaults to 1.0 *
        ******************/
        tscal = 1.0;
        status =0;
        fits_clear_errmsg();
    } else if(status) {
        /*******************
        * unexpected error *
        *******************/
        throw FITSException("While reading TSCAL for column "+name,
                            fp, status, __FILE__, __LINE__ );
    }

    /****************************
    * fill in a default comment *
    ****************************/
    if(!strcmp(comment, "%") ) {
        sprintf(key, "TTYPE%d", colnum);
        char value[FLEN_VALUE];

        /*******************
        * read the comment *
        *******************/
        fits_read_keyword(fp, key, value, comment, &status);
        if(status) {
            throw FITSException("While reading comment for column "+name,
                        fp, status, __FILE__, __LINE__ );
        }
    } // end if default comment was requested




} // end of constructor

