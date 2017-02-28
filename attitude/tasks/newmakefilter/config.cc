#include <iostream>
#include "config.hh"

#include "headas.h"

#include "ASCIIConfig.hh"
#include "FITSConfig.hh"

#include "FITSFileBroker.hh"


/*************************************************************************
* constructor
*************************************************************************/
Config::Config(const string& filename,
               FITSFileBroker* fits_broker, FilterFile* filter) {

    this->filename = filename;

    this->fits_broker = fits_broker;
    this->filter = filter;


} // end of Config constructor

/*************************************************************************
* destructor
*************************************************************************/
Config::~Config() {}

/******************************************************************************
* reads a configuration file and returns a fully configured FilterFile object
******************************************************************************/
FilterFile* Config::read(Param* param) throw(Exception) {

    /***********************************
    * get some things from the parfile *
    ***********************************/
    string filename = param->get_configure_file();

    FITSFileBroker* fits_broker = new FITSFileBroker(param->get_infile_root());

    FilterFile* filter = new FilterFile(param->get_outfile(), 
                                        param->get_clobber(),
                                        param->get_mission());

    /*******************************
    * initialize the config object *
    *******************************/
    Config* config = NULL;

    /*************************************************
    * determine the type of file we are dealing with *
    * first try opening it as a FITS file
    *************************************************/
    fitsfile* fp = NULL;
    int status=0;
    fits_open_file(&fp, filename.c_str(), READONLY, &status);
    if (status) {
        /**********************************************
        * opening as FITS didn't work so try as ASCII *
        **********************************************/
        headas_chat(1, "Reading ASCII configuration file %s\n", filename.c_str());
        fits_clear_errmsg();
        config = new ASCIIConfig(filename, fits_broker, filter);

    } else {
        /*************************
        * yup, it's a FITS file. *
        *************************/
        headas_chat(1, "Reading FITS configuration file %s\n", filename.c_str());
        config = new FITSConfig(fp, fits_broker, filter);
    }



    /******************************
    * read the configuration file *
    ******************************/
    while(config->read_next());
    config->close();

    return filter;

} // end of read static method


/**************************************************************************
* returns information about the next column listed in the config file
**************************************************************************/
void Config::close() throw(Exception) {

}

/**************************************************************************
* record an entry from a configuration file
**************************************************************************/
void Config::add(string incol,
                 const string& file_suffix,
                 const string& extension,
                 const string& interp,
                 const string& calib,
                 string outcol,
                 const string& comment) throw(Exception) {

    /**************************************************
    * the output columns defaults to the input column *
    **************************************************/
    if(outcol == "%") outcol = incol;
    else              outcol = outcol;

    headas_chat(4, "Configuring %s\n", outcol.c_str());

    /*********************************
    * handle on-the-fly calculations *
    *********************************/
    string column_specification("");
    if(incol[0] == '%') {
        /*********************************************
        * the input columns is calculated on the fly *
        *********************************************/
        column_specification="[COL TIME; " + outcol +" = "+ incol.substr(1) + "]";
        incol = outcol;
    } else {
        /**********************************
        * take the input column literally *
        **********************************/
        incol = incol;

    }

    /*****************************
    * now assemble the file name *
    *****************************/
    string filename = file_suffix+"["+extension+"]"+column_specification;


    /*******************************************************
    * open the FITS file using the file broker.
    *******************************************************/
    HKFile* file = fits_broker->open(filename);
    if(file == NULL) {
        /*************************
        * couldn't open the file *
        *************************/
        headas_chat(1,"Ignoring parameter %s because file *%s does not exist\n",
                    incol.c_str(), filename.c_str());
        return;

    }


    /****************************************************************
    * now create the input column object
    ****************************************************************/
    HKColumn* column = new HKColumn(incol.c_str(), file->get_fits(),
                                    calib, interp, comment.c_str(),
                                    outcol );


    filter->add(Interpolation::create(file, column));

} // end of add method


