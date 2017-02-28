#ifndef CONFIGHH
#define CONFIGHH

#include <fstream>
#include <cstring>
#include <cctype>
#include <cstdlib>
#include <vector>
#include <string>
#include <stdexcept>

#include "FITSFileBroker.hh"
#include "FilterFile.hh"

#include "hkcolumn.hh"
#include "Param.hh"

class Config {

protected:

    string filename;
    string infile_root;

    FilterFile* filter;
    FITSFileBroker* fits_broker;

protected:
    /*************************
    * constructor/destructor *
    *************************/
    Config(const string& filenam, FITSFileBroker* fits_broker, FilterFile* filter );
public:
    virtual ~Config();

public:
    /*****************************************************
    * use this factory method instead of the constructor *
    *****************************************************/
    static FilterFile* read(Param* param) throw(Exception);

protected:

    virtual void add(string incol,
                     const string& file_suffix,
                     const string& extension,
                     const string& interp,
                     const string& calib,
                     string outcol,
                     const string& comment) throw(Exception);

    virtual bool read_next() throw(Exception) {return false;}
    virtual void close() throw(Exception);


}; // end of Config class

#endif
