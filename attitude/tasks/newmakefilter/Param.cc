#include "Param.hh"

#include "pil.h"

/*******************************************************************************
* constructor
*******************************************************************************/
Param::Param() throw(PILException) {

    configure_file = read_string("configure");
    infile_root    = read_string("infileroot");
    outfile        = read_string("outfile");
    mission        = read_string("mission");
    clobber        = read_boolean("clobber");

} // end of constructor

/****************************************************************************
*
****************************************************************************/
double Param::read_double(const string& name) throw(PILException) {

    int status=0;
    double value;

    status = PILGetReal(name.c_str(), &value);
    if(status) {
        throw PILException("While reading "+name+" parameter", status,
                           __FILE__, __LINE__);
    }

    return value;

} // end of read_string method


/****************************************************************************
*
****************************************************************************/
bool Param::read_boolean(const string& name) throw(PILException) {

    int status=0;
    int value;

    status = PILGetBool(name.c_str(), &value);
    if(status) {
        throw PILException("While reading "+name+" parameter", status,
                           __FILE__, __LINE__);
    }

    return (bool)value;

} // end of read_string method


/****************************************************************************
*
****************************************************************************/
string& Param::read_string(const string& name) throw(PILException) {

    int status=0;
    char buffer[512];

    status = PILGetString(name.c_str(), buffer);
    if(status) {
        throw PILException("While reading "+name+" parameter", status,
                           __FILE__, __LINE__);
    }

    string* result = new string(buffer);
    return *result;

} // end of read_string method


