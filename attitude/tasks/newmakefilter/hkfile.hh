#ifndef   HKFILEHH
#define   HKFILEHH

#include <iostream>
#include <string>


#include "hkcolumn.hh"
extern "C" {
#include "fitsio.h"
}


/***************************************************************************
* This class is basicly a wrapper around a fitsfile. It is mostly used
* to hold together a bunch of metadata about an input HK file.
***************************************************************************/
class HKFile {

    string filename;
    fitsfile *fitspt;
    long nrows;

    int timecol;
    double tzero;

    double tstart;
    double tstop;

public:
    HKFile(const string& filename ) throw(FITSException);

    virtual ~HKFile();

    fitsfile* get_fits() { return fitspt;}

    const string& get_filename() { return filename;}
    int           get_timecol()  { return timecol;}
    double        get_tzero()    { return tzero;}
    long          get_nrows()    { return nrows;}
    double        get_tstart()   { return tstart; }
    double        get_tstop()    { return tstop; }

};
#endif
