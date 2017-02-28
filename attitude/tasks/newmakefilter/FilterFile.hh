#ifndef FILTER_FILE_INCLUDED
#define FILTER_FILE_INCLUDED

#include <vector>
#include <string>
#include "Interpolation.hh"

extern "C" {
#include "fitsio.h"
}

class FilterFile {

private:

    string filename;
    fitsfile* fp;
    long row;

    string mission;
    
    double tzero;
    double tstart;
    double tstop;
    
    double last_time;

    vector<Interpolation*> columns;
    
public:

    FilterFile(const string& filename, bool clobber, const string& mission) throw(FITSException);
    virtual ~FilterFile();
    
    virtual int get_ncolumns() { return columns.size(); }

    virtual void add(Interpolation* interp);
    virtual void write_header() throw(Exception);
    
    virtual bool write_row() throw(Exception);
    
    virtual void close() throw(FITSException);

}; // end of FilterFile class

#endif // FILTER_FILE_INCLUDED
