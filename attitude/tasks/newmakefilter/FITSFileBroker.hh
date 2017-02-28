#ifndef FITS_FILE_BROKER_INCLUDED
#define FITS_FILE_BROKER_INCLUDED

extern "C" {
#include "fitsio.h"
}

#include <string>
#include <map>
#include <set>
#include <vector>
#include <stdexcept>

#include "hkfile.hh"

/********************************************************************************
* This class is a repository for fitsfile* pointers. If a particular file
********************************************************************************/
class FITSFileBroker {

public:


private:
    map<string, HKFile*> files;
    vector<HKFile*> list;
    set<string> bad;
    
    string filename_root;


public:
    FITSFileBroker(const string& filename_root);

    virtual ~FITSFileBroker() {}
    virtual HKFile* open(const string& filename) throw(FITSException);
    
    virtual int get_nfiles() { return list.size(); }
    virtual HKFile* get_file(int i) { return list[i]; }

}; // end of FITSFileBroker class

#endif // FITS_FILE_BROKER_INCLUDED
