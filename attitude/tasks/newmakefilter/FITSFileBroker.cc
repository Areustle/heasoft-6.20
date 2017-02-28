#include "FITSFileBroker.hh"

#include "headas.h"

/**************************************************************************
*
**************************************************************************/
FITSFileBroker::FITSFileBroker(const string& filename_root) {

    this->filename_root = filename_root;
    
} // end of constructor


/**************************************************************************
* returns an HKFile structure corresponding to a given filename.
* identical filenames will always return the same HKFile structure.
* Returns NULL if the file does not exist, and throws a FITSException
* if there was a problem opening the FITS file.
* Note only one attempt will be made to open each file. If the first attempt
* failed, then this method will return NULL on all subsequence attempts
* to open the file.
**************************************************************************/
HKFile* FITSFileBroker::open(const string& filename_suffix) throw(FITSException) {

    string filename = filename_root + filename_suffix;

    /*********************************************
    * check if this file is a known troublemaker *
    *********************************************/
    if(bad.find(filename) != bad.end() ) {
        return NULL;
    }


    /*****************************************
    * see if we have opened the file already *
    *****************************************/
    HKFile* file = files[filename];
    if(file) return file;

    /***********************************************
    * haven't heard of this one before, so open it *
    ***********************************************/
    try {
        /*****************
        * try opening it *
        *****************/
        file = new HKFile(filename.c_str());

    } catch(FITSException& e) {
        /*************************
        * couldn't open the file *
        *************************/
        bad.insert(bad.end(), filename);

        if(e.get_status() == FILE_NOT_OPENED ) return NULL;
        else                                   throw e;
    }

    /*************************************
    * opened sucessfully, so remember it *
    *************************************/
    files[filename] = file;
    list.insert(list.end(), file);

    return file;

} // end of open method
