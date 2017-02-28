#ifndef DATA_INCLUDED
#define DATA_INCLUDED

#include <iostream>

extern "C" {
#include "fitsio.h"
}

#include "FITSException.hh"
#include "hkcolumn.hh"

/********************************************************************************
* This is a common base class for all the different ways to store data in a
* FITS column. Data objects should never be created and manipulated on their own.
* Instead they should always be wrapped in a Value object. Sounds crazy, I know,
* but it's necessary in order to make operators work properly with inheritance.
* The problem is that operators like "+" have to return by value, otherwise
* thelanguage does not clean up temporary variables properly. But if you
* return a subclass by value, it gets sliced.
********************************************************************************/
class Data {
protected:
    long dimen;
    int type;

    char* nullflags;
    int anynull;
public:

    Data(long dimen, int type) {
        this->dimen = dimen;
        this->type = type;
        nullflags = new char[dimen];
        anynull=0;
    }

    virtual void set_to_null() {
        anynull=1;
        for(long i=0; i< dimen; ++i) nullflags[i] =1;
    }

    virtual ~Data() {
        delete [] nullflags;
    }
    
    static Data* create(HKColumn* column) throw(Exception);


    /**********
    * methods *
    **********/

    virtual void read( long row, int col, fitsfile* fp) throw(FITSException);
    virtual void write(long row, int col, fitsfile* fp) const throw(FITSException);

    virtual bool equals(const Data* data) const =0;
    virtual void print(ostream& out) const =0;

    virtual Data*  plus(const Data* data) const throw(Exception);
    virtual Data*  plus(double scalar)    const throw(Exception);
    
    virtual Data*  times(const Data* data) const throw(Exception);
    virtual Data*  times(double scalar)    const throw(Exception);
       
    virtual Data*  minus(     const Data* data) const throw(Exception);
    virtual Data*  minus(     double scalar)    const throw(Exception);
    virtual Data*  minus_from(double scalar)    const throw(Exception);

    virtual Data*  over(const Data* data) const throw(Exception);
    virtual Data*  over( double scalar)   const throw(Exception);
    virtual Data*  under(double scalar)   const throw(Exception);
    
protected:

    virtual void* get_pointer(long index) const=0;

    virtual void check_consistency(const Data* data) const throw(Exception);

    virtual void merge_nulls(const Data* left, const Data* right) throw(Exception);
    virtual void  copy_nulls(const Data* data)                    throw(Exception);

}; // end of Data class

#endif // DATA_INCLUDED
