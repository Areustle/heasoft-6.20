#ifndef STRING_DATA_INCLUDED
#define STRING_DATA_INCLUDED

#include "Data.hh"

class StringData : public Data {

private:
    int size;
    char* value;

public:
    StringData(int size) : Data(1l, TSTRING) {
        this->size = size+1 ; // allow for terminating null
        value = new char[size+1];
    }

    virtual ~StringData() { delete[] value; }

public:
    virtual bool equals(const Data* data) const;
    virtual void print(ostream& out) const;
    
    virtual void read( long row, int col, fitsfile* fp) throw(FITSException);

    
protected:
    virtual void* get_pointer(long index) const { return (void*)(&value); }


}; // end of StringData class



#endif // STRING_DATA_INCLUDED
