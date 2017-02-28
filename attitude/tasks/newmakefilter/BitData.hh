#ifndef BIT_DATA_INCLUDED
#define BIT_DATA_INCLUDED

#include "Data.hh"

class BitData : public Data {

private:
    int nbits;
    unsigned char* value;
    unsigned char padding_mask;

public:
    BitData(int nbits);
    virtual ~BitData() { delete[] value; }

public:
    virtual bool equals(const Data* data) const;
    virtual void print(ostream& out) const;

    virtual void read( long row, int col, fitsfile* fp) throw(FITSException);

    virtual void set_to_null();

    
protected:
    virtual void* get_pointer(long index) const { return (void*)(value+index); }


}; // end of BitData class



#endif // BIT_DATA_INCLUDED
