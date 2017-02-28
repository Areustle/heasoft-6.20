#ifndef BOOLEAN_DATA_INCLUDED
#define BOOLEAN_DATA_INCLUDED

#include "Data.hh"

class BooleanData : public Data {

private:
    char* value;

public:
    BooleanData(int dimen);
    virtual ~BooleanData() { delete[] value; }

public:
    virtual bool equals(const Data* data) const;
    virtual void print(ostream& out) const;

    
protected:
    virtual void* get_pointer(long index) const { return (void*)(value+index); }


}; // end of BooleanData class



#endif // BOOLEAN_DATA_INCLUDED
