#ifndef POINT_INCLUDED
#define POINT_INCLUDED

#include "hkfile.hh"
#include "hkcolumn.hh"
#include "Value.hh"
#include "FITSException.hh"

class Point {

private:

    long row;
    double time;
    Value* value;
    
    bool time_is_null;

public:
    Point(Value* value);
	virtual ~Point() { }

    virtual long   get_row()   { return row; }
    virtual double get_time()  { return time;  }
    virtual Value& get_value() { return *value; }
    
    virtual bool has_null_time() { return time_is_null; }
    
    virtual void read(HKFile* file, HKColumn* column, long row) throw(FITSException);


}; // end of Point class

#endif // POINT_INCLUDED
