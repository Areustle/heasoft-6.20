#ifndef INTERPOLATION_INCLUDED
#define INTERPOLATION_INCLUDED

#include "hkfile.hh"
#include "Point.hh"
#include "hkcolumn.hh"

/*******************************************************************************
*
*******************************************************************************/
class Interpolation {

protected:

    HKFile* file;
    HKColumn* column;
    
    Value* null;

protected:
    Interpolation(HKFile* file, HKColumn* column);

public:

    virtual ~Interpolation() {};
    
    virtual HKFile*   get_file()   { return file; }
    virtual HKColumn* get_column() { return column; }

    virtual void init() throw(Exception) =0;
    virtual void advance() throw(FITSException) =0;
    virtual const Value get_value(double time) throw(Exception) =0;
    
    virtual bool has_more_points()=0;
    virtual double next_time()=0;

    static Interpolation* create(HKFile* file, HKColumn* column) throw(Exception);
    
protected:
    void read_point(Point*&, long row) throw(FITSException);


}; // end of Interpolation class

#endif // INTERPOLATION_INCLUDED
