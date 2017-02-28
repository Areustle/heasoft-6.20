#ifndef LINEAR_INTERP_INCLUDED
#define LINEAR_INTERP_INCLUDED

#include "TwoPointInterp.hh"

/*******************************************************************************
* linear interpolation
*******************************************************************************/
class LinearInterp : public TwoPointInterp {

public:

    LinearInterp(HKFile* file, HKColumn* column) : TwoPointInterp(file, column) {}
    
    virtual const Value get_interpolated_value(double time) throw(Exception);


}; // end of LinearInterp class

#endif // LINEAR_INTERP_INCLUDED
