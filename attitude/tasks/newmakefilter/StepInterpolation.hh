#ifndef STEP_INTERPOLATION_INCLUDED
#define STEP_INTERPOLATION_INCLUDED

#include "TwoPointInterp.hh"

/*******************************************************************************
* Step interpolation returns the last value in the table with a time before
* the given one. If the time is before the first entry in the table, it returns
* a null value. If the time is after the last entry in the table, it returns the
* last entry in the table.
*******************************************************************************/
class StepInterpolation : public TwoPointInterp {

public: 
    StepInterpolation(HKFile* file, HKColumn* column) :
    TwoPointInterp(file, column) {}

    virtual const Value get_interpolated_value(double time) throw(Exception);

}; // end of StepInterpolation class

#endif // STEP_INTERPOLATION_INCLUDED
