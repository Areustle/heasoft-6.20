#include "StepInterpolation.hh"

/******************************************************************************
* return an interpolated value note the superclass guarantees that this
* method will only be called withing a valid interval.
******************************************************************************/
const Value StepInterpolation::get_interpolated_value(double time) throw(Exception) {

    /********************************************************
    * the first value remains valid throughout the interval *
    ********************************************************/
    return point1->get_value();

} // end of get_value method



