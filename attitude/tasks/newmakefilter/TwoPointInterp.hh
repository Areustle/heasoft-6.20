#ifndef TWO_POINT_INTERP_INCLUDED
#define TWO_POINT_INTERP_INCLUDED

#include "Interpolation.hh"

/********************************************************************************
* This is a generic base classes for interpolation methods which consider
* only two points at a time, Such as StepInterpolation and LinearInterp.
* It maintains three points: point1, point2, and point3. The current interval
* is between point1 and point2. Point3 is a look-ahead buffer which is necessary
* because the advance method skips rows which have identical values.
* The points can be null pointers. Point3 will be null if and only if you have
* reached the
* last interval in the file. Point1 and point2 will only be null if the input
* table has one or no rows. Subclasses should never include point3 in their get_value
* calculation, except to check for the last interval in the file.
********************************************************************************/
class TwoPointInterp : public Interpolation {

protected:
    Point* point1;
    Point* point2;
    Point* point3;

    bool have_written_first;
    bool have_written_last;

protected:

    TwoPointInterp(HKFile* file, HKColumn* column) throw(Exception);
    
public:

    virtual ~TwoPointInterp() {};

    virtual void init() throw(Exception);
    virtual void advance() throw(FITSException);
    virtual bool has_more_points();
    virtual double next_time();

    virtual const Value get_value(double time) throw(Exception);

protected:
    bool has_redundant_times();
    virtual bool has_redundant_point();
    virtual const Value get_interpolated_value(double time) throw(Exception) =0;

}; // end of TwoPointInterp class

#endif // TWO_POINT_INTERP_INCLUDED
