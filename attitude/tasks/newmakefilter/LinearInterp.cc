#include "LinearInterp.hh"

/********************************************************************************
*
********************************************************************************/
const Value LinearInterp::get_interpolated_value(double time) throw(Exception) {

    /*********************************
    * find our place in the interval *
    *********************************/
    double hat = (              time - point1->get_time())/
                 (point2->get_time() - point1->get_time());


    /*******************************************************************
    * interpolate
    * Note we do the calculation this way to minimize the number of
    * operations involving scalars, since scalar operaitons can
    * result in loss of accuracy. This is because operations return
    * the same data type as the operands, so the result of multiplying
    * a non-integer by an IntegerValue is clipped back to an integer.
    *******************************************************************/
    Value result = point1->get_value() +
                   hat*(point2->get_value() - point1->get_value());


//     cout << "value1="<<point1->get_value()
//         << " value2="<<point2->get_value()
//         << " hat="<<hat
//         << " result="<<result
//         <<endl;

    return result;


} // end of get_value method


