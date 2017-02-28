#include "Value.hh"

#include <iostream>
#include <string>

/****************************************************************************
* initialize the NULL value constant
****************************************************************************/
map<Data*, int, Value::comparator> Value::references;


/********************************************************************************
* public constructor from a tform
********************************************************************************/
Value::Value(HKColumn* column ) throw(Exception) {

    data = Data::create(column);
    ++references[data];
}

/****************************************************************************
* private constructor
****************************************************************************/
Value::Value(Data* data) {

    this->data = data;
    ++references[data];

} // end of constructor

/****************************************************************************
* copy constructor
****************************************************************************/
Value::Value(const Value& value) {

    data = value.data;
    ++references[data];
}

/****************************************************************************
*
****************************************************************************/
Value::~Value() {

    if(references[data] == 1) delete data;
    else                      --references[data];

} // end of destructor

