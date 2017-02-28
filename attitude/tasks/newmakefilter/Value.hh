#ifndef VALUE_INCLUDED
#define VALUE_INCLUDED

#include <iostream>
#include <map>

#include "hkcolumn.hh"
#include "Data.hh"

extern "C" {
#include "fitsio.h"
}

#include "FITSException.hh"

/*******************************************************************************
* This is a fairly thin wrapper around data storage for the data in a FITS
* column. You should always use one of these objects rather than directly
* create or manipulate a Data object. This is necessary to make operators
* work properly.
*******************************************************************************/
class Value {


public:
    class comparator {
        public: bool operator()(Data* left, Data* right) const {
            return left < right;
        }
    };

private:
    static map<Data*, int, comparator> references;

private:
    Data* data;
    bool owns_data;
    
public:
    Value(HKColumn* column) throw(Exception);
    Value(const Value& value);

private:
    Value(Data* data);


public:
    ~Value();

    /***********************************************
    * copy methods 
    ***********************************************/
    Value* make_new_owner();
    const Value* spawn_clone() const;

    
    /***************************************
    * methods passed on to the data object *
    ***************************************/
    void read(long row, int col, fitsfile* fp) throw(FITSException) {
        data->read(row, col, fp);
    }

    void write(long row, int col, fitsfile* fp) const throw(FITSException) {
        data->write(row, col, fp);
    }
    
    void set_to_null() { data->set_to_null(); }


    /************
    * operators *
    ************/
    bool operator==(const Value& value) const throw(Exception) {
        return data->equals(value.data); 
    }

    bool operator!=(const Value& value) const throw(Exception) {
        return !operator==(value);
    }

    friend ostream& operator<<(ostream& out, const Value& value) {
        value.data->print(out);
        return out;
    }

    /***********
    * addition *
    ***********/
    Value operator+(const Value& value) throw(Exception) {
        return Value(data->plus(value.data));
    }

    Value operator+(double scalar) throw(Exception) {
        return Value(data->plus(scalar));
    }

    friend Value operator+(double scalar, const Value& value) throw(Exception) {
        return Value(value.data->plus(scalar));
    }
    
    /*****************
    * multiplication *
    *****************/
    Value operator*(const Value& value) throw(Exception) {
        return Value(data->times(value.data));
    }

    Value operator*(double scalar) throw(Exception) {
        return Value(data->times(scalar));
    }

    friend Value operator*(double scalar, const Value& value) throw(Exception) {
        return Value(value.data->times(scalar));
    }
    
    /**************
    * subtraction *
    **************/
    Value operator-(const Value& value) throw(Exception) {
        return Value(data->minus(value.data));
    }

    Value operator-(double scalar) throw(Exception) {
        return Value(data->minus(scalar));
    }

    friend Value operator-(double scalar, const Value& value) throw(Exception) {
        return Value(value.data->minus_from(scalar));
    }

    /***********
    * division *
    ***********/
    Value operator/(const Value& value) throw(Exception) {
        return Value(data->over(value.data));
    }

    Value operator/(double scalar) throw(Exception) {
        return Value(data->over(scalar));
    }

    friend Value operator/(double scalar, const Value& value) throw(Exception) {
        return Value(value.data->under(scalar));
    }
    
}; // end of Value class

#endif // VALUE_INCLUDED
