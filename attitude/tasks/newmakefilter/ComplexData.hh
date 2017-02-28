#ifndef COMPLEX_DATA_INCLUDED
#define COMPLEX_DATA_INCLUDED

#include "Data.hh"

#include <iostream>
#include <cmath>

/*
#ifdef __APPLE_CC__
static long isnan(double value) { return __isnand(value); }
#endif
*/

#include <typeinfo>

/**************************************************************************
* this class represents actual storage for some kind of built-in
* number type (integer or floating point). Valid TYPES are
* unsigned char, short, int, long, float, and double.
* Anything else will throw an exception at run time, since we won't
* know what kind of FITS type it corresponds to.
**************************************************************************/
template<class TYPE> class ComplexData : public Data {

private:
    TYPE* value;

public:
    ComplexData(long dimen) throw(Exception);

    virtual ~ComplexData() { delete[] value; }

    virtual bool equals(const Data* data) const throw(Exception);

    virtual Data*  plus(const Data* data) const throw(Exception);
    virtual Data*  plus(double scalar)    const throw(Exception);
    
    virtual Data*  times(const Data* data) const throw(Exception);
    virtual Data*  times(double scalar)    const throw(Exception);

    virtual Data*  minus(     const Data* data) const throw(Exception);
    virtual Data*  minus(     double scalar)    const throw(Exception);
    virtual Data*  minus_from(double scalar)    const throw(Exception);
    
    virtual Data*  over(const Data* data) const throw(Exception);
    virtual Data*  over( double scalar)   const throw(Exception);
    virtual Data*  under(double scalar)   const throw(Exception);

    virtual void* get_pointer(long index) const { return value+index*2; }
    virtual void print(ostream& out) const;

private:
    static int get_fits_type() throw(Exception);

}; // end of ComplexData template

/**************************************************************************
* translate the data type into its FITS code
* This is used by the constructor
**************************************************************************/
template<class TYPE>
int ComplexData<TYPE>::get_fits_type() throw(Exception) {

    /****************************
    * find out what type we are *
    ****************************/
    if(typeid(TYPE) == typeid(float))         return TCOMPLEX;
    if(typeid(TYPE) == typeid(double))        return TDBLCOMPLEX;

    /**********************************************
    * if we get here the we don't know what it is *
    **********************************************/
    string message = "Invalid complex data type ";
    message += typeid(TYPE).name();

    throw Exception(message, __FILE__, __LINE__);

} // end of get_fits_type static method

/***************************************************************************
* constructor
***************************************************************************/
template<class TYPE>
ComplexData<TYPE>::ComplexData(long dimen) throw(Exception)
                : Data(dimen, get_fits_type()) {

    value = new TYPE[dimen*2];

} // end of constructor

/***************************************************************************
*
***************************************************************************/
template<class TYPE>
bool ComplexData<TYPE>::equals(const Data* value) const throw(Exception){

        const ComplexData<TYPE>* cast = dynamic_cast<const ComplexData<TYPE>*>(value);
        if(cast==NULL) return false;

        if(dimen != cast->dimen) return false;

        for(long i=0; i< 2*dimen; ++i) {
            if(this->value[i] != cast->value[i]) return false;
        }

        return true;

} // end of equals method



/***************************************************************************
*
***************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::plus(const Data* data) const throw(Exception) {

    /*************************************
    * create the result and handle nulls *
    *************************************/
    ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
    answer->merge_nulls(this, data);

    /**********************************************************
    * recast the data so we can get at its storage. Note that
    * the merging above guarantees that the data is of the
    * right type
    **********************************************************/
    const ComplexData<TYPE>* cast = dynamic_cast<const ComplexData<TYPE>*>(data);

    /*********************
    * do the calculation *
    *********************/
    for(long i=0; i< 2*dimen; ++i) {
        answer->value[i] = this->value[i] + cast->value[i];
    }

    return answer;

} // end of plus method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::plus(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< 2*dimen; i+=2) {
            answer->value[i] = value[i] + (TYPE)scalar;
        } // end of loop over elements

        return answer;

} // end of plus scalar method


/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::times(const Data* data) const throw(Exception) {

    /*************************************
    * create the result and handle nulls *
    *************************************/
    ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
    answer->merge_nulls(this, data);
    
    /**********************************************************
    * recast the data so we can get at its storage. Note that
    * the merging above guarantees that the data is of the 
    * right type
    **********************************************************/
    const ComplexData<TYPE>* cast = dynamic_cast<const ComplexData<TYPE>*>(data);

    /*********************
    * do the calculation *
    *********************/
    for(long i=0; i< 2*dimen; i+=2) {

        answer->value[i] = this->value[i]   * cast->value[i]  -
                           this->value[i+1] * cast->value[i+1];

        answer->value[i+1] = this->value[i]   * cast->value[i+1] +
                             this->value[i+1] * cast->value[i];
    }

    return answer;

} // end of plus method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::times(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }
        answer->copy_nulls(this);


        for(long i=0; i< 2*dimen; ++i) {
            answer->value[i] = (TYPE)((double)value[i] * scalar);
        } // end of loop over elements

        return answer;

} // end of plus scalar method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::minus(const Data* data) const throw(Exception) {

    /*************************************
    * create the result and handle nulls *
    *************************************/
    ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
    answer->merge_nulls(this, data);

    /**********************************************************
    * recast the data so we can get at its storage. Note that
    * the merging above guarantees that the data is of the
    * right type
    **********************************************************/
    const ComplexData<TYPE>* cast = dynamic_cast<const ComplexData<TYPE>*>(data);

    /*********************
    * do the calculation *
    *********************/
    for(long i=0; i< 2*dimen; ++i) {
        answer->value[i] = this->value[i] - cast->value[i];
    }

    return answer;

} // end of minus method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::minus(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< 2*dimen; i+=2) {
            answer->value[i] = value[i] - (TYPE)scalar;
        } // end of loop over elements

        return answer;

} // end of minus scalar method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::minus_from(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< 2*dimen; i+=2) {
            answer->value[i] = (TYPE)scalar - value[i];
        } // end of loop over elements

        return answer;

} // end of minus_from scalar method
/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::over(const Data* data) const throw(Exception) {

    /*************************************
    * create the result and handle nulls *
    *************************************/
    ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
    answer->merge_nulls(this, data);

    /**********************************************************
    * recast the data so we can get at its storage. Note that
    * the merging above guarantees that the data is of the
    * right type
    **********************************************************/
    const ComplexData<TYPE>* cast = dynamic_cast<const ComplexData<TYPE>*>(data);

    /*********************
    * do the calculation *
    *********************/
    for(long i=0; i< 2*dimen; i+=2) {
    
        double denom = cast->value[i]   * cast->value[i] +
                       cast->value[i+1] * cast->value[i+1];

        answer->value[i] = (this->value[i]   * cast->value[i]  +
                            this->value[i+1] * cast->value[i+1])/denom;

        answer->value[i+1] = (this->value[i]   * cast->value[i+1] +
                              this->value[i+1] * cast->value[i])/denom;
    }

    return answer;

} // end of minus method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::over(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< 2*dimen; ++i) {
            answer->value[i] = value[i] / (TYPE)scalar;
        } // end of loop over elements

        return answer;

} // end of minus scalar method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* ComplexData<TYPE>::under(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        ComplexData<TYPE>* answer = new ComplexData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< 2*dimen; i+=2) {
        
            double denom = value[i]*value[i] + value[i+1]*value[i+1];
        
            answer->value[i]   = ((TYPE)scalar * value[i]  )/denom;
            answer->value[i+1] = ((TYPE)scalar * value[i+1])/denom;
        

        } // end of loop over elements

        return answer;

} // end of under scalar method

/*****************************************************************************
* print the contents of the data. It's a somewhat ugly implementation
* but this is just for debugging, so who cares?
* Notice the special treatement for chars so that they get interpreted as
* numbers and not letters.
*****************************************************************************/
template<class TYPE>
void ComplexData<TYPE>::print(ostream& out) const {

    /**************************
    * left bracket for arrays *
    **************************/
    if(dimen>1) cout << "[";

    /****************
    * first element *
    ****************/
    if(dimen>=1) {
        cout << value[0] <<"+"<<value[1]<<"i";
    }
    
    /***************************
    * the rest of the elements *
    ***************************/
    for(long i=1; i<2*dimen; i+=2) {
        cout << ", ";
        cout << value[i] <<"+"<<value[i+1]<<"i";

    }
    
    /*****************************
    * closing bracket for arrays *
    *****************************/
    if(dimen>1) cout << "]";
    
} // end of print method

#endif // COMPLEX_DATA_INCLUDED

