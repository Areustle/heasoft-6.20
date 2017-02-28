#ifndef NUMBER_DATA_INCLUDED
#define NUMBER_DATA_INCLUDED

#include "Data.hh"

#include <iostream>
#include <cmath>

#include <typeinfo>

/**************************************************************************
* this class represents actual storage for some kind of built-in
* number type (integer or floating point). Valid TYPES are
* unsigned char, short, int, long, float, and double.
* Anything else will throw an exception at run time, since we won't
* know what kind of FITS type it corresponds to.
**************************************************************************/
template<class TYPE> class NumberData : public Data {

private:
    TYPE* value;

public:
    NumberData(long dimen) throw(Exception);

    virtual ~NumberData() { delete[] value; }

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

    virtual void* get_pointer(long index) const { return value+index; }
    virtual void print(ostream& out) const;

private:
    static int get_fits_type() throw(Exception);

}; // end of NumberData template

/**************************************************************************
* translate the data type into its FITS code
* This is used by the constructor
**************************************************************************/
template<class TYPE>
int NumberData<TYPE>::get_fits_type() throw(Exception) {

    /****************************
    * find out what type we are *
    ****************************/
    if(typeid(TYPE) == typeid(long))          return TLONG;
    if(typeid(TYPE) == typeid(int))           return TINT;
    if(typeid(TYPE) == typeid(short))         return TSHORT;
    if(typeid(TYPE) == typeid(unsigned char)) return TBYTE;
    if(typeid(TYPE) == typeid(float))         return TFLOAT;
    if(typeid(TYPE) == typeid(double))        return TDOUBLE;

    /**********************************************
    * if we get here the we don't know what it is *
    **********************************************/
    string message = "Invalid number data type ";
    message += typeid(TYPE).name();

    throw Exception(message, __FILE__, __LINE__);

} // end of get_fits_type static method

/***************************************************************************
* constructor
***************************************************************************/
template<class TYPE>
NumberData<TYPE>::NumberData(long dimen) throw(Exception)
                : Data(dimen, get_fits_type()) {

    value = new TYPE[dimen];

} // end of constructor

/***************************************************************************
*
***************************************************************************/
template<class TYPE>
bool NumberData<TYPE>::equals(const Data* value) const throw(Exception){

        const NumberData<TYPE>* cast = dynamic_cast<const NumberData<TYPE>*>(value);
        if(cast==NULL) return false;

        if(dimen != cast->dimen) return false;

        for(long i=0; i< dimen; ++i) {
            if(this->value[i] != cast->value[i]) return false;
        }

        return true;

} // end of equals method



/***************************************************************************
*
***************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::plus(const Data* data) const throw(Exception) {

    /*************************************
    * create the result and handle nulls *
    *************************************/
    NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
    answer->merge_nulls(this, data);

    /**********************************************************
    * recast the data so we can get at its storage. Note that
    * the merging above guarantees that the data is of the
    * right type
    **********************************************************/
    const NumberData<TYPE>* cast = dynamic_cast<const NumberData<TYPE>*>(data);

    /*********************
    * do the calculation *
    *********************/
    for(long i=0; i< dimen; ++i) {
        answer->value[i] = this->value[i] + cast->value[i];
    }

    return answer;

} // end of plus method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::plus(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< dimen; ++i) {
            answer->value[i] = value[i] + (TYPE)scalar;
        } // end of loop over elements

        return answer;

} // end of plus scalar method


/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::times(const Data* data) const throw(Exception) {

    /*************************************
    * create the result and handle nulls *
    *************************************/
    NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
    answer->merge_nulls(this, data);
    
    /**********************************************************
    * recast the data so we can get at its storage. Note that
    * the merging above guarantees that the data is of the 
    * right type
    **********************************************************/
    const NumberData<TYPE>* cast = dynamic_cast<const NumberData<TYPE>*>(data);

    /*********************
    * do the calculation *
    *********************/
    for(long i=0; i< dimen; ++i) {
        answer->value[i] = this->value[i] * cast->value[i];
    }

    return answer;

} // end of plus method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::times(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }
        answer->copy_nulls(this);


        for(long i=0; i< dimen; ++i) {
            answer->value[i] = (TYPE)((double)value[i] * scalar);
        } // end of loop over elements

        return answer;

} // end of plus scalar method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::minus(const Data* data) const throw(Exception) {

    /*************************************
    * create the result and handle nulls *
    *************************************/
    NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
    answer->merge_nulls(this, data);

    /**********************************************************
    * recast the data so we can get at its storage. Note that
    * the merging above guarantees that the data is of the
    * right type
    **********************************************************/
    const NumberData<TYPE>* cast = dynamic_cast<const NumberData<TYPE>*>(data);

    /*********************
    * do the calculation *
    *********************/
    for(long i=0; i< dimen; ++i) {
        answer->value[i] = this->value[i] - cast->value[i];
    }

    return answer;

} // end of minus method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::minus(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< dimen; ++i) {
            answer->value[i] = value[i] - (TYPE)scalar;
        } // end of loop over elements

        return answer;

} // end of minus scalar method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::minus_from(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< dimen; ++i) {
            answer->value[i] = (TYPE)scalar - value[i];
        } // end of loop over elements

        return answer;

} // end of minus_from scalar method
/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::over(const Data* data) const throw(Exception) {

    /*************************************
    * create the result and handle nulls *
    *************************************/
    NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
    answer->merge_nulls(this, data);

    /**********************************************************
    * recast the data so we can get at its storage. Note that
    * the merging above guarantees that the data is of the
    * right type
    **********************************************************/
    const NumberData<TYPE>* cast = dynamic_cast<const NumberData<TYPE>*>(data);

    /*********************
    * do the calculation *
    *********************/
    for(long i=0; i< dimen; ++i) {
        answer->value[i] = this->value[i] / cast->value[i];
    }

    return answer;

} // end of minus method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::over(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< dimen; ++i) {
            answer->value[i] = value[i] / (TYPE)scalar;
        } // end of loop over elements

        return answer;

} // end of minus scalar method

/*****************************************************************************
*
*****************************************************************************/
template<class TYPE>
Data* NumberData<TYPE>::under(double scalar) const throw(Exception) {

        /*************************************
        * create the result and handle nulls *
        *************************************/
        NumberData<TYPE>* answer = new NumberData<TYPE>(dimen);
        if(isnan(scalar)) {
            answer->set_to_null();
            return answer;
        }

        answer->copy_nulls(this);


        for(long i=0; i< dimen; ++i) {
            answer->value[i] = (TYPE)scalar / value[i];
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
void NumberData<TYPE>::print(ostream& out) const {

    /**************************
    * left bracket for arrays *
    **************************/
    if(dimen>1) out << "[";

    /****************
    * first element *
    ****************/
    if(dimen>=1) {
        if(typeid(TYPE) == typeid(unsigned char)) out << (int)(value[0]);
        else                                      out << value[0];
    }
    
    /***************************
    * the rest of the elements *
    ***************************/
    for(long i=1; i<dimen; ++i) {
        out << ", ";
        if(typeid(TYPE) == typeid(unsigned char)) out << (int)(value[i]);
        else                                      out << value[i];
    }
    
    /*****************************
    * closing bracket for arrays *
    *****************************/
    if(dimen>1) out << "]";
    
} // end of print method

#endif // NUMBER_DATA_INCLUDED
