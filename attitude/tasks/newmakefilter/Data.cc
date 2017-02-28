#include "Data.hh"

#include <typeinfo>

#include "BadOpException.hh"

#include "NumberData.hh"
#include "ComplexData.hh"
#include "StringData.hh"
#include "BitData.hh"
#include "BooleanData.hh"


/****************************************************************************
*
****************************************************************************/
Data* Data::create(HKColumn* column) throw(Exception) {

    /*************************
    * parse the TFORM string *
    *************************/
    int type;
    long repeat;
    long width;
    int status=0;
    
    string tform = column->get_tform();

    fits_binary_tform(const_cast<char*>(tform.c_str()),
                      &type, &repeat, &width, &status);
    if(status) {
        throw FITSException("While Parsing "+tform, NULL, status,
                            __FILE__, __LINE__);
    }

    /******************************
    * find the right type of data *
    ******************************/
    if((type == TLONG || type == TINT || type == TSHORT || type == TBYTE) &&
       column->get_tscale() == 1.0 && column->get_tzero() == 0.0 ) {
        /**********
        * integer *
        **********/
        unsigned int size = width/repeat;
        if(size <= sizeof(unsigned char)) return new NumberData<unsigned char>(repeat);
        if(size <= sizeof(short)) return new NumberData<short>(repeat);
        if(size <= sizeof(int))   return new NumberData<int>(repeat);
        if(size <= sizeof(long))  return new NumberData<long>(repeat);

        /*********************************************************
        * unknown - this should never happen on standard systems *
        *********************************************************/
        char number[32];
        sprintf(number, "%d", size);
        string message = "We don't have an integer data type big enough to hold ";
        message += number;
        message += " bytes";
        throw Exception(message, __FILE__, __LINE__);

    } else if(type == TFLOAT || type == TDOUBLE ||
              column->get_tscale() != 1.0 || column->get_tzero() != 0.0) {
        /*****************
        * floating point *
        *****************/
        unsigned int size = width/repeat;

        /***
        For integer types, the size must be significantly smaller than
        a float to avoid losing information.  For example, a 32 bit integer
        cannot be represented in a (*) 32 bit float which has 23 bits of precision.
        Note that we cannot hold a 64 bit integer exactly in a 64 bit float either.
        (*) For many platforms.  A more careful test could make use of ({FLT,DBL}_DIG).
        ***/
        if(type == TFLOAT || size < sizeof(float))  return new NumberData<float>(repeat);
        if(size <= sizeof(double)) return new NumberData<double>(repeat);

        /*********************************************************
        * unknown - this should never happen on standard systems *
        *********************************************************/
        char number[32];
        sprintf(number, "%d", size);
        string message = "We don't have a floating point data type big enough to hold ";
        message += number;
        message += " bytes";
        throw Exception(message, __FILE__, __LINE__);

    } else if(type == TCOMPLEX || type == TDBLCOMPLEX) {
        /**********
        * complex *
        **********/
        unsigned int size = width/repeat/2;

        if(size <= sizeof(float))  return new ComplexData<float>(repeat);
        if(size <= sizeof(double)) return new ComplexData<double>(repeat);

        /*********************************************************
        * unknown - this should never happen on standard systems *
        *********************************************************/
        char number[32];
        sprintf(number, "%d", size);
        string message = "We don't have a floating point data type big enough to hold ";
        message += number;
        message += " bytes";
        throw Exception(message, __FILE__, __LINE__);
        
    } else if(type == TSTRING) return new StringData(width);
      else if(type == TBIT   ) return new BitData(repeat);
      else if(type == TLOGICAL) return new BooleanData(repeat);
      else {
        /********************
        * unknown data type *
        ********************/
        string message = "Unsupported data type ";
        message += tform;
        throw Exception(message, __FILE__, __LINE__);
    }

    return 0;
} // end of create method


/****************************************************************************
*
****************************************************************************/
void Data::read(long row, int column, fitsfile* fp) throw(FITSException) {

    int status=0;

    /****************
    * read the data *
    ****************/
    fits_read_colnull(fp, type, column, row, 1l, dimen, get_pointer(0),
                      nullflags, &anynull, &status);
                   
    /*******************
    * check for errors *
    *******************/
    if(status) {
        char number[32];
        string message="While reading value from ";

        sprintf(number, "%ld", row);
        message += number;

        message += " column ";
        sprintf(number, "%d", column);
        message += column;

        throw FITSException(message, fp, status, __FILE__, __LINE__ );
    }


} // end of generic_read method

/********************************************************************************
*
********************************************************************************/
void Data::write(long row, int column, fitsfile* fp) const throw(FITSException) {

    int status=0;

    if(anynull) {
        for(long i=0; i< dimen; ++i) {

            long element = i+1l;

            if(nullflags[i]) {
                fits_write_col_null(fp, column, row, element, 1l, &status);
            } else {
                fits_write_col(fp, type, column, row, element, 1l, get_pointer(i), &status);
            }
        } // end of loop over vector elements
    } else {
        /*************************************
        * we don't have to worry about nulls *
        *************************************/
        fits_write_col(fp, type, column, row, 1l, dimen, get_pointer(0), &status);
    }

    /*******************
    * check for errors *
    *******************/
    if(status) {
        char number[32];
        string message="While writing value to ";

        sprintf(number, "%ld", row);
        message += number;

        message += " column ";
        sprintf(number, "%d", column);
        message += column;

        throw FITSException(message, fp, status, __FILE__, __LINE__ );
    }


} // end of read method

/*****************************************************************************
* Throws an exception if the given Data cannot be combined with this one
* in an operator.
*****************************************************************************/
void Data::check_consistency(const Data* data) const throw(Exception) {

    if(typeid(*this) != typeid(*data) ) {
        /*******************************
        * the classes are not the same *
        *******************************/
        string message = "Trying to add ";
        message += typeid(*this).name();
        message += " and ";
        message += typeid(*data).name();

        throw Exception(message);
    }


    if(dimen != data->dimen) {
        /**********************************
        * the dimensions are not the same *
        **********************************/
        char number[32];
        string message ="Trying to add values with inconsistent dimensions: ";
        
        sprintf(number, "%ld", dimen);
        message += number;
        message += " and ";
        
        sprintf(number, "%ld", data->dimen);
        message += number;

        throw Exception(message);
    }


} // end of check_consistency method

/***************************************************************************
*
***************************************************************************/
void Data::merge_nulls(const Data* left, const Data* right) throw(Exception) {

    /*****************************
    * make sure dimensions match *
    *****************************/
    check_consistency(left);
    check_consistency(right);

    /**********************************
    * handle the null flag accounting *
    **********************************/
    anynull=0;
    for(long i=0; i< dimen; ++i) {
        if(left->nullflags[i] || right->nullflags[i] ) {
            /***************
            * null element *
            ***************/
            nullflags[i] = 1;
            anynull=1;
        } else {
            /****************
            * regular value *
            ****************/
            nullflags[i]=0;
        }
    }

} // end of merge_nulls method


/***************************************************************************
*
***************************************************************************/
void Data::copy_nulls(const Data* data) throw(Exception) {

    check_consistency(data);

    anynull = data->anynull;
    
    for(long i=0; i< dimen; ++i) {
        nullflags[i] = data->nullflags[i];
    }

} // end of copy_nulls method


/*****************************************************************************
*
*****************************************************************************/
Data* Data::plus(const Data* data) const throw(Exception) {

   throw BadOpException("Addition", this);

} // end of plus method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::plus(double scalar) const throw(Exception) {

    throw BadOpException("Scalar addition", this);

} // end of plus method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::times(const Data* data) const throw(Exception) {

   throw BadOpException("Multiplication", this);


} // end of times method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::times(double scalar) const throw(Exception) {

  throw BadOpException("Scalar multiplication", this);

} // end of times method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::minus(const Data* data) const throw(Exception) {

   throw BadOpException("Subtraction", this);


} // end of minus method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::minus(double scalar) const throw(Exception) {

  throw BadOpException("Scalar subtraction", this);

} // end of minus scalar method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::minus_from(double scalar) const throw(Exception) {

  throw BadOpException("Subtraction from a scalar", this);

} // end of minus_from scalar method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::over(const Data* data) const throw(Exception) {

   throw BadOpException("Division", this);


} // end of over method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::over(double scalar) const throw(Exception) {

  throw BadOpException("Division by a scalar", this);

} // end of over scalar method

/*****************************************************************************
*
*****************************************************************************/
Data* Data::under(double scalar) const throw(Exception) {

  throw BadOpException("Division from a scalar", this);

} // end of under scalar method
