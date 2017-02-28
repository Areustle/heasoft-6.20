#include "StringData.hh"

#include <cstring>

/********************************************************************************
* returns true if two strings are equal up to the number of characters in the
* smallest string. Maybe it would be best to check the full lenght of both strings,
* but we don't expect to really be comparing different sized strings, so this
* is good enough.
********************************************************************************/
bool StringData::equals(const Data* data) const {

    const StringData* cast = dynamic_cast<const StringData*>(data);
    if(cast == NULL) return false;
    
    int min_size = size;
    if(min_size > cast->size) min_size = cast->size;

    if(strncmp(value, cast->value, min_size) ) return false;
    else                                      return true;
    
} // end of equals method

/********************************************************************************
*
********************************************************************************/
void StringData::print(ostream& out) const {

    /******************
    * just to be safe *
    ******************/
    value[size-1] = '\0';

    out << value;

} // end of print method


/****************************************************************************
* we override the read method to make sure we are not reading more
* string than we can handle. OK, so this is a bit paranoid, since this
* class should never be used to read a column other than the one it was
* created for, and checking this for every row is going to impact performance.
* But computers are real fast these days, and you can never be too paranoid.
****************************************************************************/
void StringData::read(long row, int column, fitsfile* fp) throw(FITSException) {

    /****************
    * get the width *
    ****************/
    int status=0;
    int nbytes;
    fits_get_col_display_width(fp, column, &nbytes, &status);

    /*******************
    * check for errors *
    *******************/
    if(status) {
        char number[32];
        sprintf(number, "%d", column);
        string message = "Can't get string width for column ";
        message += number;

        throw FITSException(message, fp, status, __FILE__, __LINE__);
    }
    
    /*****************
    * check the size *
    *****************/
    if( nbytes >= size) {
        char number[32];
        string message = "Trying to read ";
        
        sprintf(number, "%d", nbytes);
        message += number;
        message += " characters from column ";
        
        sprintf(number, "%d", column);
        message += number;
        
        message += " into a ";
        sprintf(number, "%d", size-1);
        message += number;
        message += " byte string";

        throw FITSException(message, fp, status, __FILE__, __LINE__);
    }


    Data::read(row, column, fp);
    
} // end of read method
