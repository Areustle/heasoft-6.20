#include "Point.hh"

#include <iomanip>
/******************************************************************************
*
******************************************************************************/
Point::Point(Value* value) { 
    this->value = value; 
    time=0.0;
    time_is_null=true;

} // end of constructor

/******************************************************************************
*
******************************************************************************/
void Point::read(HKFile* file, HKColumn* column, long row) throw(FITSException) {

    /*********************
    * set the row number *
    *********************/
    this->row = row;

    /****************
    * read the time *
    ****************/
    fitsfile* fp = file->get_fits();
    int status=0;
    double null=1.0;
    int anynull=0;
    fits_read_col(fp, TDOUBLE, file->get_timecol(), row, 1l, 1l,
                  &null, &time, &anynull, &status);
                  


    if(anynull) {
        /*************************
        * the time value is null *
        *************************/
        time_is_null = true;

    } else {
        /****************************
        * apply the TIMEZERO offset *
        ****************************/
        time_is_null = false;
        time += file->get_tzero();
    }
    
    if(status) {
        throw FITSException("While reading TIME", fp, status, __FILE__, __LINE__ );
    }

  //  cout << "read time"<<setprecision(14)<<time<<endl;

    /*****************
    * read the value *
    *****************/
    value->read(row, column->get_colnum(), fp);


} // end of read method
