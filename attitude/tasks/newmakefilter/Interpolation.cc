#include "Interpolation.hh"

#include "StepInterpolation.hh"
#include "LinearInterp.hh"

#include "headas.h"

/*******************************************************************************
*
*******************************************************************************/
Interpolation::Interpolation(HKFile* file, HKColumn* column) {

    this->file = file;
    this->column = column;
    
    null = new Value(column);
    null->set_to_null();


} // end of constructor


/*******************************************************************************
*
*******************************************************************************/
Interpolation* Interpolation::create(HKFile* file, HKColumn* column) throw(Exception) {


    string method = column->get_interpolation();

    Interpolation* interp=NULL;

    if(method == "D") {
        /*******
        * step *
        *******/
        interp = new StepInterpolation(file, column);

    } else if(method == "I") {
        /*********
        * linear *
        *********/
        interp = new LinearInterp(file, column);
    }
    
    /*********************************
    * check for errors *
    *******************/
    if(interp == NULL) {
        throw Exception("Unknown interpolation method "+method,
                        __FILE__, __LINE__);
    }

    /*******************************
    * initialize the interpolation *
    *******************************/
    interp->init();

    return interp;



} // end of create method

/****************************************************************************
* reads the next point from the given row. If the time value in that
* row is null, it will continue reading consecutive rows until it finds
* one with non-null time. If it reaches the end of the table, it will
* delete the point object and set the pointr to NULL.
* Subclasses should use this method rwather than reading Points directly
* in order to preserve the uniform behavior of skipping rows with null time.
****************************************************************************/
void Interpolation::read_point(Point*& point, long row) throw(FITSException) {

    if(point==NULL) return;

    long nrows = file->get_nrows();

    /*************************************
    * loop until we get a non-null value *
    *************************************/
    do {
        /****************
        * check for EOF *
        ****************/
        if(row > nrows) {
            delete point;
            point = NULL;
            return;
        }
        
        /********************
        * read the next row *
        ********************/
        point->read(file, column, row++);

        
        if(point->has_null_time()) {
            /***************************
            * warn about skipped nulls *
            ***************************/
            headas_chat(2, "Ignoring null TIME in file %s row %ld for column %s\n",
                        file->get_filename().c_str(), row-1l, column->get_name().c_str() );
        }

    } while(point->has_null_time());

} // end of read_point method

