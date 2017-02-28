#include "TwoPointInterp.hh"

#include <iomanip>

#include "headas.h"


/********************************************************************************
*
********************************************************************************/
TwoPointInterp::TwoPointInterp(HKFile* file, HKColumn* column) throw(Exception)
               :Interpolation(file, column)  {

    /************************
    * initialize the points *
    ************************/
    point1 = new Point(new Value(column));
    point2 = new Point(new Value(column));
    point3 = new Point(new Value(column));
    
    have_written_first=false;
    have_written_last=false;

} // end of constructor


/****************************************************************************
* initialize the points by reading 2 and 3
* note that an advance discards point1
****************************************************************************/
void TwoPointInterp::init() throw(Exception) {


    /***************************
    * read the first point *
    ***************************/
    read_point(point2, 1l);
    if(point2 == NULL) {
        /*********************
        * no non-null points *
        *********************/
headas_chat(3, "TwoPointInterp.init: %s:%s has null points",
				get_file()->get_filename().c_str(),
				get_column()->get_name().c_str());
        delete point1; point1=NULL;
        delete point3; point3=NULL;
        return;
    }

    /**************************
    * now read a second point *
    **************************/
    read_point(point3, point2->get_row()+1l);
    if(point3 == NULL) {
        /***************************************
        * only one non-null point
        * copy it into the point1 slot by hand
        ***************************************/
headas_chat(3, "TwoPointInterp.init: %s:%s only has one non-null point",
				get_file()->get_filename().c_str(),
				get_column()->get_name().c_str());
        delete point1;
        point1 = point2;
        point2=NULL;
        return;
    }

    /***********************************************************
    * if we got here, then we have two non-null points
    * to read the third we call the advance method, since
    * we now need to take into account redundant point removal.
    * Note how we have to fiddle the have_written_first flag
    * in order to force a read
    ***********************************************************/
    have_written_first=true;
    advance();
    have_written_first=false;

} // end of init method


/****************************************************************************
*
****************************************************************************/
bool TwoPointInterp::has_more_points() {

// cout << "has more points "<<column->get_name()
//      <<" have_written_first="<<have_written_first
//      <<" point1="<<point1<<" point2="<<point2
//      <<endl;

    if(point1==NULL) return false;
    if(point2==NULL) return have_written_first;
    return !have_written_last;

//     if(have_written_first) {
//         return point2 != NULL;
//     } else {
//         return point1 != NULL;
//     }

} // end of has_more_points method

/*****************************************************************************
*
*****************************************************************************/
double TwoPointInterp::next_time() {

    if(have_written_first) return point2->get_time();
    else                   return point1->get_time();
    
} // end of next_time method

/********************************************************************************
*
********************************************************************************/
void TwoPointInterp::advance() throw(FITSException) {

    /*************************************************
    * if we haven't written the first point yet,
    * then don't read anything, since we are still 
    * in the same interval 
    ************************************************/
    if(!have_written_first) { 
        have_written_first=true;
        return;
    }

    if(point3==NULL) {
        have_written_last = true;
        return;
    }


    /*********************
    * swap the first two *
    *********************/
    Point* temp = point1;
    point1 = point2;
    point2 = temp;
    
    /*******************************************************
    * read points until we don't have any superfluous ones *
    *******************************************************/
    do {
        /***************
        * swap 2 and 3 *
        ***************/
        temp = point2;
        point2 = point3;
        point3= temp;


        /****************************
        * read a fresh value into 3 *
        ****************************/
        if(point2 != NULL )  read_point(point3, point2->get_row()+1l);
        else if(point3 != NULL) {
            delete point3;
            point3=NULL;
        }


// cout << column->get_name();
// 
// cout << " point1=";
// if(point1 == NULL) cout << "null";
// else               cout << point1->get_value();
// 
// cout << " point2=";
// if(point2 == NULL) cout << "null";
// else               cout << point2->get_value();
// 
// cout << " point3=";
// if(point3 == NULL) cout << "null";
// else               cout << point3->get_value();
// 
// cout << endl;


    } while(point3 != NULL &&
            (has_redundant_times() || has_redundant_point()) );



// cout << "after advancing "<<column->get_name()
//      << " point1="<<point1<<" point2="<<point2<<" point3="<<point3
//      <<endl;


} // end of advance method

/******************************************************************************
* returns true if adjacent points have identical values
******************************************************************************/
bool TwoPointInterp::has_redundant_point() {

    if(point1==NULL || point2 == NULL ) return false;

    return point1->get_value() == point2->get_value();

} // end of has_redundant_point method

/******************************************************************************
* returns false if point1 and point2 have the same time. This is used
* by the advance method to skip adjacent rows with the same time.
* This method gives a warning to stderr if the values in the two rows
* are different.
******************************************************************************/
bool TwoPointInterp::has_redundant_times() {

    if(point1==NULL || point2==NULL) return false;

    /****************************
    * check for identical times *
    ****************************/
    if(point1->get_time() == point2->get_time() ) {
        /*********************
        * times are the same *
        *********************/
        if(point1->get_value() != point2->get_value() ) {
            /***********************
            * values are different *
            ***********************/
            std::cerr << "Warning rows "<<point1->get_row()
                      << " and "<<point2->get_row()
                      << " in file "<<file->get_filename()
                      << " column "<<column->get_name()
                      << endl;

            std::cerr << "have the same time "<<setprecision(14)<<point1->get_time()
                      << " but differet values: "<<point1->get_value()
                      << " and "<<point2->get_value()
                      << endl;

            std::cerr << "Ignoring the second value"<<endl;

        } // end if values are different

        return true;
    } // end if adjacent rows have the same time
    
    return false;

} // end of has_redundant_times method

/**************************************************************************
* This method handles special cases, like
* when you are on one of the end points or outside the interval.
* If the point is inside a valid interval, then it calls the
* get_interpolated_value method of the subclass to do the actual
* interpolation
**************************************************************************/
const Value TwoPointInterp::get_value(double time) throw(Exception) {

    /******************************************************
    * We have to have at least two points or we don't know 
    * anything. Even if we have one point and we are on that point
    * we shouldn't write a value there, since we don't know
    * anything about the duration over which that value will be
    * valid. Writing a point means its valus is valid until the 
    * next row, not just that it is valid at that instant.
    ******************************************************/
    if(point2 == NULL) return *null;
    
    /***************************************************************
    * this should never happen because if we get here point2!=NULL *
    ***************************************************************/
    if(point1==NULL) {
        throw Exception("poin1==NULL but point2!=NULL", __FILE__, __LINE__);
    }

    /**************************************************
    * if we are on the first point, then just return
    * that value, since it's not null if we got here
    **************************************************/
    if(time == point1->get_time()) return point1->get_value();
    
    /****************************************************************
    * if we are on the second point, then return it,
    * unless we are at the last row of the input table,
    * in which case we return null.
    * This may seem like we are throwing
    * information away, but we aren't really, since this 
    * mimics the effect of running maketime on on the original HK 
    * file. Maketime does not use the last value in the table,
    * and only uses the last time to set the end of a GTI.
    * If we didn't return a null for the last point, then
    * the value would remain valid in the filter file for an
    * unpredictable amount of time depending on the time of the
    * next row.
    ****************************************************************/
    if(time==point2->get_time() ) {
        if(point3 == NULL) return *null;
        else               return point2->get_value();
    }
    
    /*******************
    * check time order *
    *******************/
    if(point1->get_time() >= point2->get_time() ) {
        throw Exception("File "+file->get_filename()+" is out of time order",
                        __FILE__, __LINE__);
    }

    /*******************
    * no extrapolation *
    *******************/
    if( time < point1->get_time() ) return *null;
    if( time > point2->get_time() ) return *null;
    
    /**********************************************************
    * if we get here, then we are inside a valid interval.
    * so we pass on the interpolation to the subclass
    **********************************************************/
    return get_interpolated_value(time);

} // end of get_special_value method
