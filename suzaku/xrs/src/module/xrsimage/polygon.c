/****************************************************************************
* Function to determine if a given point is inside a polygon
* This function can be replaced later by an equivalent from a
* region fil ehandling library.
* This particular function has been translated to FORTRAN and used 
* in the extractor.
* Note npoly is the number of corners of the polygon, which is one less than
* the number of values stored in xpoly or ypoly. This is because the 
* first corner is repeated as the last corner to close the polygon.
* Note the corners must be listed in order either clockwise or 
* counter-clockwise
******************************************************************************/
int isInPolygon(double x, double y, double* xpoly, double* ypoly, int npoly)
{
int corner;
int next,last;
double hat;
double xintersect;
int ncrossings;

/*********************
* loop over segments *
*********************/
ncrossings=0;
for(corner=0;corner<npoly;++corner) {

    if((ypoly[corner]<y && ypoly[corner+1]>y) ||
       (ypoly[corner]>y && ypoly[corner+1]<y)   ) {
        /*****************************************************
        * path intersects horiz line, but is it on the path? *
        *****************************************************/
        hat=(y-ypoly[corner])/(ypoly[corner+1]-ypoly[corner]);
        xintersect=xpoly[corner]+hat*(xpoly[corner+1]-xpoly[corner]);
        if(xintersect>=x) ++ncrossings;          

    } else if(ypoly[corner]==y && xpoly[corner]>x) {
        /*********************
        * path hits a corner *
        *********************/

        /*********************************************
        * find the last corner not on the horiz line *
        *********************************************/
        last=corner;
        while(ypoly[last]==y) {
            --last;
            if(last<0) last=npoly-1;
        }

        /*********************************************
        * find the next corner not on the horiz line *
        *********************************************/
        next=corner;
        while(ypoly[next]==y) {
            ++next;
            if(next>npoly) next=1;
        }

        /***********************************
        * line goes right through a corner *
        ***********************************/
        if((ypoly[last]>y && ypoly[next]<y) ||
           (ypoly[last]<y && ypoly[next]>y)   ) {
            /******************************************************
            * it's a real intersection, not just skiming the edge *
            ******************************************************/
            ++ncrossings;
        }

        /**************************************
        * skip over all the adjacent corners 
        * which are on the horizontal ray 
        **************************************/
        if(next>corner) corner=next-1;
        else corner=npoly;

    } /* end if path hits a corner */
             
}/* end of loop over segments */

return(ncrossings%2);
}
