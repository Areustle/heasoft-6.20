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
int isInPolygon(double x, double y, double* xpoly, double* ypoly, int npoly);
