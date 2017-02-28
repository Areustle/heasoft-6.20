/// \file ahmath.h
/// \brief general mathematical routines
/// \author Mike Witthoeft
/// \date $Date: 2016/07/25 21:09:17 $
 
#ifndef AHMATH_AHMATH_H
#define AHMATH_AHMATH_H

#include "ahgen/ahversion.h"
AHVERSION(AHMATH_AHMATH,"$Id: ahmath.h,v 1.41 2016/07/25 21:09:17 rshill Exp $")

#include <string>
#include <vector>
#include <cmath>

/// \brief mathematical routines
/// \ingroup mod_ahmath
namespace ahmath {

/** \addtogroup mod_ahmath
 *  @{
 */

/// \brief index type for data arrays
typedef unsigned long IndexType;


/// \brief aliases interpolation types
enum interptypes {
  NEAREST,             ///< return nearest point
  TWOPOINT,            ///< linear interpolation
  NPOINT               ///< n-point Aitken method
};

/// \brief structure to hold vertices for a given polygon
struct Polygon {

  Polygon(int m_num_points = 0) : m_num_points(m_num_points), m_x(m_num_points), m_y(m_num_points),
    m_minx(0), m_maxx(0), m_miny(0), m_maxy(0), m_checked(false) {};

  int m_num_points;        ///< total number of points in the polygon

  std::vector<double> m_x; ///< vector array of x values for each vertex
  std::vector<double> m_y; ///< vector array of y values for each vertex

  double m_minx;           ///< smallest x value in the polygon
  double m_maxx;           ///< largest x value in the polygon
  double m_miny;           ///< smallest y value in the polygon
  double m_maxy;           ///< largest y value in the polygon

  bool m_checked;          ///< polygon has been validated

};


/// \brief check all the line segments in the polygon and mark it checked
/// \param[in] poly polygon structure
void checkPolygon(Polygon & poly);

/// \brief add a new point to the polygon. checks for a valid point before adding
/// \param[in] x input x point
/// \param[in] y input y point
/// \param[in] poly polygon object with all coordinates
void addPointToPolygon(const double x, const double y, Polygon & poly);

/// \brief Given two line segments, check for intersecting points
/// \param[in] x1a input x point a, line 1
/// \param[in] y1a input y point a, line 1
/// \param[in] x1b input x point b, line 1
/// \param[in] y1b input y point b, line 1
/// \param[in] x2a input x point a, line 2
/// \param[in] y2a input y point a, line 2
/// \param[in] x2b input x point b, line 2
/// \param[in] y2b input y point b, line 2
/// return true if line segments intersect
bool segmentsIntersect(const double x1a, const double y1a, 
                       const double x1b, const double y1b, 
                       const double x2a, const double y2a, 
                       const double x2b, const double y2b);

/// \brief check if given point is inside polygonal space
/// \param[in] x x coordinate
/// \param[in] y y coordinate
/// \param[in] poly polygon object with all coordinates
/// \return true if inside polygon, false if outside
bool isPointInsidePolygon(const double x, 
                          const double y,
                          const Polygon poly);

/// \brief convert interpolation type string into enumeration type
/// \param[in] interp interpolation type string
/// \return interpolation enumeration
int interpolation_type(const std::string & interp);


/// \brief Perform nearest-point interpolation, Y(X), using two points: Y1(X1) 
///  and Y2(X2).  Will select the Y-value whose X-value is closest to the given
///  X-value.
/// \param[in] x value to interpolate at
/// \param[in] x1 X-value of first point
/// \param[in] y1 Y-value of first point
/// \param[in] x2 X-value of second point
/// \param[in] y2 Y-value of second point
/// \return interpolated value
double interpolate_point_nearest(double x, double x1, double y1, double x2,
                                 double y2);


/// \brief Perform linear interpolation, Y(X), using two points: Y1(X1) and 
///  Y2(X2).
/// \param[in] x value to interpolate at
/// \param[in] x1 X-value of first point
/// \param[in] y1 Y-value of first point
/// \param[in] x2 X-value of second point
/// \param[in] y2 Y-value of second point
/// \return interpolated value
double interpolate_point_twopoint(double x, double x1, double y1, double x2,
                                  double y2);


/// \brief Perform aitken method of interpolation, Y(X), using n points: Y1(X1), Y2(X2), ..., Y_n(X_n) 
/// \param[in] x value to interpolate at
/// \param[in] xarr Arrays of X-values 
/// \param[in] yarr Arrays of Y-values
/// \param[in] idx index of array identifying points used for interpolation
/// \param[in] npoints Number of points in interpolation
/// \return interpolated value
double interpolate_point_npoint(double x, double * xarr, double * yarr, int npoints, IndexType idx0=0);

/// \brief Search an array for the given value and return the index of the
///  array whose value is immediately before the given value.  If the given
///  value is outside the bounds of the array, return the index of the
///  appropriate end-point and set the extrapolation flag to true.
/// \param[in] x value to search for in array
/// \param[in] xarr array of values to search
/// \param[in] narr length of xarr array
/// \param[out] extrap true if x is not in range of xarr
/// \param[in] (optional) start searching at this index; will reset to zero
///  if given x value less than xarr[idx0]
/// \return index near solution to be used in interpolation function
IndexType search(double x, double* xarr, IndexType narr, bool & extrap, 
                 IndexType idx0=0);

/// \brief Search an array for the given value and return the index of the
///  array whose value is the nearest n-points surrounding the given value.  
///  If the given value is outside the bounds of the array, return the index of the
///  appropriate end-point and set the extrapolation flag to true. If the given value
///  is near the beginning of the  bounds of the array, return the first element of the
///  array if it's within the npoints given. If the given value is near the end of the 
///  bounds of the array, return the element n-points before the end of the array.
///  The npoint interpolation function has a minimum requirement of three points.
/// \param[in] x value to search for in array
/// \param[in] xarr array of values to search
/// \param[in] npoints Number of points in interpolation
/// \param[in] narr length of xarr array
/// \param[out] extrap true if x is not in range of xarr
/// \param[in] (optional) start searching at this index; will reset to zero
///  if given x value less than xarr[idx0]
/// \return index near solution to be used in interpolation function
IndexType search_npoint(double x, double* xarr, unsigned long npoints, IndexType narr, 
                        bool & extrap, IndexType idx0=0);

/// \brief Search a sawtooth function, Y(X), for the given value of X and 
///  return the index of the point occuring immediately before the given value.
///  The sawtooth function is defined as a monotonic function with a positive
///  slope, Y(X), with the exception that the Y-value resets to zero upon 
///  reaching a maximum value ymax.  
/// \param[in] x X-value to search for
/// \param[in] xarr array of X-values to search
/// \param[in] yarr array of Y-values to search
/// \param[in] narr length of xarr and yarr arrays
/// \param[out] extrap true if X-value associated with given Y-value is not in
///  range of xarr
/// \param[in] itype interpolation type (enumerated value)
/// \param[in] (optional) start searching at this index; will reset to zero
///  if given x value less than xarr[idx0]
/// \return index near solution to be used in interpolation function
IndexType search_sawX2Y(double x, double* xarr, double* yarr, 
                        IndexType narr, bool & extrap, int itype, 
                        IndexType idx0=0);

/// \brief Search a sawtooth function, Y(X), for the given value of Y and 
///  return the index of the point occuring immediately before the given value.
///  The sawtooth function is defined as a monotonic function with a positive
///  slope, Y(X), with the exception that the Y-value resets to zero upon 
///  reaching a maximum value ymax.  Since a given y value can occur multiple 
///  times, a rough X-value must be supplied to locate the 'tooth' in which to
///  search.  An error will be thrown if the given Y-value is outside the range
///  [0:ymax].  The routine attempts to return the largest array index whose
///  Y-value is below the given Y-value; however, this index will be adjusted 
///  to guarantee that both points from idx and (idx+1) are in range of the
///  input arrays and in the same sawtooth segment (or tooth).
/// \param[in] xrough estimate of X-value to locate sawtooth segment in which
///  to search
/// \param[in] y Y-value to search for
/// \param[in] ymax largest allowed Y-value
/// \param[in] xarr array of X-values to search
/// \param[in] yarr array of Y-values to search
/// \param[in] narr length of xarr and yarr arrays
/// \param[in] revfrac search in opposite direction if y is further than
///  revfrac*ymax from the y-value near xrough
/// \param[out] extrap true if X-value associated with given Y-value is not in
///  range of xarr
/// \param[in] (optional) start searching at this index; will reset to zero
///  if given x value less than xarr[idx0]
/// \return index near solution to be used in interpolation function
IndexType search_sawY2X(double xrough, double y, double ymax, double* xarr, 
                        double* yarr, IndexType narr, double revfrac,
                        bool & extrap, IndexType idx0=0);


/// \brief Interpolate function, Y(X), as X->Y
/// \param[in] x X-value to interpolate function at
/// \param[in] xarr array of X-values
/// \param[in] yarr array of Y-values
/// \param[in] narr length of xarr and yarr arrays
/// \param[in] itype interpolation type (enumerated value)
/// \param[in] idx index of array identifying points used for interpolation
/// \return interpolated value
double interpolate(double x, double* xarr, double* yarr, IndexType narr, 
                   int itype, IndexType idx);

/// \brief Interpolate sawtooth function, Y(X), as X->Y.  
/// \param[in] x X-value to interpolate function at
/// \param[in] ymax largest legal Y-value
/// \param[in] xarr array of X-values
/// \param[in] yarr array of Y-values
/// \param[in] narr length of xarr and yarr arrays
/// \param[in] itype interpolation type (enumerated value)
/// \param[in] idx index of array identifying points used for interpolation
/// \return interpolated value
double interpolate_sawX2Y(double x, double ymax, double* xarr, double* yarr,
                          IndexType narr, int itype, IndexType idx);

/// \brief Overloaded interpolate function, including n-point interpolation, Y(X), as X->Y
/// \param[in] x X-value to interpolate function at
/// \param[in] xarr array of X-values
/// \param[in] yarr array of Y-values
/// \param[in] narr length of xarr and yarr arrays
/// \param[in] itype interpolation type (enumerated value)
/// \param[in] idx index of array identifying points used for interpolation
/// \param[in] npoints Number of points in interpolation
/// \return interpolated value
double interpolate(double x, double* xarr, double* yarr, IndexType narr, 
                   int itype, IndexType idx, int npoints);

/// \brief Interpolate sawtooth function, Y(X), as Y->X.  
/// \param[in] y Y-value to search for
/// \param[in] xarr array of X-values
/// \param[in] yarr array of Y-values
/// \param[in] narr length of xarr and yarr arrays
/// \param[in] itype interpolation type (enumerated value)
/// \param[in] idx index of array identifying points used for interpolation
/// \return interpolated value
double interpolate_sawY2X(double y, double* xarr, double* yarr, IndexType narr,
                          int itype, IndexType idx);


/// \brief Calculate first derivative of tabulated function using 3-point method
/// \param[in] mesh array of mesh points
/// \param[in] func array of function values
/// \param[in] nmesh length of mesh, func, and out arrays
/// \param[out] out output array of derivative values
///
/// The output array, out, must be properly sized before this function is
/// called.  It is assumed that the mesh has fixed spacings.
void calcFirstDerivative(double* mesh, double* func, IndexType nmesh,
                         double* out);


/// \brief shift tabulated function, f(x), by given delta-x
/// \param[in] mesh array of mesh points
/// \param[in] func array of function values
/// \param[in] nmesh length of mesh, func, and out arrays
/// \param[in] deltax amount to shift
/// \param[out] out shifted function
///
/// The output array, out, must be properly sized before this function is
/// called.  It is assumed that the mesh has fixed spacings.  Function is 
/// assumed to be constant beyond mesh end points.
void shiftData(double* mesh, double* func, IndexType nmesh, double deltax,
               double* out);


/// \brief calculate functional for profile fitting method
/// \param[in] mesh array of mesh points
/// \param[in] func array of binned data values
/// \param[in] prof array of profile values on binned mesh
/// \param[in] dprof array of profile derivatives
/// \param[in] nmesh length of mesh, func, prof, and dprof arrays
/// \param[out] z output functional value
/// \param[out] scale output parameter: scale
/// \param[out] bgrnd output parameter: bgrnd
void calcProfileFunctional(double* mesh, double* dat, double* prof, 
                           double* dprof, IndexType nmesh, double& z, 
                           double& scale, double& bgrnd);


/// \brief fit binned data set to given profile using the bisection method
/// \param[in] mesh array of mesh points
/// \param[in] func array of binned data values
/// \param[in] prof array of profile values on binned mesh
/// \param[in] dprof array of profile derivatives
/// \param[in] nmesh length of mesh, func, prof, and dprof arrays
/// \param[out] shift output parameter: shift
/// \param[out] scale output parameter: scale
/// \param[out] bgrnd output parameter: bgrnd
/// \param[out] fit output fit array
void fitProfile(double* mesh, double* dat, double* prof, double* dprof, 
                IndexType nmesh, double shift1, double shift2, 
                double& out_shift, double& out_scale, double& out_bgrnd, 
                double* fit);

/// \brief convolve a function with a Gaussian of given width
/// \param[in] mesh array of mesh points
/// \param[in] func array of binned data values
/// \param[in] nmesh length of mesh, func, and out arrays
/// \param[in] sigma width of Gaussian as standard deviation
/// \param[out] out array of convolved function
void convolveWithGaussian(double* mesh, double* func, IndexType nmesh, 
                          double sigma, double* out);

/// \brief convolve a function with a Gaussian of given width assuming that
///  the mesh has a fixed width (~20x faster than convolveWithGaussian)
/// \param[in] mesh array of mesh points
/// \param[in] func array of binned data values
/// \param[in] nmesh length of mesh, func, and out arrays
/// \param[in] sigma width of Gaussian as standard deviation
/// \param[out] out array of convolved function
void convolveWithGaussianFixed(double* mesh, double* func, IndexType nmesh, 
                               double sigma, double* out);
  
/// \brief Calculate the coefficient of determination (R^2) between a data set
///  and a fit.
/// \param[in] dat array of data values
/// \param[in] fit array of fitted values
/// \param[in] nmesh length of dat and fit
/// \return coefficient of determination 
double calcR2(double* dat, double* fit, int nmesh);

/// \brief Calculate Chi^2 between a data set and a fit.
/// \param[in] dat array of data values
/// \param[in] fit array of fitted values
/// \param[in] nmesh length of dat and fit
/// \param[in] nparam number of parameters used in fit
/// \return Chi^2
double calcChi2(double* dat, double* fit, int nmesh);

/// \brief Calculate reduced Chi^2 between a data set and a fit.
/// \param[in] dat array of data values
/// \param[in] fit array of fitted values
/// \param[in] nmesh length of dat and fit
/// \param[in] nparam number of parameters used in fit
/// \return reduced Chi^2
double calcReducedChi2(double* dat, double* fit, int nmesh, int nparam);

    
/// \brief convert sigma to FWHM
/// \param[in] sigma, fwhm = 2*sqrt(2log(2)) * sigma
/// \param[out] fwhm, fwhm = 2*sqrt(2log(2)) * sigma
double convertsigma2FWHM(double sigma); 

  
/// \brief converts sigma to FWHM
/// \param[in] fwhm, fwhm = 2*sqrt(2log(2)) * sigma
/// \param[out] sigma, sigma = fwhm/2/sqrt(2log(2))
double convertFWHM2sigma(double fwhm);


/// \brief compute the determinant of a 2x2 matrix
/// \param[in] a11 row 1 column 1
/// \param[in] a12 row 1 column 2
/// \param[in] a21 row 2 column 1
/// \param[in] a22 row 2 column 2
/// \return value of determinant
double determinant2x2(double a11, double a12, 
                      double a21, double a22);

/// \brief compute the determinant of a 3x3 matrix
/// \param[in] a11 row 1 column 1
/// \param[in] a12 row 1 column 2
/// \param[in] a13 row 1 column 3
/// \param[in] a21 row 2 column 1
/// \param[in] a22 row 2 column 2
/// \param[in] a23 row 2 column 3
/// \param[in] a31 row 3 column 1
/// \param[in] a32 row 3 column 2
/// \param[in] a33 row 3 column 3
/// \return value of determinant
double determinant3x3(double a11, double a12, double a13, 
                      double a21, double a22, double a23,
                      double a31, double a32, double a33);

/// \brief compute the inverse of a 3x3 matrix
/// \param[in] a11 input row 1 column 1
/// \param[in] a12 input row 1 column 2
/// \param[in] a13 input row 1 column 3
/// \param[in] a21 input row 2 column 1
/// \param[in] a22 input row 2 column 2
/// \param[in] a23 input row 2 column 3
/// \param[in] a31 input row 3 column 1
/// \param[in] a32 input row 3 column 2
/// \param[in] a33 input row 3 column 3
/// \param[out] b11 output row 1 column 1
/// \param[out] b12 output row 1 column 2
/// \param[out] b13 output row 1 column 3
/// \param[out] b21 output row 2 column 1
/// \param[out] b22 output row 2 column 2
/// \param[out] b23 output row 2 column 3
/// \param[out] b31 output row 3 column 1
/// \param[out] b32 output row 3 column 2
/// \param[out] b33 output row 3 column 3
/// \param[out] okay true if inverse exists
void inverse3x3(double a11, double a12, double a13, 
                double a21, double a22, double a23,
                double a31, double a32, double a33,
                double& b11, double& b12, double& b13,
                double& b21, double& b22, double& b23,
                double& b31, double& b32, double& b33,
                bool& okay);


/// \brief Evaluates a polynomial using Horner's method
/// param[in] x          independent variable in polynomial functioin y(x)
/// param[in] y          dependent variable in polynomial funtion y(x)
/// param[in] polyorder  order of polynomial to fit x and y
/// param[out] polycoeff coefficients of the fitting polynomial
void evaluatepoly(double x, double & y, int polyorder,
                  double * polycoeff);

/// \brief Solve for quadratic coefficients in y=a*x^2+b*x+c given three points.
/// \param[in] x1 X coordinate of first point
/// \param[in] y1 Y coordinate of first point
/// \param[in] x2 X coordinate of second point
/// \param[in] y2 Y coordinate of second point
/// \param[in] x3 X coordinate of third point
/// \param[in] y3 Y coordinate of third point
/// \param[out] a coefficient of x**2 term
/// \param[out] b coefficient of x**1 term
/// \param[out] c coefficient of x**0 term
/// \param[out] okay false if calculation failed
///
/// This function will set up a system of equations: y_i = a*x_i^2 + b*x_i +c
/// and solve for a,b,c using matrix inversion.  If the determinant of the 
/// matrix is singular (at least two points are identical), then okay=false.
void getQuadraticCoefficients(double x1, double y1, double x2, double y2,
                              double x3, double y3, double& a, double& b,
                              double& c, bool& okay);



// -----------------------------------------------------------------------------

/** @} */

}  // namespace ahmath

#endif /* AHMATH_AHMATH_H */

/* Revision Log
 $Log: ahmath.h,v $
 Revision 1.41  2016/07/25 21:09:17  rshill
 Recommit to pick up the log comment for r1.39, which was
 inadvertently dropped in the course of editing.

 Revision 1.40  2016/07/25 20:58:51  rshill
 Add a const to isPointInsidePoly arg list.

 Revision 1.39  2016/07/22 18:25:43  mwitthoe
 bug-fix for sawtooth interpolation functions to handle case where there is a step in the X coordinate in addition to the Y coordinate; temporarily revert changes to polygon routines

 Revision 1.38  2016/07/21 01:48:26  rshill
 Added new function to check complete polygon and removed this
 functionality from the routine that adds a point to a polygon.  Added const to several
 polygon function parameters.

 Revision 1.37  2016/03/31 20:41:09  mdutka
 revised function evaluate poly, now more efficient and unessary pass by references have been removed

 Revision 1.36  2015/11/19 21:11:27  mwitthoe
 ahmath library: add function to compute quadratic polynomial coefficients from three points: getQuadraticCoefficients

 Revision 1.35  2015/10/26 16:17:09  mwitthoe
 ahmath: fix prototype of calcChi2() function

 Revision 1.34  2015/10/21 17:55:27  mwitthoe
 ahmath: add function to compute Chi^2; correct calculation in reduced Chi^2 function

 Revision 1.33  2015/10/06 17:09:03  mdutka
 Changing arguments list of evaluatepoly to use dynamic array not vector

 Revision 1.32  2015/07/28 16:16:42  mwitthoe
 ahmath library: 1) remove obsolete function: search_sawY2X_old, 2) a) added AH_DEBUG statements to functions used by the ahtime tool

 Revision 1.31  2015/07/21 19:42:19  asargent
 added new segmentsIntersect function, added doxygen comments

 Revision 1.30  2015/07/20 18:53:42  asargent
 New polygon struct and function to check if point is inside a given polygon

 Revision 1.29  2015/04/08 18:35:48  asargent
 Updated interpolate_point_npoint to allow starting index. added new function search_npoint to search for starting index. overloaded interpolate function to include npoint interpolation. Updated ahmath tests.

 Revision 1.28  2015/04/07 19:34:22  asargent
 Added new n-point aitken method of interpolation to ahmath library

 Revision 1.27  2015/04/01 20:30:16  mwitthoe
 ahmath library: remove obsolete functions - calcGaussianPoint, calcExponentialPoint, and calcLinearPoint

 Revision 1.26  2015/01/21 17:16:26  mwitthoe
 ahmath: add functions to compute inverse of a 3x3 matrix

 Revision 1.25  2014/11/05 21:36:03  mwitthoe
 ahmath: modify search_sawY2X() to allow for case when xrough is in a different sawtooth than the desired result; see issue 458

 Revision 1.24  2014/07/21 15:07:09  mwitthoe
 ahmath library: add functions to calculate R^2 and Chi^2

 Revision 1.23  2014/06/06 19:26:45  mwitthoe
 ahmath: added new function, convolveWithGaussianFixed(), which does the same thing as convolveWithGaussian, but assumes fixed mesh spacing -- the new function is about 20 times faster than the old; updated calcGaussianPoint(), convertsigma2FWHM(), and convertFWHM2sigma to use static constants to improve performance when called repeatedly

 Revision 1.22  2014/05/21 18:57:18  asargent
 Courtesy of Mike Dutka: Added math functions calcGaussianPoint(), calcExponentialPoint(), calcLinearPoint(), convertSigma2FWHM() and convertFWHM2Sigma()

 Revision 1.21  2013/11/20 22:58:18  mwitthoe
 ahmath library: replace all std::vectors with C arrays as convoluted consequence of issue #315; see that issue for more details

 Revision 1.20  2013/07/01 14:31:53  mwitthoe
 add functions to ahmath to assist with profile fitting (in sxsdrift)

 Revision 1.19  2013/04/10 15:15:36  mwitthoe
 ahmath library: restore interpolation_type() function

 Revision 1.18  2013/04/08 21:18:14  mwitthoe
 remove old versions of interpolation functions from ahmath

 Revision 1.17  2013/04/04 21:26:25  mwitthoe
 restructure interpolation routines in ahmath; separate search and interpolation functionality into separate routines

 Revision 1.16  2012/10/31 18:10:13  mwitthoe
 removed ahlookup library from ahmath

 Revision 1.15  2012/10/25 01:21:29  mwitthoe
 add lookup table library to ahmath

 Revision 1.14  2012/10/16 21:09:18  mwitthoe
 ahmath: move interpolation functions into separate files, ahinterp, remove ahmath.cxx, and reduce ahmath.h to just an include statement to ahinterp; this change is to help organize future additions to the ahmath library

 Revision 1.13  2012/10/16 20:29:37  mwitthoe
 ahmath: add single-value interpolation routine for double sawtooth functions; test code added

 Revision 1.12  2012/09/24 15:41:22  mwitthoe
 revamp interpolation routines in ahmath; now support interpolation of saw tooth functions

 Revision 1.11  2012/09/18 01:46:36  mwitthoe
 revamp interpolation to reduce code duplication; add sawtooth (X->Y) interpolation method

 Revision 1.10  2012/09/14 23:58:25  mwitthoe
 apply version standards to ahmath

 Revision 1.9  2012/08/29 20:59:17  mwitthoe
 moved ahmath tests from library to test code

 Revision 1.8  2012/08/24 19:07:25  mwitthoe
 clean up argument list for ahmath library

 Revision 1.7  2012/08/23 16:50:33  mwitthoe
 standardized tests for ahmath

 Revision 1.6  2012/08/17 20:40:56  mwitthoe
 apply standards to ahmath

 Revision 1.5  2012/07/31 20:01:34  mwitthoe
 change long long to long in ahmath interpolation routine

 Revision 1.4  2012/06/28 23:14:48  mwitthoe
 ahmath: add function to convert string into interpolation type enumerated index; ahfits: small fix to error message

 Revision 1.3  2012/06/28 02:05:33  mwitthoe
 ahmath: add option in interpolation functions to allow for extrapolation without throwing an error

 Revision 1.2  2012/06/25 14:06:31  mwitthoe
 add interpolation functions to ahmath which work on integer X-values and double Y-values

 Revision 1.1  2012/06/21 19:03:02  mwitthoe
 add ahmath with an interpolation function


*/
