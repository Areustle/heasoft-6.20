/// \file ahmath.cxx
/// \brief general mathematical routines
/// \author Mike Witthoeft
/// \date $Date: 2016/07/25 22:09:02 $
///
/// \internal
/// \note Several functions in this library deal with sawtooth functions. The
///   sawtooth functions are assumed to be strictly monotonic (with the 
///   exception of the drop-off (i.e. cliff) between two points) and have a
///   positive slope.  In the comments, I will refer to the "incline" and 
///   "cliff" regions of the sawtooth function.  I also refer to the incline 
///   area as a single cycle, so that, when moving across a cliff region, we 
///   enter a new cycle.

#define AHLABEL ahmath_ahmath
#define AHCVSID "$Id: ahmath.cxx,v 1.57 2016/07/25 22:09:02 rshill Exp $"

#include "ahmath/ahmath.h"
#include "ahlog/ahlog.h"

#include <algorithm>
#include <sstream>
#include <cmath>
#include <vector>

// #include <iomanip>    // MCW: temporary; for debugging

namespace ahmath {

// -----------------------------------------------------------------------------

void checkPolygon(Polygon & poly) {

  // Make sure that there are enough points to make a polygon.
  // Make sure that line segments not sharing an endpoint 
  //  don't cross.
  if (poly.m_num_points > 2) {

    double x1=0, y1=0, x2=0, y2=0;

    for (int ii = 0; ii<poly.m_num_points; ++ii) {

      if (0 == ii) {
        x1 = poly.m_x[poly.m_num_points - 1];
        y1 = poly.m_y[poly.m_num_points - 1];
        x2 = poly.m_x[0];
        y2 = poly.m_y[0];
      } else {
        x1 = poly.m_x[ii - 1];
        y1 = poly.m_y[ii - 1];
        x2 = poly.m_x[ii];
        y2 = poly.m_y[ii];
      }

      for (int jj=1; jj < poly.m_num_points-2; ++jj) {

        int j1 =  (jj + ii) % poly.m_num_points;
        int j2 =  (jj + ii + 1) % poly.m_num_points;

        //std::cout << "ii=" << ii << " jj=" << jj << " j2=" << j2 << std::endl;
        //std::cout << x1 << " " << y1 << " " << x2 << " " << y2 << " " << std::endl;
        //std::cout << poly.m_x[j1] << " " << poly.m_y[j1] << " " 
        //   << poly.m_x[j2] << " " << poly.m_y[j2] << " " << std::endl;

        // Check for intersecting lines. If lines intersect, throw an error
        if(segmentsIntersect(x1,y1,x2,y2,
           poly.m_x[j1],poly.m_y[j1],
           poly.m_x[j2],poly.m_y[j2])) 
          AH_THROW_RUNTIME("Ill-formed polygon. Points added may have invalid insertion order.");

      } // end inner loop
    } // end outer loop

  } else {
    AH_THROW_RUNTIME("Polygon has fewer than 3 points.");
  }

  poly.m_checked = true;

}

// -----------------------------------------------------------------------------

void addPointToPolygon(const double x1, const double y1, Polygon & poly) {

  // Check if the new point is already in the polygon
  // If it's already there, there is nothing to be done
  for (int ii = 0 ; ii < poly.m_num_points ; ++ii) {
    if(x1 == poly.m_x[ii] && y1 == poly.m_y[ii]) return;
  }

  // add point to polygon
  poly.m_x.push_back(x1);
  poly.m_y.push_back(y1);

  // Update minimum and maximum x and y values
  if(poly.m_num_points == 0) {
    poly.m_minx = x1;
    poly.m_maxx = x1;
    poly.m_miny = y1;
    poly.m_maxy = y1;
  } else {
    poly.m_minx = fmin(x1,poly.m_minx);
    poly.m_maxx = fmax(x1,poly.m_maxx);
    poly.m_miny = fmin(y1,poly.m_miny);
    poly.m_maxy = fmax(y1,poly.m_maxy);
  }

  // Update number of points in polygon
  poly.m_num_points+=1;

  // Polygon has changed, so needs check before use
  poly.m_checked = false;

}

// -----------------------------------------------------------------------------

bool segmentsIntersect(const double x1a, const double y1a, 
                       const double x1b, const double y1b, 
                       const double x2a, const double y2a, 
                       const double x2b, double y2b) {

  double m1 = 0., m2 = 0.;   // slope of line 1 and line 2
  double b1 = 0., b2 = 0.;   // y-intersect of line 1 and line 2
  double dx1 = 0., dx2 = 0.; // delta-x to check for vertical lines
  double x = 0.;             // test point

  // if y-positions do not overlap, then lines cannot intersect
  if(fmin(y1a,y1b) > fmax(y2a,y2b)) return false;
  if(fmax(y1a,y1b) < fmin(y2a,y2b)) return false;

  // get delta-x for each segment to check for vertical lines
  dx1 = x1b - x1a;
  dx2 = x2b - x2a;

  if (dx1 == 0. && dx2 == 0.) { // Both lines are vertical
    if (x1a != x2a) return false; 
  } else if (dx1 == 0.) { // First line is vertical
    if ((x2a-x1a)*(x2b-x1a) > 0.) return false;
  } else if (dx2 == 0) { // Second line is vertical
    if ((x1a-x2a)*(x1b-x2a) > 0.) return false;
  } else { // Neither line is vertical

    // define line for first segment y1 = m1*x + b1
    m1 = (y1b-y1a)/dx1;
    b1 = y1a-m1*x1a;

    // define line for second segment y2 = m2*x + b2
    m2 = (y2b-y2a)/dx2;
    b2 = y2a-m2*x2a;

    // if the slopes are the same, need to check the intercepts
    // and check for overlap
    if( m1 == m2 ) {
      // slopes are equal
      if (b1 != b2) { 
        return false; // intercepts are different
      } else if (   fmin(x1a,x1b) >= fmax(x2a,x2b) 
                 || fmin(x2a,x2b) >= fmax(x1a,x1b) ) {
        // domains do not overlap
        return false;
      }
    } else { // Lines must intersect (if they stretch to infinity)
      x = -(b2-b1)/(m2-m1); // where two lines intersect
      if (    x < fmin(x1a,x1b) || x > fmax(x1a,x1b)
           || x < fmin(x2a,x2b) || x > fmax(x2a,x2b) ) return false;
    }

  }

  // All intersection tests pass, lines intersect
  return true;

}

// -----------------------------------------------------------------------------

bool isPointInsidePolygon(const double x, 
                          const double y,
                          const Polygon poly) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int edgeCrossings = 0;
  double nudgeFactor = 0.0;
  double critFactor = 0;
  double xIntercept = 0.0;
  double yProduct = 0.0;
  int numPoints = poly.m_num_points;
  Polygon newVertices(numPoints);
  
  // -------------------------------------

  // Polygon must be checked before use
  if (!poly.m_checked) {
    AH_THROW_LOGIC("Polygon not validity checked; exiting.");
  }
  
  // Check if point lies outside the bounding box of the polygon
  if( poly.m_minx > x || poly.m_maxx < x || poly.m_miny > y || poly.m_maxy < y ) {
    return false;
  }
  
  // Transform the vertices so that the input point is at the origin
  // Check if the test line from point will intercept one of the vertex 
  // y values and also test if the point lies on origin
  for (int ii = 0 ; ii < numPoints ; ++ii) {
    
    newVertices.m_x[ii] = poly.m_x[ii] - x;
    newVertices.m_y[ii] = poly.m_y[ii] - y;
    
    //if the vertex lies on the origin the point is inside the polygon
    if ((newVertices.m_x[ii] == 0.0) && (newVertices.m_y[ii] == 0.0)) {
      // Point lies on a vertex
      return true;
    }
    
    /* Method: We check if a line drawn from the point crosses edges and how
    many. For the transformed polygon that means effectively the x-axis. If this line
    passes through one or more vertices there is a problem because two edges
    converge on to a single point and are then counted as one giving the wrong
    result. The method to overcome this is to nudge the vertex away from the line
    by a small amount because it will then give the right result. */
    if ((newVertices.m_y[ii] == 0.0) && (nudgeFactor == 0.0)) {
      if (y == 0.0) {
        nudgeFactor = 1e-4;
      } else {
        nudgeFactor = 1e-4 * (poly.m_maxy-poly.m_miny);
      }
    }
  }
  
  // Transform the y-coordinates of the vertices, applying the nudge factor
  if (nudgeFactor > 0.0) {
    for (int ii = 0 ; ii < numPoints ; ++ii) {
      newVertices.m_y[ii] += nudgeFactor;
    }
  }
  
  for (int ii = 1 ; ii < numPoints ; ++ii) {
    // Does this vertex lie to the right of the y-axis and does the edge 
    // formed with the previous vertex cross y=0? The following basically 
    // demands that there is an intercept on the x-axis between two points 
    // and that the intercept should have x>0
    critFactor = newVertices.m_y[ii-1] * 
                  (newVertices.m_x[ii] - newVertices.m_x[ii-1]) / 
                  (newVertices.m_y[ii] - newVertices.m_y[ii-1]);
    xIntercept = newVertices.m_x[ii-1] - critFactor;
    yProduct = newVertices.m_y[ii] * newVertices.m_y[ii-1];
    if ((xIntercept > 0) && (yProduct <= 0.0)) {
      edgeCrossings++;
    }
  }
  
  // The loop did not do the last edge (last vertex joining 1st vertex)/ 
  // so do it now
  critFactor = newVertices.m_y[0] * 
               (newVertices.m_x[numPoints-1] - newVertices.m_x[0]) / 
               (newVertices.m_y[numPoints-1] - newVertices.m_y[0]);
  xIntercept = newVertices.m_x[0] - critFactor;
  yProduct = newVertices.m_y[numPoints-1] * newVertices.m_y[0];
  if ((xIntercept > 0) && (yProduct <= 0.0)) {
    edgeCrossings++;
  }
  
  if ( (edgeCrossings % 2) == 0) {
    // even number of edge crossings, so it's not inside
    return false;
  } else {
    return true;
  }

} // end isPointInsidePolygon()


// -----------------------------------------------------------------------------

int interpolation_type(const std::string & interp) {
  std::string chk=(std::string)interp;
  std::transform(chk.begin(),chk.end(),chk.begin(),::tolower);  // lowercase
  if (chk == "nearest")
    return NEAREST;
  else if (chk == "twopoint")
    return TWOPOINT;
  else if (chk == "npoint")
    return NPOINT;
  else
    AH_THROW_RUNTIME("invalid interpolation type");

  // should never reach here
  return NEAREST;
}

// -----------------------------------------------------------------------------

double interpolate_point_nearest(double x, double x1, double y1, double x2,
                                 double y2) {
  if (std::abs(x-x1) < std::abs(x-x2)) return y1;
  return y2;
}

// -----------------------------------------------------------------------------

double interpolate_point_twopoint(double x, double x1, double y1, double x2,
                                  double y2) {
  if (x1 == x2)
    AH_THROW_RUNTIME("interpolation error: x1=x2; divide by zero");

  return y1+(y2-y1)*(x-x1)/(x2-x1);

}

// -----------------------------------------------------------------------------

double interpolate_point_npoint(double x, double * xarr, double * yarr, int npoints, IndexType idx0) {

  // exact match?
  if (xarr[idx0] == x) return yarr[idx0];

  double y_bar = 0.;

  double * arr;
  arr = new double[npoints];
  
  // Copy yarr to new array
  for (int jj = 0; jj < npoints; ++jj) arr[jj] = yarr[jj]; 

  // Use aitken method of interpolation to approximate y
  for(int ii = idx0+1; ii < npoints; ++ii) {
    for(int jj = ii; jj < npoints; ++jj) {
      arr[jj] = (arr[ii-1]*(xarr[jj]-x)-arr[jj]*(xarr[ii-1]-x))/(xarr[jj]-xarr[ii-1]);
    }
  }

  // Copy calculation to output value
  y_bar = arr[npoints-1];

  // Unallocate array 
  delete [] arr;

  return y_bar;

}

// -----------------------------------------------------------------------------

IndexType search(double x, double* xarr, IndexType narr, bool & extrap, 
                 IndexType idx0) {
  // starting index okay?
  if (x < xarr[idx0]) idx0=0;
  if (idx0 >= narr-1) idx0=narr-1;

  // check if x out-of-range (only needed if searching from beginning)
  if (idx0 == 0 && x < xarr[0]) {
    extrap=true;
    return 0;
  }
  if (x > xarr[narr-1]) {
    extrap=true;
    return narr-2;     // idx+1 must be valid for interpolation function
  }

  // do search
  extrap=false;
  IndexType i=idx0;
  while (xarr[i] < x) i++;

  // exact match?
  if (xarr[i] == x) return i;

  // correct index to be below given point
  if (i > 0) i--;

  return i;
}

// -----------------------------------------------------------------------------

IndexType search_npoint(double x, double* xarr, IndexType narr, unsigned long npoints, 
                        bool & extrap, IndexType idx0) {

  if (narr<2) AH_THROW_RUNTIME("Size of xarr is less than two");

  // Check if npoints is less than size of array
  if(npoints > narr) {
    AH_THROW_RUNTIME("Number of points to search exceeds size of xarr");
  }

  // starting index okay?
  if (x < xarr[idx0]) idx0=0;
  if (idx0 >= narr-1) idx0=narr-1;

  // check if x out-of-range (only needed if searching from beginning)
  if (idx0 == 0 && x < xarr[0]) {
    extrap=true;
    return 0;
  }
  if (x > xarr[narr-1]) {
    extrap=true;
    return narr-npoints;     // idx+1 must be valid for interpolation function
  }

  // do search, find closest value to x
  extrap=false;
  IndexType i=idx0;
  while (xarr[i] < x) i++;

  // exact match?
  if (xarr[i] == x) return i;
  // Why would you interpolate with one point?
  if (npoints == 1) return i;
  // It's also silly to aitken interpolate with two points
  if (npoints == 2) return i;

  IndexType j=1;
  IndexType k=1;
  int foundpoints = npoints-1;
  while(foundpoints) {
    // Check boundary conditions
    if(i-j<=0) break;
    if(k+i>=narr) break;
    // Step through xarr, finding closest elements
    // Assume xarr is ordered
    if((xarr[i]-xarr[i-j])<=(xarr[i+k]-xarr[i])) {
      j++;
    } else {
      k++;
    }
    foundpoints--;
  }

  // Set correct index value
  if(i-j<=0) i=0;
  else if(i+k>=narr) i=narr-npoints;
  else i-=j;

  return i;
}
// -----------------------------------------------------------------------------

IndexType search_sawX2Y(double x, double* xarr, double* yarr, 
                        IndexType narr, bool & extrap, int itype, 
                        IndexType idx0) {

  // get location in xarr
  IndexType out=search(x,xarr,narr,extrap,idx0);

  // if not nearest-point interpolation: check if (out, out+1) span sawtooth 
  // segment boundary
  if (itype != NEAREST && yarr[out] > yarr[out+1]) {
    if (out == 0) AH_THROW_RUNTIME("only one point in segment; cannot interpolate");
    out--;
  }
  return out;
}

// -----------------------------------------------------------------------------

IndexType search_sawY2X(double xrough, double y, double ymax, double* xarr, 
                        double* yarr, IndexType narr, double revfrac,
                        bool & extrap, IndexType idx0) {

  AH_DEBUG << "Inputs: " << ahlog::setprecision(15) << std::endl;
  AH_DEBUG << "  xrough:         " << xrough << std::endl;
  AH_DEBUG << "  y:              " << y << std::endl;
  AH_DEBUG << "  ymax:           " << ymax << std::endl;
  AH_DEBUG << "  xarr range:     " << xarr[0] << " : " << xarr[narr-1] << std::endl;
  AH_DEBUG << "  yarr range:     " << yarr[0] << " : " << yarr[narr-1] << std::endl;
  AH_DEBUG << "  xarr/yarr size: " << narr << std::endl;
  AH_DEBUG << "  revfrac:        " << revfrac << std::endl;
  AH_DEBUG << std::endl;

  // check if y in range of [0:ymax]
  if (y < 0.) {
    std::stringstream msg;
    msg << "y-value of sawtooth function cannot be negative; y=" << y;
    AH_THROW_RUNTIME(msg.str());
  }
  if (y > ymax) {
    std::stringstream msg;
    msg << "y-value of sawtooth function cannot exceed ymax; " << y << " > " << ymax;
    AH_THROW_RUNTIME(msg.str());
  }

  // Get starting index for search using xrough.  Need to do a special search
  // if the given xarr also goes through a sawtooth.  It is assumed that the
  // length of the xarr range is much less than the period of the X sawtooth.
  // This is a safe assumption for the Hitomi mission where the range of xarr
  // is on the order of a few days and the X sawtooth period is 2.9 years.
  extrap=false;
  IndexType idx=0;
  if (xarr[narr-1] < xarr[0]) {
    AH_DEBUG << "Sawtooth discontinuity discovered in xarr" << std::endl;
    if (std::abs(xrough-xarr[0]) < std::abs(xrough-xarr[narr-1])) {   // xrough occurs before sawtooth discontinuity
      AH_DEBUG << "  xrough occurs before discontinuity" << std::endl;
      // search forwards 
      idx=idx0;
      if (xrough < xarr[idx]) idx=0;
      while (xarr[idx] < xarr[idx+1]) {   // while before the discontinuity
        if (xrough < xarr[idx]) break;
        idx++;
      }
      if (xarr[idx+1] > xarr[idx]) idx--;  // back up one step if right at the discontinuity
    } else {   // xrough occurs after sawtooth discontinuity
      AH_DEBUG << "  xrough occurs after discontinuity" << std::endl;
      // check for xrough out-of-range
      if (xrough > xarr[narr-1]) {
        AH_DEBUG << "  xrough occurs after end of xarr" << std::endl;
        idx=narr-2;
      } else {   // xrough in range of xarr
        // if idx0 is before discontinuity, move starting index to right-side of discontinuity
        idx=idx0;
        while (std::abs(xarr[idx]-xarr[0]) < std::abs(xarr[idx]-xarr[narr-1])) idx++;
  
        // now do search as normal
        while (xrough > xarr[idx]) idx++;
      }
    }
  } else {
    // get starting index for search using xrough
    idx=search(xrough,xarr,narr,extrap,idx0);
  }
  AH_DEBUG << "Search position using xrough: idx = " << idx << std::endl;

  // if xrough out-of-range, use first (last) two points to see if xrough
  // truly in first (last) segment (tooth); if not, then throw error since
  // extrapolation not possible
  if (extrap) {
    AH_DEBUG << "  ...search position out-of-range of input array" << std::endl;
    AH_DEBUG << "  ...checking if extrapolated point in y range" << std::endl;

    if (idx == 0) {
      double ytmp=interpolate_point_twopoint(xrough,xarr[0],yarr[0],xarr[1],
                                             yarr[1]);
      if (ytmp < 0.) {
        std::stringstream msg;
        msg << "xrough too small; cannot extrapolate into previous tooth; ";
        msg << "xrough, yrough, ymax: " << xrough << ", " << ytmp << ", " << ymax << std::endl;
        AH_THROW_RUNTIME(msg.str());
      }
    } else {   // has to be last point
      double ytmp=interpolate_point_twopoint(xrough,xarr[idx-1],yarr[idx-1],
                                             xarr[idx],yarr[idx]);
      if (ytmp > ymax) {
        std::stringstream msg;
        msg << "xrough too large; cannot extrapolate into next tooth; ";
        msg << "xrough, yrough, ymax: " << xrough << ", " << ytmp << ", " << ymax << std::endl;
        AH_THROW_RUNTIME(msg.str());
      }
    }
    AH_DEBUG << "  ...okay!" << std::endl;
  }

  // Check y-value near xrough against the y-value to interpolate at.  If the
  // values are too far away, then reverse direction of search. This is only
  // applicable for forwards or backwards searches.
  bool reverse=false;
  if (std::abs(y-yarr[idx]) > revfrac*ymax) reverse=true;

  // Determine if need to search forwards, backwards, or if xrough is in
  // step-down region.  If extrapolating, then no further search required
  // and idx will not change.
  std::string srch;
  if (yarr[idx] > yarr[idx+1]) {
    srch="stepdown";
    AH_DEBUG << "Rough search position is in step-down region of sawtooth between indices " << idx << " and " << idx+1 << std::endl;
  } else if (idx > 0 && y < yarr[idx]) {
    srch="backwards";
    if (reverse) srch="forwards";
    AH_DEBUG << "Need to search " << srch << " for exact position" << std::endl;
  } else if (idx < narr-1 && y > yarr[idx]) {
    srch="forwards";
    if (reverse) srch="backwards";
    AH_DEBUG << "Need to search " << srch << " for exact position" << std::endl;
  } else {
    srch="extrap";
  }

  // If in stepdown region, then check which tooth the current point belongs
  // based on how close its Y-value is to the Y values of the lookup table on
  // either side of the step-down region.  After this is determined, we still
  // need to search forwards or backwards for the exact position to interpolate
  // at since the the xrough value is not exact.
  if (srch == "stepdown") {
    if (std::abs(y-yarr[idx]) < std::abs(y-yarr[idx+1])) {
      if (y < yarr[idx]) srch="backwards";    // need to look for points surrounding Y value
      idx--;                                  // grab two higher-region points for interpolation
    } else {   // if (y <= yarr[idx+1]) {     // point belongs to lower region
      if (y > yarr[idx+1]) srch="forwards";   // need to look for points surrounding Y value
      idx++;                                  // grab two lower-region points for interpolation
    }
  }

  // Depending on value of y relative to yarr[idx], search forward or backwards;
  // resulting index should be largest value whose y-value is smaller than y,
  // unless at the beginning/end of the input arrays, or in step-down region,
  // then idx will be the closest point to y within the same segment as xrough
  // so that the (idx+1) point is also in the same segment.
  if (srch == "backwards") {
    // if in reverse-mode, need to move to next tooth from xrough
    if (reverse) {
      idx--;
      while (idx > 0 && (yarr[idx] < yarr[idx+1])) idx--;
    }

    // perform search in tooth segment
    while (idx > 0 && (y < yarr[idx])) {
      idx--;
      if (idx == 0) {                         // at beginning of array
        break;
      } else if (yarr[idx] > yarr[idx+1]) {   // going into next segment
        idx++;
        break;
      }
    }

    // if in reverse-mode, then check if in step-down region and correct
    if (reverse) {
      if (yarr[idx] > yarr[idx+1]) idx--;
    }
  } else if (srch == "forwards") {
    // if in reverse-mode, need to move to next tooth from xrough
    if (reverse) {
      idx++;
      while (idx < narr-1 && (yarr[idx] > yarr[idx-1])) idx++;
    }

    // perform search in tooth segment
    while (idx < narr-1 && (y > yarr[idx])) {
      idx++;
      if (idx == narr-1) {                    // at end of array
        break;
      } else if (yarr[idx] < yarr[idx-1]) {   // going into next segment
        idx--;
        break;
      }
    }
    if (yarr[idx-1] < yarr[idx]) idx--;    // shift so that yarr[idx] < y
  }

  AH_DEBUG << "Final search position; idx = " << idx << std::endl;
  return idx;
}

// -----------------------------------------------------------------------------

double interpolate(double x, double* xarr, double* yarr, IndexType narr, 
                   int itype, IndexType idx) {

  // perform interpolation to get output Y-value
  double out=0.;
  switch (itype) {

    case NEAREST:
      AH_DEBUG << "Using nearest-neighbor interpolation" << std::endl;
      out=interpolate_point_nearest(x,xarr[idx],yarr[idx],xarr[idx+1],
                                    yarr[idx+1]);
      break;

    case TWOPOINT:
      AH_DEBUG << "Using linear interpolation" << std::endl;
      AH_DEBUG << "Point 1: X,Y = " << xarr[idx] << ", " << yarr[idx] << std::endl;
      AH_DEBUG << "Point 2: X,Y = " << xarr[idx+1] << ", " << yarr[idx+1] << std::endl;
      AH_DEBUG << "Interpolate at X = " << x << std::endl;
      out=interpolate_point_twopoint(x,xarr[idx],yarr[idx],xarr[idx+1],
                                     yarr[idx+1]);
      break;

    case NPOINT:
      AH_THROW_RUNTIME("Interpolate NPOINT missing parameter: npoints");
      break;

    default:
      AH_THROW_RUNTIME("invalid interpolation type; see enum interptypes");
  }

  return out;

}

// -----------------------------------------------------------------------------

double interpolate(double x, double* xarr, double* yarr, IndexType narr, 
                   int itype, IndexType idx, int npoints) {

  // perform interpolation to get output Y-value
  double out=0.;
  switch (itype) {

    case NEAREST:
      out=interpolate(x,xarr,yarr,narr,itype,idx);
      break;

    case TWOPOINT:
      out=interpolate(x,xarr,yarr,narr,itype,idx);
      break;

    case NPOINT:
      out=interpolate_point_npoint(x,xarr,yarr,npoints,idx);
      break;

    default:
      AH_THROW_RUNTIME("invalid interpolation type; see enum interptypes");
  }

  return out;

}

// -----------------------------------------------------------------------------

double interpolate_sawX2Y(double x, double ymax, double* xarr, double* yarr,
                          IndexType narr, int itype, IndexType idx) {

  // note: the input idx value should be set so that the interpolate() call
  //       will not span sawtooth segments; i.e. use search() to get the 
  //       correct index

  // perform interpolation to get output Y-value
  double out=interpolate(x,xarr,yarr,narr,itype,idx);

  // ensure that output Y-value is in range
  while (out < 0.) out+=ymax;
  while (out >= ymax) out-=ymax;

  return out;
}

// -----------------------------------------------------------------------------

double interpolate_sawY2X(double y, double* xarr, double* yarr, IndexType narr,
                          int itype, IndexType idx) {

  // The input idx value should be set so that the interpolate() call
  // will not span Y sawtooth segments.  Use search_sawY2X() to get the
  // correct index.  However, it is possible that the idx and idx+1 span
  // an X sawtooth boundary. In this case, we perform two interpolations
  // (idx-1:idx) and (idx+1:idx+2) and take the result where the
  // interpolated results is in the range [0:ymax].

  if (xarr[idx] > xarr[idx+1]) {    // [idx:idx+1] spans X sawtooth boundary
    double out1=0.;
    double out2=0.;
    bool okay1=false;
    bool okay2=false;
    if (idx > 0) {
      okay1=true;
      out1=interpolate(y,yarr,xarr,narr,itype,idx-1);
    }
    if (idx < narr-2) {
      okay2=true;
      out2=interpolate(y,yarr,xarr,narr,itype,idx+1);
    }

    if (!okay2) {
      return out1;
    } else if (!okay1) {
      return out2;
    } else {
      // if two results were calculated, use 2nd if it is not negative
      if (out2 >= 0.) 
        return out2;
      else
        return out1;
    }
  } else {
    AH_DEBUG << "Calling ahmath::interpolate with xarr/yarr switched" << std::endl;
    return interpolate(y,yarr,xarr,narr,itype,idx);
  }

}

// -----------------------------------------------------------------------------

void calcFirstDerivative(double* mesh, double* func, IndexType nmesh, 
                         double* out){

  // calculate factor used in derivative expression
  double dmesh=mesh[1]-mesh[0];
  double factor=0.5/dmesh;

  // first and last points use end-point formula with same accuracy as standard
  // 3-point formula
  IndexType ix=0;
  out[ix]=factor*(-3.*func[0]+4.*func[1]-func[2]);
  for (ix=1; ix < nmesh-1; ix++) out[ix]=factor*(func[ix+1]-func[ix-1]);
  ix=nmesh-1;
  out[ix]=factor*(3.*func[ix]-4.*func[ix-1]+func[ix-2]);
}

// -----------------------------------------------------------------------------

void shiftData(double* mesh, double* func, IndexType nmesh, double deltax, 
               double* out) {

  IndexType ixp=0;
  for (IndexType ix=0; ix < nmesh; ix++) {
    double xp=mesh[ix]-deltax;

    if (xp < mesh[0])
      out[ix]=func[0];        // constant background to left
    else if (xp > mesh[nmesh-1])
      out[ix]=func[nmesh-1];    // constant background to right
    else {
      while (mesh[ixp] < xp) ixp++;
      out[ix]=ahmath::interpolate_point_twopoint(xp,mesh[ixp-1],func[ixp-1],
                                                 mesh[ixp],func[ixp]);
    }
  }
}

// -----------------------------------------------------------------------------

void calcProfileFunctional(double* mesh, double* dat, double* prof, 
                           double* dprof, IndexType nmesh, double& z, 
                           double& scale, double& bgrnd) {

  double sum_F=0.;     // sum f(x_i-mu)
  double sum_Fp=0.;    // sum df(x_i-mu)/dmu
  double sum_F2=0.;    // sum f^2(x_i-mu)
  double sum_F2p=0.;   // sum f(x_i-mu) df(x_i-mu)/dmu
  double sum_G=0.;     // sum g_i
  double sum_H=0.;     // sum g_i f(x_i-mu)
  double sum_Hp=0.;    // sum g_i df(x_i-mu)/dmu
  for (IndexType ix=0; ix < nmesh; ix++) {
    sum_F+=prof[ix];
    sum_Fp+=dprof[ix];
    sum_F2+=prof[ix]*prof[ix];
    sum_F2p+=prof[ix]*dprof[ix];
    sum_G+=dat[ix];
    sum_H+=dat[ix]*prof[ix];
    sum_Hp+=dat[ix]*dprof[ix];
  }
  double dbl_nmesh=(double)nmesh;

  scale=(sum_H-sum_G*sum_F/dbl_nmesh)/(sum_F2-sum_F*sum_F/dbl_nmesh);
  bgrnd=(sum_G*sum_F2-sum_H*sum_F)/(dbl_nmesh*sum_F2-sum_F*sum_F);
  z=sum_Hp-scale*sum_F2p-bgrnd*sum_Fp;
}

// -----------------------------------------------------------------------------

void fitProfile(double* mesh, double* dat, double* prof, 
                double* dprof, IndexType nmesh, double shift1, double shift2,
                double& out_shift, double& out_scale, double& out_bgrnd,
                double* fit) {

  double tol=0.25*(mesh[1]-mesh[0]); // set tolerance to quarter of mesh spacing
  int maxit=20;         // maximum number of iterations

  // declare variables
  double z1=0.;        // lower functional value
  double scale1=0.;    // lower scaling parameter
  double bgrnd1=0.;    // lower background parameter
  double z2=0.;        // upper functional value
  double scale2=0.;    // upper scaling parameter
  double bgrnd2=0.;    // upper background parameter
  double zp=0.;        // temporary functional value
  double scalep=0.;    // temporary scaling parameter
  double bgrndp=0.;    // temporary background parameter
  double dxp=0.;       // intermediate shift
  double* sprof;        // shifted profile
  double* dsprof;       // derivative of shifted profile

  // store original search range
  double oshift1=shift1;
  double oshift2=shift2;

  // allocate sprof, dsprof
  sprof=new double[nmesh];
  dsprof=new double[nmesh];

  // calculate fitting functional for the 1st initial condition
  shiftData(mesh,prof,nmesh,shift1,sprof);
  shiftData(mesh,dprof,nmesh,shift1,dsprof);
  calcProfileFunctional(mesh,dat,sprof,dsprof,nmesh,z1,scale1,bgrnd1);

  // calculate fitting functional for the 2nd initial condition
  shiftData(mesh,prof,nmesh,shift2,sprof);
  shiftData(mesh,dprof,nmesh,shift2,dsprof);
  calcProfileFunctional(mesh,dat,sprof,dsprof,nmesh,z2,scale2,bgrnd2);

  if (z1*z2 > 0.) {
    delete [] sprof;
    delete [] dsprof;
    AH_THROW_RUNTIME("bisection method failure; either zero or an even number of roots exist");
  }

  // start bisection loop
  int it=0;        // iteration counter
  while (1) {
    // not converging?
    it++;
    if (it > maxit) {
      delete [] sprof;
      delete [] dsprof;
      AH_THROW_RUNTIME("bisection method does not converge");
    }

    // calculate change in shift (dx) to check if finished
    double ddx=std::abs(shift2-shift1);
    if (ddx <= tol) break;

    // get next shift
    dxp=0.5*(shift1+shift2);

    // calculate next functional
    shiftData(mesh,prof,nmesh,dxp,sprof);
    shiftData(mesh,dprof,nmesh,dxp,dsprof);
    calcProfileFunctional(mesh,dat,sprof,dsprof,nmesh,zp,scalep,bgrndp);

    // determine next interval
    if (z1*zp < 0.) {
      z2=zp;
      scale2=scalep;
      bgrnd2=bgrndp;
      shift2=dxp;
    } else {
      z1=zp;
      scale1=scalep;
      bgrnd1=bgrndp;
      shift1=dxp;
    }
  }
  out_shift=dxp;
  out_bgrnd=bgrndp;
  out_scale=scalep;

  // check if result within tolerance range limits
  if (out_shift-oshift1 <= tol || oshift2-out_shift <= tol) {
    AH_INFO(ahlog::LOW) << " *** Result of profile fitting is at limit of search "
                        << "range; expand search range to find true minimum" 
                        << std::endl;
  }

  // apply scale and background to fitted profile
  for (IndexType ix=0; ix < nmesh; ix++) 
    fit[ix]=out_scale*sprof[ix]+out_bgrnd;

  // unallocate temporary arrays
  delete [] sprof;
  delete [] dsprof;

}

// -----------------------------------------------------------------------------

void convolveWithGaussian(double* mesh, double* func, IndexType nmesh,
                          double sigma, double* out) {

  // using Simpson's Rule
  for (IndexType i=0; i < nmesh; i++) {
    double asum=0.;     // main integration
    double bsum=0.;     // area of Gaussian applied

    // first point has factor of 1/2
    double xp=(mesh[i]-mesh[0])/sigma;
    double dint=0.5*exp(-0.5*xp*xp);
    asum+=func[0]*dint;
    bsum+=dint;

    // middle points
    for (IndexType j=1; j < nmesh-1; j++) {
      xp=(mesh[i]-mesh[j])/sigma;
      dint=exp(-0.5*xp*xp);
      asum+=func[j]*dint;
      bsum+=dint;
    }

    // last point has factor of 1/2
    xp=(mesh[i]-mesh[nmesh-1])/sigma;
    dint=0.5*exp(-0.5*xp*xp);
    asum+=func[nmesh-1]*dint;
    bsum+=dint;

    // Dividing by the area of the Gaussian in order to correct for incomplete
    // convolution near the end points of the tabulated function.  At the 
    // end points, bsum will be 1/2 and approach unity at the middle of the
    // tabulated mesh.
    out[i]=asum/bsum;
  }
}

// -----------------------------------------------------------------------------

void convolveWithGaussianFixed(double* mesh, double* func, IndexType nmesh,
                               double sigma, double* out) {

  // With a fixed mesh, only N=nmesh different exponential terms are used in
  // all the integrations.  So, we pre-calculate these exponential terms 
  // instead of computing an exponential N^2 times.  Furthermore, we split the
  // inner loop into two pieces: i<j and i>=j, in order to avoid the call
  // to the absolute value function (abs).

  // if given a width of zero, just copy the input function to the output
  if (sigma == 0.) {
    for (IndexType i=0; i < nmesh; i++) out[i]=func[i];
    return;
  }

  // pre-calculate exponential terms
  double* expval=new double[nmesh];
  double dx=mesh[1]-mesh[0];       // mesh spacing (which is constant)
  for (IndexType i=0; i < nmesh; i++) {
    expval[i]=exp(-0.5*dx*dx*i*i/sigma/sigma);
  }

  // using Simpson's Rule
  for (IndexType i=0; i < nmesh; i++) {
    double asum=0.;     // main integration
    double bsum=0.;     // area of Gaussian applied

    // first point has factor of 1/2
    asum+=0.5*func[0]*expval[i-0];
    bsum+=0.5*expval[i-0];

    // middle points; with j < i
    for (IndexType j=1; j < i; j++) {
      asum+=func[j]*expval[i-j];
      bsum+=expval[i-j];
    }

    // middle points; with j >= i
    IndexType jstart=std::max(1,(int)i);    // needed for correct starting position when i=0
    for (IndexType j=jstart; j < nmesh-1; j++) {
      asum+=func[j]*expval[j-i];
      bsum+=expval[j-i];
    }

    // last point has factor of 1/2
    asum+=0.5*func[nmesh-1]*expval[nmesh-1-i];
    bsum+=0.5*expval[nmesh-1-i];

    // Dividing by the area of the Gaussian in order to correct for incomplete
    // convolution near the end points of the tabulated function.  At the 
    // end points, bsum will be 1/2 and approach unity at the middle of the
    // tabulated mesh.
    out[i]=asum/bsum;
  }

  // deallocate exponential value array
  delete [] expval;
  expval=0;

}

// -----------------------------------------------------------------------------

double calcR2(double* dat, double* fit, int nmesh) {

  double avgdat=0.;            // data average
  double sum_tot=0.;           // summation 1
  double sum_res=0.;           // summation 2
  double out=0.;               // output R^2

  // average of data
  for (int ip=0; ip < nmesh; ip++) avgdat+=dat[ip];
  avgdat=avgdat/(double)nmesh;

  for (int ip=0; ip < nmesh; ip++) {
    double tot=dat[ip]-avgdat;
    double res=dat[ip]-fit[ip];
    sum_tot+=tot*tot;
    sum_res+=res*res;
  }
  out=1.-sum_res/sum_tot;
  return out;

}

// -----------------------------------------------------------------------------

double calcChi2(double* dat, double* fit, int nmesh) {

  double sum_count=0.;
  for (int ip=0; ip < nmesh; ip++) {
    if (dat[ip] == 0.) continue;        // skip points with no counts
    double diff=dat[ip]-fit[ip];
    sum_count+=diff*diff/dat[ip];
  }
  return sum_count;
}

// -----------------------------------------------------------------------------

double calcReducedChi2(double* dat, double* fit, int nmesh, int nparam) {

  double sum_count=0.;
  long n=0;                             // number of points used
  for (int ip=0; ip < nmesh; ip++) {
    if (dat[ip] == 0.) continue;        // skip points with no counts
    n++;
    double diff=dat[ip]-fit[ip];
    sum_count+=diff*diff/dat[ip];
  }
  double dof=(double)(n-nparam-1);      // degrees of freedom
  return sum_count/dof;
}

// -----------------------------------------------------------------------------

double convertsigma2FWHM(double sigma){
  static double factor= 2. * std::sqrt(2. * log(2.));
  return sigma * factor;
}
  
// -----------------------------------------------------------------------------

double convertFWHM2sigma(double fwhm){
  static double factor= 0.5 / sqrt(2. * log(2.));
  return fwhm *factor;
}

// -----------------------------------------------------------------------------

double determinant2x2(double a11, double a12, 
                      double a21, double a22) {
  return a11*a22-a12*a21;
}

// -----------------------------------------------------------------------------

double determinant3x3(double a11, double a12, double a13, 
                      double a21, double a22, double a23,
                      double a31, double a32, double a33) {
  return +a11*determinant2x2(a22,a23,a32,a33)
         -a12*determinant2x2(a21,a23,a31,a33)
         +a13*determinant2x2(a21,a22,a31,a32);
}

// -----------------------------------------------------------------------------

void inverse3x3(double a11, double a12, double a13, 
                double a21, double a22, double a23,
                double a31, double a32, double a33,
                double& b11, double& b12, double& b13,
                double& b21, double& b22, double& b23,
                double& b31, double& b32, double& b33,
                bool& okay) {

  okay=true;
  double det=determinant3x3(a11,a12,a13,a21,a22,a23,a31,a32,a33);
  if (det == 0.) {
    okay=false;
    return;
  }

  b11=determinant2x2(a22,a23,a32,a33)/det;
  b12=determinant2x2(a13,a12,a33,a32)/det;
  b13=determinant2x2(a12,a13,a22,a23)/det;
  b21=determinant2x2(a23,a21,a33,a31)/det;
  b22=determinant2x2(a11,a13,a31,a33)/det;
  b23=determinant2x2(a13,a11,a23,a21)/det;
  b31=determinant2x2(a21,a22,a31,a32)/det;
  b32=determinant2x2(a12,a11,a32,a31)/det;
  b33=determinant2x2(a11,a12,a21,a22)/det;

}

// -----------------------------------------------------------------------------

void evaluatepoly(double x, double & y, int polyorder,
                  double * polycoeff) {
  
  y =0.;
  for (int ii=polyorder; ii >= 0; ii--) {
    y=polycoeff[ii]+y*x;
  }
  
  

}

// -----------------------------------------------------------------------------

void getQuadraticCoefficients(double x1, double y1, double x2, double y2,
                              double x3, double y3, double& a, double& b,
                              double& c, bool& okay) {

  // the set of simultaneous equations in matrix form is
  //   [ y1 ]     [ x1^2    x1    1 ]  [ a ]
  //   [ y2 ]  =  [ x2^2    x2    1 ]  [ b ]
  //   [ y3 ]     [ x3^2    x3    1 ]  [ c ]
  //
  // we then solve the following
  //
  //   [ a ]     [ x1^2    x1    1 ]^-1  [ y1 ]
  //   [ b ]  =  [ x2^2    x2    1 ]     [ y2 ]
  //   [ c ]     [ x3^2    x3    1 ]     [ y3 ]

  // set up 3x3 matrix
  double m11=x1*x1;
  double m12=x1;
  double m13=1.;
  double m21=x2*x2;
  double m22=x2;
  double m23=1.;
  double m31=x3*x3;
  double m32=x3;
  double m33=1.;

  // initialize variables for inverse matrix
  double q11=0.,q12=0.,q13=0.;
  double q21=0.,q22=0.,q23=0.;
  double q31=0.,q32=0.,q33=0.;

  // compute inverse of 3x3 matrix
  ahmath::inverse3x3(m11,m12,m13,m21,m22,m23,m31,m32,m33,
                     q11,q12,q13,q21,q22,q23,q31,q32,q33,okay);
  if (!okay) return;

  // multiply inverse matrix with y vector to get coefficients
  a=q11*y1+q12*y2+q13*y3;
  b=q21*y1+q22*y2+q23*y3;
  c=q31*y1+q32*y2+q33*y3;

}

// -----------------------------------------------------------------------------

}  // namespace ahmath

/* Revision Log
 $Log: ahmath.cxx,v $
 Revision 1.57  2016/07/25 22:09:02  rshill
 Correct the tests for intersecting segmants:  (1) Collinear line segments
 were excluded but now are OK so long as X domains do not overlap.  (2) X coordinate of
 intersecting line segments must be in domain of both segments, not just one,
 for the segments to intersect.

 Revision 1.56  2016/07/25 20:59:36  rshill
 Restore the changes to polygon processing that were temporarily reverted.

 Revision 1.55  2016/07/22 18:25:43  mwitthoe
 bug-fix for sawtooth interpolation functions to handle case where there is a step in the X coordinate in addition to the Y coordinate; temporarily revert changes to polygon routines

 Revision 1.54  2016/07/21 01:48:37  rshill
 Added new function to check complete polygon and removed this
 functionality from the routine that adds a point to a polygon.  Added const to several
 polygon function parameters.

 Revision 1.53  2016/05/13 18:35:50  mwitthoe
 ahmath library: fix bug in search_sawY2X where return index was incorrect in the step-down region when the rough position was inaccurate

 Revision 1.52  2016/03/31 20:41:09  mdutka
 revised function evaluate poly, now more efficient and unessary pass by references have been removed

 Revision 1.51  2016/03/18 15:00:56  asargent
 Replaced AH_WARN with AH_INFO

 Revision 1.50  2015/11/19 21:11:27  mwitthoe
 ahmath library: add function to compute quadratic polynomial coefficients from three points: getQuadraticCoefficients

 Revision 1.49  2015/11/16 22:08:20  mdutka
 checking in bug fix to evaluate poly

 Revision 1.48  2015/11/10 23:04:07  mdutka
 bug fix for evaluatepoly

 Revision 1.47  2015/10/21 17:55:27  mwitthoe
 ahmath: add function to compute Chi^2; correct calculation in reduced Chi^2 function

 Revision 1.46  2015/10/19 21:01:18  mwitthoe
 ahmath: correct expression in calcR2()

 Revision 1.45  2015/10/06 17:09:03  mdutka
 Changing arguments list of evaluatepoly to use dynamic array not vector

 Revision 1.44  2015/08/11 01:58:56  mwitthoe
 ahmath library: the bug-fix in the previous version was incomplete, you should not perform a search if both xrough and Y are in the step-down region

 Revision 1.43  2015/08/11 00:45:56  mwitthoe
 ahmath: fix bug in search_sawY2X where the search index was sometimes incorrect when the xrough position was in the step-down region of the sawtooth function; the code was using xrough to determine which tooth to search in instead of the Y-value (this is wrong because xrough is only approximate and therefore sometimes chooses the wrong place); this bug was not discovered before because in the library test day, xrough contained the exact value, not an estimate

 Revision 1.42  2015/08/03 19:32:42  mwitthoe
 ahmath library: add more precision to debug statements

 Revision 1.41  2015/07/28 16:16:42  mwitthoe
 ahmath library: 1) remove obsolete function: search_sawY2X_old, 2) a) added AH_DEBUG statements to functions used by the ahtime tool

 Revision 1.40  2015/07/21 20:23:28  asargent
 Updated error printout in addPointToPolygon

 Revision 1.39  2015/07/21 19:57:09  asargent
 removed unnecesary equality check in addPointToPolygon function

 Revision 1.38  2015/07/21 19:41:45  asargent
 added new segmentsIntersect function, updated addPointToPolygon function

 Revision 1.37  2015/07/20 18:55:03  asargent
 New polygon struct and function to check if point is inside a given polygon

 Revision 1.36  2015/04/09 15:24:31  asargent
 Use of 1D array in interpolate_point_npoint, allow any value of npoints in interpolate_point_npoint and search_npoint, remove use of abs() function in search_npoint

 Revision 1.35  2015/04/08 18:35:48  asargent
 Updated interpolate_point_npoint to allow starting index. added new function search_npoint to search for starting index. overloaded interpolate function to include npoint interpolation. Updated ahmath tests.

 Revision 1.34  2015/04/07 19:34:22  asargent
 Added new n-point aitken method of interpolation to ahmath library

 Revision 1.33  2015/04/01 20:30:16  mwitthoe
 ahmath library: remove obsolete functions - calcGaussianPoint, calcExponentialPoint, and calcLinearPoint

 Revision 1.32  2015/01/21 17:16:26  mwitthoe
 ahmath: add functions to compute inverse of a 3x3 matrix

 Revision 1.31  2015/01/09 22:38:27  mwitthoe
 ahmath library: fix typo in error message

 Revision 1.30  2015/01/08 18:44:13  mwitthoe
 ahmath: improve error message in sawtooth search function

 Revision 1.29  2014/11/05 21:36:03  mwitthoe
 ahmath: modify search_sawY2X() to allow for case when xrough is in a different sawtooth than the desired result; see issue 458

 Revision 1.28  2014/07/21 15:07:09  mwitthoe
 ahmath library: add functions to calculate R^2 and Chi^2

 Revision 1.27  2014/06/06 19:26:45  mwitthoe
 ahmath: added new function, convolveWithGaussianFixed(), which does the same thing as convolveWithGaussian, but assumes fixed mesh spacing -- the new function is about 20 times faster than the old; updated calcGaussianPoint(), convertsigma2FWHM(), and convertFWHM2sigma to use static constants to improve performance when called repeatedly

 Revision 1.26  2014/05/21 18:55:45  asargent
 Courtesy of Mike Dutka: Added math functions calcGaussianPoint(), calcExponentialPoint(), calcLinearPoint(), convertSigma2FWHM() and convertFWHM2Sigma().

 Revision 1.25  2013/11/20 22:58:18  mwitthoe
 ahmath library: replace all std::vectors with C arrays as convoluted consequence of issue #315; see that issue for more details

 Revision 1.24  2013/09/04 17:24:35  mwitthoe
 ahmath library: add a check in fitProfile for cases where there are zero or an even number of roots in the given parameter range meaning that the bisection method cannot proceed; add a test in testahmath that triggers this case

 Revision 1.23  2013/09/04 14:39:55  mwitthoe
 ahmath library: add warning to fitProfile if result of bisection method is one of the given end points indicating a possible failure (see issue 281)

 Revision 1.22  2013/07/19 15:00:31  mwitthoe
 ahmath: improve error messages in search_sawsearchY2X

 Revision 1.21  2013/07/01 14:31:53  mwitthoe
 add functions to ahmath to assist with profile fitting (in sxsdrift)

 Revision 1.20  2013/04/10 15:15:36  mwitthoe
 ahmath library: restore interpolation_type() function

 Revision 1.19  2013/04/08 21:18:14  mwitthoe
 remove old versions of interpolation functions from ahmath

 Revision 1.18  2013/04/04 21:26:26  mwitthoe
 restructure interpolation routines in ahmath; separate search and interpolation functionality into separate routines

 Revision 1.16  2012/10/16 20:29:37  mwitthoe
 ahmath: add single-value interpolation routine for double sawtooth functions; test code added

 Revision 1.15  2012/09/24 15:41:22  mwitthoe
 revamp interpolation routines in ahmath; now support interpolation of saw tooth functions

 Revision 1.14  2012/09/18 01:46:37  mwitthoe
 revamp interpolation to reduce code duplication; add sawtooth (X->Y) interpolation method

 Revision 1.13  2012/09/14 23:58:25  mwitthoe
 apply version standards to ahmath

 Revision 1.12  2012/09/07 17:41:56  mwitthoe
 remove obsolete ahtest include in ahmath source

 Revision 1.11  2012/08/29 20:59:18  mwitthoe
 moved ahmath tests from library to test code

 Revision 1.10  2012/08/28 18:27:53  mwitthoe
 add many-point interpolation functions to ahmath

 Revision 1.9  2012/08/24 19:07:25  mwitthoe
 clean up argument list for ahmath library

 Revision 1.8  2012/08/23 23:06:15  mwitthoe
 add extrapolation tests to ahtime unit test

 Revision 1.7  2012/08/23 16:50:33  mwitthoe
 standardized tests for ahmath

 Revision 1.6  2012/08/17 20:40:56  mwitthoe
 apply standards to ahmath

*/

