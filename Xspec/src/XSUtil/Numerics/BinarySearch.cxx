
#include <BinarySearch.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSstreams.h>

namespace Numerics {

  // Binary search on a RealArray to return the index of the element immediately
  // before the input value. Assumes that the RealArray is in ascending order

  int BinarySearch(const RealArray& x, const Real& y)
  {
    RealArray yArr(1);
    IntegerArray elementArray(1);
    yArr[0] = y;
    elementArray = BinarySearch(x, yArr);
    return elementArray[0];
  }

  // Binary search on a RealArray to return the indices of the element immediately
  // before the input values assuming the input values are in ascending order.

  IntegerArray BinarySearch(const RealArray& x, const RealArray& y)
  {

    size_t nX = x.size();
    size_t nY = y.size();
    IntegerArray elementArray(nY);
    for (size_t i=0; i<nY; i++) elementArray[i] = -1;

    if ( nX == 1 ) return elementArray;

    int low = 0;
    Real xmin = x[0];
    Real xmax = x[nX-1];

    for (size_t i=0; i<nY; i++) {

      Real yval = y[i];

      if ( yval > xmax ) break;

      // catch the special case of the y value being at the minimum of the range
      // we have to do this otherwise elementArray ends up incorrectly as -1.

      if ( yval == xmin ) {
	elementArray[i] = 0;
	break;
      }

      if ( yval > xmin ) {
	
	int high = nX-1;

	int bisearch(low);
	while ( (high-low) > 1 ) {

	  bisearch = (low + high) / 2;
	  if ( yval > x[bisearch-1] ) {
	    low = bisearch;
	  } else {
	    high = bisearch;
	  }

	}

	if ( yval > x[bisearch] ) {
	  bisearch = high;
	} else {
	  bisearch = low;
	}

	elementArray[i] = bisearch-1;

      }

    }

    return elementArray;

  }

}
