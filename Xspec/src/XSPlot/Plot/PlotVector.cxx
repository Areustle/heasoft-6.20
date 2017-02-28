//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// PlotVector
#include <XSPlot/Plot/PlotVector.h>

// Class PlotVector 

PlotVector::PlotVector()
	: data(), 
	  errors(),
          styles()
{
}

PlotVector::PlotVector (size_t arraySize)
	: data(arraySize,0), 
	  errors(),
          styles()
{
}

void PlotVector::getLimits(Real& low, Real& high)
{
  low = 1.0e99;
  high = -1.0e99;

  const size_t npts = data.size();

  if ( npts == 0 ) {
    low = 0.0;
    high = 0.0;
    return;
  }

  bool oneSideErrors = false;
  bool twoSideErrors = false;

  if ( errors.size() == 2 && errors[0].size() == npts ) twoSideErrors = true;
  if ( errors.size() == 1 && errors[0].size() == npts ) oneSideErrors = true;

  for (size_t i=0; i<npts; i++) {

    Real ylow = data[i];
    Real yhigh = ylow;
    if ( oneSideErrors ) {
      ylow -= errors[0][i];
      yhigh += errors[0][i];
    }
    if ( twoSideErrors ) {
      ylow -= errors[0][i];
      yhigh += errors[1][i];
    }

    low = std::min(low,ylow);
    high = std::max(high,yhigh);

  }
  return;

}
