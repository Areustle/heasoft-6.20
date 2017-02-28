//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <GlobalContainer/DataContainer.h>

// PlotGroup
#include <XSPlot/Plot/PlotGroup.h>
// PlotStyle
#include <XSPlot/Plot/PlotStyle.h>
#include <XSContainer.h>


// Class PlotGroup 

PlotGroup::PlotGroup (int arraySize, size_t nMods, bool useYdata)
  : objectIndex(0),
    channelPlotSpacing(0),
    n(arraySize),
    is2Dcontour(false),
    critSignificance(0),
    critNumChans(1),
    errorType(PlotSettings::STD),
    single(false),
    lastInputChannel(0),
    bin(0),
    bundledPlotVectors(),
    plotVectorBoundaries(3),
    saveData(),
    background(),
    sources(nMods),
    xAxis(arraySize),
    yData(useYdata ? arraySize : 0),
    model(nMods + static_cast<size_t>(nMods > 1), PlotVector(arraySize)),
    auxData()
{
  xAxis.styles.symbolStyle = PlotStyle::DOT;
  xAxis.errors.push_back(std::vector<Real>(arraySize,0));
  if (useYdata)
  {
     yData.errors.push_back(std::vector<Real>(arraySize,0));     
  }	
}

PlotGroup::PlotGroup (const std::vector<Real>& xVals, const std::vector<Real>& yVals)
  : objectIndex(0),
    channelPlotSpacing(0),
    n(xVals.size()*yVals.size()),
    is2Dcontour(true),
    critSignificance(0),
    critNumChans(0),
    errorType(PlotSettings::STD),
    single(false),
    lastInputChannel(0),
    bin(0),
    bundledPlotVectors(),
    plotVectorBoundaries(0),
    saveData(),
    background(),
    sources(),
    xAxis(xVals.size()),
    yData(yVals.size()),
    model(1),
    auxData()
{
   xAxis.data = xVals;
   yData.data = yVals;
   model[0].data.resize(n);
}


void PlotGroup::copyAttributes (const PlotGroup& right)
{
   channelPlotSpacing = right.channelPlotSpacing;
   critSignificance = right.critSignificance;
   critNumChans = right.critNumChans;
   errorType = right.errorType;
   single = right.single;
   lastInputChannel = right.lastInputChannel;
   bin = right.bin;
}

bool operator< (const PlotGroup& left, const PlotGroup& right)
{
        return left.objectIndex < right.objectIndex;       
}

void PlotGroup::getLimits(Real& low, Real& high)
{
  if ( n == 0 ) {
    low = 0.0;
    high = 0.0;
    return;
  }

  low  = 1.0e99;
  high = -1.0e99;
  Real ylow, yhigh;

  // first get limits of basic y data
  yData.getLimits(ylow, yhigh);
  if (!(ylow == yhigh && ylow == 0.0)) {
    low = std::min(low, ylow);
    high = std::max(high, yhigh);
  }

  // now loop over models
  for (size_t i=0; i<model.size(); i++) {
    model[i].getLimits(ylow,yhigh);
    if (!(ylow == yhigh && ylow == 0.0)) {
      low = std::min(low, ylow);
      high = std::max(high, yhigh);
    }
  }

  // and over any auxiliary data

  for (size_t i=0; i<auxData.size(); i++) {
    auxData[i].getLimits(ylow,yhigh);
    if (!(ylow == yhigh && ylow == 0.0)) {
      low = std::min(low, ylow);
      high = std::max(high, yhigh);
    }
  }

  // and over background

  background.getLimits(ylow, yhigh);
  if (!(ylow == yhigh && ylow == 0.0)) {
    low = std::min(low, ylow);
    high = std::max(high, yhigh);
  }

  return;

}
