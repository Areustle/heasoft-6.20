//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/PlotGroupCreatorClasses.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

#include <XSContainer.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSUtil/Error/Error.h>

#include <algorithm>
#include <cfloat>

// PlotEfficiency
#include <XSPlot/Commands/PlotEfficiency.h>


// Class PlotEfficiency 

PlotEfficiency::PlotEfficiency()
  : PlotCommand("efficiency", new CreateEfficiencyPlotGroups())
{
   isDataRequired(true);
   isActiveModelRequired(false);
   isDivisibleByArea(false);
   addCompLevel(0);
   isBackgroundApplicable(false);

   PlotAttributes styles;
   styles.symbolStyle = PlotStyle::BLANK;
   styles.lineStyle = PlotStyle::DASHDOT;
   setStyleMap(PlotStyle::DATA, styles);
}


PlotEfficiency::~PlotEfficiency()
{
}


void PlotEfficiency::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   size_t iPlotGroup = 0;
   const size_t nSpec = XSContainer::datasets->numberOfSpectra();
   for (size_t iSpec=1; iSpec<=nSpec; ++iSpec)
   {
     const SpectralData* spectrum = XSContainer::datasets->lookup(iSpec);
     const std::vector<Response*>& detectors = spectrum->detector();
     const size_t nDets = detectors.size();
     for (size_t iDet=0; iDet<nDets; ++iDet)
     {
        const Response* resp = detectors[iDet];
        if (resp)
        {
           PlotGroup* gr = plotGroups[iPlotGroup];
           const RealArray& efficiency = resp->efficiency();
           if (gr->yData.data.size() != efficiency.size())
           {
              throw RedAlert("PlotGroup/Efficiency array size mismatch.");
           }
           const size_t nPts = gr->n;
           for (size_t k=0; k<nPts; ++k)
              gr->yData.data[k] = efficiency[k];
           gr->xAxis.errors.resize(0);
           gr->yData.errors.resize(0);
           ++iPlotGroup;
        }
     }
   }
}

void PlotEfficiency::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   Real xlow=0.0, xhigh=0.0;
   PlotGroupCreator::determineXRange(plotGroups,&xlow,&xhigh);

   PlotRange& rangeVals = rangeSettings();
   rangeVals.ranges.x1 = xlow;
   rangeVals.ranges.x2 = xhigh;

   Real yMin = DBL_MAX;
   Real yMax = -DBL_MAX;
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      const PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      for (size_t k=0; k<nPts; ++k)
      {
         Real y = pg->yData.data[k];
         yMin = (y < yMin) ? y : yMin;
         yMax = (y > yMax) ? y : yMax;
      }
   }
   // Not sure if this can ever happen:
   if (yMax < yMin)
   {
      throw YellowAlert("Unable to determine Y-axis range for plot efficiency.\n");
   }

   rangeVals.xScaleType = settings.xLog() ? PlotStyle::LOG :
                PlotStyle::LINEAR;
   if (settings.yLog())
   {
      rangeVals.yScaleType = PlotStyle::LOG;
      rangeVals.ranges.y1 = std::max(yMin, yMax*1.e-7);
      rangeVals.ranges.y2 = yMax*2.0;
   }
   else
   {
      rangeVals.yScaleType = PlotStyle::LINEAR;
      rangeVals.ranges.y1 = -.01*yMax;
      rangeVals.ranges.y2 = 1.05*yMax;
   }

   rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX +
                              PlotStyle::YMIN + PlotStyle::YMAX;
}

void PlotEfficiency::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   labels.title = settings.lookupLabelName("t:efficiency");
   settings.makeXOptionDependentLabel(labels.x);
   labels.y = settings.lookupLabelName("y:efficiency");
}

bool PlotEfficiency::requestNewXoption (const PlotSettings& settings, XaxisMode& newOption) const
{
   bool doChange = false;
   if (settings.xOption() == CHANNELS)
   {
      newOption = ENERGY;
      doChange = true;
   }
   return doChange;
}

// Additional Declarations
