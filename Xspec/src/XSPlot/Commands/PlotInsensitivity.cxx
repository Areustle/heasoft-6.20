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
#include <XSModel/GlobalContainer/ModelContainer.h>

// PlotInsensitivity
#include <XSPlot/Commands/PlotInsensitivity.h>


// Class PlotInsensitivity 

PlotInsensitivity::PlotInsensitivity()
  : PlotCommand("insensitivity", new CreateEfficiencyPlotGroups())
{
   isDataRequired(true);
   isActiveModelRequired(true);
   isDivisibleByArea(false);
   addCompLevel(0);
   isBackgroundApplicable(false);

   PlotAttributes styles;
   styles.symbolStyle = PlotStyle::BLANK;
   styles.lineStyle = PlotStyle::DASHDOT;
   setStyleMap(PlotStyle::DATA, styles);
}


PlotInsensitivity::~PlotInsensitivity()
{
}


void PlotInsensitivity::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
  using namespace XSContainer;
  static const Real PHOT_TO_MJY (662.22);

   size_t iPlotGroup = 0;
   bool anyModels = false;
   const size_t nSpec = datasets->numberOfSpectra();
   for (size_t iSpec=1; iSpec<=nSpec; ++iSpec)
   {
     // A separate plot group must exist for every response of every spectrum,
     // though not all of these necessarily have an active model associated
     // with them. If they don't, they will be "turned off".

     // Since this is using CreateEfficiencyPlotGroups for its PlotGroup
     // generator, it will arrive here with yData fully sized.  But if
     // we clear it, PlotDirector will recognize it as turned off.
     SpectralData* spectrum = datasets->lookup(iSpec);
     std::vector<const Model*> specMods = models->getModsForSpec(spectrum);
     const std::vector<Response*>& detectors = spectrum->detector();
     const size_t N = detectors.size();
     std::vector<RealArray> sensitivity(spectrum->sensitivity());
     for (size_t iDet=0; iDet<N; ++iDet)
     {
        size_t l=0;
        if (detectors[iDet])
        {
           PlotGroup* gr = plotGroups[iPlotGroup];
           // sensitivity is in units of area^2/variance = cm^4-s^2/phot
           RealArray& thisSensitivity = sensitivity[l];
           const RealArray& energy = 
                const_cast<const Response*>(detectors[iDet])->energies();
           if (specMods[iDet])
           {
              anyModels = true;
              for (size_t k=0; k<gr->n; ++k)
              {
                 gr->yData.data[k] = 
                     PHOT_TO_MJY*sqrt(energy[k]*energy[k+1]/thisSensitivity[k]);
              }
           }
           else
           {
              gr->yData.data.clear(); 
              gr->n = 0;      
           }
           gr->yData.errors.clear();


           ++iPlotGroup, ++l;
        }
     } 
   }
   if (!anyModels)
   {
      throw YellowAlert("\nCannot plot sensitivity, no models found.\n");
   }
}

void PlotInsensitivity::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   static const Real BIG (1.E10);
   PlotRange& rangeVals = rangeSettings();
   Real yMin = DBL_MAX;
   Real yMax = -DBL_MAX;
   // First find the largest y value that's less than BIG.
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      const PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      for (size_t k=0; k<nPts; ++k)
      {
         Real y = pg->yData.data[k];
         if (y < BIG)
         {
            yMin = (y < yMin) ? y : yMin;
            yMax = (y > yMax) ? y : yMax;
         }
      }
   }
   if (yMax < yMin)
   {
      throw YellowAlert("Unable to determine Y-axis range for plot sensitivity.\n");
   }

   // Now set all y values that are >= BIG to yMax.
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      for (size_t k=0; k<nPts; ++k)
      {
         if (pg->yData.data[k] >= BIG)
         {
            pg->yData.data[k] = yMax;
         }
      }
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

   rangeVals.useRangeFlags = PlotStyle::YMIN + PlotStyle::YMAX;
}

void PlotInsensitivity::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   // Can only have ENERGY for x-axis.
   labels.title = settings.lookupLabelName("t:insensitivity");
   string xLabel = settings.lookupLabelName("x:energy");
   settings.insertUnitLabel(xLabel, false);
   labels.x = xLabel;
   labels.y = settings.lookupLabelName("y:insensitivity");
}

bool PlotInsensitivity::requestNewXoption (const PlotSettings& settings, XaxisMode& newOption) const
{
   bool doChange = false;
   if (settings.xOption() != ENERGY)
   {
      newOption = ENERGY;
      doChange = true;
   }
   return doChange;
}

// Additional Declarations
