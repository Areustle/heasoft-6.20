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
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Error/Error.h>
#include <cfloat>

// PlotSensitivity
#include <XSPlot/Commands/PlotSensitivity.h>


// Class PlotSensitivity 

PlotSensitivity::PlotSensitivity()
  : PlotCommand("sensitivity", new CreateEfficiencyPlotGroups())
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


PlotSensitivity::~PlotSensitivity()
{
}


void PlotSensitivity::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   using namespace XSContainer;

   size_t iPlotGroup = 0;
   bool anyModels = false;
   Real yMax = 0.0;
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
           const RealArray& thisSensitivity = sensitivity[l];
           if (specMods[iDet])
           {
              anyModels = true;
              RealArray fluxSq = specMods[iDet]->modelFlux(iSpec);
              fluxSq *= fluxSq;
              // fluxSq is now in phot/cm^4/s^2
              std::vector<Real>& dX = gr->xAxis.errors[0];
              std::vector<Real>& X =  gr->xAxis.data;
              for (size_t k=0; k<gr->n; ++k)
              {
                 // the '4' comes from the fact that dX is half of the
                 // step width.
                 Real y = thisSensitivity[k]*fluxSq[k]*X[k]/
                                   (4.*dX[k]*dX[k]);
                 gr->yData.data[k] = y;
                 if (y > yMax)
                    yMax = y;    
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

   // And finally normalize everything by yMax.
   if (yMax != 0.0)
   {
      for (size_t i=0; i<plotGroups.size(); ++i)
      {
         PlotGroup* pg = plotGroups[i];
         const size_t nPts = pg->n;
         for (size_t k=0; k<nPts; ++k)
            pg->yData.data[k] /= yMax;
      }
   }
}

void PlotSensitivity::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   PlotRange& rangeVals = rangeSettings();
   Real yMin = DBL_MAX;
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      const PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      for (size_t k=0; k<nPts; ++k)
      {
         Real y = pg->yData.data[k];
         if (y < yMin)
            yMin = y;
      }
   }
   if (yMin > 1.0)
   {
      // Can this even happen?
      throw YellowAlert("Unable to determine Y-axis range for plot sensitivity.\n");
   }

   rangeVals.xScaleType = settings.xLog() ? PlotStyle::LOG :
                PlotStyle::LINEAR;
   if (settings.yLog())
   {
      rangeVals.yScaleType = PlotStyle::LOG;
      rangeVals.ranges.y1 = std::max(yMin, 1.e-7);
      rangeVals.ranges.y2 = 1.1;
   }
   else
   {
      rangeVals.yScaleType = PlotStyle::LINEAR;
      rangeVals.ranges.y1 = -.01;
      rangeVals.ranges.y2 = 1.05;
   }

   rangeVals.useRangeFlags = PlotStyle::YMIN + PlotStyle::YMAX;
}

void PlotSensitivity::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   // Can only have ENERGY for x-axis.
   labels.title = settings.lookupLabelName("t:sensitivity");
   string xLabel = settings.lookupLabelName("x:energy");
   settings.insertUnitLabel(xLabel,false);
   labels.x = xLabel;
   labels.y = settings.lookupLabelName("y:sensitivity");
}

bool PlotSensitivity::requestNewXoption (const PlotSettings& settings, XaxisMode& newOption) const
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
