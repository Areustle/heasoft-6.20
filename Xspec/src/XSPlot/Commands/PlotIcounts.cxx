//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

// PlotIcounts
#include <XSPlot/Commands/PlotIcounts.h>


// Class PlotIcounts 

PlotIcounts::PlotIcounts()
  : PlotCommand("icounts", new CreateBinnedPlotGroups(false, true))
{
  isDataRequired(true);
  isActiveModelRequired(false);
  isDivisibleByArea(false);
  addCompLevel(2);
  isBackgroundApplicable(false);
  isPlotGroupDonor(true);

  PlotAttributes styles;
  styles.symbolStyle = PlotCommand::standardDataStyle();
  styles.lineStyle = PlotStyle::NONE;
  setStyleMap(PlotStyle::DATA, styles);
  styles.symbolStyle = PlotStyle::BLANK;
  styles.lineStyle = PlotCommand::standardModelStyle();
  styles.lineStep = true;
  styles.lineWidth = 2;
  setStyleMap(PlotStyle::MODEL, styles);
  styles.lineStep = false;
  styles.lineStyle = PlotStyle::DOTTED;
  setStyleMap(PlotStyle::SOURCES, styles);
}


PlotIcounts::~PlotIcounts()
{
}


void PlotIcounts::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   for (size_t iGr=0; iGr<plotGroups.size(); ++iGr)
   {
      PlotGroup* pg = plotGroups[iGr];
      size_t arraySize = pg->n;
      size_t nMods = pg->model.size();
      size_t nSources = pg->sources.size();
      std::vector<Real>& yData = pg->yData.data;
      pg->yData.errors.clear();
      std::vector<Real>& dX = pg->xAxis.errors[0];

      yData[0] *= 2*dX[0]; 
      for (size_t i=0; i<nMods; ++i)
         pg->model[i].data[0] *= 2*dX[0];
      if (settings.showAddComponent())
      {
         for (size_t i=0; i<nSources; ++i)
         {
            PlotVectorList::iterator itSl = pg->sources[i].begin();
            PlotVectorList::iterator itSlEnd = pg->sources[i].end();
            while (itSl != itSlEnd)
            {
               itSl->data[0] *= 2*dX[0];
               ++itSl;
            }
         }
      }

      for (size_t j=1; j<arraySize; ++j)
      {
         const Real dX2_j = 2*dX[j];
         yData[j] *= dX2_j;
         yData[j] += yData[j-1];
         for (size_t i=0; i<nMods; ++i)
         {
            pg->model[i].data[j] *= dX2_j;
            pg->model[i].data[j] += pg->model[i].data[j-1];
         }
         if (settings.showAddComponent())
         {
            for (size_t i=0; i<nSources; ++i)
            {
               PlotVectorList::iterator itSl = pg->sources[i].begin();
               PlotVectorList::iterator itSlEnd = pg->sources[i].end();
               while (itSl != itSlEnd)
               {
                  itSl->data[j] *= dX2_j;
                  itSl->data[j] += itSl->data[j-1];
                  ++itSl;
               }               
            }
         }
      } // end loop over arraySize

      // Now normalize
      const Real totalCounts = yData[arraySize-1];
      if (totalCounts != 0.0)
      {
         for (size_t j=0; j<arraySize; ++j)
         {
            yData[j] /= totalCounts;
            for (size_t i=0; i<nMods; ++i)
               pg->model[i].data[j] /= totalCounts;
            if (settings.showAddComponent())
            {
               for (size_t i=0; i<nSources; ++i)
               {
                  PlotVectorList::iterator itSl = pg->sources[i].begin();
                  PlotVectorList::iterator itSlEnd = pg->sources[i].end();
                  while (itSl != itSlEnd)
                  {
                     itSl->data[j] /= totalCounts;
                     ++itSl;
                  }
               }
            }
         }
      }      
   }
}

void PlotIcounts::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   Real xlow=0.0, xhigh=0.0;
   PlotGroupCreator::determineXRange(plotGroups,&xlow,&xhigh);

   PlotRange& rangeVals = rangeSettings();
   rangeVals.ranges.x1 = xlow;
   rangeVals.ranges.x2 = xhigh;
   if (settings.xOption() == CHANNELS)
      rangeVals.xScaleType = PlotStyle::LINEAR;
   else
      rangeVals.xScaleType = settings.xLog() ? PlotStyle::LOG :
                PlotStyle::LINEAR;
   rangeVals.yScaleType = settings.yLog() ? PlotStyle::LOG :
                PlotStyle::LINEAR;

   if (rangeVals.yScaleType == PlotStyle::LOG)
   {
      rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX;
   }
   else
   {
      // Force yMin range value to 0.
      rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX + 
                   PlotStyle::YMIN;
      rangeVals.ranges.y1 = 0.0;
   }   
}

void PlotIcounts::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   settings.makeXOptionDependentLabel(labels.x);
   labels.y = settings.lookupLabelName("y:icounts");
   labels.title = settings.lookupLabelName("t:icounts");
}

// Additional Declarations
