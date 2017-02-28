//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

// PlotRatio
#include <XSPlot/Commands/PlotRatio.h>


// Class PlotRatio 

PlotRatio::PlotRatio()
  : PlotCommand("ratio", new CreateBinnedPlotGroups(false,false))
{
   isDataRequired(true);
   isActiveModelRequired(true);
   isDivisibleByArea(false);
   addCompLevel(0);
   isBackgroundApplicable(false);

   PlotAttributes styles;
   styles.symbolStyle = PlotCommand::standardDataStyle();
   styles.lineStyle = PlotStyle::NONE;
   setStyleMap(PlotStyle::DATA, styles);
}


PlotRatio::~PlotRatio()
{
}


void PlotRatio::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   const Real NO_VAL = settings.badDataValue();
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      std::vector<Real>& data  = pg->yData.data;
      const std::vector<Real>& model  = pg->model[0].data;
      for (size_t j=0; j<nPts; ++j)
      {
         if (model[j] == 0.0)
         {
            data[j] = NO_VAL;
         }
         else
         {
            data[j] /= model[j];
         }
      }
      // Can't assume yData errors exist.  They won't if this is 
      // following an icounts plot.
      if (pg->yData.errors.size())
      {
         std::vector<Real>& error = pg->yData.errors[0];
         for (size_t j=0; j<nPts; ++j)
         {
            if (model[j] == 0.0)
            {
               error[j] = NO_VAL;
            }
            else
            {
               error[j] /= model[j];
            }
         }
      }
   }
}

void PlotRatio::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
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

   rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX;

   // Draw a horizontal line at y = 1.0
   rangeVals.horizontalLine.first = true;
   rangeVals.horizontalLine.second = 1.0;
}

void PlotRatio::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   settings.makeXOptionDependentLabel(labels.x);
   labels.y = settings.lookupLabelName("y:ratio");
   labels.title = settings.lookupLabelName("t:ratio");
}

std::vector<PlotGroup*> PlotRatio::makePlotGroups (const PlotSettings& settings, const std::vector<const PlotGroup*>& donatedPlotGroups)
{
   // Plot ratio should show different behavior if following an icounts 
   // or data plot.  For example, following icounts it will show a ratio 
   // of the cumulative totals.  To do this, it must make use of the
   // previous plot's donated plot groups.
   std::vector<PlotGroup*> newGroups;
   if (donatedPlotGroups.size())
   {
      for (size_t i=0; i<donatedPlotGroups.size(); ++i)
      {
         const PlotGroup* origGroup = donatedPlotGroups[i];
         PlotGroup* newGroup = new PlotGroup(origGroup->n, 1);

         // Need only the x, y, and model[0] vectors from the original group.
         newGroup->xAxis = origGroup->xAxis;
         newGroup->yData = origGroup->yData;
         newGroup->model[0] = origGroup->model[0];
         newGroup->copyAttributes(*origGroup);
         newGroups.push_back(newGroup);
      }
   }
   else
      newGroups = plotGroupStrategy()->createPlotGroups(settings);
   return newGroups;
}

// Additional Declarations
