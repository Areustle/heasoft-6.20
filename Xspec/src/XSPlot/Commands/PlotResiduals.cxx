//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

// PlotResiduals
#include <XSPlot/Commands/PlotResiduals.h>


// Class PlotResiduals 

PlotResiduals::PlotResiduals()
  : PlotCommand("residuals", new CreateBinnedPlotGroups(false,false))
{
   isDataRequired(true);
   isActiveModelRequired(true);
   isDivisibleByArea(true);
   addCompLevel(0);
   isBackgroundApplicable(false);

   PlotAttributes styles;
   styles.symbolStyle = PlotCommand::standardDataStyle();
   styles.lineStyle = PlotStyle::NONE;
   setStyleMap(PlotStyle::DATA, styles);
}


PlotResiduals::~PlotResiduals()
{
}


void PlotResiduals::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      std::vector<Real>& data  = pg->yData.data;
      const std::vector<Real>& model  = pg->model[0].data;
      for (size_t j=0; j<nPts; ++j)
      {
         data[j] -= model[j];
      }

   }
}

void PlotResiduals::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
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

   rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX;

   rangeVals.horizontalLine.first = true;
   rangeVals.horizontalLine.second = 0.0;
}

void PlotResiduals::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   string xKey(settings.makeXOptionDependentLabel(labels.x));
   labels.title = settings.lookupLabelName("t:residuals");

   string fullKey(string("y:data") + xKey.substr(1));
   string yLabel = settings.lookupLabelName(fullKey);
   settings.insertUnitLabel(yLabel,true);
   labels.y = yLabel;

   if (settings.divideByArea())
   {
      string inverseArea = settings.lookupLabelName("y:suffix:area");
      yLabel += " ";
      yLabel += inverseArea;
      labels.y = yLabel;       
   }
}

std::vector<PlotGroup*> PlotResiduals::makePlotGroups (const PlotSettings& settings, const std::vector<const PlotGroup*>& donatedPlotGroups)
{
   // Plot residuals should show different behavior if following an icounts 
   // or data plot.  For example, following icounts it will show residuals 
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
