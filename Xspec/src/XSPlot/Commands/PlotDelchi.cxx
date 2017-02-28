//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSContainer.h>

#include <XSstreams.h>
#include <sstream>

// PlotDelchi
#include <XSPlot/Commands/PlotDelchi.h>


// Class PlotDelchi 

PlotDelchi::PlotDelchi()
  : PlotCommand("delchi", new CreateBinnedPlotGroups(false, false))
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


PlotDelchi::~PlotDelchi()
{
}


void PlotDelchi::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   const StatMethod* stat = XSContainer::fit->statManager()->usingSingleStat();
   const Real NO_VAL = settings.badDataValue();
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      PlotGroup* pg = plotGroups[i];
      size_t arraySize = pg->n;
      std::vector<Real>& yData  = pg->yData.data;
      std::vector<Real>& yError = pg->yData.errors[0];
      std::vector<Real>& dX = pg->xAxis.errors[0];
      const std::vector<Real>& model  = pg->model[0].data;
      for (size_t j=0; j<arraySize; ++j)
      {
         Real obs = yData[j];
         Real mod = model[j];
         Real err = yError[j];

	 // include the energy bin width to convert to counts
	 Real areaTime = pg->saveData[j]*2*dX[j];

	 yData[j]  = stat->plotDeltaChi(obs,mod,err,areaTime,NO_VAL);
         yError[j] = 1.;
      }
   }
}

void PlotDelchi::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
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

   // Draw a horizontal line at y = 0.0
   rangeVals.horizontalLine.first = true;
   rangeVals.horizontalLine.second = 0.0;
}

void PlotDelchi::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
  settings.makeXOptionDependentLabel(labels.x);
  labels.y = settings.lookupLabelName("y:delchi");
  labels.title = settings.lookupLabelName("t:delchi");
}

// Additional Declarations
