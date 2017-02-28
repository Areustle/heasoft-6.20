//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>
#include <XSPlot/Plot/PlotGroupCreatorClasses.h>

#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSUtil/Utils/XSstream.h>


// PlotGoodness
#include <XSPlot/Commands/PlotGoodness.h>


// Class PlotGoodness 

PlotGoodness::PlotGoodness()
  : PlotCommand("goodness",new CreateGoodnessPlotGroups())
{
   isDataRequired(false);
   isActiveModelRequired(false);
   isDivisibleByArea(false);
   addCompLevel(0);
   isBackgroundApplicable(false);

   PlotAttributes styles;
   styles.symbolStyle = PlotStyle::BLANK;
   styles.lineStyle = PlotStyle::SOLID;
   styles.lineStep = true;
   setStyleMap(PlotStyle::DATA, styles);
}


PlotGoodness::~PlotGoodness()
{
}


void PlotGoodness::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   Real xlow=0.0, xhigh=0.0;
   PlotGroupCreator::determineXRange(plotGroups,&xlow,&xhigh);

   Real actualStat = XSContainer::fit->statManager()->totalTestStatistic();
   if ( actualStat < xlow ) xlow = actualStat*(7.0/8.0);
   if ( actualStat > xhigh ) xhigh = actualStat*(9.0/8.0);

   PlotRange& rangeVals = rangeSettings();
   rangeVals.ranges.x1 = xlow;
   rangeVals.ranges.x2 = xhigh;
   // Always plot X,y scales as linear
   rangeVals.xScaleType = PlotStyle::LINEAR;
   rangeVals.yScaleType = PlotStyle::LINEAR;
   rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX;

   // set up vertical line indicating statistic for actual data.

   rangeVals.verticalLine.first = true;
   rangeVals.verticalLine.second = actualStat;
}

void PlotGoodness::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   const StatMethod* singleStat = XSContainer::fit->statManager()->usingSingleTestStat();
   const string scriptName = singleStat ? singleStat->scriptName() :
                                string("Total Statistic");
   labels.title = "Histogram from goodness command.";
   labels.x = scriptName;
   labels.y = settings.lookupLabelName("y:goodness");
}

// Additional Declarations
