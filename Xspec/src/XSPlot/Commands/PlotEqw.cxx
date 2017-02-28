//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>
#include <XSPlot/Plot/PlotGroupCreatorClasses.h>

// PlotEqw
#include <XSPlot/Commands/PlotEqw.h>


// Class PlotEqw 

PlotEqw::PlotEqw()
  : PlotCommand("eqw",new CreateEqwPlotGroups())
{
   isDataRequired(true);
   isActiveModelRequired(true);
   isDivisibleByArea(false);
   addCompLevel(0);
   isBackgroundApplicable(false);

   PlotAttributes styles;
   styles.symbolStyle = PlotStyle::BLANK;
   styles.lineStyle = PlotStyle::SOLID;
   styles.lineStep = true;
   setStyleMap(PlotStyle::DATA, styles);
}


PlotEqw::~PlotEqw()
{
}


void PlotEqw::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   Real xlow=0.0, xhigh=0.0;
   PlotGroupCreator::determineXRange(plotGroups,&xlow,&xhigh);
   xlow -= xlow/8.0;
   xhigh += xhigh/8.0;

   PlotRange& rangeVals = rangeSettings();
   rangeVals.ranges.x1 = xlow;
   rangeVals.ranges.x2 = xhigh;
   // Always plot X scale as linear
   rangeVals.xScaleType = PlotStyle::LINEAR;
   rangeVals.yScaleType = settings.yLog() ? PlotStyle::LOG :
                PlotStyle::LINEAR;
   rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX;

}

void PlotEqw::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   labels.x = settings.lookupLabelName("x:eqw");
   labels.y = settings.lookupLabelName("y:eqw");
}

// Additional Declarations
