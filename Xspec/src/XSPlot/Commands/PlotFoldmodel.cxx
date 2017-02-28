//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

// PlotFoldmodel
#include <XSPlot/Commands/PlotFoldmodel.h>


// Class PlotFoldmodel 

PlotFoldmodel::PlotFoldmodel()
  : PlotCommand("foldmodel", new CreateBinnedPlotGroups(false, false))
{
  isDataRequired(true);
  isActiveModelRequired(true);
  isDivisibleByArea(false);
  addCompLevel(2);
  isBackgroundApplicable(false);
  isLineIDApplicable(true);

  PlotAttributes styles;
  styles.symbolStyle = PlotStyle::BLANK;
  styles.lineStyle = PlotCommand::standardModelStyle();
  styles.lineStep = true;
  styles.lineWidth = 2;
  setStyleMap(PlotStyle::MODEL, styles);
  styles.lineStep = false;
  styles.lineStyle = PlotStyle::DOTTED;
  setStyleMap(PlotStyle::SOURCES, styles);
}


PlotFoldmodel::~PlotFoldmodel()
{
}


void PlotFoldmodel::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
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

   Real ylow=0.0, yhigh=0.0;
   PlotGroupCreator::determineYRange(plotGroups,&ylow,&yhigh);

   rangeVals.yScaleType = settings.yLog() ? PlotStyle::LOG : 
             PlotStyle::LINEAR;

   if (rangeVals.yScaleType == PlotStyle::LOG) {
     ylow = std::max(ylow, yhigh*1.0e-4);
     rangeVals.ranges.y1 = ylow;
   } else {
      rangeVals.ranges.y1 = 0.0;
   }
   rangeVals.ranges.y2 = yhigh;

   rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX
                        + PlotStyle::YMIN + PlotStyle::YMAX;
}

void PlotFoldmodel::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   string xKey(settings.makeXOptionDependentLabel(labels.x));
   string yKey(string("y:data") + xKey.substr(1));
   string yLabel = settings.lookupLabelName(yKey);
   settings.insertUnitLabel(yLabel, true); 
   labels.y = yLabel;
   labels.title = settings.lookupLabelName("t:foldmodel");  
}

// Additional Declarations
