//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

// PlotBackground
#include <XSPlot/Commands/PlotBackground.h>


// Class PlotBackground 

PlotBackground::PlotBackground()
  : PlotCommand("background", new CreateBinnedPlotGroups(false, false))
{
  isDataRequired(true);
  isActiveModelRequired(false);
  isDivisibleByArea(true);
  addCompLevel(2);
  isBackgroundApplicable(false);
  isLineIDApplicable(true);

  // It's a bit confusing, but for this command the values in the background
  // PlotVector will be swapped with the yData PlotVector (in the manipulate 
  // function).  The background PlotVector is intended for plotting only
  // when it is supplementary to separately existing yData.
  PlotAttributes styles;
  styles.symbolStyle = PlotStyle::ASTERISK;
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


PlotBackground::~PlotBackground()
{
}


void PlotBackground::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   Real xlow=0.0, xhigh=0.0;
   PlotGroupCreator::determineXRange(plotGroups,&xlow,&xhigh);

   PlotRange& rangeVals = rangeSettings();
   rangeVals.ranges.x1 = xlow;
   rangeVals.ranges.x2 = xhigh;
   rangeVals.yScaleType = settings.yLog() ? PlotStyle::LOG : 
             PlotStyle::LINEAR;
   if (settings.xOption() == CHANNELS)
      rangeVals.xScaleType = PlotStyle::LINEAR;
   else 
      rangeVals.xScaleType = settings.xLog() ? PlotStyle::LOG :
                PlotStyle::LINEAR;

   if (rangeVals.yScaleType == PlotStyle::LOG)
   {
      // Let plotting package determine y ranges
      rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX;
   }
   else
   {
      // Plotting package determines upper y
      rangeVals.ranges.y1 = 0.0;
      rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX
                        + PlotStyle::YMIN;
   }   
}

void PlotBackground::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   labels.title = activeModelStatus() ? settings.lookupLabelName("t:data:foldmodel") :
                        settings.lookupLabelName("t:data");

   string xKey(settings.makeXOptionDependentLabel(labels.x));
   // Now y label:
   string yLabel;
   string fullKey(string("y:data") + xKey.substr(1));
   yLabel = settings.lookupLabelName(fullKey);
   settings.insertUnitLabel(yLabel, true);   
   labels.y = yLabel;

   if (settings.divideByArea())
   {
      string inverseArea = settings.lookupLabelName("y:suffix:area");
      yLabel += " ";
      yLabel += inverseArea;
      labels.y = yLabel;       
   }
}

void PlotBackground::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      std::vector<Real>& back = plotGroups[i]->background.data;
      std::vector<Real>& backErr = plotGroups[i]->background.errors[0];
      std::vector<Real>& data = plotGroups[i]->yData.data;
      std::vector<Real>& dataErr = plotGroups[i]->yData.errors[0];
      data.swap(back);
      dataErr.swap(backErr);
   }
}

// Additional Declarations
