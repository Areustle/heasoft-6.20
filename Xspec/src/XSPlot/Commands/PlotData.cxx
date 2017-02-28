//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotTypes.h>

// PlotData
#include <XSPlot/Commands/PlotData.h>


// Class PlotData 

PlotData::PlotData (const string& name, bool isCounts)
  : PlotCommand(name, new CreateBinnedPlotGroups(false,isCounts)),
   m_isCounts(isCounts)
{
  isDataRequired(true);
  isActiveModelRequired(false);
  isDivisibleByArea(true);
  addCompLevel(2);
  isBackgroundApplicable(true);
  isLineIDApplicable(true);
  isPlotGroupDonor(true);

  PlotAttributes styles;
  styles.symbolStyle = PlotCommand::standardDataStyle();
  styles.lineStyle = PlotStyle::NONE;
  setStyleMap(PlotStyle::DATA, styles);
  styles.symbolStyle = PlotStyle::ASTERISK;
  styles.lineStyle = PlotStyle::NONE; 
  setStyleMap(PlotStyle::BACKGROUND, styles); 
  styles.symbolStyle = PlotStyle::BLANK;
  styles.lineStyle = PlotCommand::standardModelStyle();
  styles.lineStep = true;
  styles.lineWidth = 2;
  setStyleMap(PlotStyle::MODEL, styles);
  styles.lineStep = false;
  styles.lineStyle = PlotStyle::DOTTED;
  setStyleMap(PlotStyle::SOURCES, styles);
}


PlotData::~PlotData()
{
}


void PlotData::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   Real xlow=0.0, xhigh=0.0;
   PlotGroupCreator::determineXRange(plotGroups,&xlow,&xhigh);

   PlotRange& rangeVals = rangeSettings();
   rangeVals.ranges.x1 = xlow;
   rangeVals.ranges.x2 = xhigh;

   if (cmdName() == "ldata" || cmdName() == "lcounts")
   {
      // Y-scale will be log regardless of setplot setting.
      rangeVals.yScaleType = PlotStyle::LOG;
   }
   else
      rangeVals.yScaleType = settings.yLog() ? PlotStyle::LOG : 
                PlotStyle::LINEAR;
   if (settings.xOption() == CHANNELS)
      rangeVals.xScaleType = PlotStyle::LINEAR;
   else 
      rangeVals.xScaleType = settings.xLog() ? PlotStyle::LOG :
                PlotStyle::LINEAR;


   Real ylow=0.0, yhigh=0.0;
   PlotGroupCreator::determineYRange(plotGroups,&ylow,&yhigh);

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

void PlotData::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   labels.title = activeModelStatus() ? settings.lookupLabelName("t:data:foldmodel") :
                        settings.lookupLabelName("t:data");

   // Can't simply use cmdName() for this since "lcounts" and "ldata" won't work.                    
   string cmdStr = m_isCounts ? string("counts") : string("data");
   string xKey(settings.makeXOptionDependentLabel(labels.x));
   // Now y label:
   string yLabel;
   string fullKey(string("y:") + cmdStr + xKey.substr(1));
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

// Additional Declarations
