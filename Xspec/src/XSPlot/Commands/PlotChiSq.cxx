//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSModel/Data/SpectralData.h>
#include <XSUtil/Utils/XSstream.h>

// PlotChiSq
#include <XSPlot/Commands/PlotChiSq.h>


// Class PlotChiSq 

PlotChiSq::PlotChiSq()
  : PlotCommand("chisq", new CreateBinnedPlotGroups(false, false))
{
   isDataRequired(true);
   isActiveModelRequired(true);
   isDivisibleByArea(false);
   addCompLevel(0);
   isBackgroundApplicable(false);

   PlotAttributes styles;
   styles.symbolStyle = PlotStyle::BLANK;
   styles.lineStyle = PlotCommand::standardModelStyle();
   styles.lineStep = true;
   setStyleMap(PlotStyle::DATA, styles);
}


PlotChiSq::~PlotChiSq()
{
}


void PlotChiSq::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
  const StatMethod* singleStat = XSContainer::fit->statManager()->usingSingleStat();
  const string statName = singleStat ? singleStat->fullName() : 
                                string("Total Statistic");
  const string scriptName = singleStat ? singleStat->scriptName() :
                                string("Total Statistic");
  if (statName == string("C-Statistic") ||
      statName == string("L-statistic") || 
      statName == string("Total Statistic"))
  {
     tcout <<"Note: When using cstat or lstat, \"plot chisq\" is only valid\n"
           <<"      for data sets with no background files." << std::endl;
  }
  labels.title = settings.lookupLabelName("t:chisq") + " " + statName;

  settings.makeXOptionDependentLabel(labels.x);
  labels.y = settings.lookupLabelName("y:chisq") + " " + scriptName;
}

void PlotChiSq::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{

   // This would be extremely difficult to do (if not impossible) if all
   // spectra within a plot group are not using the same statistic.  Would
   // this even make sense in such a case?

   const StatManager* stats = XSContainer::fit->statManager();
   const Real NO_VAL = settings.badDataValue();
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      PlotGroup* pg = plotGroups[i];
      size_t arraySize = pg->n;
      std::vector<Real>& yData  = pg->yData.data;
      const std::vector<Real>& yError = pg->yData.errors[0];
      const std::vector<Real>& model  = pg->model[0].data;

      bool allSameStat = true;
      const StatMethod* stat = stats->usingSingleStat();
      // Shortcut: If only 1 stat is in use, no need to do the gymnastics below.
      if (!stat)
      {
         std::pair<SpecGroup::const_iterator,SpecGroup::const_iterator> itRange = 
              settings.spectra().equal_range(i+1);
         SpecGroup::const_iterator itSpec = itRange.first;
         string statName;
         while (allSameStat && itSpec != itRange.second)
         {
            stat = stats->getStatMethodForSpectrum(itSpec->second->spectrumNumber());
            if (statName.empty())
               statName = stat->name();
            else if (statName != stat->name())
               allSameStat = false;
            ++itSpec;
         }
      }

      if (allSameStat)
      {
         for (size_t j=0; j<arraySize; ++j)
         {
            Real obs (yData[j]);
            Real mod (model[j]);
            Real err (yError[j]);
            Real areaTime = pg->saveData[j];
            yData[j]  = stat->plotChi(obs,mod,err,areaTime, NO_VAL);         
         }
         pg->yData.errors.resize(0);
      }
      else
      {
         string msg("\nCannot do plot chisq unless all spectra in plot group");
         msg += "\n are using the same fit statistic.";
         throw YellowAlert(msg);
      }   
   }
}

void PlotChiSq::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
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

// Additional Declarations
