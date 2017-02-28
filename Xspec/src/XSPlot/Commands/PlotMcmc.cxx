//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/PlotGroupCreatorClasses.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

#include <XSFit/MCMC/Chain.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <algorithm>

// PlotMcmc
#include <XSPlot/Commands/PlotMcmc.h>


// Class PlotMcmc 

PlotMcmc::PlotMcmc()
  : PlotCommand("chain", new CreateChainPlotGroups()),
    m_paneCounter(-1),
    m_parNamesForPanes(),
    m_xIsRowNumForPanes()
{
   doesTakeParameters(true);
   isDataRequired(false);
   isActiveModelRequired(false);
   isDivisibleByArea(false);
   addCompLevel(0);
   isBackgroundApplicable(false);

   PlotAttributes styles;
   styles.symbolStyle = PlotStyle::CIRCLE;
   styles.lineStyle = PlotStyle::NONE;
   setStyleMap(PlotStyle::DATA, styles);
}


PlotMcmc::~PlotMcmc()
{
}


void PlotMcmc::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   // Only one plot group will exist for this command.
   // Can't assume xAxis data is ordered.
   const PlotGroup* chainGroup = plotGroups[0];

   PlotRange& rangeVals = rangeSettings();
   std::vector<Real>::const_iterator 
       beg_xdata = chainGroup->xAxis.data.begin(),
       end_xdata = chainGroup->xAxis.data.end();

   rangeVals.ranges.x1 = *std::min_element(beg_xdata, end_xdata);
   rangeVals.ranges.x2 = *std::max_element(beg_xdata, end_xdata);

   Real fraction = (rangeVals.ranges.x2 - rangeVals.ranges.x1) / 32.0;

   rangeVals.ranges.x1 -= fraction;
   rangeVals.ranges.x2 += fraction;

   std::vector<Real>::const_iterator 
       beg_ydata = chainGroup->yData.data.begin(),
       end_ydata = chainGroup->yData.data.end();

   rangeVals.ranges.y1 = *std::min_element(beg_ydata, end_ydata);
   rangeVals.ranges.y2 = *std::max_element(beg_ydata, end_ydata);

   fraction = (rangeVals.ranges.y2 - rangeVals.ranges.y1) / 32.0;

   rangeVals.ranges.y1 -= fraction;
   rangeVals.ranges.y2 += fraction;

   rangeVals.xScaleType = PlotStyle::LINEAR;
   rangeVals.yScaleType = PlotStyle::LINEAR;
   rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX +
                              PlotStyle::YMIN + PlotStyle::YMAX;
}

void PlotMcmc::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   string label = settings.lookupLabelName("t:chain");
   label += " MCMC";
   labels.title = label;

   // May need this for fit statistic label.  We could use
   // any of the chains.
   const Chain* firstChain = 
             ChainManager::Instance()->chains().begin()->second;

   if (m_paneCounter < 0)
      throw RedAlert("Invalid pane index in PlotMcmc::makeLabels");
      
   const bool xUsesRowNum = m_xIsRowNumForPanes[m_paneCounter];
   const std::pair<string,string>& parNames = m_parNamesForPanes[m_paneCounter];
   
   if (xUsesRowNum)
      labels.x = settings.lookupLabelName("x:chain:row");
   else if (parNames.first.length())
   {
      // X-axis is a parameter
      labels.x = settings.lookupLabelName("x:chain:parameter");
      labels.x += " " + parNames.first;
   }
   else
   {
      // X-axis is a fit statistic, borrow contour plot's label.
      labels.x = settings.lookupLabelName("y:contour:statistic");
      labels.x += " " + firstChain->statistic();
   }

   if (parNames.second.length())
   {
      labels.y = settings.lookupLabelName("x:chain:parameter");
      labels.y += " " + parNames.second;
   }
   else
   {
      labels.y = settings.lookupLabelName("y:contour:statistic");
      labels.y += " " + firstChain->statistic();
   }
}

void PlotMcmc::processAdditionalParams (const StringArray& args, const IntegerArray& argIndices)
{
   ChainManager* chainManager = ChainManager::Instance();
   if (!chainManager->chains().size())
      throw YellowAlert("No chains are currently loaded.\n");

   const size_t nArgs = args.size();
   size_t iPar = 0;
   if (nArgs < 1)
      throw YellowAlert("Plot chain requires 1 (y) or 2 (x,y) parameters.\n");

   string xModelName; 
   string yModelName;
   size_t xParam = string::npos;
   size_t yParam = string::npos;
   bool xUsesRowNum = false;
   static size_t nThin=1;
   size_t testThin = string::npos;
   
   if (XSutility::lowerCase(args[0]) == string("thin"))
   {
      bool isError=false;
      if (nArgs < 2)
         isError = true;
      else
      {
         testThin = XSutility::isInteger(args[1]);
         if (testThin == string::npos || !testThin)
            isError = true;
      }
      if (isError)
      {
         string msg("Invalid argument for \'thin <n>\' option.\n");
         msg +="   <n> must be an integer >= 1\n";
         throw YellowAlert(msg);
      }
      if (nArgs < 3)
      {
         throw YellowAlert("Plot chain also requires 1 (y) or 2 (x,y) parameters.\n");
      }
      iPar += 2;
      // "thin <n>" has been validated, but don't assign to static nThin
      // until all other arguments are also validated.
   }
   
   if (nArgs-iPar == 1)
   {
      xUsesRowNum = true;
      m_xIsRowNumForPanes.push_back(true);
      XSparse::stringIntPair(args[iPar], yModelName, yParam);
      if (yParam == string::npos)
         throw YellowAlert("Invalid y parameter number in chain plot.\n");
      if (!yModelName.length())
         yModelName = Model::DEFAULT();
   }
   else
   {
      m_xIsRowNumForPanes.push_back(false);
      XSparse::stringIntPair(args[iPar], xModelName, xParam);
      if (xParam == string::npos)
         throw YellowAlert("Invalid x parameter number in chain plot.\n");
      if (!xModelName.length())
         xModelName = Model::DEFAULT();
      XSparse::stringIntPair(args[iPar+1], yModelName, yParam);
      if (yParam == string::npos)
         throw YellowAlert("Invalid y parameter number in chain plot.\n");
      if (!yModelName.length())
         yModelName = Model::DEFAULT();
   }
   // These are initialized to the last column in the chain file, which
   // is the fit statistic. They'll remain that way if the user enters
   // 0 for the param number.  If only 1 param number is entered, xColNum
   // will be set to 0 to let CreateChainPlotGroups know to use row numbers.  
   // (Column numbers are 1-based.)
   size_t xColNum = chainManager->width();
   size_t yColNum = chainManager->width();
   // Assume all chains have same parameters as the first chain.
   const Chain* firstChain = chainManager->chains().begin()->second;
   std::vector<size_t> loc;
   std::vector<Chain::ParamID> testPar(1);
   std::pair<string,string> parNames;

   if (xUsesRowNum)
      xColNum = 0;
   else if (xParam > 0)
   {
      testPar[0].modName = xModelName;
      testPar[0].index = xParam;
      firstChain->findParsInChain(testPar, loc);
      if (!loc.size())
         throw YellowAlert("Cannot locate specified x parameter in chain file.\n");
      xColNum = loc[0] + 1;
      parNames.first = testPar[0].parName;
   }

   if (yParam > 0)
   {
      testPar[0].modName = yModelName;
      testPar[0].index = yParam;
      firstChain->findParsInChain(testPar, loc);
      if (!loc.size())
         throw YellowAlert("Cannot locate specified y parameter in chain file.\n");
      yColNum = loc[0] + 1;
      parNames.second = testPar[0].parName;
   }
   m_parNamesForPanes.push_back(parNames);
   
   CreateChainPlotGroups* groupCreator = 
        dynamic_cast<CreateChainPlotGroups*>(plotGroupStrategy());
   groupCreator->addColNums(xColNum, yColNum);
   if (testThin != string::npos)
      nThin = testThin;
   groupCreator->addNSkip(nThin);
}

void PlotMcmc::cleanup()
{
   m_paneCounter = -1;
   m_parNamesForPanes.clear();
   m_xIsRowNumForPanes.clear();
   CreateChainPlotGroups* groupCreator = 
        dynamic_cast<CreateChainPlotGroups*>(plotGroupStrategy());
   groupCreator->clearCmdArgs();
}

std::vector<PlotGroup*> PlotMcmc::makePlotGroups(const PlotSettings& settings, const std::vector<const PlotGroup*>& donatedPlotGroups)
{
   CreateChainPlotGroups* groupCreator = 
        dynamic_cast<CreateChainPlotGroups*>(plotGroupStrategy());
   
   // Must increment this PRIOR to usage rather than after.  It's last used
   // in makeLabels but can't increment from there (or anyplace after).
   ++m_paneCounter;
   groupCreator->setPaneIndex(m_paneCounter);
   return groupCreator->createPlotGroups(settings);

}
