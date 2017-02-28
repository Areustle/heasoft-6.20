//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/EmissionLines/LineList.h>
#include <XSPlot/Plot/PlotCommandCreator.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotGroupCreator.h>
#include <XSPlot/Plot/PlotPane.h>
#include <XSPlot/Plot/PlotPkgCreator.h>
#include <XSPlot/Plot/PlotVector.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>
#include <fstream>
#include <memory>
#include <sstream>
#include <cctype>
#include <typeinfo>
#include <utility>

// PlotPkg
#include <XSPlot/Plot/PlotPkg.h>
// PlotCommand
#include <XSPlot/Plot/PlotCommand.h>
// PlotDirector
#include <XSPlot/Plot/PlotDirector.h>

PlotDirector* XSContainer::plot = 0;


// Class PlotDirector 
PlotDirector* PlotDirector::s_instance = 0;
const size_t PlotDirector::s_MAX_IN_STACK = 3;
const size_t PlotDirector::s_MAX_STACKS = 2;

PlotDirector::PlotDirector (const string& plottingPackage, const string& labelDictionaryPath)
  :m_labelDictionaryFile("plotLabels.dat"),
   m_unitsInformationFile("plotUnits.dat"),
   m_energyUnitsInformation(),
   m_waveUnitsInformation(),
   m_plottingPackageName(plottingPackage),
   m_lastCommand(1, "data"),
   m_lastCommandIndices(1, 0),
   m_savedPlotArray(),
   m_groups(),
   m_setplot(),
   m_plotPackage(0),
   m_plotCommands() // Non-owning
{
  loadLabelDictionary(labelDictionaryPath);
  loadUnitsInformation(labelDictionaryPath);
  selectUnits("keV",ENERGY);
  selectUnits("Angstrom",WAVELENGTH);
  // Musn't throw after this:
  setPlottingPackage(plottingPackage);
}


PlotDirector::~PlotDirector()
{
  // PlotGroups can still exist if coming from Python interface.  See
  // note in makePlot.  
  clearPlotGroups();
  delete m_plotPackage;
  PlotCommandCreator::destroy();
  s_instance = 0;
}


PlotDirector* PlotDirector::Instance (const string& plottingPackage, const string& labelDictionaryPath)
{
  if (s_instance == 0)
  {
     s_instance = new PlotDirector(plottingPackage, labelDictionaryPath);
  }
  return s_instance;
}

void PlotDirector::makePlot (const std::vector<PlotCommand*>& commands)
{
   size_t nComs = commands.size();

   if (!nComs) // This should never happen
      return; 
   if (nComs > (s_MAX_IN_STACK*s_MAX_STACKS))
   {
      std::ostringstream oss;
      oss << "Maximum number of plot panes is limited to "
         << s_MAX_IN_STACK*s_MAX_STACKS << "\n";
      throw YellowAlert(oss.str());
   }

   if (m_setplot.saveArrayInfo().first == PlotSettings::ALL_AR)
   {
      // This is for the case of coming from the Python interface.
      // PlotGroups are KEPT at the end of a makePlot call, and deleted
      // here just prior to displaying a new plot.  In standard XSPEC,
      // the PlotGroups are deleted at the END of this function rather
      // than here.  Standard XSPEC does not need the groups around since
      // it can only retrieve plot arrays via "tclout plot".
      clearPlotGroups();
   }
   m_setplot.initializeSpectra();
   PlotCommand::commonInit();

   // Some plot commands may enforce their own xOption settings,
   // but we'll eventually want to restore the user's.
   const XaxisMode usersXoption = m_setplot.xOption();

   // The folding of source components is a VERY expensive operation.
   // Only do it if we come across a command that requires it, and then
   // make sure to only do it once.
   bool areSourcesCalculated = false;
   bool areSourcesFolded = false;

   m_plotCommands.clear();
   // If PlotSum is entered, it will be the only object in the
   // commands array, and we'll need to replace it with 2 pairs of
   // PlotData and PlotResiduals.  Also will need to toggle the
   // chan/eng/wave setting after the 2nd pane.
   const bool isPlotSum = (commands[0]->cmdName() == "sum");
   size_t plotSumCount=0;
   if (isPlotSum)
   {
      for (size_t i=0; i<2; ++i)
      {
         m_plotCommands.push_back(PlotCommandCreator::commands("data"));
         m_plotCommands.push_back(PlotCommandCreator::commands("residuals"));
      }
      nComs = m_plotCommands.size();
   }
   else
      m_plotCommands = commands;

   // ASSUME m_groups has been properly emptied before getting here.
   try
   {
      size_t groupCount=0;
      // For cases where one plot may influence the data in a 
      // succeeding plot ...
      std::vector<const PlotGroup*> donatedPlotGroups;
      for (size_t i=0; i<nComs; ++i)
      {
         PlotCommand* plotCom = m_plotCommands[i];
         // This may throw
         plotCom->verifyState();
         m_setplot.setDivideByArea(1,plotCom->isDivisibleByArea());
         m_setplot.setAddCompLevel(plotCom->addCompLevel());
         m_setplot.setShowBackground(1,plotCom->isBackgroundApplicable());
         m_setplot.setShowLineIDs(1,plotCom->isLineIDApplicable());

         if (m_setplot.showAddComponent() && !areSourcesCalculated)
         {
  	    XSContainer::models->clearSources();
  	    XSContainer::models->makeSourceComponents();
            areSourcesCalculated = true;
         }
         if (m_setplot.showFoldedAddComponent() && !areSourcesFolded)
         {
            // If in here, assume sources have been calculated.
            XSContainer::models->foldSources();
            areSourcesFolded = true;
         }

         // Some PlotCommands may wish to (temporarily) override 
         // the user's xOption setting.
         XaxisMode tmpXoption = CHANNELS;
         if (isPlotSum)
         {
            if (plotSumCount < 2)
               m_setplot.xOption(CHANNELS);
            else
            {
               if (usersXoption == CHANNELS)
                  m_setplot.xOption(ENERGY);
               else
                  m_setplot.xOption(usersXoption);
            }
            ++plotSumCount;
         }         
         else if (plotCom->requestNewXoption(m_setplot, tmpXoption))
         {
            m_setplot.xOption(tmpXoption);
         }

         // PlotDirector owns newGroups from after this call to
         // their destruction.
         std::vector<PlotGroup*> newGroups = 
                plotCom->makePlotGroups(m_setplot, donatedPlotGroups);
         if (newGroups.empty())
         {
            string err("No plot groups found for plot ");
            err += plotCom->cmdName() + "\n";
            throw YellowAlert(err);
         }
         // objectIndex is 1-based.
         for (size_t j=0; j<newGroups.size(); ++j)
            newGroups[j]->objectIndex = groupCount + j + 1;

         donatedPlotGroups.clear();
         if (plotCom->isPlotGroupDonor())
         {
            // Keep track of newGroups pointers for possible usage by next 
            // plotCom.  Note that it makes no difference whether we store 
            // these before or after the processPlotGroups function (which
            // will call PlotCommand's manipulate()).  By the time they
            // are applied to the next plot, they will point to post-maniuplated
            // PlotGroups.
            for (size_t j=0; j<newGroups.size(); ++j)
               donatedPlotGroups.push_back(newGroups[j]);
         }

         m_groups.push_back(newGroups);
         groupCount += newGroups.size();
         plotCom->processPlotGroups(newGroups, m_setplot);

         fillPlotVectorAttributes(i);

         StandardLabels labels;
         plotCom->makeLabels(m_setplot, labels);

         std::auto_ptr<PlotPane> 
                apPane(new PlotPane(plotCom->getRangeSettings(),labels, newGroups));
         if (m_setplot.showLineIDs() && m_setplot.xOption() != CHANNELS)
         {
            buildLineIDList(plotCom->getRangeSettings(),apPane->lineIDs());
         }
         // PlotPkg immediately takes ownership of PlotPane 
         //  (but not newGroups).
         m_plotPackage->addPane(apPane.release());

         // In case PlotComand has messed with this above...
         m_setplot.xOption(usersXoption);
      }

      std::vector<size_t> panesPerStack;
      std::vector<bool> shareX;
      determinePanesPerStack(panesPerStack);
      determineSharedXaxis(panesPerStack, shareX);
      m_plotPackage->panesInStack(panesPerStack);
      m_plotPackage->shareXaxis(shareX);

      m_plotPackage->setUserCommands(&m_setplot.userCommands());
      m_plotPackage->isInteractive(m_setplot.isInteractive());

      m_plotPackage->display();
      m_plotPackage->flushHardcopy();
   }
   catch (...)
   {
      m_savedPlotArray.clear();
      m_setplot.saveArrayInfo(std::pair<PlotSettings::SaveArrayOption,int>
                (PlotSettings::NO_SAVE,1));
      m_setplot.xOption(usersXoption);
      if (areSourcesCalculated)
         XSContainer::models->clearSources();
      m_plotPackage->clearPanes();
      clearPlotGroups();
      m_plotPackage->flushHardcopy();
      for (size_t i=0; i<nComs; ++i)
         m_plotCommands[i]->cleanup();
      throw;
   }
   if (m_setplot.saveArrayInfo().first != PlotSettings::ALL_AR)
      saveTcloutArray();
   if (areSourcesCalculated)
      XSContainer::models->clearSources();
   m_plotPackage->clearPanes();
   // See note at top of this function regarding Python interface
   // and the saving of PlotGroup objects.
   if (m_setplot.saveArrayInfo().first != PlotSettings::ALL_AR)
      clearPlotGroups();
   for (size_t i=0; i<nComs; ++i)
      m_plotCommands[i]->cleanup();
}

void PlotDirector::loadLabelDictionary (const string& dictionaryPath)
{
   string fullPathName = dictionaryPath + string("/") + m_labelDictionaryFile;
   std::ifstream labelFile;
   labelFile.open(fullPathName.c_str());

   if (!labelFile)
   {
      string message = "Cannot open label dictionary file: " + 
      			m_labelDictionaryFile;
      throw RedAlert(message);
   }

   std::map<string,string>& labelStorage = m_setplot.labelNames();
   while (!labelFile.eof())
   {
      string line;
      getline(labelFile, line);
      size_t sz = line.size();
      if (sz != 0)
      {
         size_t i;
         string key("");
	 // key will equal the first sequential group of non-whitespace
	 // characters encountered in the line.
	 for (i=0; i<sz; ++i)
	 {
	    if (!isspace(line[i]))  break;
	 }
	 while (i < sz)
	 {
	    if (isspace(line[i]))  break;
	    key += line[i];
	    ++i;
	 }

	 if (key.size() != 0)
	 {
	    // Now proceed to the next non-whitespace character then read from
	    // it until the end of the line into string val.
	    string val;
	    while (i < sz)
	    {
	       if (!isspace(line[i]))
	       {
	          val = line.substr(i,sz-i);
		  break;
	       }
	       ++i;
	    }
	    // Remove trailing whitespace from end of val and insert into map.
	    int j;
	    for (j=val.size()-1; j>=0; --j)
	    {
	       if (!isspace(val[j]))  break;
	    }
	    if (j >= 0)
	    {
	       labelStorage.insert(std::pair<string,string>
			  (key, string(val,0,j+1))); 
	    }
	 }

      }
   }
   if (labelStorage.size() == 0)
   {
      string message = "No valid entries found in " + m_labelDictionaryFile;
      throw RedAlert(message);
   }
}

void PlotDirector::loadUnitsInformation (const string& path)
{

   // In each section, Energy or Wavelength, look for 3 fields per line.

   // The first field is the unitID which the user enters in setplot.

   // The second field is the unit label as it will appear in the plot.  This
   // allows us to enter escape chars for Greek symbols, subscripts, etc.  If
   // a '%' is entered, it simply uses the unitID string (without the appended
   // character).

   // The third field is the unit conversion factor. 

   string fullPathName = path + string("/") + m_unitsInformationFile;
   std::ifstream unitsFile;
   unitsFile.open(fullPathName.c_str());

   if (!unitsFile)
   {
      string message = "Cannot open plot units file: " + 
      			m_unitsInformationFile;
      throw RedAlert(message);
   }

   char labelSection = 'e';
   const string WS(" \t");
   while (!unitsFile.eof())
   {
      string line;
      getline(unitsFile, line);
      string::size_type curPos=line.find_first_not_of(WS);
      size_t iArg=0;
      string key;
      string unitLabel;
      Real conversion=0.0;
      while (iArg < 3 && curPos < line.length())
      {
         string::size_type endPos = line.find_first_of(WS,curPos);
         string arg = line.substr(curPos,endPos-curPos);
         switch (iArg)
         {
            case 0:
               if (arg == "Energy:")
                  labelSection = 'e';
               else if (arg == "Wavelength:")
                  labelSection = 'w';
               else
                  key = arg;
               break;
            case 1:
               unitLabel = (arg == "%") ? key : arg;
               break;
            case 2:
               {
                  std::istringstream iss(arg);
                  if (!(iss >> conversion) || !iss.eof())
                  {
                     string errMsg("Invalid conversion factor in plotUnits.dat: ");
                     errMsg += arg;
                     throw RedAlert(errMsg);
                  }
               }
               break;
            default:
               break;
         }
         // All 3 fields must exist to insert into map.
         if (key.length() && iArg == 2)
         {
            UnitsContainer::value_type 
                        entry(key,std::pair<string,Real>(unitLabel,conversion));
            if (labelSection == 'e')
            {
               if (!m_energyUnitsInformation.insert(entry).second)
               {
                  string errMsg("Duplicate energy entries found in plotUnits.dat");
                  throw RedAlert(errMsg);
               }
            }
            else
            {
               if (!m_waveUnitsInformation.insert(entry).second)
               {
                  string errMsg("Duplicate wavelength entries found in plotUnits.dat");
                  throw RedAlert(errMsg);
               }
            }
         }

         curPos = line.find_first_not_of(WS, endPos);
         ++iArg;
      }
   }
   if (!m_energyUnitsInformation.size() || !m_waveUnitsInformation.size())
   {
      string message = "Empty energy or wavelength section in " + m_unitsInformationFile;
      throw RedAlert(message);
   }
}

void PlotDirector::setPlottingDevice (const string& device, bool splashOn)
{
   m_plotPackage->setDevice(device,splashOn);
}

const string& PlotDirector::getPlottingDeviceName ()
{
   return m_plotPackage->deviceName();
}

void PlotDirector::clearPlotGroups () throw ()
{
   PlotGroupContainer::iterator itGroups = m_groups.begin();
   PlotGroupContainer::iterator itEnd = m_groups.end();
   while (itGroups != itEnd)
   {
      std::vector<PlotGroup*>& groupsVec = *itGroups;
      for (size_t i=0; i<groupsVec.size(); ++i)
         delete groupsVec[i];
      ++itGroups;
   }
   m_groups.clear();
}

void PlotDirector::fillPlotVectorAttributes (size_t iCmd)
{
   std::vector<PlotGroup*>& groups = m_groups[iCmd];
   const std::map<PlotStyle::VectorCategory,PlotAttributes>& styleContainer = 
                m_plotCommands[iCmd]->styleMap();
   std::map<PlotStyle::VectorCategory,PlotAttributes>::const_iterator itEnd = 
                styleContainer.end();
   const int MAXCOLORS = 15;
   int iColor = 1;

   // PlotCommands have a styleContainer entry for every PlotVector type
   // they could potentially want to plot.  But that doesn't mean that
   // all of them are used in EVERY plot.  We must figure that out here
   // before turning the vector "on" (by assigning it an actual plot symbol
   // or line style).

   for (size_t i=0; i<groups.size(); ++i)
   {
      PlotGroup* gr = groups[i];
      gr->bundledPlotVectors.clear();
      gr->bundledPlotVectors.push_back(&gr->xAxis);
      gr->plotVectorBoundaries.clear();

      std::map<PlotStyle::VectorCategory,PlotAttributes>::const_iterator itStyle =
                   styleContainer.find(PlotStyle::DATA);
      if (itStyle != itEnd)
      {
         // DATA rule: If y-data exists, turn it on.
         const PlotAttributes& styles = itStyle->second;
         PlotVector& dataVect = gr->yData;
         if (dataVect.data.size())
         {
            dataVect.styles = styles;
            dataVect.styles.color = PlotStyle::Colour(iColor);
            gr->bundledPlotVectors.push_back(&gr->yData);            
         }
      }
      gr->plotVectorBoundaries.push_back(static_cast<int>
                (gr->bundledPlotVectors.size()));

      itStyle = styleContainer.find(PlotStyle::BACKGROUND);
      if (itStyle != itEnd)
      {
         // BACKGROUND rule: If background exists AND setplot is
         // chosen to plot it, turn it on.
         const PlotAttributes& styles = itStyle->second;
         PlotVector& backVect = gr->background;
         if (m_setplot.showBackground() && backVect.data.size())
         {
            backVect.styles = styles;
            backVect.styles.color = PlotStyle::Colour(iColor);
            gr->bundledPlotVectors.push_back(&gr->background);
         }

      }
      gr->plotVectorBoundaries.push_back(static_cast<int>
                (gr->bundledPlotVectors.size()));

      itStyle = styleContainer.find(PlotStyle::MODEL);
      if (itStyle != itEnd)
      {
         // MODEL rule: Any existing model vectors will be turned on.
         const PlotAttributes& styles = itStyle->second;
         std::vector<PlotVector>& mods = gr->model;
         for (size_t j=0; j<mods.size(); ++j)
         {
            PlotVector& modVect = mods[j];
            if (modVect.data.size())
            {
               modVect.styles = styles;
               modVect.styles.color = PlotStyle::Colour(iColor);
               gr->bundledPlotVectors.push_back(&gr->model[j]);
            }
         }
      }
      gr->plotVectorBoundaries.push_back(static_cast<int>
                (gr->bundledPlotVectors.size()));

      itStyle = styleContainer.find(PlotStyle::SOURCES);
      if (itStyle != itEnd)
      {
         // SOURCES rule: If sources exist AND setplot is
         // chosen to plot them, turn them on.
         const PlotAttributes& styles = itStyle->second;
         if (m_setplot.showAddComponent())
         {
            std::vector<PlotVectorList>& allSources = gr->sources;
            for (size_t j=0; j<allSources.size(); ++j)
            {
               PlotVectorList::iterator itList = allSources[j].begin();
               PlotVectorList::iterator itListEnd = allSources[j].end();
               while (itList != itListEnd)
               {
                  PlotVector& sourceVect = *itList;
                  if (sourceVect.data.size())
                  {
                     sourceVect.styles = styles;
                     sourceVect.styles.color = PlotStyle::Colour(iColor);
                     gr->bundledPlotVectors.push_back(&sourceVect);
                  }
                  ++itList;
               }
            }
         }
      }
      gr->plotVectorBoundaries.push_back(static_cast<int>
                (gr->bundledPlotVectors.size()));

      ++iColor;
      if (iColor > MAXCOLORS)
         iColor = 1;
   } // end plot groups loop

}

void PlotDirector::determinePanesPerStack (std::vector<size_t>& panesPerStack) const
{

   // This relies on the constraint that nCommands == nPanes.
   // ASSUME nCommands != 0 and has already been tested not to exceed limits.

   // Rule 1: Create as few stacks as possible.
   // Rule 2: If more than one stack, keep things balanced.  Number of panes
   //         in a stack should not vary by more than 1.
   // So for example if MAX_IN_STACK=3 and 3 panes, all would go into a single
   // stack, while 4 panes are split 2-2 and 7 are split 3-2-2.  

   const size_t nCom = m_plotCommands.size();
   const size_t nStacks = 1 + (nCom-1)/s_MAX_IN_STACK;
   const size_t minPerStack = nCom/nStacks;
   const size_t remaining = nCom % nStacks;

   panesPerStack.clear();
   panesPerStack.resize(nStacks,minPerStack);
   for (size_t i=0; i<remaining; ++i)
      panesPerStack[i] += 1;

}

void PlotDirector::determineSharedXaxis (const std::vector<size_t>& panesPerStack, std::vector<bool>& shareX) const
{

   // Determine if all panes in a vertical stack may share a single
   // x-axis on the bottom plot.  For now, the question of whether
   // various plot commands are able to share an x-axis is NEARLY
   // the same as the question of whether they use the same type of
   // PlotGroupCreator strategy, so we can get away without having
   // to add another attribute to plot command classes.  Two caveats
   // are: 1) the case where a plot command (ie. ufspec, model) may request
   // an xOption change that causes it to differ from other panes in
   // the stack, and 2) chain plots -- can't assume x axes are the same
   // since they are dependent on the selected parameter number.

   const size_t nStacks = panesPerStack.size();
   shareX.clear();
   shareX.resize(nStacks,true);
   size_t iCom = 0;
   for (size_t iStack=0; iStack<nStacks; ++iStack)
   {
      bool allSame = true;
      for (size_t iPane=0; iPane<panesPerStack[iStack]; ++iPane)
      {
         if (allSame && iPane > 0)
         {
            const PlotCommand* prevCom = m_plotCommands[iCom-1];
            const PlotCommand* plotCom = m_plotCommands[iCom];

            XaxisMode prevXoption(m_setplot.xOption());
            XaxisMode thisXoption(m_setplot.xOption());
            prevCom->requestNewXoption(m_setplot, prevXoption);
            plotCom->requestNewXoption(m_setplot, thisXoption);
            if (prevXoption != thisXoption || plotCom->cmdName()=="chain")
            {
               shareX[iStack] = false;
               allSame = false;
            }
            
            if (allSame && typeid(*plotCom->plotGroupStrategy()) !=
                typeid(*prevCom->plotGroupStrategy()))
            {
               shareX[iStack] = false;
               // No need to keep looking through rest of stack,
               // typeid is expensive (but we do need to keep
               // incrementing iCom).
               allSame = false;
            }
         }
         ++iCom;
      }
   }
}

void PlotDirector::selectUnits (const string& unitID, XaxisMode mode)
{

   // Assuming UnitsContainer is defined with a case-insensitive comparison
   // function, map searches will be case-insensitive (but not string "find"
   // functions of course).

   const string lcSearchKey(XSutility::lowerCase(unitID));
   bool isFound = false;
   UnitsContainer::const_iterator itUnit = (mode == ENERGY) ?
                m_energyUnitsInformation.lower_bound(lcSearchKey) :
                m_waveUnitsInformation.lower_bound(lcSearchKey);
   const UnitsContainer::const_iterator itUnitBegin = (mode == ENERGY) ?
                m_energyUnitsInformation.begin() :
                m_waveUnitsInformation.begin();
   const UnitsContainer::const_iterator itUnitEnd = (mode == ENERGY) ?
                m_energyUnitsInformation.end() :
                m_waveUnitsInformation.end();
   if (itUnit != itUnitEnd)
   {
      const string lcKey(XSutility::lowerCase(itUnit->first));
      if (lcKey.find(lcSearchKey) == 0)
         isFound = true;
   }

   if (!isFound)
   {
      string errMsg("Unrecognized units entry: ");
      errMsg += unitID + "\n";
      errMsg += "Valid (case-insensitive) units for ";
      errMsg += (mode == ENERGY) ? string("energy are:\n")
                : string("wavelength are:\n");
      itUnit = itUnitBegin; 
      while (itUnit != itUnitEnd)
      {
         errMsg += "  " + itUnit->first;
         ++itUnit;
      }
      errMsg += "\n\n";
      if (mode == WAVELENGTH)
      {
         errMsg +="Or for toggling wavelength Y-axis display:\n";
         errMsg += "     setplot wave perHz       ;Y-values plotted per Hz\n";
         errMsg += "     setplot wave perHz on\n";
         errMsg += "     setplot wave perHz off   ;Y-values plotted per length unit\n\n";
      }
      throw YellowAlert(errMsg);
   }

   // Messy appearance due to "const T"  type storage of key in map.
   std::pair<string,std::pair<string,Real> > 
        unitSetting(itUnit->first,std::pair<string,Real>(itUnit->second));
   if (mode == ENERGY)
   {
      m_setplot.energyUnit(unitSetting);
      SpectralData::engUnits(unitSetting.second.second);
   }
   else
   {
      m_setplot.waveUnit(unitSetting);
      SpectralData::waveUnits(unitSetting.second.second);
   }
}

void PlotDirector::setPlottingPackage (const string& packageName)
{
   // This may throw:
  m_plotPackage = PlotPkgCreator::MakePlotPkg(packageName);
  m_setplot.badDataValue(m_plotPackage->BADVALUE());
}

void PlotDirector::saveTcloutArray ()
{
   // Do not throw any YellowAlerts from here.  If saveArrayInfo
   // does not correspond to an existing plot group or array, 
   // just issue warning and leave m_savedPlotArray empty.
   if (m_savedPlotArray.size())  
                m_savedPlotArray.resize(0);
   // If coming from "tclout plot", we can ASSUME we have ONLY 1 PLOT PANE
   // to deal with.  If not coming from "tclout plot", saveArrayInfo
   // will be set to NO_SAVE and nothing else will be done in here.
   if (m_setplot.saveArrayInfo().first != PlotSettings::NO_SAVE)
   {
      int groupNum = m_setplot.saveArrayInfo().second;
      if (groupNum < 1 || static_cast<size_t>(groupNum) > m_groups[0].size())
      {
         tcout << "***Warning: Saved plot array group number specifier = "
           << groupNum << ",\n   does not correspond to an existing plot group.  "
           << "No plot array will be saved." << std::endl; 
      }
      else
      {
         // Remember, there's only 1 plot pane.
         const PlotGroup* pg = m_groups[0][groupNum - 1];
         if (pg->objectIndex != static_cast<size_t>(groupNum))
         {
            throw RedAlert("In Plot::saveArray: plot group nums not sequentially numbered.");
         }
         switch (m_setplot.saveArrayInfo().first)
         {
            case PlotSettings::X_AR:
               m_savedPlotArray = pg->xAxis.data;
               break;
            case PlotSettings::XERR_AR:
               if (pg->xAxis.errors.size())
                  m_savedPlotArray = pg->xAxis.errors[0];
               else
                  tcout << "***Warning: No x-axis error array exists." << std::endl;
               break;
            case PlotSettings::Y_AR:
               m_savedPlotArray = pg->yData.data;
               break;
            case PlotSettings::YERR_AR:
               if (pg->yData.errors.size())
                  m_savedPlotArray = pg->yData.errors[0];
               else
                  tcout << "***Warning: No y-axis error array exists." << std::endl;
               break;
            case PlotSettings::MODEL_AR:
               if (pg->model.size())
                  m_savedPlotArray = pg->model[0].data;
               else
                  tcout << "***Warning: No model plot array exists." << std::endl;
               break;
            default:
               break;
         }
         const size_t nPts = pg->n;
         // For all plots other than contour plots, nPts <= plot vector array size.
         // nPts will be less than the array size for cases of setplot rebin and
         // for ignored channels.  (For contour plots nPts = nX*nY, hence is always
         // larger than the individual array sizes.)
         if (nPts < m_savedPlotArray.size())
            m_savedPlotArray.resize(nPts);
      }
      m_setplot.saveArrayInfo(std::pair<PlotSettings::SaveArrayOption,int>
                (PlotSettings::NO_SAVE,1));
   }
}

void PlotDirector::buildLineIDList (const PlotRange& plotRange, LineIDContainer& lineIDs) const
{
   // Must convert the output of the LineList class into plot label strings
   // for the LineIDContainer.  Also calculate the x-axis position for
   // the label.  We leave it up to the PlotPkg subclasses to determine
   // the y-axis positions.

   // Assume that if we're here, xOption is NOT CHANNELS.
   const bool isWave = (m_setplot.xOption() == WAVELENGTH);
   const Real unitFactor = isWave ? m_setplot.getUnitFactor(WAVELENGTH)
        : m_setplot.getUnitFactor(ENERGY);
   const Real zFact = 1.0 + m_setplot.redshiftLinesToObs();
   Real lowE=0.0, highE=0.0;
   // Apec LineList class expects lowE, highE to be in keV for
   // ENERGY mode and Angstroms for WAVELENGTH.
   if (isWave)
   {
      lowE = plotRange.ranges.x1/zFact/unitFactor;
      highE = plotRange.ranges.x2/zFact/unitFactor;
   }
   else
   {
      lowE = plotRange.ranges.x1*zFact/unitFactor;
      highE = plotRange.ranges.x2*zFact/unitFactor;
   }
   if ( m_setplot.IDLowEnergy() > 0.0 || m_setplot.IDHighEnergy() > 0.0 ) {
     if ( lowE < m_setplot.IDLowEnergy() ) lowE = m_setplot.IDLowEnergy();
     if ( highE > m_setplot.IDHighEnergy() ) highE = m_setplot.IDHighEnergy();
   }
   std::auto_ptr<LineList> lineList(LineList::get("apec"));
   // This could throw:
   lineList->initialize(lowE, highE, m_setplot.temperature(), 
              m_setplot.emisLimit(), isWave);
   std::ostringstream listStream;
   lineList->showList(listStream);
   lineIDs.clear();
   std::istringstream allLines(listStream.str());
   // Not doing any format checking here.  Assuming line strings
   // are in a fixed format as written out in Apec::report. 
   while (!allLines.eof())
   {
      string dummy;
      string elem, ion;
      Real energ;
      allLines >> dummy;
      if (!allLines.eof())
      {
         allLines >> dummy >> energ >> elem >> ion;
         string label(elem);
         label += " ";
         label += ion;
         if (isWave)  
            energ *= zFact*unitFactor;
         else
            energ /= zFact/unitFactor;
         lineIDs.push_back(std::pair<string,Real>(label,energ));
         // Swallow the rest of the parameters on the line
         std::getline(allLines, dummy); 
      }
   }
}


const std::vector<Real>& PlotDirector::getPlotArray(size_t iPane, size_t iGroup, PlotSettings::SaveArrayOption arrType, size_t& nPts) const
{
   //

   if (iPane >= m_groups.size())
      throw YellowAlert("Requested plot pane index is out-of-bounds.\n");
   const std::vector<PlotGroup*>& groups = m_groups[iPane];
   if (iGroup >= groups.size())
      throw YellowAlert("Requested plot group index is out-of-bounds.\n");
   PlotGroup* plotGroup = groups[iGroup];

   std::vector<Real>* pArray=0;
   switch (arrType)
   {
      case PlotSettings::X_AR:
         pArray = &plotGroup->xAxis.data;
         break;
      case PlotSettings::XERR_AR:
         if (plotGroup->xAxis.errors.size())
            pArray = &plotGroup->xAxis.errors[0];
         break;
      // Following fillPlotVectorAttributes scheme, only use array
      // if it is "on" -- has a symbol or line style.  A background
      // array for example may be filled but not "on" (ie. not displayed).
      // In that case we won't use it.
      case PlotSettings::Y_AR:
      case PlotSettings::YERR_AR:
         { 
            PlotVector& vect = plotGroup->yData;
            if (vect.data.size() && 
               (vect.styles.symbolStyle != PlotStyle::BLANK ||
                vect.styles.lineStyle != PlotStyle::NONE))
            {
               if (arrType == PlotSettings::Y_AR)
                  pArray = &vect.data;
               else if (vect.errors.size())
                  pArray = &vect.errors[0];
            }
         }
         break;
      case PlotSettings::MODEL_AR:
         {
            if (plotGroup->model.size())
            {
               PlotVector& vect = plotGroup->model[0];
               if (vect.data.size() && 
                  (vect.styles.symbolStyle != PlotStyle::BLANK ||
                   vect.styles.lineStyle != PlotStyle::NONE))
               {
                  pArray = &vect.data;
               } 
            }           
         }
         break;
      case PlotSettings::BACK_AR:
         {
            PlotVector& vect = plotGroup->background;
            if (vect.data.size() && 
               (vect.styles.symbolStyle != PlotStyle::BLANK ||
                vect.styles.lineStyle != PlotStyle::NONE))
            {
               pArray = &vect.data;
            }            
         }
         break;
      default:
         break;
   }
   if (!pArray)
   {
      throw YellowAlert("Requested array does not exist for this plot.\n");
   }
   
   nPts = plotGroup->n;
   
   return *pArray;
} // end getPlotArray
