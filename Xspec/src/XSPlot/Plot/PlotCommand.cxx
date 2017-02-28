//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSPlot/Plot/PlotGroupCreator.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>

// PlotCommand
#include <XSPlot/Plot/PlotCommand.h>


// Class PlotCommand 
bool PlotCommand::s_dataStatus = false;
bool PlotCommand::s_activeModelStatus = false;
const PlotStyle::LineStyle PlotCommand::s_standardModelStyle = PlotStyle::SOLID;
const PlotStyle::Symbol PlotCommand::s_standardDataStyle = PlotStyle::DOT;


PlotCommand::PlotCommand (const string& cmdName, PlotGroupCreator* plotGroupStrategy)
  : m_cmdName(cmdName),
    m_plotGroupStrategy(plotGroupStrategy),
    m_doesTakeParameters(false),
    m_isDataRequired(false),
    m_isActiveModelRequired(false),
    m_isDivisibleByArea(false),
    m_addCompLevel(0),
    m_isBackgroundApplicable(false),
    m_isLineIDApplicable(false),
    m_isPlotGroupDonor(false),
    m_isContour(false),
    m_styleMap(),
    m_rangeSettings()
{
}


PlotCommand::~PlotCommand()
{
   delete m_plotGroupStrategy;
}


void PlotCommand::commonInit ()
{
  using namespace XSContainer;
  const ModelMap& mods = models->modelSet();
  ModelMapConstIter mm (mods.begin());
  ModelMapConstIter mmEnd (mods.end());  
  s_activeModelStatus = false;
  while (mm != mmEnd)
  {
     if (mm->second->isActive())
     {
        s_activeModelStatus = true;
	break;
     }
     ++mm;
  }

  s_dataStatus = datasets->numberOfSpectra();
}

void PlotCommand::verifyState () const
{
  if (m_isDataRequired && !s_dataStatus)
  {
     throw YellowAlert(" No data loaded\n");
  }

  if (m_isActiveModelRequired && !s_activeModelStatus)
  {
     throw YellowAlert(" No active models found\n");
  }

  // If command is capable of plotting data and model together,
  // and both currently exist, give a warning if the fit is not 
  // up to date.
  if (m_styleMap.find(PlotStyle::DATA) != m_styleMap.end() &&
      m_styleMap.find(PlotStyle::MODEL) != m_styleMap.end() &&
      s_dataStatus && s_activeModelStatus &&
      !XSContainer::fit->isStillValid())
  {
     tcout << "***Warning: Fit is not current." << std::endl;
  }
}

void PlotCommand::setStyleMap (PlotStyle::VectorCategory category, const PlotAttributes& attributes)
{
   // No checking for pre-existing entries.
   m_styleMap[category] = attributes;
}

void PlotCommand::processPlotGroups (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   manipulate(plotGroups, settings);
   setRanges(plotGroups, settings);
}

void PlotCommand::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
}

void PlotCommand::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
}

bool PlotCommand::requestNewXoption (const PlotSettings& settings, XaxisMode& newOption) const
{
   // Don't do anything here in the base class.
   return false;
}

void PlotCommand::processAdditionalParams (const StringArray& args, const IntegerArray& argIndices)
{
}

void PlotCommand::cleanup()
{
}

string PlotCommand::processSuperAndSub (const string& inString)
{
   // This expects inString to come from the parameter names and 
   // optional parentheses-enclosed unit strings stored in in model.dat.
   // Therefore it is rather simplistic.  It only treats the first 
   // subscript ('_') found in the name portion of inString, and the
   // first superscript ('^') found in the optional units portion.
   // The subscript is assumed to run to the end of the name portion,
   // while the superscript is assumed to run until a ')'.

   // If the user has a more complicated mixture of super/subscripts
   // in their own lmodel.dat, chances are this isn't going to do what
   // they want, in which case they'll need to modify it with their
   // own PLT command.


   string outString;
   // The (npos-6) case should never arise, but each replacement nets
   // +3 characters, and we want to make sure there's room for both. 
   if (inString.length() && inString.length() < (string::npos - 6))
   {
      const string WS(" \t");
      const string::size_type firstWS = inString.find_first_of(WS);
      const string::size_type subLoc = inString.find('_');
      if (subLoc < firstWS)
      {
         outString = inString.substr(0, subLoc);
         outString += "\\d";
         outString += inString.substr(subLoc+1, firstWS-(subLoc+1));
         outString += "\\u";
      }
      else
         // Underscore either doesn't exist at all or is in the 2nd
         // portion of inString.
         outString = inString.substr(0,firstWS);

      if (firstWS != string::npos)
      {
         const string::size_type unitLoc = inString.find_first_not_of(WS,firstWS);
         if (unitLoc != string::npos)
         {
            // Pick up the whitespace between the 2 portions.
            outString += inString.substr(firstWS, unitLoc-firstWS);
            const string::size_type superLoc = inString.find('^',unitLoc);
            if (superLoc != string::npos)
            {
               outString += inString.substr(unitLoc, superLoc-unitLoc);
               outString += "\\u";
               string::size_type endLoc = inString.find(')');
               // If ')' doesn't exist, this will still work.
               outString += inString.substr(superLoc+1, endLoc-(superLoc+1));
               outString += "\\d";
               if (endLoc < inString.length())
               {
                  outString += inString.substr(endLoc);
               }

            }
            else
               outString += inString.substr(unitLoc);
         }
      }
   }

   return outString;
}

std::vector<PlotGroup*> PlotCommand::makePlotGroups (const PlotSettings& settings, const std::vector<const PlotGroup*>& donatedPlotGroups)
{
   // Default version: don't use any donated plot groups.
   return m_plotGroupStrategy->createPlotGroups(settings);
}

// Additional Declarations
