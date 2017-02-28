//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// PlotSettings
#include <XSPlot/Plot/PlotSettings.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Parse/XSparse.h>
#include <sstream>


// Class PlotSettings::RebinInfo 

// Class PlotSettings 

PlotSettings::PlotSettings()
  : m_xOption(CHANNELS),
    m_userCommands(),
    m_prevGroupRange(0,0),
    m_temperature(1.0),
    m_emisLimit(1.0e-18),
    m_redshiftLinesToObs(.0),
    m_redshiftToSource(.0),
    m_IDLowEnergy(.0),
    m_IDHighEnergy(.0),
    m_groupsRebinInfo(),
    m_lastRebinEntry(),
    m_splashPage(true),
    m_xLog(true),
    m_yLog(false),
    m_energyUnit(), // Leave it to PlotDirector to set initial units.
    m_waveUnit(),
    m_isWavePerHz(true),
    m_divideByArea(false,false),
    m_showAddComponent(false,0),
    m_showBackground(false,false),
    m_showLineIDs(false,false),
    m_spectra(),
    m_isInteractive(false),
    m_saveArrayInfo(PlotSettings::NO_SAVE, 1),
    m_labelNames(), // Filled from PlotDirector
    m_badDataValue(.0), // Filled from PlotDirector
    m_contBackImage(true) 
{
  RebinInfo initEntry;
  initEntry.sigma = 0; initEntry.maxBins = 1; initEntry.mode = STD;
  m_lastRebinEntry = std::pair<int, RebinInfo>(-1, initEntry);
  m_groupsRebinInfo.insert(m_lastRebinEntry);
}


PlotSettings::~PlotSettings()
{
}


void PlotSettings::setXOption (const string& option)
{
  if (option == "channel")
  {
     m_xOption = CHANNELS;
     SpectralData::chanEngWave(0);
  }
  else if (option == "energy")
  {
     m_xOption = ENERGY;
     SpectralData::chanEngWave(1);
  }
  else if (option == "wavelength")
  {
     m_xOption = WAVELENGTH;
     SpectralData::chanEngWave(2);
  }
  else throw RedAlert(" Programmer Error: X-axis mode");
}

int PlotSettings::addUserCommand (const string& command)
{
  m_userCommands.push_back(command);
  return static_cast<int>(m_userCommands.size());
}

void PlotSettings::removeUserCommand (int numCmd)
{
  if (numCmd > static_cast<int>(m_userCommands.size()))
  {
     std::ostringstream msg;
     msg << "Cannot delete command number " << numCmd << " because it does not exist.\n";
     throw YellowAlert(msg.str());
  }

  std::list<std::string>::iterator iC = m_userCommands.begin();

  for (int i=1; i<numCmd; ++i)
  {
     ++iC;
  }
  m_userCommands.erase(iC);  
}

void PlotSettings::removeUserCommandRange (int firstCmd, int lastCmd)
{
  if (lastCmd > static_cast<int>(m_userCommands.size()) || firstCmd < 1)
  {
     std::ostringstream msg;
     msg << "Cannot delete command range " << firstCmd << " to " << lastCmd 
	 << " because valid range is 1 to " << m_userCommands.size() << ".\n";
     throw YellowAlert(msg.str());
  }

  std::list<std::string>::iterator iCfirst = m_userCommands.begin();
  std::list<std::string>::iterator iClast = m_userCommands.begin();

  for (int i=1; i<firstCmd; ++i) ++iCfirst;
  for (int i=1; i<=lastCmd; ++i) ++iClast;

  // note that the list erase method does not erase the last iterator position
  // given so had to go one beyond lastCmd

  m_userCommands.erase(iCfirst, iClast);  
}

int PlotSettings::numberUserCommands ()
{
  return m_userCommands.size();
}

void PlotSettings::setPlotGroupNums (const string& rangesString)
{
    using namespace XSContainer;

    size_t sz (datasets->numberOfSpectra());
    if (!sz)
    {
       tcout << "No spectra loaded, cannot assign plot group numbers."<<std::endl;
       return;
    }
    if (!m_prevGroupRange.first)  m_prevGroupRange.first = 1;
    if (!m_prevGroupRange.second) m_prevGroupRange.second = sz;
    std::pair<size_t,size_t> limits = std::make_pair(1,sz);

    string::size_type iSpace = rangesString.find_first_of(' ',0);
    string::size_type prevSpace = 0;
    while(prevSpace != string::npos)
    {
	string subString = rangesString.substr(prevSpace, iSpace-prevSpace);
	std::pair<size_t,size_t> range = 
                XSparse::wildRange(subString, limits, m_prevGroupRange);
        size_t begin = range.first;
        size_t end = range.second;

	if (begin > sz || end > sz)
	{
	    std::ostringstream msg;
	    if (begin == end)
	    {
		msg <<'\n'<< begin <<" is out of range of existing spectra";
	    }
	    else
	    {
		msg <<'\n' << begin << '-' << end 
		    << " extends out of range of existing spectra";
	    }   
	    throw XSparse::InvalidRange(msg.str());
	}

	// Begin and end are 1-based.

	// Check that the bins match up before attempting to group
	// spectra together.
	bool isOKtoGroup = true;
	SpectralData* sd=datasets->lookup(begin);

	for (size_t i=begin + 1; i <= end; ++i)
	{
	    SpectralData* sdata = datasets->lookup(i);

	    if (!sdata->energiesEqual(sd))
	    {
		tcout << "***Warning:  Spectra " << begin << '-' << end
		      << " do not contain the same energy bins and/or channels.\n"
		      << "   They will not be grouped.\n";
		isOKtoGroup = false; 
		break;
	    }
	}

	if (isOKtoGroup)
	{     
	    // Find highest group number prior to spectrum[begin-1].
	    int high = 0;
	    for (size_t i = 0; i < begin-1; ++i)
	    {
		if (datasets->plotGroupNums(i) > high)
		{
		    high = datasets->plotGroupNums(i);
		}
	    }     
	    // Assign spectra begin-1 to end-1 their new group number.
	    ++high;
	    for (size_t i = begin; i <= end; ++i)
	    {
		datasets->setPlotGroupNums(i-1, high);
	    }     
	    // Now correct the later spectra accordingly.
	    datasets->renumberPlotGroups(end+1, high);
	}     
	// prevSpace is actually set here to the next char after the space.
	prevSpace = rangesString.find_first_not_of(' ', iSpace);
	iSpace = rangesString.find_first_of(' ', prevSpace);
    }
}

void PlotSettings::setIDs (const std::vector<Real>& values)
{
   m_showLineIDs.first = true;
   size_t sz = values.size();
   if (sz)  m_temperature = values[0];
   if (sz > 1)  m_emisLimit = values[1];
   if (sz > 2)  m_redshiftLinesToObs = values[2];
   if (sz > 3)  m_IDLowEnergy = values[3];
   if (sz > 4)  m_IDHighEnergy = values[4];
}

void PlotSettings::showUserCommands () const
{
  if (m_userCommands.empty())
  {
     tcout << "No plot commands have been entered." <<std::endl;
  }
  else
  {
     std::list<string>::const_iterator iC = m_userCommands.begin();
     std::list<string>::const_iterator iCEnd = m_userCommands.end();
     int i = 1;
     while (iC != iCEnd)
     {
        tcout << "   " << i << ": " << *iC << std::endl;
        ++i, ++iC;
     }
  }
}

void PlotSettings::ungroupAll ()
{
  using namespace XSContainer;

  int sz = datasets->numberOfSpectra();
  for (int i=0; i<sz; ++i)
  {
     datasets->setPlotGroupNums(i, i+1);
  }
  // This call is needed to update datasets' m_numberOfPlotGroups.
  datasets->renumberPlotGroups(1, 0);
}

void PlotSettings::initializeSpectra ()
{
  m_spectra.clear();

  size_t N (XSContainer::datasets->numberOfSpectra());
  for ( size_t j = 1; j <= N; ++j)
  {
     SpectralData* sp (XSContainer::datasets->lookup(j));       
     if (sp)
     {
        m_spectra.insert(SpecGroup::value_type(sp->plotGroup(),sp));
     }
     else
     {
        throw RedAlert(" data container is corrupted: missing spectra ");
     }   
  }
}

bool PlotSettings::showAddComponent () const
{
   return (m_showAddComponent.second > 2 || (m_showAddComponent.first &&
                m_showAddComponent.second > 0));
}

bool PlotSettings::showFoldedAddComponent () const
{
   return (m_showAddComponent.second > 3 || (m_showAddComponent.first &&
                m_showAddComponent.second == 2));
}

const string& PlotSettings::getUnitID (XaxisMode mode) const
{
   // Channel mode is irrelevant in this context.
   if (mode == ENERGY)
      return m_energyUnit.first;
   else if (mode == WAVELENGTH)
      return m_waveUnit.first;
   else
   {
      std::ostringstream oss;
      oss << "Programming Error: Unrecognized mode for unit ID in PlotSettings: "
           << mode;
      throw RedAlert(oss.str());
   }
}

const string& PlotSettings::getUnitPlotLabel (XaxisMode mode) const
{
   if (mode == ENERGY)
      return m_energyUnit.second.first;
   else if (mode == WAVELENGTH)
      return m_waveUnit.second.first;
   else
   {
      std::ostringstream oss;
      oss << "Programming Error: Unrecognized mode for unit label in PlotSettings: "
           << mode;
      throw RedAlert(oss.str());
   }
}

Real PlotSettings::getUnitFactor (XaxisMode mode) const
{
   if (mode == ENERGY)
      return m_energyUnit.second.second;
   else if (mode == WAVELENGTH)
      return m_waveUnit.second.second;
   else
   {
      std::ostringstream oss;
      oss << "Programming Error: Unrecognized mode for unit factor in PlotSettings: "
           << mode;
      throw RedAlert(oss.str());
   }
}

string PlotSettings::makeXOptionDependentLabel (string& xLabel) const
{
   string fullKey = "x:";
   switch (m_xOption)
   {
      default:
      case CHANNELS:
	 fullKey += "channel";
	 break;
      case ENERGY:
	 fullKey += "energy";
	 break;
      case WAVELENGTH:
	 fullKey += "wave";
	 break;
   }
   xLabel = lookupLabelName(fullKey);
   insertUnitLabel(xLabel,false);
   return fullKey;
}

void PlotSettings::insertUnitLabel (string& plotLabel, bool checkPerHz) const
{
   // Assuming CHANNEL mode labels have NO unit placeholders, so this 
   // should do nothing to them.
   const string placeholder("$U");
   const string::size_type len = placeholder.length();
   string curUnitLabel;
   if (m_xOption == WAVELENGTH)
   {
      if (checkPerHz && m_isWavePerHz)
         curUnitLabel = "Hz";
      else
         curUnitLabel = m_waveUnit.second.first;
   }
   else
      curUnitLabel = m_energyUnit.second.first;
   string::size_type iPos = plotLabel.find(placeholder);
   while (iPos != string::npos)
   {
      plotLabel.replace(iPos, len, curUnitLabel);
      // Increment iPos to prevent an infinite loop in the extremely
      // unlikely case that curUnitLabel were to equal placeholder.
      ++iPos;
      iPos = plotLabel.find(placeholder, iPos);
   }
}

const string& PlotSettings::lookupLabelName (const string& labelKey) const
{
  std::map<string,string>::const_iterator itLabel = m_labelNames.find(labelKey);
  if (itLabel == m_labelNames.end())
  { 
     string err(" no plot label for key: ");
     err += labelKey + "\n";
     throw YellowAlert(err);
  }
  return itLabel->second;
}

void PlotSettings::xOption (XaxisMode value)
{
  m_xOption = value;
  if (m_xOption == CHANNELS)
  {
     SpectralData::chanEngWave(0);
  }
  else if (m_xOption == ENERGY)
  {
     SpectralData::chanEngWave(1);
  }
  else
  {
     SpectralData::chanEngWave(2);
  }
}

// Additional Declarations
