//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include "XSstreams.h"

// LineList
#include <XSModel/Model/EmissionLines/LineList.h>


// Class LineList::CannotOpen 

LineList::CannotOpen::CannotOpen (const string& msg)
  : YellowAlert(" cannot open line list file named: ")
{
  tcerr << msg << std::endl;
}


// Class LineList::FileFormatError 

LineList::FileFormatError::FileFormatError (const string& msg)
  : YellowAlert("")
{
  tcerr << msg << std::endl;
}


// Class LineList 

std::map<std::string, LineList*> LineList::s_lineLists;

LineList::LineList()
  : m_fileName(""),
    m_energyRange(std::pair<Real,Real>(.0,.0)),
    m_plasmaTemperature(.0),
    m_minEmissivity(.0),
    m_file(0),
    m_isWave(false)
{
}

LineList::LineList(const LineList &right)
  : m_fileName(right.m_fileName),
    m_energyRange(right.m_energyRange),
    m_plasmaTemperature(right.m_plasmaTemperature),
    m_minEmissivity(right.m_minEmissivity),
    m_file(0),
    m_isWave(right.m_isWave)
{
}


LineList::~LineList()
{
  delete m_file;
}


LineList* LineList::get (const std::string& name)
{
  std::map<std::string,LineList*>::iterator il(s_lineLists.find(name));
  if (il != s_lineLists.end()) return (*il).second->clone();
  else return 0;
}

void LineList::registerLineList (const std::string& name, LineList* lineList)
{
  s_lineLists[name] = lineList;
}

void LineList::initialize (Real lowEnergy, Real highEnergy, Real plasmaTemp, Real minEmissivity, bool isWave)
{
  m_energyRange.first = lowEnergy;
  m_energyRange.second = highEnergy;
  m_plasmaTemperature = plasmaTemp;
  m_minEmissivity = minEmissivity;
  m_isWave = isWave;
  openFile();
  readData();
  findLines();
}

void LineList::showList (std::ostream& os)
{
  std::ios_base::fmtflags saveState(os.flags());
  report(os);
  os.flags(saveState);
}

void LineList::openFile ()
{
  using namespace CCfits;
  if (m_file)
  {
     throw RedAlert("LineList: file pointer not properly set.");
  }
  try
  {
     m_file = new FITS(m_fileName);
  }
  catch (FitsException&)
  {
     throw CannotOpen(m_fileName);
  }
}

void LineList::readData ()
{
}

void LineList::findLines ()
{
}

void LineList::report (std::ostream& os) const
{
}

void LineList::clearLineLists ()
{
  std::map<std::string,LineList*>::iterator itLl = s_lineLists.begin();
  std::map<std::string,LineList*>::iterator itEnd = s_lineLists.end();
  while (itLl != itEnd)
  {
     delete itLl->second;
     ++itLl;
  }
  s_lineLists.clear();
}

// Additional Declarations
