//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSUtil/Numerics/Numerics.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSUtil/FunctionUtils/xsFortran.h>
#include "XSstreams.h"
#include "XSContainer.h"
#include <sstream>
#include <fstream>

// Apec
#include <XSModel/Model/EmissionLines/Apec.h>


// Class Apec 
const string Apec::s_KT = "kT";
const string Apec::s_LAMBDA = "Lambda";
const string Apec::s_EPSILON = "Epsilon";
const string Apec::s_ELEMENT = "Element";
const string Apec::s_ION = "Ion";
const string Apec::s_UPPERLEV = "UpperLev";
const string Apec::s_LOWERLEV = "LowerLev";
StringArray Apec::s_colNames;
StringArray Apec::s_symbols;
StringArray Apec::s_ionsyms;

Apec::Apec()
{
  // This should only be entered when constructing the prototype.
  s_colNames.push_back(s_LAMBDA);
  s_colNames.push_back(s_EPSILON);
  s_colNames.push_back(s_ELEMENT);
  s_colNames.push_back(s_ION);
  s_colNames.push_back(s_UPPERLEV);
  s_colNames.push_back(s_LOWERLEV); 

  const char *tmpSyms[] = {"H","He","Li","Be","B","C","N","O","F","Ne","Na","Mg","Al",
                           "Si","P","S","Cl","Ar","K","Ca","Sc","Ti","V","Cr","Mn","Fe",
                           "Co","Ni","Cu","Zn"};
  s_symbols.resize(30);
  for (size_t i=0; i<30; ++i)
  {
     s_symbols[i] = string(tmpSyms[i]);
  }

  const char *tmpIons[] = {"I","II","III","IV","V","VI","VII","VIII","IX","X",
			   "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX",
			   "XX","XXI","XXII","XXIII","XXIV","XXV","XXVI","XXVII",
                           "XXVIII","XXIX","XXX","XXXI"};
  s_ionsyms.resize(31);
  for (size_t i=0; i<31; ++i)
  {
     s_ionsyms[i] = string(tmpIons[i]);
  }   
}

Apec::Apec(const Apec &right)
  : LineList(right)
{
  const string& datadir = FunctionUtility::modelDataPath();
  static string ovalue;
  string pname("APECROOT");
  string pvalue(FunctionUtility::getModelString(pname));

  string version = FunctionUtility::atomdbVersion();
  string apecFile = "apec_v" + version;
  string fullName("");
  const string apecSuffix("_line.fits");

  if ( !pvalue.length() || pvalue == FunctionUtility::NOT_A_KEY()) {
    fullName = datadir + apecFile + apecSuffix;
  } else {
    // if APECROOT has been set first test whether only a version number has
    // changed. Then assume that only a root filename has been given. If neither
    // these work then assume that an entire directory path was included.

    fullName = datadir + "apec_v" + pvalue + apecSuffix;
    std::ifstream test1(fullName.c_str());
    if (!test1) {
      fullName = datadir + pvalue + apecSuffix;
      std::ifstream test2(fullName.c_str());
      if (!test2) {
	fullName = pvalue + apecSuffix;            
      }
    }
    if (ovalue != pvalue) {
      ovalue = pvalue;
      string msg("Reading APEC line data from " + fullName);
      xs_write(const_cast<char*>(msg.c_str()), 10);
    }
  }
  fileName(fullName);     
}


Apec::~Apec()
{
}


LineList* Apec::clone () const
{
  return new Apec(*this);
}

void Apec::readData ()
{
  using namespace CCfits;
  if (!file())
  {
     throw RedAlert("Attempting to read from unopened Line List file.");
  }
  long nRows=0;
  RealArray kT;
  try 
  {
     // Following xspec11, find parameter extension by index number.
     ExtHDU& parameters = file()->extension(1);
     nRows = parameters.rows();
     if (!nRows)
     {
        string msg("Error finding the required data in parameters extension of file: ");
        throw FileFormatError(msg + fileName());
     }
     parameters.column(s_KT).read(kT, 1, nRows);
  }
  catch (FitsException&)
  {
     string msg("Error finding the required data in parameters extension of file: ");
     throw FileFormatError(msg + fileName());
  }
  // Find energy closest to input plasma temperature.
  RealArray diffArray = std::abs(kT - plasmaTemperature());
  Real minDiff = diffArray[0];
  size_t closestE=0;
  for (size_t i=1; i < static_cast<size_t>(nRows); ++i)
  {
     if (diffArray[i] < minDiff)
     {
        minDiff = diffArray[i];
        closestE = i;
     }
  }
  // Get the corresponding emissivity extension and its data.
  try
  {
     ExtHDU& emissivity = file()->extension(closestE+2);
     nRows = emissivity.rows();
     if (!nRows)
     {
        std::ostringstream msg;
        msg << "Error finding data in emissivity extension index # " 
            << closestE+2 << "\n in file: " << fileName();
        throw FileFormatError(msg.str());
     }
     emissivity.readData(true, s_colNames);
     emissivity.column(s_LAMBDA).read(m_lambda, 1, nRows);
     if (!isWave())
     {
        m_lambda = Numerics::KEVTOA/m_lambda;
     }
     emissivity.column(s_EPSILON).read(m_epsilon, 1, nRows);
     emissivity.column(s_ELEMENT).read(m_element, 1, nRows);
     emissivity.column(s_ION).read(m_ion, 1, nRows);
     emissivity.column(s_UPPERLEV).read(m_upperlev, 1, nRows);
     emissivity.column(s_LOWERLEV).read(m_lowerlev, 1, nRows);
  }
  catch (FitsException&)
  {
     std::ostringstream msg;
     msg << "Error finding data in emissivity extension index # " << closestE+2
         << "\n in file: " << fileName();
     throw FileFormatError(msg.str());
  } 
}

void Apec::report (std::ostream& os) const
{
  using namespace std;
  size_t nSyms = s_symbols.size();
  size_t nIonSyms = s_ionsyms.size();
  os << std::endl;
  size_t sz = m_iRows.size();
  for (size_t i=0; i<sz; ++i)
  {
     int iRow = m_iRows[i];
     // NOTE:  m_element and m_ion values ARE 1-BASED!
     string symbol = (m_element[iRow] <= (int)nSyms && m_element[iRow] >= 1) ?
                   s_symbols[m_element[iRow]-1] : string("***");
     string ionSym = (m_ion[iRow] <= (int)nIonSyms && m_ion[iRow] >= 1) ?
                   s_ionsyms[m_ion[iRow]-1] : string("***");
     os << setw(20) << left <<"   APEC" << ": " << right << setw(8) << fixed <<
           showpoint << setprecision(4) << m_lambda[iRow] << setw(7) <<
           symbol << " " << setw(6) << left << ionSym << right << setw(5) << 
           m_upperlev[iRow] << setw(5) << m_lowerlev[iRow] << setw(15) << 
           scientific << setprecision(3) << m_epsilon[iRow] << std::endl;
  }
}

void Apec::findLines ()
{
  Real lowE = energyRange().first;
  Real highE = energyRange().second;
  Real minEmiss = minEmissivity();
  size_t sz = m_lambda.size();
  m_iRows.clear();
  for (size_t i=0; i<sz; ++i)
  {
     if (m_lambda[i] >= lowE && m_lambda[i] <= highE &&
                m_epsilon[i] >= minEmiss)
     {
        m_iRows.push_back(i);
     }
  }
}

// Additional Declarations
