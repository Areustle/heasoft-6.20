//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSstream.h>
#include <sstream>

// Chain
#include <XSFit/MCMC/Chain.h>
// AsciiChain
#include <XSFit/MCMC/AsciiChain.h>



// Class AsciiChain 
const size_t AsciiChain::s_LENGTHFIELDWIDTH = 14;
const int AsciiChain::s_TEMPERFIELDWIDTH = 200;
const string AsciiChain::s_FORMATNAME = string("ascii");
const string AsciiChain::s_CHAINTYPE = string("!Type:");
const string AsciiChain::s_NWALKERS = string("!NWalkers:");

AsciiChain::AsciiChain (string fileName)
  : m_file(),
    m_lengthInfoPos(0),
    m_pointsStartPos(0),
    m_temperInfoPos(-1),
    m_chain(0) // non-owning
{
   if (fileName.length())
   {
      // We are attempting to load an already existing chain file,
      // but we don't yet know if its format is Ascii.
      // This may throw.
      testFile(fileName);      
   }
}


AsciiChain::~AsciiChain()
{
}


void AsciiChain::doOpenForReadPoints () const
{
  using namespace std;
  m_file.open(m_chain->getFileName().c_str(), ios_base::in);
  if (!m_file)
  {
     string msg("Unable to open chain file ");
     msg += m_chain->getFileName().c_str();
     throw ChainIOError(msg);
  }
  m_file.seekg(m_pointsStartPos);
}

void AsciiChain::doCloseFile () const
{
  if (m_file.is_open())
     m_file.close();  
}

void AsciiChain::doOpenForWrite ()
{
  // Not checking results of open again.  It should already
  // have been checked.
  m_file.open(m_chain->getFileName().c_str(), 
        std::ios_base::in|std::ios_base::out);
}

void AsciiChain::doReadPoint (std::vector<Real>& chainVals) const
{
  // Assumes file is open and currently positioned at the 
  // proper place - the start of a line of nVal chain vals.
  const size_t nVals = m_chain->width();
  chainVals.resize(nVals, .0);
  for (size_t i=0; i<nVals; ++i)
  {
     m_file >> chainVals[i];
  }
  if (!m_file)
  {
     throw ChainIOError("Format error detected while reading chain values.");
  }
}

void AsciiChain::doReadPointFromLine (size_t lineNum, RealArray& parVals) const
{
   // Assumes m_file is already open and parVals is the proper size.
   const size_t nPars = parVals.size();
   if (nPars+1 != m_chain->width())
   {
      throw RedAlert("Parameter array size mismatch with chain file.");
   }
   moveToLine(lineNum);
   // This is more complicated than just streaming directly from
   // m_file to parVals, but want to make sure all vals are 
   // indeed coming from the same line in the file.
   string valsLineStr;
   std::getline(m_file, valsLineStr);
   std::istringstream valsLine(valsLineStr);
   for (size_t i=0; i<nPars; ++i)
   {
      if (!(valsLine >> parVals[i]))
      {
         std::ostringstream errMsg;
         errMsg <<"Unable to read par values at values line " << lineNum
            <<".\n   Chain file format is wrong.";
         throw ChainIOError(errMsg.str());
      }
   }
}

void AsciiChain::doReadPointFromLine (size_t lineNum, RealArray& parVals, Real& statVal) const
{
   // Assumes m_file is already open and parVals is the proper size.
   const size_t nPars = parVals.size();
   if (nPars+1 != m_chain->width())
   {
      throw RedAlert("Parameter array size mismatch with chain file.");
   }
   moveToLine(lineNum);
   // This is more complicated than just streaming directly from
   // m_file to parVals, but want to make sure all vals are 
   // indeed coming from the same line in the file.
   string valsLineStr;
   std::getline(m_file, valsLineStr);
   std::istringstream valsLine(valsLineStr);
   for (size_t i=0; i<nPars; ++i)
   {
      if (!(valsLine >> parVals[i]))
      {
         std::ostringstream errMsg;
         errMsg <<"Unable to read par values at values line " << lineNum
            <<".\n   Chain file format is wrong.";
         throw ChainIOError(errMsg.str());
      }
   }
   if (!(valsLine >> statVal))
   {
      std::ostringstream errMsg;
      errMsg <<"Unable to read stat value at values line " << lineNum
         <<".\n   Chain file format is wrong.";
      throw ChainIOError(errMsg.str());
   }
}

const string& AsciiChain::getFormatName () const
{
   return s_FORMATNAME;
}

void AsciiChain::readFileInfo ()
{
  m_file.open(m_chain->getFileName().c_str(), std::ios_base::in);
  if (!m_file)
  {
     string msg("Unable to open chain file ");
     msg += m_chain->getFileName();
     throw ChainIOError(msg);
  }

  try
  {
     string paramLine;
     // Swallow message in 1st 2 lines:
     std::getline(m_file, paramLine);
     std::getline(m_file, paramLine);
     paramLine.erase();
     string testStr;
     m_file >> testStr;
     if (!testStr.length() || testStr[0] != '!') throw YellowAlert();
     if (testStr.substr(1) != string(ChainIO::LENGTHLABEL()+":")) 
                throw YellowAlert();
     size_t chainLength=0;
     m_lengthInfoPos = m_file.tellg();
     m_file >> chainLength >> testStr;
     if (!chainLength)
        throw YellowAlert("Chain length must be > 0\n");
     m_chain->length(chainLength);

     if (testStr != string(ChainIO::WIDTHLABEL()+":")) 
        throw YellowAlert();
     size_t chainWidth=0;
     m_file >> chainWidth;
     if (!chainWidth)
        throw YellowAlert("Chain width must be > 0\n");
     m_chain->width(chainWidth);

     // Need to find only whitespace then '\n', else throw.
     char c = ' ';
     while (isspace(c))
     {
        m_file.get(c);
        if (c == '\n') break;
     }
     if (c != '\n') throw YellowAlert();
     
     // Allowed formats:
     // G-W: Type
     //      Walkers
     //      Parameters
     // M-H: [Type]
     //      [Tempering]
     //      Parameters
     std::fstream::pos_type startPos = m_file.tellg();
     std::getline(m_file, testStr);
     if (testStr.find(s_CHAINTYPE) == 0)
     {
        std::istringstream iss(testStr.substr(s_CHAINTYPE.length()));
        string chType;
        iss >> chType;
        if (chType != "GoodmanWeare" && chType != "MetropolisHastings")
           throw YellowAlert();
        m_chain->chainType(chType);
        if (chType == "GoodmanWeare")
        {
           std::getline(m_file, testStr);
           if (testStr.find(s_NWALKERS) != 0)
              throw YellowAlert();
           size_t nWalkers=0;
           std::istringstream iss2(testStr.substr(s_NWALKERS.length()));
           iss2 >> nWalkers;
           if (!nWalkers)
              throw YellowAlert("Improper nWalkers value\n");
           m_chain->walkers(nWalkers);
        }
     }
     else
     {
        m_chain->chainType("MetropolisHastings");
        m_file.seekg(startPos);       
     }
     
     if (m_chain->chainType() == "MetropolisHastings")
     {
        // Get tempering info.  If it's not there (older files),
        // this should return the next line containing
        // parameter info.
        paramLine = readTemperInfo();      
     }
     // 3nd line, get param info (if it hasn't been retrieved above).
     std::vector<Chain::ParamID> paramIDs;
     if (!paramLine.length())
        std::getline(m_file, paramLine);
     m_pointsStartPos = m_file.tellg();
     if (!paramLine.length() || paramLine[0] != '!') throw YellowAlert();
     const string WS(" \n\t");
     string::size_type start = 1;
     while (start != string::npos)
     {
        string::size_type end = paramLine.find_first_of(WS, start);
        string::size_type n = (end == string::npos) ? string::npos : end-start;
        string tmp(paramLine.substr(start, n));
        string modName;
        size_t index=0;
        Chain::ParamID inPar;
        if (!XSparse::stringIntPair(tmp, modName, index))
        {
	    //could be the fit statistic...
	    if (!index)
	    {
		//Note: Don't assume anything about the format of the fit
		//statistic. Could contain spaces and other chars. The only
		//character that it should not contain is a colon - check.

		//assume EVERYTHING from the start of the line until the end
		//is the name of the fit statistic
		if(end != string::npos)
		    tmp = paramLine.substr(start);

		//if a colon is found, then there is a format error
		if(tmp.find(":") != string::npos)
		    throw YellowAlert();
		else
		{
		    //fit statistic should be the last column. so break
                    m_chain->statistic(tmp);
		    start = string::npos;
		    break;
		}
	    }
	    inPar.modName = Model::DEFAULT();
	    inPar.index = index;
        }
        else if (index)
        {
           inPar.modName = modName;
           inPar.index = index;
        }
        else
           throw YellowAlert();
        start = paramLine.find_first_not_of(WS, end);
        if (start == string::npos) throw YellowAlert();
        end = paramLine.find_first_of(WS, start);
        if (end == string::npos) throw YellowAlert();
        n = end-start;
        inPar.parName = paramLine.substr(start, n);
        start = paramLine.find_first_not_of(WS, end);
        if (start == string::npos) throw YellowAlert();
        end = paramLine.find_first_of(WS, start);        
        n = (end == string::npos) ? string::npos : end-start;
        string tmpUnit = paramLine.substr(start, n);
        inPar.units = (tmpUnit == "0" ? string("") : tmpUnit);

        paramIDs.push_back(inPar);
        start = paramLine.find_first_not_of(WS, end);
     }
     if (paramIDs.size() != m_chain->width()-1) 
        throw YellowAlert("Mismatch between Width setting and parameter labels.\n");
     m_chain->paramIDs(paramIDs);
     m_file.close();
  }
  catch (...)
  {
     m_file.close();
     string msg("Format error in chain file ");
     msg += m_chain->getFileName();
     throw ChainIOError(msg);
  }

}

void AsciiChain::createFile ()
{
  m_file.open(m_chain->getFileName().c_str(), std::ios_base::out);
  if (!m_file)
  {
     string msg("Unable to create output file ");
     msg += m_chain->getFileName();
     throw ChainIOError(msg);
  }
  tcout << xsverbose(25)<<"Writing chain to " << m_chain->getFileName()
     << std::endl << xsverbose();
  m_file << std::setprecision(ChainIO::PRECISION()) << std::right;
}

void AsciiChain::writePoint ()
{
  using namespace XSContainer;
  // Function assumes m_file is open and ready for writing.  
  // It also assumes it has already had its precision 
  // and justification set.
  // NOTE: If anything changes in here, readPointFromLine
  // may need a corresponding readjustment.
  const std::map<int,ModParam*>& varPars = fit->variableParameters();
  std::map<int,ModParam*>::const_iterator it = varPars.begin();
  std::map<int,ModParam*>::const_iterator itEnd = varPars.end();
  while (it != itEnd)
  {
     m_file << std::setw(ChainIO::FWIDTH()) << it->second->value();
     ++it;
  }
  m_file << std::setw(ChainIO::FWIDTH()) << fit->statistic() << std::endl;
}

void AsciiChain::writePoint (RealArray& paramVals, Real& statVal)
{
  // Function assumes m_file is open and ready for writing.  
  // It also assumes it has already had its precision 
  // and justification set.
  // NOTE: If anything changes in here, readPointFromLine
  // may need a corresponding readjustment.
  for (size_t i=0; i<paramVals.size(); i++)
  {
    m_file << std::setw(ChainIO::FWIDTH()) << paramVals[i];
  }
  m_file << std::setw(ChainIO::FWIDTH()) << statVal << std::endl;
}

void AsciiChain::writeFileInfo ()
{
  using namespace std;

  m_file << "!" << " Markov chain file generated by xspec \"chain\" command."
         << "\n!" << "    Do not modify, else file may not reload properly."
         << "\n!" << ChainIO::LENGTHLABEL() << ":" << flush;
  // m_length needs a fixed field width in case we want to modify it
  // later in adjustLengthInfo.
  m_lengthInfoPos = m_file.tellp();
  m_file << right << setw(s_LENGTHFIELDWIDTH) << m_chain->length() << "  " 
                << ChainIO::WIDTHLABEL() << ": " << m_chain->width() << endl;
  m_file << s_CHAINTYPE << "  " << m_chain->chainType() <<endl;              
  if (m_chain->chainType() == "GoodmanWeare")
  {
     m_file << s_NWALKERS << "  " << m_chain->walkers() << endl;
  }
  else if (m_chain->chainType() == "MetropolisHastings")
  {
     m_file << "!" << ChainIO::TEMPERLABEL() << ":";
     m_temperInfoPos = m_file.tellp();
     adjustTemperInfo();
  }              
                
  // output param IDs
  m_file << "!";
  const vector<Chain::ParamID>& parIDs = m_chain->paramIDs();
  size_t nPars = parIDs.size();
  for (size_t i=0; i<nPars; ++i)
  {
     const Chain::ParamID& parID = parIDs[i];
     if (parID.modName != Model::DEFAULT())
     {
        m_file << parID.modName << ':';
     }
     m_file << parID.index << " " << parID.parName << " ";
     string units = parID.units.length() ? parID.units : string("0");
     m_file << units << "   ";
  }

  const StatMethod* statMeth = XSContainer::fit->statManager()->usingSingleStat();
  const string statName = statMeth ? statMeth->fullName() : string("Total_Stat");
  m_file << statName << endl;
  m_pointsStartPos = m_file.tellp();
}

void AsciiChain::adjustLengthInfo ()
{
   // This assumes m_lengthInfoPos has already been set to the
   // location of the length value entry in the file.  
   m_file.seekp(m_lengthInfoPos, std::ios_base::beg);
   m_file << std::right << std::setw(s_LENGTHFIELDWIDTH) 
        << m_chain->length() << std::flush;
}

void AsciiChain::moveToLine (const size_t lineNum) const
{
   m_file.seekg(m_pointsStartPos);
   if (!m_file)
   {
      string errMsg("Chain file format error in ");
      errMsg += m_chain->getFileName();
      throw ChainIOError(errMsg);
   }
   // The current pos in file should now be the start of the first line
   // of parameter vals.  WARNING: The step below ASSUMES each line of 
   // values is strictly formatted by writePoint.  This is to avoid 
   // having to do a line-by-line read to get to the point of interest.
   // In each line there should be nPar+1 values including the fit statistic,
   // plus a newline character.
   std::fstream::off_type offset = lineNum*(ChainIO::FWIDTH()*m_chain->width() + 1); 
   if (offset != 0)
   {
      // We want this to land at the preceding newline char to
      // do a bare minimum check.
      m_file.seekg(offset-1, std::ios_base::cur);
      if (!m_file)
      {
         std::ostringstream errMsg;
         errMsg <<"Unable to move to line " << lineNum 
            <<" in chain file " << m_chain->getFileName();
         throw ChainIOError(errMsg.str());
      }
      char testChar(' ');
      m_file.get(testChar);
      if (testChar != '\n')
      {
         std::ostringstream errMsg;
         errMsg <<"Unable to move to line " << lineNum
            <<".\n   No newline character detected before this line.";
         throw ChainIOError(errMsg.str());
      }
   }
}

void AsciiChain::appendFile (RealArray& startingVals)
{
  using namespace std;
  using namespace XSContainer;
  // ASSUMES calling function has already saved the original
  // parameter values, and so is capable of restoring
  // them.  

  // Starting point should be the last set of values in the file.
  m_file.open(m_chain->getFileName().c_str(), ios_base::in);
  if (!m_file)
  {
     string msg("Unable to open file for reading to begin append operation: ");
     msg += m_chain->getFileName();
     throw ChainIOError(msg);
  }
  startingVals.resize(m_chain->width()-1, .0);
  try
  {
     doReadPointFromLine(m_chain->length()-1, startingVals);
  }
  catch (YellowAlert&)
  {
     m_file.close();
     throw;
  }
  m_file.close();

  // Now reopen file for appending.
  m_file.open(m_chain->getFileName().c_str(), ios_base::app|ios_base::out);
  if(!m_file)
  {
      string msg("Unable to open chain file for appending: ");
      msg += m_chain->getFileName();
      throw ChainIOError(msg);
  }
  m_file << setprecision(ChainIO::PRECISION()) << right;
}

void AsciiChain::appendFile (RealArray& startingVals, Real& startingStatVal)
{
  using namespace std;
  using namespace XSContainer;
  // ASSUMES calling function has already saved the original
  // parameter values, and so is capable of restoring
  // them.  

  // Starting point should be the last set of values in the file.
  m_file.open(m_chain->getFileName().c_str(), ios_base::in);
  if (!m_file)
  {
     string msg("Unable to open file for reading to begin append operation: ");
     msg += m_chain->getFileName();
     throw ChainIOError(msg);
  }
  startingVals.resize(m_chain->width()-1, .0);
  try
  {
    doReadPointFromLine(m_chain->length()-1, startingVals, startingStatVal);
  }
  catch (YellowAlert&)
  {
     m_file.close();
     throw;
  }
  m_file.close();

  // Now reopen file for appending.
  m_file.open(m_chain->getFileName().c_str(), ios_base::app|ios_base::out);
  if(!m_file)
  {
      string msg("Unable to open chain file for appending: ");
      msg += m_chain->getFileName();
      throw ChainIOError(msg);
  }
  m_file << setprecision(ChainIO::PRECISION()) << right;
}

void AsciiChain::adjustTemperInfo ()
{
   // This assumes m_temperInfoPos has already been set to the
   // location of the start of tempering array vals in the file.
   // Also assume required field width has already been checked 
   // to be less than s_TEMPERFIELDWIDTH.
   using namespace std;
   ios_base::fmtflags saveFmt(m_file.flags());
   streamsize savePrec(m_file.precision());  
   m_file.seekp(m_temperInfoPos);
   m_file.precision(ChainIO::TEMPERPREC());
   const Chain::TemperInfoContainer& tempers = m_chain->tempering();
   for (size_t i=0; i<tempers.size(); ++i)
   {
      m_file << " " << tempers[i].first << " "
                << scientific << tempers[i].second;
   }
   m_file << flush;
   int nPadding = s_TEMPERFIELDWIDTH - 
        static_cast<int>(m_file.tellp() - m_temperInfoPos);
   if (nPadding < 0)
      throw RedAlert("Tempering info output field width error.");
   for (int i=0; i<nPadding; ++i)
      m_file.put(' ');
   m_file << "\n" << flush;
   m_file.precision(savePrec);
   m_file.flags(saveFmt);
}

bool AsciiChain::checkTemperField (const Real temperature) const
{
   using namespace std;
   ostringstream testStream;
   testStream.precision(ChainIO::TEMPERPREC());
   const Chain::TemperInfoContainer& tempers = m_chain->tempering();
   for (size_t i=0; i<tempers.size(); ++i)
   {
      testStream << " " << tempers[i].first << " "
              << scientific << tempers[i].second;
   }
   testStream << " " << m_chain->length() + 1 << " " << scientific 
                << temperature << flush;
   bool willFit = (testStream.str().length() <= (size_t)s_TEMPERFIELDWIDTH);
   return willFit;
}

string AsciiChain::readTemperInfo ()
{
   // This assumes file is positioned at the start of the temperature
   // info line.
   // If the parametersLine return string has any length, it will 
   // indicate that this is an older chain file format which has no 
   // tempering information.  m_temperInfoPos will be left at -1
   // in this case.
   string tempLine;
   string parametersLine;
   std::fstream::pos_type orgPos = m_file.tellg();
   std::getline(m_file, tempLine);
   string indicator("!");
   indicator += ChainIO::TEMPERLABEL() + ":";
   if (tempLine.find(indicator) != 0)
   {
      parametersLine = tempLine;
      // No temp field, so can only use default temp value.
      Chain::TemperInfoContainer defaultTemp;
      defaultTemp.push_back(std::make_pair((size_t)1,(Real)1.0));
      m_chain->tempering(defaultTemp);
   }
   else
   {
      // This line should contain tempering information.
      tempLine = tempLine.substr(indicator.length());
      if (!tempLine.length())
      {
         string errMsg("File contains Tempering label with no values: ");
         errMsg += m_chain->getFileName();
         throw ChainIOError(errMsg);
      }
      Chain::TemperInfoContainer temperArray;
      std::istringstream iss(tempLine);
      while (iss)
      {
         size_t startLength = 0;
         Real temperature = 0.0;
         iss >> startLength;
         if (!iss || !startLength)
         {
            string errMsg("Tempering line format error in file: ");
            errMsg += m_chain->getFileName();
            throw ChainIOError(errMsg);
         }
         iss >> temperature;
         // iss could be at eof, which is OK
         if (temperature == 0.0)
         {
            string errMsg("Tempering line format error in file: ");
            errMsg += m_chain->getFileName();
            throw ChainIOError(errMsg);
         }
         temperArray.push_back(std::make_pair(startLength, temperature)); 

         // Swallow any whitespace after val.  This is necessary
         // in case this is the last temperature val, where trailing
         // whitespace would cause another loop iteration and hence
         // failure during the startLength read attempt.
         char c;
         while (iss.get(c)) 
         {
            if (!std::isspace(c))
            {
               iss.putback(c);
               break;
            }
         }       
      }
      m_chain->tempering(temperArray);
      m_temperInfoPos = orgPos + 
                static_cast<std::streamoff>(indicator.length());
   }

   return parametersLine;
}

void AsciiChain::setParent (Chain* chain)
{
   m_chain = chain;
}

void AsciiChain::testFile (const string& fileName) const
{
  // Very basic test on a pre-existing chain, just intended to 
  // distinguish between an Ascii chain file and a FITS chain file.
  // If it can open the file but does not recognize it as Ascii, 
  // it should throw without printing any error message.
  using namespace std;
  m_file.open(fileName.c_str(), ios_base::in);
  if (!m_file)
  {
     string msg("Unable to open chain file ");
     msg += fileName;
     throw ChainIOError(msg);
  }

  const size_t NCHARS = 9;
  char testChars[NCHARS];
  // The read function does NOT place a trailing 0 in the array.
  testChars[NCHARS-1] = 0;
  m_file.read(testChars,NCHARS-1);
  if (!m_file)
  {
     m_file.close();
     throw YellowAlert();
  }
  const string testStr(testChars);
  if (testStr != string("! Markov"))
  {
     m_file.close();
     throw YellowAlert();
  }
  m_file.close();
}

bool AsciiChain::allowsTemper () const
{
   return (int)m_temperInfoPos != -1;
}

// Additional Declarations
