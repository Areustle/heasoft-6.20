//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <CCfits/CCfits>
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSFit/MCMC/Chain.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSFit/MCMC/FITSChain.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Utils/XSutility.h>
#include <sstream>
#include <fstream>


// Class FITSChain 
const string FITSChain::s_FORMATNAME = string("fits");
const string FITSChain::s_EXTNAME = string("CHAIN");
const string FITSChain::s_TEMPERKEY = string("TEMPR");
const string FITSChain::s_ROWKEY = string("STROW");
const string FITSChain::s_FLOATTYPE = string("D");
const string FITSChain::s_STATCOL = string("FIT_STATISTIC");
const size_t FITSChain::s_TEMPERDIGITS = 3;
const string FITSChain::s_COLNAMEDELIM = string("__");
const string FITSChain::s_CHAINTYP = string("CHAINTYP");
const string FITSChain::s_NWALKERS = string("NWALKERS");

FITSChain::FITSChain (string fileName)
  : m_chain(0), //non-owning
    m_iRow(1),
    m_doFileFlush(false),
    m_fits(0)
{
   if (fileName.length())
   {
      // We are attempting to load an already existing chain file,
      // but we don't yet know if its format is FITS.  This is
      // just a simple test, after which we close the file.
      try
      {
         m_fits = new CCfits::FITS(fileName, CCfits::Read);
         delete m_fits;
         m_fits = 0;
      }
      catch (...)
      {
         throw YellowAlert();
      }
   }
}


FITSChain::~FITSChain()
{
   delete m_fits;
}


void FITSChain::doOpenForReadPoints () const
{
   try
   {
      m_fits = new CCfits::FITS(m_chain->getFileName(), CCfits::Read, s_EXTNAME);
      m_iRow = 1;
   }
   catch (CCfits::FitsException&)
   {
      // Should really never get in here since file ought to have
      // been checked by createFile or readFileInfo.
      std::ostringstream oss;
      oss << "File re-open failure in FITSChain::doOpenForReadPoints: "
         << m_chain->getFileName();
      throw RedAlert(oss.str());
   }
}

void FITSChain::doCloseFile () const
{
   delete m_fits;
   m_fits = 0;
}

void FITSChain::doOpenForWrite ()
{
   try
   {
      m_fits = new CCfits::FITS(m_chain->getFileName(), CCfits::Write, s_EXTNAME);
   }
   catch (CCfits::FitsException&)
   {
      string errMsg("While attempting to open file ");
      errMsg += m_chain->getFileName();
      errMsg += " for writing.";
      throw ChainIOError(errMsg);
   }
}

void FITSChain::doReadPoint (std::vector<Real>& chainVals) const
{
   using namespace CCfits;
   // Assumes m_fits points to a valid FITS object open for reading.
   if (m_iRow > static_cast<long>(m_chain->length()))
   {
      // This can happen if someone were to externally modify
      // the chain file after loading or writing it.  So could
      // lots of other bad things.
      std::ostringstream oss;
      oss << "Attempting to read row " << m_iRow << " during sequential chain read."
        <<"\n   This is beyond end of table in file " << m_chain->getFileName();
      throw ChainIOError(oss.str());
   }
   const size_t nVals = m_chain->width();
   chainVals.resize(nVals, .0);
   fitsfile* fptr = m_fits->currentExtension().fitsPointer();
   int status = 0;
   int anynul = 0;
   for (int i=0; i<static_cast<int>(nVals); ++i)
   {
      fits_read_col_dbl(fptr, i+1, m_iRow, 1, 1, 0, &chainVals[i], &anynul, &status);
   }
   ++m_iRow;
}

void FITSChain::doReadPointFromLine (size_t lineNum, RealArray& parVals) const
{
   // Assumes m_fits is already open and parVals is the proper size.
   const size_t nPars = parVals.size();
   // Convert to 1-Based for CFITSIO
   lineNum += 1;
   long longLineNum = static_cast<long>(lineNum);
   if (nPars+1 != m_chain->width())
   {
      std::ostringstream oss;
      oss << "Number of parameters in file (" << nPars << ") does not match that in the current model (" << m_chain->width()-1 << ").";
      throw RedAlert(oss.str());
   }
   if (lineNum > m_chain->length())
   {
      std::ostringstream oss;
      oss << "Attempting to read row " << lineNum << " during random-access chain read."
         <<"\n   This is beyond end of table in file " << m_chain->getFileName();
      throw ChainIOError(oss.str());
   }
   fitsfile* fptr = m_fits->currentExtension().fitsPointer();
   int status = 0;
   int anynul = 0;
   for (int i=0; i<static_cast<int>(nPars); ++i)
   {
      fits_read_col_dbl(fptr, i+1, longLineNum, 1, 1, 0, &parVals[i], &anynul, &status);
   }
   if (status)
   {
      string errMsg("While attempting to read parameter values from ");
      errMsg += m_chain->getFileName();
      throw ChainIOError(errMsg);
   }
}

void FITSChain::doReadPointFromLine (size_t lineNum, RealArray& parVals, Real& statVal) const
{
   // Assumes m_fits is already open and parVals is the proper size.
   const size_t nPars = parVals.size();
   // Convert to 1-Based for CFITSIO
   lineNum += 1;
   long longLineNum = static_cast<long>(lineNum);
   if (nPars+1 != m_chain->width())
   {
      std::ostringstream oss;
      oss << "Number of parameters in file (" << nPars << ") does not match that in the current model (" << m_chain->width()-1 << ").";
      throw RedAlert(oss.str());
   }
   if (lineNum > m_chain->length())
   {
      std::ostringstream oss;
      oss << "Attempting to read row " << lineNum << " during random-access chain read."
         <<"\n   This is beyond end of table in file " << m_chain->getFileName();
      throw ChainIOError(oss.str());
   }
   fitsfile* fptr = m_fits->currentExtension().fitsPointer();
   int status = 0;
   int anynul = 0;
   for (int i=0; i<static_cast<int>(nPars); ++i)
   {
      fits_read_col_dbl(fptr, i+1, longLineNum, 1, 1, 0, &parVals[i], &anynul, &status);
   }
   fits_read_col_dbl(fptr, nPars+1, longLineNum, 1, 1, 0, &statVal, &anynul, &status);
   if (status)
   {
      string errMsg("While attempting to read parameter values and statistic from ");
      errMsg += m_chain->getFileName();
      throw ChainIOError(errMsg);
   }
}

const string& FITSChain::getFormatName () const
{
   return s_FORMATNAME;
}

void FITSChain::readFileInfo ()
{
  using namespace CCfits;
  string errLabel(s_EXTNAME);
  errLabel += " extension";
  try
  {
     m_fits = new FITS(m_chain->getFileName(), Read, s_EXTNAME);
     ExtHDU& ext = m_fits->extension(s_EXTNAME);
     Table* table = dynamic_cast<Table*>(&ext);
     if (!table)
     {
        string msg(errLabel);
        msg += "is not a table in ";
        msg += m_chain->getFileName();
        throw ChainIOError(msg);
     }

     errLabel = "NAXIS2 keyword";
     // Shouldn't trust Table::rows() since we go into cfitsio
     // directly with column reads and writes.
     int status=0;
     long nRows=0;
     fitsfile* fptr = m_fits->currentExtension().fitsPointer();
     fits_get_num_rows(fptr, &nRows, &status);
     if (!nRows)
        throw YellowAlert("Chain length must be > 0\n");
     if (status)
        throw FitsError(status,true);
     m_chain->length(nRows);

     errLabel = "number of columns";
     m_chain->width(table->column().size());
     if (m_chain->width() < 2)
        throw YellowAlert("Chain tables must have at least 2 columns.\n");

     // For backwards compatibility, don't expect CHAINTYP to exist.
     // In that case just assume it's type is MetropolisHastings.
     try
     {
        string chainType;
        ext.readKey(s_CHAINTYP, chainType);
        if (chainType == "GoodmanWeare")
        {
           m_chain->chainType("GoodmanWeare");
        }
     }
     catch (...)
     {
     }
     if (m_chain->chainType()=="GoodmanWeare")
     {
        int nWalkers=0;
        ext.readKey(s_NWALKERS, nWalkers);
        m_chain->walkers((size_t)nWalkers);
     }
     
     // The following 2 functions should only throw YellowAlerts,
     // not FitsExceptions. 
     if (m_chain->chainType() == "MetropolisHastings")  
        readTemperInfo(table);  
     readParamInfo(table);      

     delete m_fits;
     m_fits = 0;
  }
  catch (FitsException&)
  {
     string msg("while attempting to read ");
     msg += errLabel + " in ";
     msg += m_chain->getFileName();
     delete m_fits;
     m_fits = 0;
     throw ChainIOError(msg);
  }
  catch (YellowAlert&)
  {
     delete m_fits;
     m_fits = 0;
     throw;
  }
}

void FITSChain::createFile ()
{
   using namespace CCfits;
   try
   {
      string rmString("rm -f ");
      std::ifstream testFile(m_chain->getFileName().c_str());
      if (testFile)
      {
         // We must ASSUME by this point that the user has been
         // notified of a pre-existing file and has given the OK
         // to replace it.
         if (!XSparse::validateForSystem(m_chain->getFileName()))
         {
            string errMsg("Invalid chars in file name: ");
            errMsg += m_chain->getFileName();
            errMsg +="\n***Existing file (if any) will NOT be overwritten.\n";
            throw ChainIOError(errMsg);
         }
         rmString += m_chain->getFileName();
         std::system(rmString.c_str());
      }
      m_fits = new FITS(m_chain->getFileName(), Write);
      // To view FITS chain in progress, writePoint must call
      // for a file flush AFTER the first line is written.
      // It will need to do it again if appending.
      m_doFileFlush = true; 
   }
   catch (FitsException&)
   {
      string msg("Unable to create output FITS file ");
      msg += m_chain->getFileName();
      m_fits = 0;
      throw ChainIOError(msg);
   }
   tcout << xsverbose(25)<<"Writing chain to " << m_chain->getFileName() 
      << std::endl << xsverbose();
}

void FITSChain::writePoint ()
{
  // Function assumes m_fits is open and ready for writing.  
  // NOTE: If anything changes in here, doReadPointFromLine
  // may need a corresponding readjustment.
  using namespace XSContainer;

  const std::map<int,ModParam*>& varPars = fit->variableParameters();
  std::map<int,ModParam*>::const_iterator it = varPars.begin();
  RealArray paramVals(varPars.size());
  Real statVal;

  for (size_t i=0; i<varPars.size(); i++)
  {
    paramVals[i] = it->second->value();
    it++;
  }
  statVal = fit->statistic(); 
  writePoint(paramVals, statVal);
}

void FITSChain::writePoint (RealArray& paramVals, Real& statVal)
{
  // Function assumes m_fits is open and ready for writing.  
  // NOTE: If anything changes in here, doReadPointFromLine
  // may need a corresponding readjustment.

  int status = 0;
  fitsfile* fptr = m_fits->currentExtension().fitsPointer();
  long iRow = 1;
  fits_get_num_rows(fptr, &iRow, &status);
  ++iRow;
  for (size_t i=0; i<paramVals.size(); i++)
  {
 //    CCfits::Column& column = table.column(iCol);
     Real val = paramVals[i];
//     column.write(&val, (long)1, iRow);
     fits_write_col_dbl(fptr, i+1, iRow, 1, 1, &val, &status);
  }
//  table.column(iCol).write(&statVal, (long)1, iRow); 
  fits_write_col_dbl(fptr, paramVals.size()+1, iRow, 1, 1, &statVal, &status);
   if (m_doFileFlush)
   {
      fits_flush_file(fptr, &status);
      m_doFileFlush = false;
   }
   else
   {
      fits_flush_buffer(fptr, 0, &status);
      char updateKey[] = "NAXIS2";
      fits_update_key(fptr, TLONG, updateKey, &iRow, 0, &status);
   }
}

void FITSChain::writeFileInfo ()
{
   using namespace CCfits;
   // Assumes new file is open and ready for writing.
   const std::vector<Chain::ParamID>& parIDs = m_chain->paramIDs();
   const size_t nCols = parIDs.size() + 1;
   std::vector<string> colName(nCols);
   std::vector<string> colFmt(nCols, s_FLOATTYPE);
   std::vector<string> colUnit(nCols);

   for (size_t i=0; i<parIDs.size(); ++i)
   {
      const Chain::ParamID& parID = parIDs[i];
      std::ostringstream ssIndex;
      ssIndex << parID.parName << s_COLNAMEDELIM;            
      if (parID.modName != Model::DEFAULT())
      {
         ssIndex << parID.modName << s_COLNAMEDELIM;
      }
      ssIndex << parID.index;
      colName[i] = ssIndex.str();
      colUnit[i] = parID.units;
   }
   colName[nCols-1] = s_STATCOL;
   const StatMethod* statMeth = XSContainer::fit->statManager()->usingSingleStat();
   const string statName = statMeth ? statMeth->fullName() : string("Total_Stat");
   colUnit[nCols-1] = statName;

   // Due to possible ctrl-C interrupt, we don't know how
   // many rows to set at this point.
   Table* table = m_fits->addTable(s_EXTNAME, 0, colName, colFmt, colUnit);
   if ( m_chain->chainType() == "MetropolisHastings" )
   {
      adjustTemperInfo();
      table->addKey(s_CHAINTYP, string("MetropolisHastings"), string(""));
   }
   if (m_chain->chainType() == "GoodmanWeare")
   {
      table->addKey(s_CHAINTYP, string("GoodmanWeare"), string(""));
      table->addKey(s_NWALKERS, (int)m_chain->walkers(), string(""));
   }
   writeHistory(false);
}

void FITSChain::appendFile (RealArray& startingVals)
{
  // ASSUMES calling function has already saved the original
  // parameter values, and so is capable of restoring
  // them.  

  // Starting point should be the last set of values in the file.
   doOpenForWrite();
   writeHistory(true);
   startingVals.resize(m_chain->width()-1, .0);
   try
   {
      doReadPointFromLine(m_chain->length()-1, startingVals);
      // To view in-progress in fv, need to flush file
      // after first appended line is written in writePoint.
      m_doFileFlush = true;             
   }
   catch (YellowAlert&)
   {
      delete m_fits;
      m_fits = 0;
      throw;
   }
}
void FITSChain::appendFile (RealArray& startingVals, Real& startingStatVal)
{
  // ASSUMES calling function has already saved the original
  // parameter values, and so is capable of restoring
  // them.  

  // Starting point should be the last set of values in the file.
   doOpenForWrite();
   writeHistory(true);
   startingVals.resize(m_chain->width()-1, .0);
   try
   {
     doReadPointFromLine(m_chain->length()-1, startingVals, startingStatVal);
      // To view in-progress in fv, need to flush file
      // after first appended line is written in writePoint.
      m_doFileFlush = true;             
   }
   catch (YellowAlert&)
   {
      delete m_fits;
      m_fits = 0;
      throw;
   }
}

void FITSChain::adjustTemperInfo ()
{
   // Assumes m_fits is open and that newest temperature has just
   // been entered into chain's tempering array.
   const Chain::TemperInfoContainer& tempers = m_chain->tempering();
   CCfits::ExtHDU& table = m_fits->extension(s_EXTNAME);
   const string comment("");
   int iTemp = tempers.size();  // iTemp is 1-based
   const string keyIdx(intToKeyIndex(iTemp));
   const string tempKeyword(s_TEMPERKEY+keyIdx);
   const string rowKeyword(s_ROWKEY+keyIdx);
   table.addKey(tempKeyword, tempers[iTemp-1].second, comment);
   table.addKey(rowKeyword, tempers[iTemp-1].first, comment);
}

bool FITSChain::checkTemperField (const Real temperature) const
{
   // Can we add 1 more temperature keyword?
   // The actual input temperature arg is irrelevant here.
   size_t upperLimit = 1;
   for (size_t i=0; i<s_TEMPERDIGITS; ++i)
      upperLimit *= 10;
   return (m_chain->tempering().size() < upperLimit-1);
}

void FITSChain::setParent (Chain* chain)
{
   m_chain = chain;
}

string FITSChain::intToKeyIndex (int iEntry)
{
   std::ostringstream intString;
   int upperLimit = 1;
   for (size_t i=0; i<s_TEMPERDIGITS; ++i)
      upperLimit *= 10;
   if (iEntry > 0 && iEntry < upperLimit)
   {
      int iPow = static_cast<int>(std::log10((Real)iEntry));
      for (int i=static_cast<int>(s_TEMPERDIGITS-1); i>iPow; --i)
         intString << "0";
      intString << iEntry;
   }   
   return intString.str();
}

void FITSChain::readTemperInfo (CCfits::Table* table)
{
   using namespace CCfits;
   Chain::TemperInfoContainer temperArray;
   try
   {
      // Keep reading TEMPR/STROW pairs until no more can be found.
      // There must be a least one of these.
      int upperLimit = 1;
      for (size_t i=0; i<s_TEMPERDIGITS; ++i)
         upperLimit *= 10;
      int iEntry = 1;
      while (iEntry < upperLimit)
      {
         string tempKey(s_TEMPERKEY);
         string rowKey(s_ROWKEY);
         string suffix(intToKeyIndex(iEntry));
         tempKey += suffix;
         rowKey += suffix;
         Real tempVal = 0.0;
         int rowVal = 0;
         table->readKey(tempKey, tempVal);
         table->readKey(rowKey, rowVal);
         if (tempVal <= 0.0)
         {
            std::ostringstream oss;
            oss << "Invalid " << tempKey << " value: " << tempVal 
              << " in file " << m_chain->getFileName() <<"\n";
            throw YellowAlert(oss.str());
         }
         if (rowVal < 1 || static_cast<size_t>(rowVal) > m_chain->length())
         {
            std::ostringstream oss;
            oss << "Invalid " << rowKey << " value: " << rowVal 
                << " in file " << m_chain->getFileName() <<"\n";
            throw YellowAlert(oss.str());
         }
         temperArray.push_back(std::make_pair(static_cast<size_t>(rowVal), tempVal));
         ++iEntry;
      }
   }
   catch (FitsException&)
   {
      // A FitsException is to be expected.  This is how it
      // exits the keywords loop.
      if (temperArray.empty())
      {
         string errMsg("At least 1 pair of ");
         errMsg += s_TEMPERKEY + " and " + s_ROWKEY;
         errMsg += " keywords are required in file ";
         errMsg + m_chain->getFileName() + "\n";
         throw YellowAlert(errMsg);
      }
   }
   m_chain->tempering(temperArray);
}

void FITSChain::readParamInfo (CCfits::Table* table)
{
   // Chain width should already have been set to the number of
   // columns in the table, and must be at least 2 if it got to 
   // this point.
   const int nPars = static_cast<int>(m_chain->width())-1;
   if (nPars < 1)
      throw RedAlert("Chain width error in FITSChain::readParamInfo.");
   std::vector<Chain::ParamID> paramIDs((size_t)nPars);
   bool isLabelErr = false;
   int iCol = 1;
   while (iCol <= nPars)
   {
      Chain::ParamID& inPar = paramIDs[iCol-1];
      const CCfits::Column& column = table->column(iCol);
      const string& colName = column.name();
      const string& colFormat = column.format();
      const string& colUnits = column.unit();
      if (colFormat != s_FLOATTYPE)
      {
         std::ostringstream oss;
         oss << "Column " << iCol << " in chain extension of file "
           << m_chain->getFileName() << "\n    is the wrong format type.";
         throw ChainIOError(oss.str());
      }

      // Label format should be  <parName>__[<modName>__]<parIndex>
      const string WS(" \t");
      string paramIndex;
      string paramName;
      string modelName;
      size_t index = string::npos;
      string::size_type iPos = colName.find_first_not_of(WS);
      if (iPos == string::npos)
      {
         isLabelErr = true;
         break;
      }
      string::size_type delimPos = colName.find(s_COLNAMEDELIM, iPos);
      if (delimPos == string::npos)
      {
         isLabelErr = true;
         break;
      }
      paramName = colName.substr(iPos, delimPos - iPos);

      // If no more delimiters, remainder of string should just be an index.
      iPos = delimPos + s_COLNAMEDELIM.size();
      delimPos = colName.find(s_COLNAMEDELIM, iPos);
      if (delimPos != string::npos)
      {
         modelName = colName.substr(iPos, delimPos - iPos);
         iPos = delimPos + s_COLNAMEDELIM.size();
      }
      paramIndex = colName.substr(iPos);
      index = XSutility::isInteger(paramIndex);
      if (index == string::npos)
      {
         isLabelErr = true;
         break;
      }

      inPar.index = index;
      inPar.modName = modelName.length() ? modelName : Model::DEFAULT();
      inPar.parName = paramName;
      inPar.units = colUnits; 
      ++iCol;     
   } // end par loop
   if (isLabelErr)
   {
      std::ostringstream oss;
      oss << "Column " << iCol << " has an invalid parameter label "
          << "\n   in chain extension of file " << m_chain->getFileName();
      throw ChainIOError(oss.str());
   }

   m_chain->paramIDs(paramIDs);

   const CCfits::Column& column = table->column(nPars+1);
   if (column.format() != s_FLOATTYPE)
   {
      std::ostringstream oss;
      oss << "Column " << nPars+1 << " in chain extension of file "
        << m_chain->getFileName() << "\n    is the wrong format type.";
      throw ChainIOError(oss.str());
   }
   if (column.name() != s_STATCOL)
   {
      std::ostringstream oss;
      oss << "Column " << nPars+1 << " in chain extension of file "
        << m_chain->getFileName() << "\n    should have label "
        << s_STATCOL;
      throw ChainIOError(oss.str());
   }
   m_chain->statistic(column.unit());
}

void FITSChain::writeHistory (bool isAppend) const
{
   using namespace std;
   using namespace XSContainer;
   tcout.flush();

   // The various data/model report functions are hardcoded to write
   // to tcout.  We can reroute and grab the output by temporarily
   // replacing tcout's streambuf (type XSstreambuf) with a
   // stringbuf (the type of streambuf used by stringstreams).  

   // Make sure chatter levels are no higher than 10, otherwise
   // data output will include grouping map.
   const int savConChatter = tpout.consoleChatterLevel();
   const int savLogChatter = tpout.logChatterLevel();
   tpout.consoleChatterLevel(10);
   tpout.logChatterLevel(10);

   stringbuf strBuf;
   streambuf* saveStreamBuf = tcout.rdbuf(&strBuf);
   try
   {
      const size_t fieldWidth = 72;

      char datetime[20];
      int timeref=0, status=0;
      if (fits_get_system_time(datetime, &timeref, &status))
      {
         throw YellowAlert("Unable to read system time\n");
      }
      string history;
      CCfits::ExtHDU& chainExt = m_fits->currentExtension();
      if (isAppend)
      {
         ostringstream oss;
         oss << "Appended to XSPEC chain after line " << m_chain->length();
         chainExt.writeComment("   ");
         history = oss.str();
      }
      else
      {
         history = "Created by XSPEC's chain run command";
      }
      chainExt.writeHistory(history);
      chainExt.writeHistory(string(datetime));

      fit->chainManager()->reportChainProposal();
      if (!isAppend) {
	tcout << "Burn length = " << m_chain->burnLength() << endl;
	  if (m_chain->chainType() != "GoodmanWeare") {
	    tcout <<"Chain randomization is " << (m_chain->rand() ? "on" : "off") << endl;
	  }
      }
      datasets->showData(false);
      models->showModel(); 
      fit->statManager()->reportStats();
      tcout << " Weighting method: " << DataUtility::statWeight().name()
          << endl;
      string::size_type iPos = 0;
      const string WS(" \t\n");
      while (iPos != string::npos)
      {
         string xspecInfo(XSparse::stringSegment(strBuf.str(), fieldWidth, &iPos));
         if (xspecInfo.find_first_not_of(WS) != string::npos)
            chainExt.writeComment(xspecInfo);
      }
   }
   catch (...)
   {
      tcout.rdbuf(saveStreamBuf);
      tpout.consoleChatterLevel(savConChatter);
      tpout.logChatterLevel(savLogChatter);
      throw ChainIOError("Error during write to COMMENTS and HISTORY section of chain extension.");
   }

   tcout.rdbuf(saveStreamBuf);
   tpout.consoleChatterLevel(savConChatter);
   tpout.logChatterLevel(savLogChatter);
}

// Additional Declarations
