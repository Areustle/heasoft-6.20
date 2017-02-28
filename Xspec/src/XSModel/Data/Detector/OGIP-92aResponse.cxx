//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <fstream>
#include <iomanip>
#include "Data/DataUtility.h"

// SpectralData
#include <XSModel/Data/SpectralData.h>
// DataSet
#include <XSModel/Data/DataSet.h>
// typeinfo
#include <typeinfo>
// XSparse
#include <XSUtil/Parse/XSparse.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>
// OGIP-92aResponse
#include <XSModel/Data/Detector/OGIP-92aResponse.h>
#include "XSstreams.h"
#include <CCfits/CCfits>
#include <string>
#include <sstream>
using std::string;
#include "Utils/XSstream.h"
#include "XSsymbol.h"


// Class OGIP_92aResponse 

OGIP_92aResponse::OGIP_92aResponse()
{
}

OGIP_92aResponse::OGIP_92aResponse(const OGIP_92aResponse &right)
      : RealResponse(right), OGIP_92aIO(right)
{
}


OGIP_92aResponse::~OGIP_92aResponse()
{
}


size_t OGIP_92aResponse::read (const string& fileName, bool readFlag)
{
  using namespace CCfits;

  // fileName may contain an extver specifier in curly brackets.
  // Must remove when sending to FITS ctor.
  string openFileName(fileName.substr(0,fileName.find_first_of('{')));

  // readFlag defaults to true here. A different class implementation may
  // actually set this to false, if it were necessary for memory requirementblocks.

  std::vector<string> dummy;
  std::auto_ptr<FITS> rsp(new FITS(openFileName, Read, extensionName(),readFlag, ResponseKeys(),
                dummy, extVers()));   

     // Look for extension containing energy bounds - if found, read it
  StringArray  searchKeys(3,"");
  searchKeys[0] = HDUCLASS();  
  searchKeys[1] = HDUCLAS1();  
  searchKeys[2] = HDUCLAS2();  


  StringArray  searchValues(3,"");
  searchValues[0] = OGIPTYPE();  
  searchValues[1] = RESPTYPE();  
  searchValues[2] = EBOUNDS();  

  std::vector<string>  hduKeys(3,"");
  hduKeys[0] = CHANNEL();  
  hduKeys[1] = MINENERGY();  
  hduKeys[2] = MAXENERGY(); 

  // find the extension with key-value pairs for EBOUNDS
  try
  {
        rsp->read(searchKeys, searchValues, readFlag, hduKeys);
        eboundsExtName(rsp->currentExtensionName());  
  }
  catch (CCfits::FITS::NoSuchHDU)    
  {
        // this time, just look for EXTNAME = EBOUNDS. 
        // If successful, we're done.
        searchKeys.resize(1);
        searchValues.resize(1);
        searchKeys[0] = string("EXTNAME");
        searchValues[0] = EBOUNDS();
        try
        {
                rsp->read(searchKeys, searchValues, readFlag, hduKeys); 
                eboundsExtName(rsp->currentExtensionName());  
        }
        catch (...)
        {
                throw;
        }

  }

  setDataSource(rsp.release());

  return 1;
}

OGIP_92aResponse* OGIP_92aResponse::clone () const
{

  return new OGIP_92aResponse(*this);
}

bool OGIP_92aResponse::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  return OGIP_92aIO::fileFormat(fileName,type);
}

void OGIP_92aResponse::setArrays ()
{

  try
  {
     CCfits::ExtHDU&  table = dataSource()->extension(extensionName(),extVers());  
     const SpectralData* data = source();

     int startChan = data->startChan() - data->firstChan();
     int endChan   = data->endChan() - data->firstChan();
     int legalStart=0, legalEnd=0;
     data->parent()->legalChannelBounds(legalStart,legalEnd);
     size_t dataDetChans = legalEnd-legalStart+1;

     int     val(0);
     size_t  detchans(0);
     try
     {
        detchans = table.keyWord(DETCHANS()).value(val);
     }
     catch (...)
     {
        tcout << "Failed to read " << DETCHANS() << " from " << extensionName() << "\n";
        throw;
     }

     size_t  nC(data->channels());

     if (detchans  != dataDetChans)
     {
        std::ostringstream msg;
        msg << "Response DETCHANS: " << detchans << ", and data DETCHANS: "
            << dataDetChans <<"\n   do not match.\n";
        throw YellowAlert(msg.str());
     }

     size_t  nE = table.rows();

     numEnergies(nE);
     numChannels(nC);

     spectrumNumber(data->spectrumNumber());
     ResponseMatrix::ChanRangeIndicators chanLimits;
     chanLimits.firstChan = data->firstChan();
     chanLimits.startChan = data->startChan();
     chanLimits.endChan = data->endChan();

     RefCountPtr<ResponseMatrix> rmf(new ResponseMatrix(chanLimits, nE, detchans, rmfName(), data->gqString()));

//     bool arfFlag = !(arfName().size()==0);

     DataUtility::readArrays(*rmf, table);
     DataUtility::groupArrays(*rmf, startChan, endChan);

     rmfData(rmf);

     // Group the EBOUNDS arrays
     groupEbounds();
     tcout << xsverbose(35);
     if (tpout.maxChatter() == 35)
     {
             using namespace std;
             const RefCountPtr<ResponseMatrix>&  rsp = rmfData(); 
	     for (size_t i=0; i<numChannels(); i++)
             {
	        tcout << "Energy: " << i+1 << " " << setw(15) 
	   	         << rsp->eboundsMin(i) << "  " 
	   	         << setw(15) << rsp->eboundsMax(i) << "\n";
	     }
     }    
     tcout << xsverbose();
  }

  catch (YellowAlert&)
  {
     // diagnostic for Xspec errors, which might be thrown by the 
     // ResponseMatrix constructor.
     throw;
  }  
}

void OGIP_92aResponse::setDescription (size_t specNum, size_t groupNum)
{

  CCfits::ExtHDU&  table = dataSource()->extension(extensionName());  

  string  keyvalue("");
  // Read TELESCOPE, INSTRUMENT and CHANTYPE keywords if exist. 
  // Comparison will be done in setSharedDescription so 
  // reference counted responses will also be checked.
  try
  {
     table.keyWord(TELESCOPE()).value(keyvalue);
     rmfData()->telescope(keyvalue);
  }
  catch(CCfits::HDU::NoSuchKeyword)
  {
     tcout << "Note: TELESCOPE keyword not found in the rmf file"<<std::endl;
  }

  try
  {
     table.keyWord(INSTRUMENT()).value(keyvalue);
     rmfData()->instrument(keyvalue);
  }
  catch(CCfits::HDU::NoSuchKeyword)
  {
     tcout << "Note: INSTRUMENT keyword not found in the rmf file"<<std::endl;
  }

  try
  {
     table.keyWord(CHANNELTYPE()).value(keyvalue);         
     rmfData()->channelType(keyvalue);
  }

  catch(CCfits::HDU::NoSuchKeyword)
  {
    tcout << "Note: CHANTYPE keyword not found in the rmf file"<<std::endl;
  }

  // Gain limit keywords are optional.  Do not print warning if any are
  // missing, but DO warn if they exist and there is trouble reading them.
  StringArray keyNames;
  keyNames.push_back(GSLOP_MIN());
  keyNames.push_back(GSLOP_MAX());
  keyNames.push_back(GOFFS_MIN());
  keyNames.push_back(GOFFS_MAX());
  for (size_t i=0; i<2; ++i)
  {
     Real lower = Response::NO_KEYVAL();
     Real upper = Response::NO_KEYVAL();
     Real limit = 0.0;
     try
     {
        table.keyWord(keyNames[i*2]).value(limit);
        lower = limit;
     }
     catch (CCfits::HDU::NoSuchKeyword&)
     {
     }
     catch (CCfits::FitsException&)
     {
        tcout << "***Warning: Unable to read value in keyword "
           << keyNames[i*2] << std::endl;
     }
     try
     {
        table.keyWord(keyNames[i*2+1]).value(limit);
        upper = limit;
     }
     catch (CCfits::HDU::NoSuchKeyword&)
     {
     }
     catch (CCfits::FitsException&)
     {
        tcout << "***Warning: Unable to read value in keyword "
           << keyNames[i*2] << std::endl;
     }
     i ? setOffsetKeyLimits(lower, upper) :
         setSlopeKeyLimits(lower, upper); 
  }

  setSharedDescription(specNum, groupNum);
}

void OGIP_92aResponse::closeSourceFiles ()
{
  OGIP_92aIO::closeFile();
}

void OGIP_92aResponse::groupEbounds ()
{
   SpectralData*  data=source();
   RefCountPtr<ResponseMatrix> rmf(rmfData());

   IntegerArray grouping;
   IntegerArray quality;  
   rmf->decodeGQ(grouping, quality);

   bool qualitySet = !quality.empty();
   bool grouped = !grouping.empty();

   int  startChan = data->startChan();
   int  endChan   = data->endChan();
   int legalStart=0, legalEnd=0;
   data->parent()->legalChannelBounds(legalStart,legalEnd);
   const int dataDetChans = legalEnd-legalStart+1;

   size_t  ungroupedChannels=endChan - startChan + 1;
   size_t  groupedChannels=0;
   // Load the EBOUNDS energies into ResponseMatrix object
   CCfits::ExtHDU&  eboundsTable = dataSource()->extension(eboundsExtName());
   int detChans=0;
   bool isDetChansKeyword=false;
   try
   {
      eboundsTable.readKey(DETCHANS(), detChans);
      isDetChansKeyword = true;
   }
   catch (...)
   {
      // Allow case of no DETCHANS, just take value from nRows.
      detChans = static_cast<int>(eboundsTable.rows());
   }
   if (detChans != dataDetChans)
   {
      std::ostringstream msg;
      if (isDetChansKeyword)
      {
         msg << "Response EBOUNDS extension DETCHANS: " << detChans
            << ", and data DETCHANS: " << dataDetChans << " do not match.\n";
      }
      else
      {
         msg << "Response EBOUNDS number of rows does not match data DETCHANS: "
            << dataDetChans << "\n";
      }
      throw YellowAlert(msg.str());
   }
   if (eboundsTable.rows() != static_cast<long>(detChans))
   {
      throw YellowAlert("Number of rows in response EBOUNDS does not match DETCHANS.\n");
   }
   
   RealArray  eMin;
   RealArray  eMax;

   eboundsTable.column(MINENERGY()).read(eMin, startChan, endChan);
   eboundsTable.column(MAXENERGY()).read(eMax, startChan, endChan);
   
   // Prevent zero or neg energy from getting through.
   for (size_t i=0; i<ungroupedChannels; ++i)
   {
      if (eMin[i] < SMALL)  eMin[i] = SMALL;
      if (eMax[i] < SMALL)  eMax[i] = SMALL;
   }

   if (!grouped)
   {
      if (!qualitySet)
      {
	 // Case I: no grouping or quality information.
	 groupedChannels = ungroupedChannels;
      }
      else
      {
         // Case II: no grouping information, but quality information.

	 for (size_t i=0; i<ungroupedChannels; i++)
	 {
	    if (quality[i] != 1)
	    {
	       eMin[groupedChannels]   = eMin[i];
	       eMax[groupedChannels++] = eMax[i];
	    }
	 }

      }
   }

   else
   {
      // Case III General case: there is grouping information.

      size_t i=0;
      // Initialize eMin[0] and eMax[0] from first bin of OK quality,
      // regardless of whether grouping is = -1.  (A -1 for the first
      // bin is actually erroneous input.  This will cover for it.)
      while (i<ungroupedChannels)
      {
	 if (!qualitySet || quality[i] != 1)
	 {
	    eMin[groupedChannels] = eMin[i];
	    eMax[groupedChannels] = eMax[i];
	    ++i;
	    ++groupedChannels;
	    break;
	 }
	 ++i;
      }

      if (eMin[0] < eMin[eMin.size()-1])
      {
	 while (i<ungroupedChannels)
	 {
	    if (!qualitySet || quality[i] != 1)
	    {
	       if (grouping[i] != -1)
	       {
		  eMin[groupedChannels] = eMin[i];
		  eMax[groupedChannels] = eMax[i];
		  ++i;
		  ++groupedChannels;
               }
	       else 
	       {
		  while (i<ungroupedChannels && grouping[i] == -1)
		  {
		     if (!qualitySet || quality[i] != 1)
		     {
		        eMax[groupedChannels-1] = eMax[i];
		     }
		     ++i;  
		  }
	       }
	    }
	    else ++i;
	 }
      }

      else  // Ebounds are in decreasing order
      {
	 while (i<ungroupedChannels)
	 {
	    if (!qualitySet || quality[i] != 1)
	    {
	       if (grouping[i] != -1)
	       {
		  eMin[groupedChannels] = eMin[i];
		  eMax[groupedChannels] = eMax[i];
		  ++i;
		  ++groupedChannels;
               }
	       else 
	       {
		  while (i<ungroupedChannels && grouping[i] == -1)
		  {
		     if (!qualitySet || quality[i] != 1)
		     {
		        eMin[groupedChannels-1] = eMin[i];
		     }
		     ++i;  
		  }
	       }
	    }
	    else  ++i;
	 }
      }

   }
   rmf->setEboundsMin(eMin[std::slice(0,groupedChannels,1)]);
   rmf->setEboundsMax(eMax[std::slice(0,groupedChannels,1)]);
}

bool OGIP_92aResponse::readAuxResponse (int rowNum)
{
  using namespace CCfits;
  SpectralData* data = source();
  // deliberately not setting this by reference since arfName() may
  // be altered in catch block below.
  string fileName = arfName();
  bool isSuccess = false;

  if (fileName.length() == 0) 
     return false;

  bool responseCheck = false;
  string extName;
  int extVers = 1;

  try
  {
     // First, simply verify that the requested extension exists and
     // has the proper keywords.  Do not read the data.
     std::auto_ptr<FITS> apFile(openFitsExtension(fileName,AuxResponseType));
     extName = apFile->currentExtensionName();
     extVers = apFile->currentExtension().version();
     responseCheck = true;
  }               
  catch (...)
  {
     arfName("");
     string newFile;
     try
     {
        XSparse::getFileNameFromUser(fileName, newFile, XSutility::ARF);
        if (newFile.length() != 0)
        {
	   std::auto_ptr<FITS> apFile(openFitsExtension(newFile,AuxResponseType));
	   extName = apFile->currentExtensionName();
           extVers = apFile->currentExtension().version();
	   responseCheck = true;
	   arfName(newFile);
           fileName = newFile;
        }
     }
     catch (XSparse::SkipThis&) 
     {
     }
     catch (FITS::CantOpen&)
     {
        throw XspecDataIO::CannotOpen(newFile);
     }
     catch (XSparse::AbortLoop&)
     {
         throw;      
     }
     catch (...)
     {
        throw XspecDataIO::RequiredDataNotPresent(newFile);
     }

  }

  if ( responseCheck )
  {
     try
     {
	bool readFlag = true;
        std::vector<string> dummy;
	std::auto_ptr<FITS> auxResponse(new FITS(fileName, Read, extName, 
                readFlag, AuxResponseKeys(), dummy, extVers));   
	ExtHDU&  table(auxResponse->extension(extName));

        // rowNum should only have the default value of -1 when this
	// code is accessed during initialization of the the DataSet and
	// its objects, by way of the Data command.  It should already
	// have a valid row number if reached here by the Arf command.
        if (rowNum == -1)
        {
           // It is possible the arf row number has already been set
           // during init(See OGIP-Data::setResponse), due to an
           // explicit row entry in the keyword or col, or if
           // == npos it is asking for row by row matchup.  
           rowNum = (arfRow() == string::npos) ? data->rowNumber() : arfRow();
        }

	if (rowNum == 0)
	{
           RealArray effArea;
	   size_t  nE = table.rows();
	   try
	   {
              table.column(SPECRESPTYPE()).read(effArea, 0, nE);
	   }
	   catch (Column::WrongColumnType)
	   {
	      std::ostringstream msg;
	      msg << '\n' << fileName << " is detected as Type 2: "
		     << "row number must be specified";
	      throw XspecDataIO::UnspecifiedSpectrumNumber(msg.str());
	   }
	   if (effArea.size() != rmfData()->binResponseGroups().size())
	   {
	      string msg = "\n*** Incorrect number of ebins in arf";
	      throw XspecDataIO::RequiredDataNotPresent(msg);
	   }
	   setEffectiveArea(rmfData()->normFactor()*effArea);
	   arfRow(0);
	   isSuccess = true;
	}
	else
	{
	   RealArray effArea;
	   try
	   {
              table.column(SPECRESPTYPE()).read(effArea, rowNum);
	   }
	   catch (Column::WrongColumnType)
	   {
	      std::ostringstream msg;
	      msg << '\n' << fileName << " is detected as Type 1: "
		     << "row number must not be specified";
	      throw XspecDataIO::SingleSpectrumOnly(msg.str());
	   }  
	   catch (Column::InvalidRowNumber)
	   {
	      std::ostringstream msg;
	      msg << '\n' << "Row# "<<rowNum<<" is out of bounds of file: "
		     << fileName;
	      throw XspecDataIO::RequiredDataNotPresent(msg.str());
	   }
	   if (effArea.size() != rmfData()->binResponseGroups().size())
	   {
	      string msg = "\n*** Incorrect number of ebins in arf";
	      throw XspecDataIO::RequiredDataNotPresent(msg);
	   }
	   setEffectiveArea(effArea*rmfData()->normFactor());
	   arfRow(rowNum);
	   isSuccess = true;	
	}      
     }
     catch (...)
     {
        tcout << "\n*** Error processing aux response data - file skipped\n";
        arfName("");
        arfRow(0);       
     }
  }
  tcout << std::flush;
  return isSuccess;
}

// Additional Declarations
