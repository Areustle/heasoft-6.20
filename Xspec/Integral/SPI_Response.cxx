//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <fstream>
#include <iomanip>
#include <XSstreams.h>
#include <Data/DataUtility.h>
#include <Data/DataSet.h>
#include <CCfits/CCfits>
#include <Parse/XSparse.h>
#include <XSsymbol.h>

// SPI_Response
#include "SPI_Response.h"
#include <iostream>


// Class SPI_Response 

SPI_Response::SPI_Response()
  : MultiResponse()
{
}

SPI_Response::SPI_Response(const SPI_Response &right)
      : MultiResponse(right), m_efficiency(right.m_efficiency)
{
}


SPI_Response::~SPI_Response()
{
}


size_t SPI_Response::read (const string& fileName, bool readFlag)
{
  using namespace CCfits;


  std::auto_ptr<FITS> rsp(new FITS(fileName, Read, extensionName(),readFlag, ResponseKeys()));   

     // Look for extension containing energy bounds - if found, read it
  StringArray  searchKeys(3,"");
  searchKeys[0] = HDUCLASS();  
  searchKeys[1] = HDUCLAS1();  
  searchKeys[2] = HDUCLAS2();  


  StringArray  searchValues(3,"");
  //searchValues[0] = ISDCTYPE();  
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
        ExtHDU&  ext(rsp->extension(eboundsExtName()));        
  }
  catch (CCfits::FITS::NoSuchHDU)    
  {
        // this time, look for RMFVERSN == 1992a and HDUCLAS1 = RESPONSE 
        // and HDUCLAS2 = EBOUNDS. If successful, we're done.
        searchKeys[0] = RMFVERSN();
        searchValues[0] = OGIPVERS();
        try
        {
                rsp->read(searchKeys, searchValues, readFlag, hduKeys); 
        }
        catch (...)
        {
                throw;
        }

  }

  if (!dataSource(0))
  {
  	setDataSource(rsp.release(),0);
  }
  else
  {
  	setDataSource(rsp.release(),getDataSource().size());
  }
  return 1;
}

SPI_Response* SPI_Response::clone () const
{

  return new SPI_Response(*this);
}

bool SPI_Response::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  bool format = false;
  using namespace CCfits;

  try
  {
        // find an extension that matches the HDUCLASS, HDUCLAS1, HDUVERS key values.
        std::vector<string> OGIP92a(2,"");
        OGIP92a[0] = HDUCLASS();
        OGIP92a[1] = HDUCLAS1();

        std::vector<string> OGIP92aVal(2,"");
        OGIP92aVal[0] = OGIPTYPE();

        switch (type)
        {
                default:
                case XspecDataIO::ResponseType:
                        OGIP92aVal[1] = RESPTYPE();       
                        OGIP92a.push_back(HDUCLAS2());
                        OGIP92aVal.push_back("RSP_MATRIX");
                        break;
                case AuxResponseType:
                        OGIP92aVal[1] = RESPTYPE();
                        OGIP92a.push_back(HDUCLAS2());
                        OGIP92aVal.push_back(SPECRESPTYPE());
                        break;
        }

	std::auto_ptr<FITS> p(new FITS(fileName,Read,OGIP92a,OGIP92aVal));
	// this will throw a FITS::NoSuchHDU if there's no Spectrum Extension
	// and will be caught at the bottom, returning false.
	setExtensionName(p->currentExtensionName());

	ExtHDU& spectrum = p->extension(extensionName());
	// we have a spectrum extension. 

	std::string versionString("");

	try
        {
                // look for HDUVERS keyword.
                spectrum.readKey(HDUVERS(),versionString);
                if (versionString.substr(0,2) == VERSKEY()) 
                {
                        format = true;
                }
	}
	catch (HDU::NoSuchKeyword)
        {
                  // no HDUVERS, look for HDUVERS1, HDUVERS2, HDUVERS3...
                  size_t hv = 1;
                  while (hv <= 3)
                  {
                         std::ostringstream versCheck;
                         versCheck << HDUVERS() << hv++;
                         try
                         {
                                spectrum.readKey(versCheck.str(),versionString);
                                if (versionString.substr(0,2) == VERSKEY()) 
                                {
                                        format = true;
                                        break;
                                }
                                else
                                {
                                        continue;       
                                }
                         }      
                         catch (HDU::NoSuchKeyword)
                         {
                                continue;       
                         } 
                  }
                  if (versionString.size() == 0) 
                  {
                          // no HDUVERS keywords...
                          std::ostringstream diag(fileName);
                          diag << ": no format version. check HDUVERS keywords";
                          throw FITS::NoSuchHDU(diag.str());
                  }

	}
	format = true;
  }
  catch (FITS::CantOpen)
  {
	throw XspecDataIO::CannotOpen(fileName);
  }	
  catch (FITS::NoSuchHDU)
  {
          // the "backward-compatibility clause.
          // a header containing just "PHAVERSN" identifies the file as OGIP/SPECTRUM
          // this is a deprecated usage.
         std::vector<string> OGIP92a(1,"");
         std::vector<string> OGIP92aVal(1,OGIPVERS());

         switch (type)
         {
                default:
                case XspecDataIO::ResponseType:
                        OGIP92a[0] = RMFVERSN();       
                        break;
                case AuxResponseType:
                        OGIP92a[0] = ARFVERSN();
                        break;
         }          
         try
         {               
                 std::auto_ptr<FITS> p(new FITS(fileName,Read,OGIP92a,OGIP92aVal));
                 // this will throw a FITS::NoSuchHDU if there's no Spectrum Extension
                 // and will be caught at the bottom, returning false.
                 setExtensionName(p->currentExtensionName());
                 ExtHDU& spectrum = p->extension(extensionName());
                 format = true;
         }
         catch (FITS::NoSuchHDU)
         {
                 format = false;
         }
  } 
  catch ( ... )
  {
          throw;
  }

  if (!format) setExtensionName(string());
  return format;
}

void SPI_Response::closeSourceFiles (size_t index)
{
  OGIP_92aIO::closeFile(index);
}

bool SPI_Response::readAuxResponse ()
{
  using namespace CCfits;
  SpectralData* data = source();
  bool isSuccess = false;

  size_t  rmfNum=currentRMF();


  const string& fileName = arfNames(rmfNum);
  if (fileName.length() == 0) return isSuccess;

  bool responseCheck = false;
  string ext("");

  std::vector<string> searchKeys(3,"");
  searchKeys[0] = HDUCLASS();
  searchKeys[1] = HDUCLAS1();
  searchKeys[2] = HDUCLAS2();

  std::vector<string> searchVal(3,"");
  searchVal[0] = OGIPTYPE();
  searchVal[1] = RESPTYPE();
  searchVal[2] = SPECRESPTYPE();

  try
  {
        // find an extension that matches the HDUCLASS, HDUCLAS1, HDUVERS key values.
	std::auto_ptr<FITS> p(new FITS(fileName,Read,searchKeys,searchVal));
	ext = p->currentExtensionName();
	responseCheck = true;
  }               
  catch ( FitsException & )
  {
          string newFile("");
          try
          {
                XSparse::getFileNameFromUser(fileName, newFile, XSutility::ARF);
                if (newFile.length() != 0)
                {
		   std::auto_ptr<FITS> p(new FITS(newFile,Read,searchKeys,
						  searchVal));

		   responseCheck = true;
		   ext = p->currentExtensionName();
		   arfNames(rmfNum,newFile);
                }
          }
	  catch (FITS::CantOpen)
	  {
	     throw;
	  }
	  catch (FITS::NoSuchHDU)
	  {
	     throw;
	  }
          catch (XSparse::SkipThis) 
	  {
	     throw;
	  }
          catch (XSparse::AbortLoop)
          { 
              throw;      
          }

  }

  if ( responseCheck )
  {
        try
        {
	   bool readFlag = true;
	   std::auto_ptr<FITS> auxResponse(new FITS(fileName, Read, ext, readFlag, AuxResponseKeys()));   
	   ExtHDU&  table(auxResponse->extension(ext));
	   if (static_cast<long>(data->rowNumber()) > table.rows())
	   {
	      string msg = "\n*** Spectrum has no corresponding row in ARF file";
	      throw XspecDataIO::RequiredDataNotPresent(msg);
	   }

	   RealArray effArea;
	   table.column(SPECRESPTYPE()).read(effArea, data->rowNumber());
	   if (effArea.size() != rmfData(rmfNum)->binResponseGroups().size())
	   {
	      string msg = "\n*** Incorrect number of ebins in arf";
	      throw XspecDataIO::RequiredDataNotPresent(msg);
	   }

	   // effectiveArea should already contain the RMF normFactor array
	   effectiveArea(rmfNum,effArea*rmfData(rmfNum)->normFactor());
           if ( m_efficiency.size() != effectiveArea(rmfNum).size())
           {
                   m_efficiency.resize( effectiveArea(rmfNum).size() , 0);
           }
           m_efficiency += effectiveArea(rmfNum);
	   isSuccess = true;

        }
        catch (...)
        {
                tcout << "*** Error processing aux response data - file skipped\n";
                arfNames(rmfNum,""); 
		throw;      
        }
  }

  return isSuccess;
}

void SPI_Response::setArrays ()
{
  try
  {
     size_t  rmfNum=currentRMF();

     const SpectralData* data = source();

     CCfits::ExtHDU&  table = dataSource(0)->extension(extensionName());  

     int startChan = data->startChan() - data->firstChan();
     int endChan   = data->endChan() - data->firstChan();
     int legalStart=0, legalEnd=0;
     data->parent()->legalChannelBounds(legalStart,legalEnd);
     size_t dataDetChans = legalEnd-legalStart+1;

     int     value;
     size_t  detchans = table.keyWord(DETCHANS()).value(value);     
     size_t  nC = data->channels();

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
     ResponseMatrix::ChanRangeIndicators chanLimits;
     chanLimits.firstChan = data->firstChan();
     chanLimits.startChan = data->startChan();
     chanLimits.endChan = data->endChan();

     std::auto_ptr<ResponseMatrix> rmf(new ResponseMatrix(chanLimits, nE, detchans, rmfNames(rmfNum),data->gqString(),true));


     DataUtility::readArrays(*rmf, table);
     DataUtility::groupArrays(*rmf, startChan, endChan);


     rmfData(rmfNum, RefCountPtr<ResponseMatrix>(rmf.release()));
     // Group the EBOUNDS arrays (only for first RMF)
     if (rmfNum == 0) groupEbounds();

  }

  catch (YellowAlert&)
  {
     // diagnostic for Xspec errors, which might be thrown by the 
     // ResponseMatrix constructor.
     throw;
  }  

  catch (std::exception&)
  {
     // e.g. bad_alloc.       
     throw;
  }
}

void SPI_Response::setDescription (size_t specNum, size_t groupNum)
{
  CCfits::ExtHDU&  table = dataSource(0)->extension(extensionName());  

  string  keyvalue("");  
  // Read TELESCOPE, INSTRUMENT and CHANTYPE keywords if exist. 
  // Comparison will be done in setSharedDescription so 
  // reference counted responses will also be checked.
  try
  {
     table.keyWord(TELESCOPE()).value(keyvalue);
     rmfData(currentRMF())->telescope(keyvalue);
  }

  catch(CCfits::HDU::NoSuchKeyword)
  {
     tcout << "Warning:TELESCOPE keyword not found in the rmf file" << std::endl;
  }

  try
  {
     table.keyWord(INSTRUMENT()).value(keyvalue);
     rmfData(currentRMF())->instrument(keyvalue);
  }

  catch(CCfits::HDU::NoSuchKeyword)
  {
     tcout << "Warning:INSTRUMENT keyword not found in the rmf file" << std::endl;
  }

  try
  {
     table.keyWord(CHANNELTYPE()).value(keyvalue); 
     rmfData(currentRMF())->channelType(keyvalue);        
  }

  catch(CCfits::HDU::NoSuchKeyword)
  {
    tcout << "Warning:CHANNELTYPE keyword not found in the rmf file" << std::endl;
  }

  setSharedDescription(specNum, groupNum);
}

void SPI_Response::groupEbounds ()
{

  SpectralData*  data=source();

  size_t  rmfNum=currentRMF();
  RefCountPtr<ResponseMatrix> rmf(rmfData()[rmfNum]);   

  const IntegerArray& grouping = data->groupingInfo();
  const IntegerArray& quality  = data->qualityInfo();  

  bool qualitySet = !quality.empty();
  bool grouped = !grouping.empty();

  try
  {
     int  startChan = data->startChan();
     int  endChan   = data->endChan();

     size_t  ungroupedChannels=endChan - startChan + 1;
     size_t  groupedChannels=0;

     // Load the EBOUNDS energies into ResponseMatrix object
     CCfits::ExtHDU&  eboundsTable =
     dataSource()->extension(eboundsExtName()); 

     RealArray  eMin(ungroupedChannels);
     RealArray  eMax(ungroupedChannels);

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

  catch( ... )
  {
     throw;
  }
}

// Additional Declarations
