//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <CCfits/CCfits>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>
#include <XSsymbol.h>
#include <DataFactory/DataFactory.h>
#include <GlobalContainer/ResponseContainer.h>
#include <Data/Detector/MultiResponse.h>
#include <Data/Detector/UserDummyResponse.h>
#include <Data/SpectralData.h>
#include <Data/DataUtility.h>
#include <Data/DataSetBase.h>
#include <Data/FakeDataInputRecord.h>
#include <Parse/XSparse.h>
#include <algorithm>
#include <GlobalContainer/DataSetTypes.h>

// SPI_Data
#include <SPI_Data.h>


// Class SPI_Data 
const string SPI_Data::s_RESPONSEDB = "RESPONSE_DB";
const string SPI_Data::s_NRMF = "NRMF";
const string SPI_Data::s_ISDCTYPE = "ISDC/OGIP";
const string SPI_Data::s_RESPFILEDB = "RESPFILE_DB";
std::vector<std::string> SPI_Data::s_SPIspectrumKeys;


SPI_Data::SPI_Data()
  : OGIP_92aData(),
    m_isFirstSpectrum(true),
    m_rmfNames(),
    m_arfNames()
{
  if (s_SPIspectrumKeys.empty())
  {
        s_SPIspectrumKeys.push_back(TELESCOPE());       
        s_SPIspectrumKeys.push_back(INSTRUMENT());       
        s_SPIspectrumKeys.push_back(BACKFILE());       
        s_SPIspectrumKeys.push_back(CORRFILE()); 

        s_SPIspectrumKeys.push_back(CHANNEL());       
        s_SPIspectrumKeys.push_back(CHANNELTYPE());       
        s_SPIspectrumKeys.push_back(GROUPING());       
        s_SPIspectrumKeys.push_back(QUALITY()); 
        s_SPIspectrumKeys.push_back(COUNTS());       
        s_SPIspectrumKeys.push_back(RATE());       
        s_SPIspectrumKeys.push_back(SYSTEMATIC());       
        s_SPIspectrumKeys.push_back(STATISTICAL());       
        s_SPIspectrumKeys.push_back(POISSERR());       

        s_SPIspectrumKeys.push_back(AREASCALE());       
        s_SPIspectrumKeys.push_back(BACKSCALE());       
        s_SPIspectrumKeys.push_back(CORRSCALE());               
        s_SPIspectrumKeys.push_back(EXPOSURE());   

        s_SPIspectrumKeys.push_back(s_NRMF);   

//        int N (MAXFILTER());
//        for (int i = 1; i <= N; ++i)
//        {
//                char xflt[9] = {"\0\0\0\0\0\0\0\0"};
//                sprintf(xflt,"XFLT%04i",i);
//                s_SPIspectrumKeys.push_back(string(xflt));
//        }
  }        
}

SPI_Data::SPI_Data(const SPI_Data &right)
  : OGIP_92aData(right),
    m_isFirstSpectrum(true),
    m_rmfNames(right.m_rmfNames),
    m_arfNames(right.m_arfNames)
{
}


SPI_Data::~SPI_Data()
{
}


SPI_Data* SPI_Data::clone () const
{

  return new SPI_Data(*this);
}

bool SPI_Data::fileFormat (const string& fileName, XspecDataIO::DataType type)
{

  // NOTE: When type == ResponseType, this function will check for the
  // existence of a RESPFILE_DB extension in the input fileName.  Therefore,
  // it is NOT the same as calling SPI_Response::fileFormat with ResponseType,
  // which checks the validity of individual RMFs.  

  bool format = false;
  using namespace CCfits; 
  try
  {
        // find an extension that matches the HDUCLASS, HDUCLAS1, HDUVERS key values.
        std::vector<string> OGIP92a(2,"");
        OGIP92a[0] = HDUCLASS();
        OGIP92a[1] = HDUCLAS1();

        std::vector<string> OGIP92aVal(2,"");
        OGIP92aVal[0] = s_ISDCTYPE;

        switch (type)
        {
                default:
                case SpectrumType: 
                        OGIP92aVal[1] = SPECTYPE();
                        break;
		case ResponseType:
			OGIP92aVal[1] = RESPONSEDB();
			break;		
        }

	std::auto_ptr<FITS> p(new FITS(fileName,Read,OGIP92a,OGIP92aVal));
	// this will throw a FITS::NoSuchHDU if there's no extension with the
	// matching key values, and will be caught at the bottom, 
	// returning false.

	// For case of RESPONSE_DB check, only checking for its existence at
	// this point.  Whether it has the proper keywords, etc. will be
	// handled later during setResponse.
	if (type == ResponseType)
	{
	   return true;
	}

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
	catch (CCfits::HDU::NoSuchKeyword)
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
  catch (CCfits::FitsException&)
  {
        // return with format = false.
  }
  catch ( ... )
  {
          // any other problems need to be handled by caller
          throw;
  }

  if (!format) setExtensionName(string());
  return format;
}

void SPI_Data::closeSourceFiles ()
{
  OGIP_92aIO::closeFile();
}

void SPI_Data::setResponse (size_t spectrumNumber, size_t row)
{

  using XSContainer::responses;      

  SpectralData* source = sourceData(row);
  size_t nRmf = m_rmfNames.size();

  for (int j=0; j< origNumSources(); j++)
  {  

	const DataPrototype *proto = dataSetBase()->protoType();

	std::auto_ptr<MultiResponse> rsp(static_cast<MultiResponse*>(proto->MakeResponse()));
	//setSource(rsp.get());

	rsp->setRmfNames(m_rmfNames);
	rsp->setArfNames(StringArray(nRmf,""));
	for (size_t i=0; i < nRmf; ++i)
	{
	   rsp->arfNames(i, m_arfNames[j*nRmf+i]);
	}
	rsp->sourceNumber(j+1);
	rsp->setEffectiveArea(std::vector<RealArray>(nRmf));

	//std::vector<RefCountPtr< RMF > > m_rmfData;
	bool allRmfsOk = true;
	for (size_t  i=0; i < nRmf; ++i)
	{
	   bool responseCheck = true;
	   if (m_isFirstSpectrum)
	   {
	      responseCheck = false;
	      try
	      {
        	 responseCheck = rsp->fileFormat(m_rmfNames[i],XspecDataIO::ResponseType); 
	      }               
	      catch ( XspecDataIO::CannotOpen )
	      {
        	   string newFile("");
        	   try
        	   {
                	 responseCheck = false;
                	 // throws SkipThis or AbortLoop
                	 XSparse::getFileNameFromUser(m_rmfNames[i], newFile, XSutility::RSP);
                	 if (newFile.length() != 0)
                	 {
                        	 responseCheck = rsp->fileFormat(newFile,XspecDataIO::ResponseType); 
                        	 rsp->rmfNames(i, newFile);
				 m_rmfNames[i] = newFile;
                	 }
        	   }
        	   catch (XspecDataIO::CannotOpen) { }
                   catch (XSparse::SkipThis)
                   {
                           allRmfsOk = false;
                           throw XSparse::AbortLoop();       
                   }
                   catch (XSparse::AbortLoop)
                   {
                           allRmfsOk = false;
                           throw;       
                   }

	      }
	   }
	   if ( responseCheck )
	   {
              try
              {
		  rsp->source(source);
		  rsp->setData(spectrumNumber, dataSetBase()->dataGroup(), i);
                  rsp->setEnergies();

              }
              catch (...)
              {
                      tcout << "*** Error processing response data - file skipped\n";
                      rsp->closeSourceFiles();
		      rsp->rmfNames(i,"");
		      rsp->arfNames(i,"");
		      allRmfsOk = false;
		      if (m_isFirstSpectrum)
		      {
			 m_rmfNames.clear();
			 m_arfNames.clear();
		      }
		      break;
              }
	   }
	   else
	   {
	      // Should only get here when processing the first spectrum.
              if (m_rmfNames[i].length() > 0)
              {
                      tcout << "*** Response file has format inconsistent with data: ";
              }
              tcout << " response file " << rsp->rmfNames(i) 
        	    << " read skipped" << std::endl; 
	      allRmfsOk = false;
	      m_rmfNames.clear();
	      m_arfNames.clear();
	      break;  
	   }
	}

	if (allRmfsOk)
	{
	   MultiResponse* rspSaved = rsp.release();
	   string allRmfNames("");
	   for (size_t i = 0; i < nRmf; i++)
	   {
  		 allRmfNames += m_rmfNames[i];
		 allRmfNames += " ";
	   }  

           if ((j < static_cast<int>(source->detector().size())) && source->detector(j))
	   {
	      // SpectralData::removeResponses will also call on the response
	      // container to delete the response object.
	      source->removeResponses(j+1);
	   }     
           responses->addToList(allRmfNames, rspSaved);
           source->attachDetector(responses->responseList(allRmfNames,rspSaved->index()),j);
	}

  }
  m_isFirstSpectrum = false; 
}

size_t SPI_Data::read (const string& fileName, bool readFlag)
{
  using namespace CCfits;

  try
  {


     std::auto_ptr<FITS> source(new FITS(fileName,Read,extensionName(),readFlag,s_SPIspectrumKeys));   

     // Look for extension containing RMF/ARF names  - if found, read it
     StringArray  searchKeys(2,"");
     searchKeys[0] = HDUCLASS();  
     searchKeys[1] = HDUCLAS1();  


     StringArray  searchValues(2,"");
     searchValues[0] = ISDCTYPE();  
     searchValues[1] = RESPONSEDB();  

     StringArray  hduKeys(1,"");
     hduKeys[0] = NRMF();
     source->read(searchKeys, searchValues, readFlag, hduKeys); 
     CCfits::ExtHDU&  ext = source->currentExtension();

     origNumSources(std::max((long)1,ext.rows())); 
     // Come to think of it, if ext.rows() is empty, stop things
     // right here ...
     if (!ext.rows())
     {
        throw XspecDataIO::RequiredDataNotPresent("Empty RESPONSE_DB table");
     }    
     setDataSource(source.release(),0);


     // COUNTS/RATE must be determined early on, prior to
     // type1 vs. type2 verification which is performed in
     // OGIP's initialize. 
     getDataType();

     // extensionName() = spectrum extension throughout this function
     return dataSource(0)->extension(extensionName()).rows();

  }

  catch (FITS::NoSuchHDU)    
  {
        throw XspecDataIO::RequiredDataNotPresent("RESPONSE_DB extension");
  }

  catch (FitsException&)
  {
        throw XspecDataIO::CannotOpen(fileName);       
  }

  // debugging

  // tcerr << dataSource()->extension(extensionName()) << std::endl;
}

void SPI_Data::reportResponse (size_t row) const
{

   // Only report an aberration from Response_db extension (ie. spectrum
   // has no response loaded or a dummy/diag response loaded).
   using namespace std;
   for (int i=0; i< origNumSources() ; i++)
   {
      if (sourceData(row)->detector(i))
      {
         if (UserDummyResponse *dr = dynamic_cast<UserDummyResponse*>
	 		(sourceData(row)->detector(i)))
	 {
	    tcout <<"For Source # "<<i+1<<":  Using Dummy Response\n";
	 }
      }
      else
      {
         tcout <<"For Source # "<<i+1<<":  No Response Loaded\n";
      }
   }
}

bool SPI_Data::setResponse (SpectralData* sourceSpectrum, size_t spectrumNumber, size_t specNum, const string& responseName, const string& arfName)
{

  tcout << "Response command is not currently implemented for SPI-type data\n";
  tcout << std::flush;

  return false;
}

void SPI_Data::initialize (DataPrototype* proto, DataInputRecord& record)
{
  OGIP_92aData::initialize(proto, record);
  m_isFirstSpectrum = true;

  getRespdbInfo(dataSource());
}

void SPI_Data::reportAll ()
{
  if (dataSetBase()->spectralData()) report(0);
  else
  {
          const XSContainer::SpectralDataMap& mSD = dataSetBase()->multiSpectralData();
	  XSContainer::SpectralDataMapConstIt iSD = mSD.begin();
          XSContainer::SpectralDataMapConstIt endList = mSD.end();
	  if (iSD != endList)
	  {
	     m_isFirstSpectrum = true;
	     report(iSD->first);
	     m_isFirstSpectrum = false;
	     ++iSD;
	  }
	  while (iSD != endList)
          {
             report(iSD->first);
	     ++iSD;
          }                

  }
  tcout << std::endl;
}

void SPI_Data::report (size_t row) const
{
  using namespace std;
  //  const SpectralData* data = sourceData(row); 
  ios_base::fmtflags save(tcout.flags());     

  if (m_isFirstSpectrum)
  {
     size_t nRmfs(m_rmfNames.size());
     for (size_t j=0; j<nRmfs; j++)
     { 
	 tcout << "RMF # " << j+1 << '\n';
         tcout << left << setw(38) << " Using Response (RMF) File " <<  
                         m_rmfNames[j]  
                        // <<  " at address " << reinterpret_cast<int>(data->detector(j)) 
                         << '\n';
     }
     if (origNumSources() > 1)
     {
     	  tcout << "\nUsing Multiple Sources\n";
     }
     for (int i=0; i< origNumSources() ; i++)
     {
	tcout << "\nFor Source # " << i+1 <<'\n';
        tcout << left << setw(38) << " Using Auxiliary Response (ARF) Files " << '\n';
	for (size_t j=0; j<nRmfs; ++j)
	{
           tcout << "     " << m_arfNames[i*nRmfs+j] << '\n';
	}         
     }
     tcout <<std::endl;
  }

  DataSet::report(row);

  tcout.flags(save);                    
  tcout << flush;
}

void SPI_Data::initializeFake (DataPrototype* proto, FakeDataInputRecord& record)
{
  const std::vector<FakeDataInputRecord::BackLocator>& inBackgrounds = record.inputBackgrounds();
  size_t nSpec = record.spectrumNumber().size();
  dataSetBase() = new DataSetBase(record.fileName(), DataSet::count(),proto);
  dataGroup(record.groupNumber());
  setRunPath();
  outputFileName(record.fileName());
  int legalStart=0, legalEnd=0;

  const SPI_Data* origDSet = dynamic_cast<SPI_Data*>(record.data());
  if (origDSet)
  {
     if (nSpec != origDSet->multiSpectralData().size())
     {
        throw RedAlert("MultiSpectralData/FakeInputRecord nSpectra size discrepancy\n");
     }
     origDSet->legalChannelBounds(legalStart, legalEnd); 
     dataSetBase()->legalStartChan(legalStart);
     dataSetBase()->legalEndChan(legalEnd);
     dataSetBase()->aScaleIsKeyword(origDSet->aScaleIsKeyword());
     dataSetBase()->bScaleIsKeyword(origDSet->bScaleIsKeyword());
  }
  else
  {
     dataSetBase()->aScaleIsKeyword(true);
     dataSetBase()->bScaleIsKeyword(true);
  }  
  if (record.inputResponses(0)[0].first == DUMMY_RSP)
  {
     origNumSources(1);
  }
  else
  { 
     // Read in RESP_DB which also sets num sources.
     getRespdbInfo(record.inputResponses(0)[0].first);
  }
  size_t nSources = static_cast<size_t>(origNumSources());
  for (size_t i=0; i<nSpec; ++i)
  {
     bool isBackSuccess = false;
     size_t firstDetChans = 0;
     // NOTE: firstDetChans and detChans are not used when origDSet.
     SpectralData::ChannelInfo chanInfo;
     getNChansForFake(record, i, chanInfo); 
     size_t nChans = chanInfo.m_endChan - chanInfo.m_startChan + 1;    
     std::auto_ptr<SpectralData> asd(new SpectralData(this, nChans, i+1, nSources));
     asd->spectrumNumber(record.spectrumNumber(i));
     asd->exposureTime(record.exposureTime());
     asd->correctionScale(record.correctionNorm());

     if (!origDSet)
     {
        asd->initializeFake(chanInfo);
        asd->statName(record.statName());
        if (i == 0)
        {
           firstDetChans = chanInfo.m_detChans;
           dataSetBase()->legalStartChan(chanInfo.m_firstChan);
           dataSetBase()->legalEndChan(chanInfo.m_detChans+chanInfo.m_firstChan-1);
        }
        if (chanInfo.m_detChans != firstDetChans)
        {
           string msg("Attempting to create a fake typeII data set ");
           msg += record.fileName();
           msg += ",\n   containing spectra with differing DETCHANS.";
           msg += "\n  Check DETCHANS keywords in associated background and response files,";
           msg += "\n  and see TypeI vs TypeII output section of fakeit help.\n";
           throw YellowAlert(msg);           
        }
        if (record.inputResponses(0)[0].first == DUMMY_RSP)
        {
           const UserDummyResponse *ud = dynamic_cast<UserDummyResponse*>
                           (XSContainer::responses->responseList(DUMMY_RSP,size_t(0)));
           UserDummyResponse *newUd = new UserDummyResponse(*ud);
           newUd->dataGroup(1);
           asd->attachUserDummy(newUd, 0);
           asd->telescope("UNKNOWN");
           asd->instrument("UNKNOWN");
           asd->channelType("PHA");
        }        
     }
     else
     {
        const SpectralData* origSd = origDSet->sourceData(record.origRowNums(i));
        asd->initializeFake(origSd);
/*        // Check that we're accessing the resp_db table from the
        // same file as the original.
        if (record.inputResponses(0) == origDSet->getResponseName(0))
        {
           for (size_t j=0; j<nSources; ++j)
           {
              //UserDummyResponse *origUd = dynamic_cast<UserDummyResponse*>
                         //  (origSd->detector(
           }
        }
*/
        if (record.enteredNone())
        {
           asd->telescope("USE_FAKEIT_RMF");
           asd->instrument("USE_FAKEIT_RMF");
           asd->channelType("USE_FAKEIT_RMF");
        }
     }
     // At this point new spectrum is ready to be given over to the dataset, 
     // so the auto_ptr can relinquish ownership.
     SpectralData* sd = asd.release();
     std::map<size_t,SpectralData*>::value_type __tmp(i+1, sd);
     dataSetBase()->multiSpectralData().insert(__tmp);

     // Attach background.
     // inBackgrounds refers to the B in the M*R+B used to generate
     // the fake spectrum.  record.backgndFile refers to the output
     // file that will be a possibly Poisson randomized version of B.
     // It does not yet exist at this point (see DataSet::generateFake
     // and the output functions of this class).
     sd->backgroundFile(inBackgrounds[i].first);
     try
     {
	isBackSuccess = setBackgroundData(i+1, inBackgrounds[i].second);
        if (isBackSuccess)
        {
           outputBckFileName(record.backgndFile());
           sd->backgroundFile(record.backgndFile());
           if (!origDSet)
           {
              // If not based on original spectrum but there is an
              // original background file, re-initialize the fake
              // spectrum using the background file's settings.
              sd->reinitFromBackground();
           }
        }
     }
     catch (...)
     {
	sd->backgroundFile("");
	throw; 
     }
     if (!isBackSuccess)
     {
        if (sd->backgroundScale(0) < .0)
        {
           // backgroundScale for spectra not based on original data
           // is initialized to -1.0.  An associated background
           // file would have replaced it with its own value. 
           sd->setBackgroundScale(RealArray(1.0, sd->channels())); 
        } 
	sd->backgroundFile("");
	tcout << "\nNo background will be applied to fake spectrum #" << 
	   	     record.spectrumNumber()[i] << std::endl;
     }
     if (record.inputResponses(0)[0].first != DUMMY_RSP)
     {
        // NOTE:  If it succeeds, setResponse has inserted a new response
        //   object INTO THE GLOBAL CONTAINER.  The setResponse can
        // fail without throwing, but in that case it will have
        // cleared out the rmf and arf names arrays.
        setResponse(sd->spectrumNumber(), i+1);
        if (!m_rmfNames.size())
        {
           throw YellowAlert("Error attempting to attach responses to fake spectra\n");
        }
     }          
  } // end spectra loop
}

FakeDataInputRecord::Arfs SPI_Data::getAncillaryLocation (size_t rowNum, const FakeDataInputRecord::Detectors& respInfo) const
{
  return FakeDataInputRecord::Arfs();
}

std::pair<string,size_t>  SPI_Data::getBackCorrLocation (size_t rowNum, bool isCorr) const
{
  if (rowNum == 0)
  {
     throw YellowAlert("Improper row number for SPI data\n");
  }

  return OGIP_92aData::getBackCorrLocation(rowNum, isCorr);
}

int SPI_Data::getRespdbInfo (CCfits::FITS* openFile)
{
  try
  {
     openFile->extension(RESPFILEDB());
  }
  catch (CCfits::FITS::NoSuchHDU)
  {
     // No match found by extension name, search by HDUCLAS1 keyword instead.
     std::vector<string> searchKeys(1,"");
     searchKeys[0] = HDUCLAS1();

     std::vector<string> searchValues(1,"");
     searchValues[0] = s_RESPONSEDB;

     try
     {
        openFile->read(searchKeys, searchValues, true); 
     }
     catch (CCfits::FITS::NoSuchHDU)
     {
        throw XspecDataIO::RequiredDataNotPresent("RESPONSE_DB extension");        
     }   
  }

  CCfits::ExtHDU& ext = openFile->currentExtension();
  int nRows = static_cast<int>(ext.rows());
  // This check would have already been performed if we got here
  // through the regular initialization function, but not through
  // the initializeFake function.
  if (nRows == 0)
  {
     throw XspecDataIO::RequiredDataNotPresent("Empty RESPONSE_DB table");
  }
  origNumSources(nRows);

  int nRmf=0;
  try
  {
     ext.readKey(NRMF(),nRmf);
  }
  catch (CCfits::HDU::NoSuchKeyword)
  {
     throw XspecDataIO::RequiredDataNotPresent("NRMF keyword");
  } 

  m_rmfNames.resize(nRmf);
  m_arfNames.resize(nRmf*nRows);
  for (int  i=1; i<=nRmf; ++i)
  {
     try
     {
	 std::ostringstream  sr, sa;
	 sr << "RMF" << i;
	 ext.readKey(sr.str(), m_rmfNames[i-1]);
	 if (m_rmfNames[i-1].length() == 0)
	 {
            throw XspecDataIO::RequiredDataNotPresent("Missing SPI_Response file name");
	 }

	 sa << "ARF" << i;
	 std::vector<string> tmpFiles(nRows,"");
	 ext.column(sa.str()).read(tmpFiles,1,nRows);
	 for (int j=0; j<nRows; ++j)
	 {
	    m_arfNames[j*nRmf+i-1] = tmpFiles[j];
	 }
     }
     catch (...)
     {
	 throw XspecDataIO::RequiredDataNotPresent("Error Reading RMF and ARF File Names");
     }
  }
  return nRows;
}

int SPI_Data::getRespdbInfo (const string& fileName)
{
  CCfits::FITS *inFile=0;
  int nSources = 0;
  try
  {
     inFile = new CCfits::FITS(fileName);
  }
  catch (CCfits::FitsException&)
  {
     tcout <<"\nError: Cannot find file containing RESP_DB extension."<<std::endl;
     try
     {
        string newName;
        XSparse::getFileNameFromUser(fileName, newName, XSutility::PHA);
        inFile = new CCfits::FITS(newName);
     }
     catch (...)
     {
        delete inFile;
        throw;
     }
  }

  try 
  {
     nSources = getRespdbInfo(inFile);
  }
  catch (...)
  {
     delete inFile;
     throw;
  }
  delete inFile;
  return nSources;
}

void SPI_Data::getNChansForFake (FakeDataInputRecord& record, const size_t index, SpectralData::ChannelInfo& chanInfo)
{
  const DataSet* origDset = record.data();
  if (origDset)
  {
     const SpectralData* origSd = origDset->sourceData(record.origRowNums(index));
     chanInfo.m_startChan = origSd->startChan();
     chanInfo.m_endChan = origSd->endChan();
     int legalStartChan=0, legalEndChan=0;
     origDset->legalChannelBounds(legalStartChan, legalEndChan);
     chanInfo.m_detChans = legalEndChan - legalStartChan + 1;
  }
  else
  {
     // No original data, first see if there's a background file to 
     // base this on.  If not, then get from response.
     const string& backFile = record.inputBackgrounds(index).first;
     size_t backRow = record.inputBackgrounds(index).second;
     if (backFile.length())
     {
          // Open background file and read just enough info to get its
          // range of non-grouped, non-ignored channels , and read its 
          // DETCHANS keyword.  Any other format checking will be
          // performed at a later point.
          try
          {
             std::auto_ptr<CCfits::FITS> bFile(openFitsExtension(backFile,
                        XspecDataIO::SpectrumType));
             int start=0, end=0;
             readChannelBounds(bFile->currentExtension(), start, end, backRow);
             size_t legalFirst=0, legalEnd=0;
             readChannelLimits(bFile->currentExtension(), STD_FIRST_CHAN(), 
                        legalFirst, legalEnd);
             chanInfo.m_startChan = static_cast<size_t>(start);
             chanInfo.m_endChan = static_cast<size_t>(end);
             chanInfo.m_firstChan = legalFirst;
             chanInfo.m_detChans = legalEnd - legalFirst + 1;
          }
          catch (...)
          {
             string msg("    attempting to read channel keyword and values from: ");
             msg += backFile;
             throw XspecDataIO::CatchAllIO(msg);
          }
          if (chanInfo.m_detChans < (chanInfo.m_endChan - chanInfo.m_startChan + 1))
          {
             string msg("DETCHANS value is less than the number of channels");
             msg += "\n    found in file: ";
             msg += backFile;
             msg += "\n";
             throw YellowAlert(msg);             
          }
     }
     // No matter which spectrum, only check the responseName 
     // for index = 0.
     else if (record.inputResponses(0)[0].first == DUMMY_RSP)
     {
          chanInfo.m_detChans = XSContainer::responses->responseList(DUMMY_RSP,size_t(0))->numEnergies();
          chanInfo.m_startChan = chanInfo.m_firstChan = DataSet::STD_FIRST_CHAN();
          chanInfo.m_endChan = chanInfo.m_startChan + chanInfo.m_detChans - 1;
     }
     else
     {
        // Determine specChans from the associated response.
        // We already know that the inputResponse name leads to a 
        // RESP_DB table due to the getRespdbInfo function.  We
        // don't yet know if any of its rmf and arf names are valid.
        // For the purposes of getting DETCHANS, we'll only check
        // the first rmf.
        try
        {
           std::pair<int,int> vals = getChanInfoFromResponse(m_rmfNames[0]);
           chanInfo.m_startChan = chanInfo.m_firstChan = (vals.first >= 0 ) ?
                      static_cast<size_t>(vals.first) : DataSet::STD_FIRST_CHAN();
           chanInfo.m_detChans = static_cast<size_t>(vals.second);
           chanInfo.m_endChan = chanInfo.m_startChan + chanInfo.m_detChans - 1;
        }
        catch (XspecDataIO::CannotOpen)
        {
           string newFile("");
           XSparse::getFileNameFromUser(m_rmfNames[0], newFile, XSutility::RSP);
           std::pair<int,int> vals = getChanInfoFromResponse(newFile);
           chanInfo.m_startChan = chanInfo.m_firstChan = (vals.first >= 0 ) ?
                      static_cast<size_t>(vals.first) : DataSet::STD_FIRST_CHAN();
           chanInfo.m_detChans = static_cast<size_t>(vals.second);
           chanInfo.m_endChan = chanInfo.m_startChan + chanInfo.m_detChans - 1;
           m_rmfNames[0] = newFile;

        }
     }
  }
}

void SPI_Data::outputData ()
{
  try
  {
     write(outputFileName());
  }
  catch (YellowAlert&)
  {
     return;
  }

  const SpectralData *sd1 = sourceData(1);
  string hduName = SPECTYPE();
  size_t nChans = getMaxChannels();
  size_t nSpec = numSpectra();
  size_t nSources = origNumSources();
  CCfits::Table *tbl=0;

  BoolArray optCols(NOPTCOLS,false);
  optCols[COUNTS_COL] = sd1->isPoisson();
  optCols[STATERR_COL] = !sd1->isPoisson();
  optCols[QUAL_COL] = anyQuality();
  optCols[GROUP_COL] = anyGrouping();
  optCols[ASCALE_COL] = !aScaleIsKeyword();
  optCols[BSCALE_COL] = !bScaleIsKeyword();

  tbl = makeSpectraTable(hduName, nChans, optCols);
  writeCommonKeys(tbl);
  tbl->writeHistory("Constructed from the models: ");
  for (size_t i=1; i<=nSources; ++i)
  {
     const string& name = dataSetBase()->modelNamesForFake(i-1);
     if (name.length())
     {
        std::ostringstream os;
        os << "   " << i << ":" << "  " << name;
        tbl->writeHistory(os.str());
     }
  }
  tbl->addKey(HDUCLAS1(), SPECTYPE(), "PHA dataset (OGIP memo OGIP-92-007)");
  string buffer = sd1->channelType();
  tbl->addKey(CHANNELTYPE(), buffer, "channel type (PHA, PI etc)");
  string nullStr("");
  if (!optCols[ASCALE_COL])
  {
     tbl->addKey(AREASCALE(), sd1->areaScale(0), "area scaling factor");
  }
  tbl->addKey(BACKFILE(), nullStr, "associated background filename");
  if (!optCols[BSCALE_COL])
  {
     tbl->addKey(BACKSCALE(), sd1->backgroundScale(0), "background file scaling factor");
  }
  tbl->addKey(EXPOSURE(),sd1->exposureTime(), "exposure (in seconds)");
  std::ostringstream tlmin;
  std::ostringstream tlmax;
  int nChanCol = tbl->column(CHANNEL()).index();
  tlmin << "TLMIN" << nChanCol;
  tlmax << "TLMAX" << nChanCol;
  tbl->addKey(tlmin.str(),dataSetBase()->legalStartChan(),"Lowest legal channel number");
  tbl->addKey(tlmax.str(),dataSetBase()->legalEndChan(),"Highest legal channel number");  
  tbl->addKey(DETCHANS(),dataSetBase()->legalEndChan() -
        dataSetBase()->legalStartChan() + 1,"total number possible channels");
  tbl->addKey(CORRSCALE(), 0.0, "Correction file scaling factor");
  tbl->addKey(POISSERR(), sd1->isPoisson(), "Pois. err assumed ?");
  IntegerArray specNums(nSpec);
  for (size_t i=0; i<nSpec; ++i)
  {
     specNums[i] = i+1;
  }
  tbl->column(NUMKEY()).write(specNums, 1);
  for (size_t j=1; j<=nSpec; ++j)
  {
     const SpectralData *sd = sourceData(j);
     int nl=-9999;
     std::vector<std::valarray<int> > chans(1);
     std::vector<std::valarray<int> > quality(1);
     std::vector<std::valarray<int> > grouping(1);
     size_t ungroupedChans = sd->endChan() - sd->startChan() + 1;
     chans[0].resize(ungroupedChans);
     std::valarray<int>& chans0 = chans[0];
     size_t startChan = sd->startChan();     
     for (size_t i=0; i < ungroupedChans; ++i)
     {
        chans0[i] = i + startChan;
     }

     std::valarray<int>& quality0 = quality[0];
     std::valarray<int>& grouping0 = grouping[0];
     // sd's quality and grouping are vector<int> not valarrays, hence
     // the element by element copy.
     if (sd->qualityInfo().size())
     {
        quality0.resize(ungroupedChans);
        for (size_t i=0; i<ungroupedChans; ++i)
        {
           quality0[i] = sd->qualityInfo(i);
        }
     }
     else if (optCols[QUAL_COL])
     {
        // The table has a quality column and is expecting something 
        // even though spectrum has no quality info.  So, make a dummy
        // quality array of all 0's.
        quality0.resize(ungroupedChans,0);
     }
     if (sd->groupingInfo().size())
     {
        grouping0.resize(ungroupedChans);
        for (size_t i=0; i<ungroupedChans; ++i)
        {
           grouping0[i] = sd->groupingInfo(i);
        }
     }
     else if (optCols[GROUP_COL])
     {
        // Make a dummy gouping array of all 1's.
        grouping0.resize(ungroupedChans,1);
     }

     IntegerArray lostChans;
     DataUtility::getLostChannelNumbers(sd->qualityInfo(), sd->groupingInfo(),
                        lostChans);
     std::vector<RealArray> fillTmp(1);
     Real defVal = 0.0;
     if (sd1->isPoisson())
     {
        // output counts and no stat_err column.
        std::vector<std::valarray<int> > iFillTmp(1);
        const RealArray rcounts = sd->spectrum()*sd->exposureTime()*sd->areaScale();
        size_t nC = sd->channels();
        std::valarray<int> counts(nC);
        for (size_t i=0; i<nC; ++i)
        {
           counts[i] = static_cast<int>(rcounts[i] + .5);
        }
        int iVal = 0;
        DataUtility::fillLostChannels(lostChans, counts, iFillTmp[0], iVal);
        tbl->column(COUNTS()).writeArrays(iFillTmp, j, &nl);
     }
     else
     {
        // output rate (cts/sec) and stat_err column.
        RealArray rate(sd->channels());
        rate = sd->spectrum()*sd->areaScale();
        DataUtility::fillLostChannels(lostChans, rate, fillTmp[0], defVal);
        tbl->column(RATE()).writeArrays(fillTmp, j);
        RealArray tmpErr(sd->rawVariance()*sd->areaScale()*sd->areaScale());
        DataUtility::fillLostChannels(lostChans, tmpErr, fillTmp[0], defVal);
        fillTmp[0] = sqrt(fillTmp[0]);
        tbl->column(STATISTICAL()).writeArrays(fillTmp, j);
     }

     defVal = -999.0;
     tbl->column(CHANNEL()).writeArrays(chans, j, &nl);
     if (optCols[QUAL_COL])
     {
        tbl->column(QUALITY()).writeArrays(quality, j, &nl);
     }
     if (optCols[GROUP_COL])
     {
        tbl->column(GROUPING()).writeArrays(grouping, j, &nl);
     }
     if (optCols[ASCALE_COL])
     {
        DataUtility::fillLostChannels(lostChans, sd->areaScale(), fillTmp[0], defVal);
        tbl->column(AREASCALE()).writeArrays(fillTmp, j);
     }
     if (optCols[BSCALE_COL])
     {
        DataUtility::fillLostChannels(lostChans, sd->backgroundScale(), fillTmp[0], defVal);
        tbl->column(BACKSCALE()).writeArrays(fillTmp, j);
     }
  }

  hduName = RESPFILEDB();
  size_t nRmfs = m_rmfNames.size();
  tbl = makeRespdbTable(hduName);
  writeCommonKeys(tbl);
  tbl->addKey(HDUCLAS1(), RESPONSEDB(), "");
  tbl->addKey(NRMF(), nRmfs, "Number of RMFs to be used");
  for (size_t i=1; i<=nRmfs; ++i)
  {
     std::ostringstream rmfKey, rmfComm;
     rmfKey << "RMF" << i;
     rmfComm << "RMF file " << i << ":";
     tbl->addKey(rmfKey.str(), m_rmfNames[i-1], rmfComm.str());
  }

  for(size_t i=0; i<nSources; ++i)
  {
     int nl = -100;
     IntegerArray modNum(1, i+1);
     tbl->column(string("MODEL")).write(modNum, i+1, &nl);
     for (size_t j=0; j<nRmfs; ++j)
     {
        std::ostringstream arfCol;
        arfCol << "ARF" << j+1;
        StringArray arf(1, m_arfNames[j+i*nRmfs]);
        tbl->column(arfCol.str()).write(arf, i+1);
     }
  }

  closeFile();
}

void SPI_Data::writeCommonKeys (CCfits::Table* tbl)
{
  // Keys common to both spectrum and resp_db extensions.
  tbl->addKey(HDUCLASS(), ISDCTYPE(), "Format confirms to OGIP standard");
  tbl->addKey("HDUVERS1","1.1.0","Version of format (OGIP memo OGIP-92-007a)");
  // Needed to get around CCfits addKey const problem.
  string buffer = "INTEGRAL";
  tbl->addKey(TELESCOPE(), buffer, "mission/satellite name");
  buffer = "SPI";
  tbl->addKey(INSTRUMENT(), buffer, "instrument/detector name");
  tbl->writeDate();
  string history("Fake data file created by ");
  history += xspecVersion();
  history += " \"fakeit\" command";
  tbl->writeHistory(history);
}

CCfits::Table* SPI_Data::makeSpectraTable (string& hduName, size_t nChans, const BoolArray& optCols)
{
     std::ostringstream vectorWidth;
     string formatStr(""); 
     size_t nSpec =  numSpectra();  
     StringArray colName;
     StringArray colForm;
     StringArray colUnit;
     // The first 4 cols: specnum, id_string, channel, counts/rate,
     // are mandatory.
     colName.resize(4);
     colForm.resize(4);
     colUnit.resize(4);
     vectorWidth << nChans;
     const string vw = vectorWidth.str();
     colName[0] = NUMKEY();
     colForm[0] = string("I");
     colName[1] = string("ROWID");
     colForm[1] = string("20A");
     colName[2] = CHANNEL();
     formatStr = vectorWidth.str() + "I";
     colForm[2] = formatStr;     
     if (optCols[COUNTS_COL])
     {
        colName[3] = COUNTS();
        formatStr = vw + "J";
        colForm[3] = formatStr;
        colUnit[3] = string("");
     }
     else
     {
        colName[3] = RATE();
        formatStr = vw + "E";
        colForm[3] = formatStr;
        colUnit[3] = string("counts/s");
     }

     // optional cols: ie. quality, grouping, ascale, bscale.
     for (size_t i=COUNTS_COL+1; i<NOPTCOLS; ++i)
     {
        size_t nOffset = 1; // Due to CHANNEL col (see OGIP IO header)
        if (optCols[i])
        {
           formatStr = vw + typeIColForms()[i+nOffset];
           colName.push_back(typeIColNames()[i+nOffset]);
           colForm.push_back(formatStr);
           colUnit.push_back(typeIColUnits()[i+nOffset]);
        }
     }

     CCfits::Table* tbl = dataSource()->addTable(hduName,nSpec, colName, colForm,colUnit);
     return tbl;  
}

CCfits::Table* SPI_Data::makeRespdbTable (string& hduName)
{
     size_t nSources = origNumSources();
     size_t nRmfs = m_rmfNames.size();
     StringArray colName;
     StringArray colForm;
     StringArray colUnit;
     colName.resize(4);
     colForm.resize(4);
     colUnit.resize(4);
     colName[0] = string("MODEL");
     colForm[0] = string("I");
     colName[1] = string("SOURCE_ID");
     colForm[1] = string("25A");
     colName[2] = string("RA");
     colForm[2] = string("E");
     colUnit[2] = string("Degrees");
     colName[3] = string("DEC");
     colForm[3] = string("E");
     colUnit[3] = string("Degrees");
     for (size_t i=1; i<=nRmfs; ++i)
     {
        std::ostringstream name;
        name << "ARF" << i;
        colName.push_back(name.str());
        colForm.push_back(string("80A"));
        colUnit.push_back(string("type-2"));
     }
     CCfits::Table* tbl = dataSource()->addTable(hduName,nSources, colName, 
                                colForm,colUnit);
     return tbl;  
}

FakeDataInputRecord::Detectors SPI_Data::getResponseName (size_t rowNum) const
{
    // All SPI can do is give it's own filename INDEPENDENT of rowNum,
    // and the caller can get whatever response info it needs from the 
    // resp_db extension.
    FakeDataInputRecord::Detectors fullPathLoc(1);
    fullPathLoc[0].first = getFullPathName();
    fullPathLoc[0].second = 0;  // sourceNum is irrelevant here.
    return fullPathLoc;
}

// Additional Declarations
