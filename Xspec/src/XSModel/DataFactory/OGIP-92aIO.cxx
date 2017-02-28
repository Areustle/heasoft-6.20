//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <sstream>
#include <XSModel/DataFactory/OGIP-92aIO.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSModel/Data/DataUtility.h>
#include <XSUtil/Utils/XSstream.h>
#include <CCfits/CCfits>
#include <algorithm>
#include <XSstreams.h>
#include <fstream>
#include <cstdlib>


// Class OGIP_92aIO::IncorrectDataType 

OGIP_92aIO::IncorrectDataType::IncorrectDataType (const string& diag)
  : YellowAlert(" file keyword (or column) is wrong data type:  ")  
{
  tcerr << diag << '\n';
}


// Class OGIP_92aIO 
const string OGIP_92aIO::s_HDUCLASS = "HDUCLASS";
const string OGIP_92aIO::s_HDUVERS = "HDUVERS";
const string OGIP_92aIO::s_PHAVERSN = "PHAVERSN";
const string OGIP_92aIO::s_VERSKEY = "1.";
const string OGIP_92aIO::s_SPECTYPE = "SPECTRUM";
const string OGIP_92aIO::s_HDUCLAS1 = "HDUCLAS1";
const string OGIP_92aIO::s_RMFVERSN = "RMFVERSN";
const string OGIP_92aIO::s_ARFVERSN = "ARFVERSN";
const string OGIP_92aIO::s_OGIPVERS = "1992a";
const string OGIP_92aIO::s_RESPTYPE = "RESPONSE";
const string OGIP_92aIO::s_SPECRESPTYPE = "SPECRESP";
const string OGIP_92aIO::s_HDUCLAS2 = "HDUCLAS2";
const string OGIP_92aIO::s_OGIPTYPE = "OGIP";
const string OGIP_92aIO::s_NUMKEY = "SPEC_NUM";
const string OGIP_92aIO::s_CHANNEL = "CHANNEL";
const string OGIP_92aIO::s_TELESCOPE = "TELESCOP";
const string OGIP_92aIO::s_INSTRUMENT = "INSTRUME";
const string OGIP_92aIO::s_BACKFILE = "BACKFILE";
const string OGIP_92aIO::s_RESPFILE = "RESPFILE";
const string OGIP_92aIO::s_CORRFILE = "CORRFILE";
const string OGIP_92aIO::s_ANCRFILE = "ANCRFILE";
const string OGIP_92aIO::s_AREASCALE = "AREASCAL";
const string OGIP_92aIO::s_BACKSCALE = "BACKSCAL";
const string OGIP_92aIO::s_CORRSCALE = "CORRSCAL";
const string OGIP_92aIO::s_EXPOSURE = "EXPOSURE";
const string OGIP_92aIO::s_FILTER = "XFLT";
const string OGIP_92aIO::s_GROUPING = "GROUPING";
const string OGIP_92aIO::s_QUALITY = "QUALITY";
const size_t OGIP_92aIO::s_MAXFILTER = 10;
const string OGIP_92aIO::s_CHANNELTYPE = "CHANTYPE";
const string OGIP_92aIO::s_COUNTS = "COUNTS";
const string OGIP_92aIO::s_RATE = "RATE";
const string OGIP_92aIO::s_SYSTEMATIC = "SYS_ERR";
const string OGIP_92aIO::s_STATISTICAL = "STAT_ERR";
const string OGIP_92aIO::s_POISSERR = "POISSERR";
const string OGIP_92aIO::s_MINENERGY = "E_MIN";
const string OGIP_92aIO::s_MAXENERGY = "E_MAX";
const string OGIP_92aIO::s_ENERGYLO = "ENERG_LO";
const string OGIP_92aIO::s_ENERGYHI = "ENERG_HI";
const string OGIP_92aIO::s_NGROUP = "N_GRP";
const string OGIP_92aIO::s_NCHANNEL = "N_CHAN";
const string OGIP_92aIO::s_FCHANNEL = "F_CHAN";
const string OGIP_92aIO::s_MATRIX = "MATRIX";
const string OGIP_92aIO::s_DETNAM = "DETNAM";
const string OGIP_92aIO::s_DETCHANS = "DETCHANS";
const string OGIP_92aIO::s_RSPVERSN = "RSPVERSN";
const string OGIP_92aIO::s_EBOUNDS = "EBOUNDS";
const string OGIP_92aIO::s_RSPMATRIXTYPE = "RSP_MATRIX";
const string OGIP_92aIO::s_HDUCLAS3 = "HDUCLAS3";
const string OGIP_92aIO::s_GSLOP_MIN = string("GSLOP_MIN");
const string OGIP_92aIO::s_GSLOP_MAX = string("GSLOP_MAX");
const string OGIP_92aIO::s_GOFFS_MIN = string("GOFFS_MIN");
const string OGIP_92aIO::s_GOFFS_MAX = string("GOFFS_MAX");
std::vector<std::string> OGIP_92aIO::s_SpectrumKeys;

std::vector<std::string> OGIP_92aIO::s_ResponseKeys;
std::vector<std::string> OGIP_92aIO::s_AuxResponseKeys;
std::vector<std::string> OGIP_92aIO::s_typeIColNames;
std::vector<std::string> OGIP_92aIO::s_typeIColForms;
std::vector<std::string> OGIP_92aIO::s_typeIColUnits;

OGIP_92aIO::OGIP_92aIO()
      : m_extensionName(""),
        m_net(false),
        m_counts(false),
        m_netIsSet(false),
        m_qualityStorage(OGIP_92aIO::NO_STORE),
        m_groupingStorage(OGIP_92aIO::NO_STORE),
        m_extVers(-1),
        m_dataSource(1,static_cast<CCfits::FITS*>(0))
{
  if (s_SpectrumKeys.empty())
  {
        s_SpectrumKeys.push_back(s_TELESCOPE);       
        s_SpectrumKeys.push_back(s_INSTRUMENT);       
        s_SpectrumKeys.push_back(s_BACKFILE);       
        s_SpectrumKeys.push_back(s_RESPFILE);       
        s_SpectrumKeys.push_back(s_ANCRFILE);       
        s_SpectrumKeys.push_back(s_CORRFILE); 

        s_SpectrumKeys.push_back(s_CHANNEL);       
        s_SpectrumKeys.push_back(s_CHANNELTYPE);       
	s_SpectrumKeys.push_back(s_DETCHANS);  
        s_SpectrumKeys.push_back(s_GROUPING);       
        s_SpectrumKeys.push_back(s_QUALITY); 
        s_SpectrumKeys.push_back(s_COUNTS);       
        s_SpectrumKeys.push_back(s_RATE);       
        s_SpectrumKeys.push_back(s_SYSTEMATIC);       
        s_SpectrumKeys.push_back(s_STATISTICAL);       
        s_SpectrumKeys.push_back(s_POISSERR);       

        s_SpectrumKeys.push_back(s_AREASCALE);       
        s_SpectrumKeys.push_back(s_BACKSCALE);       
        s_SpectrumKeys.push_back(s_CORRSCALE);               
        s_SpectrumKeys.push_back(s_EXPOSURE);   

        s_SpectrumKeys.push_back(s_BACKSCALE);       
        s_SpectrumKeys.push_back(s_CORRSCALE);  

        for (size_t i = 1; i <= s_MAXFILTER; ++i)
        {
                char xflt[9] = {"\0\0\0\0\0\0\0\0"};
                sprintf(xflt,"XFLT%04i",(int)i);
                s_SpectrumKeys.push_back(string(xflt));
        }
  }        

  if (s_ResponseKeys.empty())
  {
        s_ResponseKeys.push_back(s_TELESCOPE);       
        s_ResponseKeys.push_back(s_INSTRUMENT);       
	s_ResponseKeys.push_back(s_CHANNELTYPE);  

	s_ResponseKeys.push_back(s_RMFVERSN);  
	s_ResponseKeys.push_back(s_RSPVERSN);  
	s_ResponseKeys.push_back(s_HDUCLASS);  
	s_ResponseKeys.push_back(s_HDUCLAS1);  
	s_ResponseKeys.push_back(s_HDUCLAS2);  
	s_ResponseKeys.push_back(s_DETNAM);  
	s_ResponseKeys.push_back(s_DETCHANS);  

	s_ResponseKeys.push_back(s_CHANNEL);  
	s_ResponseKeys.push_back(s_MINENERGY);  
	s_ResponseKeys.push_back(s_MAXENERGY);  

	s_ResponseKeys.push_back(s_ENERGYLO);  
	s_ResponseKeys.push_back(s_ENERGYHI);  
	s_ResponseKeys.push_back(s_NGROUP);  
	s_ResponseKeys.push_back(s_NCHANNEL);  
	s_ResponseKeys.push_back(s_FCHANNEL);  
	s_ResponseKeys.push_back(s_MATRIX);         
	s_ResponseKeys.push_back(s_GSLOP_MIN);         
	s_ResponseKeys.push_back(s_GSLOP_MAX);         
	s_ResponseKeys.push_back(s_GOFFS_MIN);         
	s_ResponseKeys.push_back(s_GOFFS_MAX);         
  } 

  if (s_typeIColNames.empty())
  {
     s_typeIColNames.resize(NOPTCOLS+1);
     s_typeIColForms.resize(NOPTCOLS+1);
     s_typeIColUnits.resize(NOPTCOLS+1);
     s_typeIColNames[0] = s_CHANNEL;
     s_typeIColForms[0] = string("J");
     s_typeIColNames[COUNTS_COL+1] = s_RATE;
     s_typeIColForms[COUNTS_COL+1] = string("E");
     s_typeIColUnits[COUNTS_COL+1] = string("counts/s");
     s_typeIColNames[STATERR_COL+1] = s_STATISTICAL;
     s_typeIColForms[STATERR_COL+1] = string("E");
     s_typeIColUnits[STATERR_COL+1] = string("counts/s");
     s_typeIColNames[QUAL_COL+1] = s_QUALITY;
     s_typeIColForms[QUAL_COL+1] = string("I");
     s_typeIColNames[GROUP_COL+1] = s_GROUPING;
     s_typeIColForms[GROUP_COL+1] = string("I");
     s_typeIColNames[ASCALE_COL+1] = s_AREASCALE;
     s_typeIColForms[ASCALE_COL+1] = string("E");
     s_typeIColUnits[ASCALE_COL+1] = string("cm**2");
     s_typeIColNames[BSCALE_COL+1] = s_BACKSCALE;
     s_typeIColForms[BSCALE_COL+1] = string("E");     
  }       
}

OGIP_92aIO::OGIP_92aIO(const OGIP_92aIO &right)
      :   m_extensionName(right.m_extensionName),
          m_net(right.m_net),
          m_counts(right.m_counts),
          m_netIsSet(right.m_netIsSet),
          m_qualityStorage(right.m_qualityStorage),
          m_groupingStorage(right.m_groupingStorage),
          m_extVers(right.m_extVers),
        // This will NOT do a deep copy of any right m_dataSource pointer.  
        // That would require a FITS::clone, which is currently unimplemented.
        // But since copy ctor should only be called from a prototype, there
        // ought not to be any non-null right m_dataSource pointers.
          m_dataSource(right.m_dataSource.size(),0)
{
   // Lets enforce the no non-null dataSource pointers rule described above.
   for (size_t i=0; i<right.m_dataSource.size(); ++i)
   {
      if (right.m_dataSource[i])
         throw RedAlert("Cannot perform a deep copy of a FITS pointer in OGIP_92aIO copy constructor.");
   }
}


OGIP_92aIO::~OGIP_92aIO()
{
  // will call FITS::~FITS() which calls FITS::destroy() throw()
  std::vector<CCfits::FITS*>::iterator df(m_dataSource.begin());
  std::vector<CCfits::FITS*>::iterator dfEnd(m_dataSource.end());
  while (df != dfEnd)
  {
          delete *df;
          ++df;
  }
  m_dataSource.clear();      
}


size_t OGIP_92aIO::read (const string& fileName, bool readFlag)
{
  using namespace CCfits;

  // remove any extension specifiers from fileName because they are
  // included in m_extensionName and m_extVers

  string::size_type squareLoc = string::npos;
  string::size_type plusLoc = string::npos;
  bool isExtended = XSparse::checkExtendedSyntax(fileName,squareLoc,plusLoc);
  string openFileName(fileName);
  if ( squareLoc != string::npos ) openFileName = fileName.substr(0,squareLoc);
  if ( plusLoc != string::npos ) openFileName = fileName.substr(0,plusLoc);

  try
  {
     std::vector<string> dummy;
     m_dataSource[0] = new FITS(openFileName,Read,m_extensionName,readFlag,s_SpectrumKeys,
                                dummy, m_extVers);
     ExtHDU& dataExt = m_dataSource[0]->extension(m_extensionName);

     // Need to determine if this is type1 or type2.  specNum relies on
     // COUNTS or RATE col to figure this out.  Therefore must first call
     // getDataType().
     getDataType();
     if ( specNum(dataExt) ) 
     {
	return dataExt.rows();
     }
     else 
        return 1;
  }
  catch (FitsException&)
  {
     throw XspecDataIO::CannotOpen(openFileName);       
  }
  // debugging

  // tcerr << dataSource()->extension(extensionName()) << std::endl;
}

void OGIP_92aIO::write (const string& fileName)
{
  using namespace CCfits;
  // If fileName already exists, this function WILL NOT APPEND.
  // It will ERASE the file and start again.  We must assume
  // by this point that this is OK with user.
  try
  {
     string rmString("rm -f ");
     if (m_dataSource[0])
     {
        throw RedAlert("Attempting to write while input file is still open\n");
     }
     std::ifstream testFile(fileName.c_str());
     if (testFile)
     {
        // see note at top
        if (!XSparse::validateForSystem(fileName))
        {
           string errMsg("Invalid chars in file name: ");
           errMsg += fileName;
           errMsg +="\n***Existing file (if any) will NOT be overwritten.\n";
           throw YellowAlert(errMsg);
        }
        rmString += fileName;
        std::system(rmString.c_str());
     }
     m_dataSource[0] = new FITS(fileName,Write);
  }
  catch (FitsException&)
  {
     throw XspecDataIO::CannotOpen(fileName);
  }
}

bool OGIP_92aIO::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  bool format = false;
  bool foundHDUKeys = false;
  bool foundVersion = true;
  using namespace CCfits;

  // save the current chatter output level for cases where we switch to
  // a higher level for diagnostic output

  tcout << xsverbose(25)<<" fileFormat check for " << fileName << " ... "
    <<std::endl << xsverbose();

  try
  {
     // This will throw a FITS::NoSuchHDU if not recognized
     // as OGIP-style header (including deprecated format),
     // and will return false.

     std::auto_ptr<FITS> p(openFitsExtension(fileName, type));
     m_extVers = p->currentExtension().version();
     m_extensionName = p->currentExtensionName();
     foundHDUKeys = true;
     tcout << xsverbose(25)<<"found " << m_extensionName << std::endl<<xsverbose(); 

     // This may also throw.
     ExtHDU& spectrum = p->extension(m_extensionName);

     tcout <<xsverbose(25)<< " and moved there... " << std::endl<<xsverbose();

     // we have a spectrum extension. 
     foundVersion = false;
     std::string versionString("");          
     try
     {
        // look for HDUVERS keyword.
        spectrum.readKey(s_HDUVERS,versionString);
        if (versionString.substr(0,2) == s_VERSKEY) 
        {
                format = true;
        }
     }
     catch (CCfits::HDU::NoSuchKeyword)
     {

       tcout << xsverbose(25)<<"\n  Failed to find HDUVERS in " 
         << fileName << std::endl <<xsverbose();

        // no HDUVERS, look for HDUVERS1, HDUVERS2, HDUVERS3...
        size_t hv = 1;
        while (hv <= 3)
        {
           std::ostringstream versCheck;
           versCheck << s_HDUVERS << hv++;
           try
           {
              spectrum.readKey(versCheck.str(),versionString);
              if (versionString.substr(0,2) == s_VERSKEY) 
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
	     tcout << xsverbose(25)<<"\n  Failed to find " 
               << versCheck.str() << " in " << fileName 
               << std::endl <<xsverbose();
              continue;       
           } 
        }
        if (versionString.size() == 0) 
        {
           // no HDUVERS keywords...
           // Check if this the deprecated case of "PHAVERSN"
           // (see openFitsExtension function).
           string versKey;
           switch (type)
           {
              default:
              case SpectrumType: 
                      versKey = s_PHAVERSN;
                      break;
              case ResponseType:
                      versKey = s_RMFVERSN;       
                      break;
              case AuxResponseType:
                      versKey = s_ARFVERSN;
                      break;
           }
	   try
	   {
              spectrum.readKey(versKey,versionString);
	   }
	   catch (HDU::NoSuchKeyword)
	   {
	      tcout << xsverbose() <<"\n  Failed to find " << versKey 
                 << " in " << fileName << std::endl <<xsverbose();
	      throw;
	   }
           // If above call doesn't throw, it is old-style.
           format = (versionString == s_OGIPVERS);
        }
     } // end catch initial NoSuchKeyword
  } // end outer try block
  catch (CCfits::FitsException& )
  {
     string typeString;
     string versKey;
     switch (type)
     {
        default:
        case SpectrumType:
           typeString = "spectrum";
           versKey = s_PHAVERSN;
           break;
        case ResponseType:
           typeString = "rmf";
           versKey = s_RMFVERSN;
           break;
        case AuxResponseType:
           typeString = "arf";
           versKey = s_ARFVERSN;
     }
     // Don't want these messages if just looking for data prototype.
     // There must be a better way to do all this.
     if (!foundHDUKeys)
     {
        tcerr << "*** Check for missing or improperly set HDUCLASS keywords in HDU of " 
              << typeString << " file"
	      << "\n    (or " << versKey << " keyword for old-style files)."<<std::endl;
     }
     if (!foundVersion)
     {
        tcerr <<"*** Check for missing or improperly set HDUVERS(n) keyword in HDU of "
              << typeString << " file"
	      <<"\n    (or " << versKey << " keyword for old-style files)."<<std::endl;
     }
  }

  if (!format) setExtensionName(string());
  tpout.setVerbose(25);
  if ( format ) {
     tcout << "successful" << std::endl;
  } else {
     tcout << "unsuccessful" << std::endl;
  }
  tpout.setVerbose();
  return format;
}

void OGIP_92aIO::channelBounds (int& startChannel, int& endChannel, size_t row) const
{

  // dataSource is a vector of pointers to objects of identical format,
  // spectrum number: but this function will actually only be called on
  // a spectrum not any other kind of OGIP_IO object, in which there
  // is only one file per object.
  try
  {
     CCfits::ExtHDU& ext = m_dataSource[0]->extension(extensionName());
     readChannelBounds(ext, startChannel, endChannel, row);    
  }
  catch (CCfits::Column::WrongColumnType&)
  {
     throw XspecDataIO::UnspecifiedSpectrumNumber(dataSource()->name());                
  }
  catch (CCfits::Table::NoSuchColumn&)
  {
     // Apply default channel numbering, 1,...,N.
     startChannel = 1;
     CCfits::ExtHDU& ext = m_dataSource[0]->extension(extensionName());
     if (row == 0)
     {
        endChannel = static_cast<int>(ext.rows());
     }
     else
     {
        // Must determine from the length of the only guaranteed
        // existing vector column - COUNTS or RATE.  
       CCfits::Column* tmpCol = 0;
        if (m_counts)
        {
           tmpCol = &ext.column(s_COUNTS);
        }
        else
        {
           tmpCol = &ext.column(s_RATE);
        }
        RealArray sizeTester;
        tmpCol->read(sizeTester, row);
        endChannel = sizeTester.size();
     }     
  }
  catch (...)
  {
     throw XspecDataIO::RequiredDataNotPresent(dataSource()->name());                
  }
}

int OGIP_92aIO::verifyQualGroup (const int startChan, const int endChan, IntegerArray& qual, IntegerArray& group, size_t row)
{
  int numRows = endChan - startChan + 1; 
  qual.resize(numRows, 0);
  group.resize(numRows, 1);

  CCfits::ExtHDU& table = dataSource()->extension(extensionName());

  int first = 0;
  int last  = endChan   - startChan;

  int groupedChannels  (numRows);
  if (m_qualityStorage != NO_STORE || m_groupingStorage != NO_STORE)
  {    
     if ( m_qualityStorage == COLSTORE)
     {
        try
        {
           // initially all channels are good.
           if (row == 0)
           {
              table.column(QUALITY()).read(qual,first,last);
           }
           else
           {
              // type II file
              CCfits::Column& col = table.column(QUALITY());
              std::valarray<short int> vqCard;  
              if (col.repeat() > 1)
              {
                 col.read(vqCard,row);  
                 for (int k = first; k <= last; ++k) qual[k-first] = vqCard[k];
              }
              else
              {
                 // Scalar column, same value for every channel.
                 col.read(vqCard,row,row);
                 for (int k = first; k <= last; ++k) qual[k-first] = vqCard[0];
              }
           }   
           int N (qual.size());
           for (int j = 0; j < N; ++j)
           {
              if ( qual[j] < 0 || qual[j] > 5 )
              {
                 tcout << "***Warning: Error in quality array: Channel " << j+startChan 
                       << " has quality outside range {0,5}: \n"
                       << "     It will be reset to 0." << std::endl;
                 qual[j] = 0;
              }
           }
        }
        catch (...)
        {
           throw YellowAlert("QUALITY column has the wrong format.\n");
        }       
     }
     else if (m_qualityStorage == KEYSTORE)
     {
        int qualVal; 
        bool err = false;
        try
        {
           table.keyWord(QUALITY()).value(qualVal);
           if (qualVal < 0 || qualVal > 5)
           {
              err = true;
           }
           else
           {
              for (int i=0; i<numRows; ++i)
              {
                 qual[i] = qualVal;
              }
           }
        }
        catch (...)
        {
           err = true;
        }
        if (err)
        {
           tcout << "***Warning: Improper value for QUALITY keyword.  Valid range is {0,5}\n"
                 << "       All channels in spectrum will be set to QUALITY = 0."
               << std::endl;
           m_qualityStorage = NO_STORE;
        }
     }


  // Is there a grouping card? If so, count all the channels that correspond
  // to single bins (the +1's in the grouping card, not the -1's).  Grouping
  // = 0 is only allowed as a keyword value.

     if (m_groupingStorage == COLSTORE)
     {
        try
        {
           if (row == 0)
           {     
              table.column(GROUPING()).read(group,first,last);
           }
           else
           {
              // type II file
              CCfits::Column& col = table.column(GROUPING());
              std::valarray<short int> vgCard;  
              if (col.repeat() > 1)
              {
                 col.read(vgCard,row);  
                 for (int k = first; k <= last; ++k) group[k-first] = vgCard[k];
              }
              else
              {
                 // A scalar column, the only allowed value to cover all channels
                 // is 0 (which sets all grouping vals to 1).    
                 col.read(vgCard,row,row);
                 if (vgCard[0] != 0)
                 {
                    tcout << "***Warning: Improper GROUPING value for row " << row
                       <<".  All channels in spectrum will be set to GROUPING = 1."
                       << std::endl;
                 }
                 // This setting is meant to apply to ALL spectra in set, but if
                 // one spectrum gets to here, all of them will.
                 m_groupingStorage = NO_STORE;              }
           }

           bool anyGood = false;
           bool groupingModified = false;

           for (int i=0; i<numRows; ++i)
           {
              if (group[i] != 1 && group[i] != -1)
              {
                 if (qual[i] == 0)
                 {
                    anyGood = true;
                 }
                 groupingModified = true;
	         group[i] = 1;
              }
           }
	   if (groupingModified)
	   {
	       tcout << "***Warning: Unrecognized grouping for channel(s). "
		     << "It/they will be reset to 1." << std::endl;
               if (!anyGood)
                  tcout << "  HOWEVER NOTE: Resets only occurred for channels of bad quality" 
                        << std::endl;
	   }
        }
        catch (...)
        {
           throw YellowAlert("GROUPING column has the wrong format.\n");
        }
     }
     else
     {
        // For GROUPING keyword entry only allowed value is 0,
        // meaning ungrouped, same as NO_STORAGE.
        int groupVal;
        bool err = false;
        try
        {
           table.keyWord(GROUPING()).value(groupVal);
           if (groupVal != 0)  err = true;
        }
        catch (...)
        {
           err = true;
        }
        if (err)
        {
           tcout << "***Warning: Improper value for GROUPING keyword.\n"
                 << "       All channels in spectrum will be set to GROUPING = 1."
               << std::endl;
        }
        m_groupingStorage = NO_STORE;
     }         

     // patch the grouping array dependent on the quality array, if it exists.
     // NOTE: if either Q or G == NO_STORE, qCard or gCard is still
     // correctly filled by this point (with 0 or 1 respectively).
     int bins = numRows;
     if (m_groupingStorage != NO_STORE) 
     {
        DataUtility::patchGrouping(qual,group);

        // check the grouping card
        const short PLUS (+1);

      #ifndef STD_COUNT_DEFECT
        bins = std::count(group.begin(),group.end(),PLUS);
      #else
        bins = 0;
        std::count(group.begin(),group.end(),PLUS,bins);
      #endif

     }
     // with the grouping vector corrected for quality, the 
     // number of channels should simply be the number of grouping values
     // that are not == -1 (bins), minus the number of these entries
     // that have quality == 1.
     groupedChannels = bins;
     if (m_qualityStorage != NO_STORE)
     {
        for ( int j = 0; j < numRows; ++j)
        {
           if (group[j] == 1 && qual[j] == 1) --groupedChannels;       
        }
     }
  } // end if !qualityNoStore || !groupNoStore
  return groupedChannels;   
}

bool OGIP_92aIO::isCounts () const
{
  return m_counts;
}

bool OGIP_92aIO::specNum (CCfits::ExtHDU& hdu) const
{
   // The purpose of this function is to detect whether file is type1
   // or type2 pha.  The name is somewhat obsolete, referring to when
   // it was basing its decision on the existence of a SPEC_NUM column. 

   // NOTE: This ASSUMES getDataType() has already been called to
   // determine proper m_counts setting.

  bool type2specNum = false; 
  string errMsg("COUNTS or RATE column");     
  try
  {
     CCfits::Column& testCol = m_counts ? hdu.column(s_COUNTS):
                                        hdu.column(s_RATE);
     if (testCol.repeat() > 1 || testCol.varLength())
        type2specNum = true;
     else
     {
        // OK, test column is scalar.  This still could be a
        // type2 file with just one channel in each spectrum.
        // If nRows > DETCHANS, we must be dealing with type2,
        // otherwise assume type1 (not an airtight assumption).
        int detChans = 0;
        errMsg = "Keyword DETCHANS";
        hdu.readKey(s_DETCHANS, detChans);  
        if (testCol.rows() > detChans)
           type2specNum = true;         
     }      
  }      
  catch (CCfits::FitsException&)
  {
      errMsg += " is needed or is of improper type.";
      throw XspecDataIO::RequiredDataNotPresent(errMsg);       
  }
  return type2specNum;
}

void OGIP_92aIO::closeFile (size_t  index)
{
  delete m_dataSource[index];
  m_dataSource[index] = 0;  
}


const CCfits::FITS* OGIP_92aIO::dataSource (size_t index) const
{

  return m_dataSource[index];
}

void OGIP_92aIO::setDataSource (CCfits::FITS* fitsObject, size_t index)
{
  if (index < m_dataSource.size()) m_dataSource[index] = fitsObject;
  else
  {
        m_dataSource.resize(index+1);
        m_dataSource[index] = fitsObject;       
  }
}

CCfits::FITS* OGIP_92aIO::dataSource (size_t index)
{

  return m_dataSource[index];
}

int OGIP_92aIO::getKeyIntValue (CCfits::FITS* fitsFile, const string& keyWord)
{
  using namespace CCfits;
  int keyVal = 0;
  try
  { 
     fitsFile->currentExtension().readKey(keyWord, keyVal);     
  }
  catch (HDU::NoSuchKeyword)
  {
     // No message here.  Let calling function decide if it wants to
     // report anything.
     throw YellowAlert();
  }
  catch (FitsException&)
  {
     string msg = "   searching for int value of keyword: " + keyWord;
     msg += "  in " + fitsFile->name();
     throw XspecDataIO::CatchAllIO(msg);
  }
  return keyVal;
}

CCfits::Table* OGIP_92aIO::makeType1Table (string& hduName, size_t nChans, const BoolArray& optCols)
{
  // nOffset = required cols with no modifiable options.
  const size_t nOffset = 1;
  if (optCols[COUNTS_COL])
  {
     s_typeIColNames[COUNTS_COL+nOffset] = s_COUNTS;
     s_typeIColForms[COUNTS_COL+nOffset] = string("J");
     s_typeIColUnits[COUNTS_COL+nOffset] = string("");
  }
  else
  {
     s_typeIColNames[COUNTS_COL+nOffset] = s_RATE;
     s_typeIColForms[COUNTS_COL+nOffset] = string("E");
     s_typeIColUnits[COUNTS_COL+nOffset] = string("counts/s");
  }
  StringArray colName;
  StringArray colForm;
  StringArray colUnit;
  for (size_t i=0; i<=COUNTS_COL+nOffset; ++i)
  {
     colName.push_back(s_typeIColNames[i]);
     colForm.push_back(s_typeIColForms[i]);
     colUnit.push_back(s_typeIColUnits[i]);
  }
  for (size_t i=COUNTS_COL+1; i<NOPTCOLS; ++i)
  {
     if (optCols[i])
     {
        colName.push_back(s_typeIColNames[i+nOffset]);
        colForm.push_back(s_typeIColForms[i+nOffset]);
        colUnit.push_back(s_typeIColUnits[i+nOffset]);
     }
  }
  return m_dataSource[0]->addTable(hduName, nChans, colName, colForm, colUnit);
}

CCfits::Table* OGIP_92aIO::makeType2Table (string& hduName, size_t nSpec, size_t nChans, const BoolArray& optCols)
{
   const size_t nOffset = 1;
   std::ostringstream vectorWidth;
   string formatStr("");    
   StringArray colName;
   StringArray colForm;
   StringArray colUnit;
   if (optCols[COUNTS_COL])
   {
      s_typeIColNames[COUNTS_COL+nOffset] = s_COUNTS;
      s_typeIColForms[COUNTS_COL+nOffset] = string("J");
      s_typeIColUnits[COUNTS_COL+nOffset] = string("");
   }
   else
   {
      s_typeIColNames[COUNTS_COL+nOffset] = s_RATE;
      s_typeIColForms[COUNTS_COL+nOffset] = string("E");
      s_typeIColUnits[COUNTS_COL+nOffset] = string("counts/s");
   }

   vectorWidth << nChans;
   string vw = vectorWidth.str();
   colName.push_back(NUMKEY());
   colForm.push_back(string("I"));
   colUnit.push_back(string(""));
   for (size_t i=0; i<=COUNTS_COL+nOffset; ++i)
   {
      formatStr = vw + s_typeIColForms[i];
      colName.push_back(s_typeIColNames[i]);
      colForm.push_back(formatStr);
      colUnit.push_back(s_typeIColUnits[i]);
   }
   for (size_t i=COUNTS_COL+1; i<NOPTCOLS; ++i)
   {
      if (optCols[i])
      {
         formatStr = vw + s_typeIColForms[i+nOffset];
         colName.push_back(s_typeIColNames[i+nOffset]);
         colForm.push_back(formatStr);
         colUnit.push_back(s_typeIColUnits[i+nOffset]);
      }
   }

   colName.push_back(RESPFILE());
   colName.push_back(ANCRFILE());
   colName.push_back(CORRFILE());
   for (size_t i=0; i<3; ++i)
   {
      colForm.push_back(string("80A"));
      colUnit.push_back(string(""));
   }

   return m_dataSource[0]->addTable(hduName,nSpec, colName, colForm,colUnit);
}

void OGIP_92aIO::getNetType ()
{

  // if HDUCLAS3 is present, then check its value and report if it is not
  // counts or rate. If it is not present, check for a COUNTS or RATE column.
  CCfits::ExtHDU& ext = m_dataSource[0]->extension(m_extensionName);
  try
  {
        static const string NET("NET");
        static const string UNKNOWN("UNKNOWN");
        static std::vector<string> class2keys;
        if ( class2keys.empty())
        {
                class2keys.reserve(4);
                class2keys.push_back(NET);
                class2keys.push_back("TOTAL");
                class2keys.push_back("BKG");
                class2keys.push_back(UNKNOWN);
        }
        string hduclas2("");
        ext.readKey(s_HDUCLAS2,hduclas2);
        // data type flag  is present
        m_netIsSet = (hduclas2 != UNKNOWN);
        std::vector<string>::iterator v (std::find(class2keys.begin(),class2keys.end(),hduclas2));
        if (hduclas2 == NET)
        {
                m_net = true;
        }
        else if ( v == class2keys.end() )
        {
                // net flag has been set, but the value is illega.
                tcout << xsverbose(15)<<"*** Warning: HDUCLAS2 key does not specify Total, "
                      << " Net Flux or Background  " << std::endl <<xsverbose();
        }

  }
  catch ( CCfits::HDU::NoSuchKeyword )
  {
        // m_netIsSet and  m_net are both false as initialized.
  }
}

bool OGIP_92aIO::isNet () const
{
  return m_net;
}

void OGIP_92aIO::getDataType ()
{
  // check for counts/rate column. Issue a warning if the indicating keyword
  // HDUCLAS3 is inconsistent.
  bool countSetting(false);
  CCfits::ExtHDU& ext = m_dataSource[0]->extension(m_extensionName);
  try
  {
        ext.column(s_RATE);
        m_counts = false;
  }
  catch (CCfits::Table::NoSuchColumn)
  {
        try
        {
                ext.column(s_COUNTS);
                m_counts = true;
        }
        catch (...)
        {
                // exit with a message. "silent" is set to false, so this
                // will print a message on stderr.
                throw XspecDataIO::RequiredDataNotPresent("file does not contain PHA data array");       
        }
  }
  try
  {
        static const string COUNT("COUNT");
        static const string RATE("RATE");
        string hduclas3("");
        ext.readKey(s_HDUCLAS3,hduclas3);
        countSetting = (hduclas3.substr(0,5) == COUNT); 
        if ( countSetting && !m_counts )
        {
                tcerr << "*** Warning: file contains COUNTS column but HDUCLAS3 keyword "
                      << "is set to " << hduclas3 << '\n';       
        }

  }
  catch ( CCfits::HDU::NoSuchKeyword& )
  {
         // absorb 
  }
}

void OGIP_92aIO::getQualGroupStorage (const string& keyword)
{
  StorageType& qualOrGroup = (keyword == s_QUALITY) ? m_qualityStorage :
                                                m_groupingStorage;
  const CCfits::ExtHDU& ext = dataSource()->extension(m_extensionName);
  // Attempt to find column
  try
  {
     ext.column(keyword);
     qualOrGroup = COLSTORE;
     return;      
  }
  catch (CCfits::Table::NoSuchColumn)
  {          
  }
  catch ( ... )
  {
     throw;
  }
  // Column doesn't exist, attempt to find keyword.
  try
  {
     ext.keyWord(keyword);
     qualOrGroup = KEYSTORE;
  }
  catch (CCfits::HDU::NoSuchKeyword)
  {
     qualOrGroup = NO_STORE;
  }
  catch (...)
  {
     throw;
  }
}

std::auto_ptr<CCfits::FITS> OGIP_92aIO::openFitsExtension (const string& fileName, XspecDataIO::DataType type)
{
   using namespace CCfits;
   std::auto_ptr<FITS> apFits(0);
   // This will either return a copy of apFits, a pointer to a FITS file
   // set to the appropriate current data extension, or it will
   // throw.  Calling functions can assume if they get a returned
   // auto pointer, it will be valid.
   // CAUTION: This lets all CCfits exceptions propagate except
   // FITS::CantOpen.  Calling functions must convert them to
   // YellowAlert types.

   // fileName may include an extvers specifier in curly brackets which
   // IS NOT passed on to CCfits.  Or it may include extend-syntax in
   // square brackets which IS passed on to CCfits.  However it may 
   // not include both.
   string openFileName;

   const int curlyExtVers = XSparse::getCurlyBracketInt(fileName,openFileName);
   const bool isCurl = curlyExtVers > 0;
   string::size_type squareLoc = string::npos;
   string::size_type plusLoc = string::npos;
   bool isExtended = XSparse::checkExtendedSyntax(openFileName,squareLoc,plusLoc);

   // Check for some special cases of the extended syntax.
   string extendedString("");
   size_t fileNameLength(openFileName.size());
   if ( squareLoc != string::npos ) {
     extendedString = openFileName.substr(squareLoc+1,openFileName.size()-squareLoc-2);
     fileNameLength = squareLoc;
   } else if ( plusLoc != string::npos) {
     extendedString = openFileName.substr(plusLoc+1,string::npos);
     fileNameLength = plusLoc;
   }

   std::vector<string> extraKeys;
   std::vector<string> extraVals;

   if ( extendedString == "back" ) {
     extraKeys.push_back("HDUCLAS2");
     extraVals.push_back("BKG");
     openFileName = openFileName.substr(0, fileNameLength);
     isExtended = false;
   } else if ( extendedString.find_first_of("=") != string::npos ) {
     string keyword = XSparse::returnDelimitedArgument(extendedString, "=");
     extraKeys.push_back(XSparse::trimWhiteSpace(keyword));
     extraVals.push_back(XSparse::trimWhiteSpace(extendedString));
     openFileName = openFileName.substr(0, fileNameLength);
     isExtended = false;
   }

   // debug
   //   if ( extraKeys.size() > 0 ) {
   //     tcout << "Extra keyword,value pairs:" << std::endl;
   //     for (size_t i=0; i<extraKeys.size(); i++) {
   //       tcout << "'" << extraKeys[i] << "' = '" << extraVals[i] << "'" << std::endl;
   //     }
   //     tcout << "Filename = " << openFileName << std::endl;
   //   }

   if (isExtended && isCurl)
   {
      string err("Cannot use curly bracket extension specifier together with\n");
      err += "   ftools type HDU specifier in file name: " + fileName + "\n";
      throw YellowAlert(err);
   }
   // If square bracket syntax is wrong, let CCfits/CFITSIO deal with it.

   std::vector<string> OGIP92a(2,"");
   std::vector<string> OGIP92aVal(2,"");
   // This may be modified below if using extended syntax:
   int extVers = isCurl ? curlyExtVers : 1;
   bool tryAgain = false;
   bool tryTryAgain = false;
   try
   {
      // find an extension that matches the HDUCLASS, HDUCLAS1, HDUVERS key values.
      OGIP92a[0] = s_HDUCLASS;
      OGIP92a[1] = s_HDUCLAS1;

      OGIP92aVal[0] = s_OGIPTYPE;

      switch (type)
      {
         default:
         case SpectrumType: 
                 OGIP92aVal[1] = s_SPECTYPE;
                 break;
         case ResponseType:
                 OGIP92aVal[1] = s_RESPTYPE;       
                 OGIP92a.push_back(s_HDUCLAS2);
                 OGIP92aVal.push_back(s_RSPMATRIXTYPE);
                 break;
         case AuxResponseType:
                 OGIP92a[0] = s_HDUCLAS1;
                 OGIP92aVal[0] = s_RESPTYPE;
                 OGIP92a[1] = s_HDUCLAS2;
                 OGIP92aVal[1] = s_SPECRESPTYPE;
                 break;
      }

      // Add in any extra keyword tests
      for (size_t i=0; i<extraKeys.size(); i++) {
	OGIP92a.push_back(extraKeys[i]);
	OGIP92aVal.push_back(extraVals[i]);
      }

      apFits.reset(callFITSctor(openFileName,OGIP92a,OGIP92aVal,
                isExtended,extVers));
      // Returned pointer will be NULL if using extended syntax and
      // the HDU doesn't have all the required keywords.  Otherwise
      // if missing keywords is the only problem, it will have thrown.
      if (!apFits.get())
         tryAgain = true;

   }
   catch (FITS::CantOpen&)
   {
      throw XspecDataIO::CannotOpen(openFileName);
   }
   catch (FitsException&)
   {
      if (isExtended)
      {
         // When coming from fileFormat, tcerr is turned off since
         // we generally don't want to be notified of each failing
         // format type.  But in this case we want to be notified
         // that it is specifically the missing HDU which is causing
         // the problem.  Hence output to tcout and throw an XSPEC
         // type to break from a format checking loop.
         tcout << "\n***Error: Failed to find the HDU specified by " 
            << openFileName << "\n" << std::endl;
         throw XspecDataIO::CannotOpen();
      }
      else      
         tryAgain = true;
   }

   if (tryAgain)
   {
      tryAgain = false;
      tcout << xsverbose(25)<<" Failed to open extension in " << openFileName 
        << " for " <<OGIP92a[0] << " " << OGIP92a[1] << " " << OGIP92aVal[0] 
        << " " <<OGIP92aVal[1] << " (EXTVER = " << extVers <<")"
        << std::endl <<xsverbose();    

       // the "backward-compatibility clause.
       // a header containing just "PHAVERSN" identifies the file as OGIP/SPECTRUM
       // this is a deprecated usage.
      OGIP92a.resize(1);
      OGIP92a[0] = " ";
      OGIP92aVal.resize(1);
      OGIP92aVal[0] = s_OGIPVERS;

      switch (type)
      {
             default:
             case SpectrumType: 
                     OGIP92a[0] = s_PHAVERSN;
                     break;
             case ResponseType:
                     OGIP92a[0] = s_RMFVERSN; 
                     // Added this keyword check to prevent false
                     // hits on EBOUNDS extensions: 
                     OGIP92a.push_back("EXTNAME");
                     OGIP92aVal.push_back("SPECRESP MATRIX");     
                     break;
             case AuxResponseType:
                     OGIP92a[0] = s_ARFVERSN;
                     break;
      } 
      try
      {
         // If we're here, we know file exists and is readable.
         apFits.reset(callFITSctor(openFileName,OGIP92a,OGIP92aVal,
                   isExtended,extVers));
         if (!apFits.get())
         {
            // Only in here if isExtended.  Also ANY isExtended
            // problem that could happen by this point will wind
            // up in here rather than throwing.
            if (type == ResponseType)
            {
               tcout << xsverbose(25)<<" Failed to open extension in " 
                 << openFileName << " for " << OGIP92a[0] << " " << OGIP92aVal[0]  
                 << " (EXTVER = " << extVers <<")"<< std::endl <<xsverbose();
               tryTryAgain = true;
            }
            else
               // Backwards compatibility issue: calling functions may only
               // be expecting CCfits exceptions, so don't throw YellowAlert
               // even though that would get us out of here sooner.
               throw FITS::NoSuchHDU("");
         }
      }
      catch (FITS::NoSuchHDU&)
      {
         tcout << xsverbose(25)<<" Failed to open extension in " << openFileName 
           << " for " << OGIP92a[0] << " " << OGIP92aVal[0]  
           << " (EXTVER = " << extVers <<")"<< std::endl <<xsverbose();

         if (type == ResponseType)
         {
            tryTryAgain = true;
         }
         else throw;
      }
   } // end if tryAgain

   if (tryTryAgain)
   {
      // One last attempt, but only for Response types
      OGIP92aVal[1] = s_MATRIX;
      try
      {
         apFits.reset(callFITSctor(openFileName,OGIP92a,OGIP92aVal,
                        isExtended,extVers));
         if (!apFits.get())
            throw FITS::NoSuchHDU("");
      }
      catch(FITS::NoSuchHDU&)
      {
         tcout << xsverbose(25)<<" Failed to open extension in " 
           << openFileName << " for " << OGIP92a[0] << " " << OGIP92aVal[0] 
           << " " <<OGIP92aVal[1] << " (EXTVER = " << extVers <<")"
           << std::endl <<xsverbose();
         throw;
      }
   }
   return apFits;
}

void OGIP_92aIO::channelLimits (const size_t lowDefault, size_t& legalStart, size_t& legalEnd) const
{
   CCfits::ExtHDU& ext = m_dataSource[0]->extension(m_extensionName);
   const string& fileName = m_dataSource[0]->name();
   try
   {
      readChannelLimits(ext, lowDefault, legalStart, legalEnd);
   }
   catch (...)
   {
      string msg("    reading channel limits from file: ");
      msg += fileName;
      throw XspecDataIO::CatchAllIO(msg);
   }
}

void OGIP_92aIO::readChannelBounds (CCfits::ExtHDU& ext, int& startChannel, int& endChannel, const size_t row)
{

  bool isContinuous = true;
  if ( row == 0 )
  {
     CCfits::Column& channelColumn(ext.column(s_CHANNEL));
     std::vector<int> colNum;
     int n(channelColumn.rows());
     channelColumn.read(colNum,1,n);
     startChannel = colNum[0];
     endChannel   = colNum[n-1];
     int prevChannel = startChannel;
     // test for continuity of channels
     for (int i=1; i<n; i++)
     {
        if (colNum[i] != prevChannel+1)
        {
	   isContinuous = false;
	   break;   
        }
        prevChannel = colNum[i];
     }
  }
  else
  {
        try
        {
                CCfits::Column& channelColumn(ext.column(s_CHANNEL));
                std::valarray<int> colNum;
                channelColumn.read(colNum,row);
		int sz = colNum.size();
                startChannel = colNum[0];
                endChannel = colNum[sz-1];
		int prevChannel = startChannel;
		// test for continuity of channels
		for (int i=1; i<sz; i++)
		{
		   if (colNum[i] != prevChannel+1)
		   {
		      isContinuous = false;
		      break;   
		   }
		   prevChannel = colNum[i];
		}

        }
        catch (CCfits::Column::WrongColumnType)
        {
                // take corrective action that ignores the row number.
                CCfits::Column& channelColumn(ext.column(s_CHANNEL));
                std::vector<int> colNum;
                ext.column(CHANNEL()).read(colNum,1,1);
                startChannel = colNum[0];
                size_t n(channelColumn.rows());
                channelColumn.read(colNum,n,n);
                endChannel = colNum[n-1];

        }
  }

  if (!isContinuous)
  {
     string msg = "\nDetector channel numbers missing in extension: "
		       + ext.name();
     throw XspecDataIO::RequiredDataNotPresent(msg);
  }
}

void OGIP_92aIO::readChannelLimits (CCfits::ExtHDU& ext, const size_t lowDefault, size_t& legalStart, size_t& legalEnd)
{
   int detChans = 0;
   // DETCHANS is a mandatory keyword
   try
   {
      ext.readKey(s_DETCHANS, detChans);
   }
   catch (CCfits::FitsException&)
   {
      string msg = "Keyword DETCHANS is missing or of improper type.";
      throw XspecDataIO::RequiredDataNotPresent(msg);       
   }
   try 
   {
      const CCfits::Column& channelColumn(ext.column(s_CHANNEL));
      int intval = 0;
    // TLMIN and TLMAX are optional, but if they exist, they'd
    // better be consistent with DETCHANS.
      size_t channelNo = channelColumn.index();
      std::ostringstream chanKeyMin;  
      chanKeyMin << "TLMIN" << channelNo;
      try
      {
         ext.readKey(chanKeyMin.str(),intval);
         if (intval < 0)
         {
            string msg("Invalid TLMIN value for channel column.\n");
            throw YellowAlert(msg);
         }
         legalStart = static_cast<size_t>(intval);
      }
      catch (CCfits::HDU::NoSuchKeyword) 
      {
         legalStart = lowDefault;
      } 
      std::ostringstream chanKeyMax;
      chanKeyMax << "TLMAX" << channelNo;
      try
      {
         ext.readKey(chanKeyMax.str(),intval);
         if (intval < 0)
         {
            string msg("Invalid TLMAX value for channel column.\n");
            throw YellowAlert(msg);
         }
         legalEnd = static_cast<size_t>(intval);
      }
      catch (CCfits::HDU::NoSuchKeyword) 
      {
         legalEnd = legalStart + static_cast<size_t>(detChans) - 1;
      } 
      if ((legalEnd - legalStart + 1) != static_cast<size_t>(detChans))
      {
         string msg("Conflict between channel column TLMIN,TLMAX keywords\n");
         msg += "    and DETCHANS keyword.\n";
         throw YellowAlert(msg);
      }
   }
   catch (CCfits::Table::NoSuchColumn)
   {
      legalStart = 1;
      legalEnd = static_cast<size_t>(detChans);
   }
}

std::pair<int,int> OGIP_92aIO::getChanInfoFromResponse (const string& fileName)
{
   std::pair<int,int> result(-1,-1);
   std::auto_ptr<CCfits::FITS> respFile(0);
   int colNum=0;

   try
   {
      //FITS::CantOpen will be caught and converted in openFitsExtension.  
      //All other FitsExceptions must be converted here.
      respFile = openFitsExtension(fileName, XspecDataIO::ResponseType);
      // Look for F_CHAN column's TLMIN.  This is usually column 4, but we
      // can't take that for granted.
      colNum = respFile->currentExtension().column(s_FCHANNEL).index();
   }
   catch (CCfits::FITS::NoSuchHDU&)
   {
      string msg("Cannot find response extension in file ");
      msg += fileName + "\n";
      throw YellowAlert(msg);
   }
   catch (CCfits::Table::NoSuchColumn&)
   {
      string msg("Cannot locate F_CHAN column in file ");
      msg += fileName + "\n";
      throw YellowAlert(msg);
   }
   catch (...)
   {
      string msg("FITS error while attempting to read channel info from ");
      msg += fileName + "\n";
      throw YellowAlert(msg);
   }
   std::ostringstream oss;
   oss << "TLMIN" << colNum;
   try
   {
      // We're not going to require a TLMIN keyword, though it really 
      // should be there for all newer files.  If it's not, just
      // return -1 as a flag to let calling function put in a default. 
      result.first = getKeyIntValue(respFile.get(), oss.str());
   }
   catch (...)
   {
   }
   try
   {
      // DETCHANS must exist.  getKeyIntValue converts all 
      // FitsExceptions to YellowAlerts.
      result.second = getKeyIntValue(respFile.get(), s_DETCHANS);
   }
   catch (...)
   {
      tcerr << "\n***Error: Valid DETCHANS keyword is required for file "
	        <<fileName<<std::endl;
      throw;
   }
   return result;
}

CCfits::FITS* OGIP_92aIO::callFITSctor (const string& filename, const StringArray& searchKeys, const StringArray& searchVals, const bool isExtended, int& extVers)
{
   using namespace CCfits;

   FITS* fits=0;
   if (isExtended)
   {
      // extended syntax can be [<HDU NAME>], [<HDU_NAME>,<extVers>]
      // or [<HDU IDX>].
      fits = new FITS(filename);
      ExtHDU& hdu = fits->currentExtension();
      extVers = hdu.version();

      // If the ctor didn't throw, we can assume it is pointing to the
      // proper extension.  Now see if it has the required keywords.
      const size_t nKeys = searchKeys.size();
      StringArray tmpKeys(nKeys);
      for (size_t i=0; i<nKeys; ++i)
         tmpKeys[i] = searchKeys[i];
      StringArray tmpVals;
      // This function will NOT throw if any keys are missing in HDU.
      // Instead, it removes the missing keys from the tmpKeys array
      // (which is why we had to copy the const searchKeys vector).
      hdu.readKeys(tmpKeys, tmpVals);
      size_t j=0;
      bool isMatched = (tmpKeys.size() == nKeys);
      while (isMatched && j<nKeys)
      {
         if (XSutility::lowerCase(XSparse::trimWhiteSpace(tmpVals[j])) !=
             XSutility::lowerCase(searchVals[j]))
            isMatched = false;
         ++j;
      }
      if (!isMatched)
      {
         delete fits;
         fits = 0;
      }      
   }
   else
   {
      // These are just dummy arguments needed to pass an extVers
      // number to the FITS ctor:
      const StringArray hduKeys;
      const StringArray primaryKeys;

      fits = new FITS(filename,Read,searchKeys,searchVals,
               false, hduKeys, primaryKeys, extVers);
   }
   return fits;
}

// Additional Declarations
