//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <memory>
#include <cstring> 

// DataSetBase
#include <XSModel/Data/DataSetBase.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// OGIP-92aData
#include <XSModel/Data/OGIP-92aData.h>
// Background
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/GlobalContainer/DataSetTypes.h>
#include <XSModel/DataFactory/DataFactory.h>
#include <XSModel/Data/Detector/OGIP-92aResponse.h>
#include <XSModel/Data/Detector/UserDummyResponse.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/Data/FakeDataInputRecord.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Parse/XSparse.h>
#include "XSContainer.h"
#include "XSstreams.h"
#include "XSsymbol.h"
#include <CCfits/CCfits>
#include <algorithm>
#include <memory>
#include <cassert>
#include <cmath>
#include <sstream>


// Class OGIP_92aData::OutputInfo 

OGIP_92aData::OutputInfo::OutputInfo (const string& fileName, const SpectralData* sd, const std::vector<string>& respNames, const std::vector<string>& arfNames, const string& bckName, const string& corrName, const RealArray& bckScaleRatio, const Real corrScale, const std::vector<string>& modelNames, bool aScaleIsKeyword, bool bScaleIsKeyword)
  :m_fileName(fileName), m_sd(sd), m_respNames(respNames), m_arfNames(arfNames),
     m_bckName(bckName), m_corrName(corrName), m_bckScaleRatio(bckScaleRatio),
     m_corrScale(corrScale), m_modelNames(modelNames), m_isAScaleKeyword(aScaleIsKeyword),
     m_isBScaleKeyword(bScaleIsKeyword)
{
}


// Additional Declarations

// Class OGIP_92aData 
const string OGIP_92aData::s_fkModRoot = string("FKSRC");
const string OGIP_92aData::s_fkRspRoot = string("FKRSP");
const string OGIP_92aData::s_fkArfRoot = string("FKARF");
const size_t OGIP_92aData::s_fkKeyValLen = FLEN_VALUE - 3;

OGIP_92aData::OGIP_92aData()
  : DataSet(), OGIP_92aIO()
{
}

OGIP_92aData::OGIP_92aData(const OGIP_92aData &right)
  : DataSet(right), OGIP_92aIO(right)  
{
}


OGIP_92aData::~OGIP_92aData()
{
}


OGIP_92aData* OGIP_92aData::clone () const
{

  return new OGIP_92aData(*this);
}

bool OGIP_92aData::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  // Set OGIP to be the default format when checking by response
  // type and only the singleton dummy is loaded.  This is 
  // originally only meaningful for the fakeit command.
  if (type == XspecDataIO::ResponseType && fileName == DUMMY_RSP)
  {
     return true;
  }          
  return OGIP_92aIO::fileFormat(fileName,type);
}

void OGIP_92aData::setArrays (size_t row)
{
  // it looks like this function will work equally for TypeII files

  try
  {
        // first task is to find the start and end channel numbers.  
        // Xspec11 & before marked channels out of start/end range
        // as "bad" and ignored them at read time. This function actually
        // sets the data ranges since the data container can't be
        // constructed until its size is determined from the grouping scheme.
        int first = 0;
        int last   = 0;

        channelBounds(first, last, row);

        IntegerArray qual, group;
        int groupedChannels = verifyQualGroup(first,last, qual, group, row);

        //    now, create a SpectralData object with arrays of the computed size.  
        // SpectralData*& spectrum = dataSetBase()->m_spectralData;
        // ought to throw something if this doesn't alloc.

        std::auto_ptr<SpectralData> spectrum(new SpectralData(this, groupedChannels, row, origNumSources()));
        spectrum->startChan(first);
        spectrum->endChan(last);

       // Improper quality/grouping input should have already been
       // handled in verifyQualGroup function.  Therefore no
       // exception handling is done for them here.
        spectrum->setQualityInfo(qual);
        spectrum->setGroupingInfo(group);

        CodeContainer coded;
	spectrum->gqString(DataUtility::encodeGQ(spectrum->groupingInfo(), spectrum->qualityInfo(), coded));

        if (row == 0)
        {
                dataSetBase()->spectralData(spectrum.release());
        }
        else
        {
	        std::map<size_t,SpectralData*>::value_type __tmp(row,spectrum.release());
		dataSetBase()->multiSpectralData().insert(__tmp);
        }

	//spectrum->gqString(DataUtility::encodeGQ(spectrum->groupingInfo(), spectrum->qualityInfo()));
  }
  catch (YellowAlert&)
  {
        // diagnostic for Xspec errors, which might be thrown by the 
        // SpectralData constructor.
        std::string msg("Error reading data: File ");
        msg += dataSource()->name();
        msg += '\n';
        throw YellowAlert(msg);
  }      
  catch (std::exception&)
  {
        // e.g. bad_alloc.       
        throw;
  }
}

void OGIP_92aData::setDescription (size_t spectrumNumber, size_t row)
{
   CCfits::ExtHDU& ext = dataSource()->extension(extensionName());
   // now where were we?     
   SpectralData* spectrum = sourceData(row);
   DataSetBase* const dsb = dataSetBase();

   if (dsb->legalEndChan() == 0)
   {
      // Only do for the first spectrum in a dataset.
      // We know sd->startChan and endChan by this point, but not
      // sd->firstChan.
      size_t legalStart=0, legalEnd = 0;
      size_t lowDefault = spectrum->startChan() ? 1 : 0;
      channelLimits(lowDefault, legalStart, legalEnd);
      spectrum->firstChan(legalStart);
      dsb->legalStartChan(legalStart);
      dsb->legalEndChan(legalEnd);
   } // end if first spectrum (legalEndChan not set)
   else
   {
      spectrum->firstChan(dsb->legalStartChan());
   }

   if (spectrum->startChan() < spectrum->firstChan())
   {
      string msg("Spectrum in file ");
      msg += dataSource()->name();
      msg += " has channel numbers lower than legal lower bound.\n";
      throw YellowAlert(msg);
   }
   if (spectrum->endChan() > dsb->legalEndChan())
   {
      string msg("Spectrum in file ");
      msg += dataSource()->name();
      msg += " has channel numbers higher than legal upper bound.\n";
      throw YellowAlert(msg);
   }      

   size_t offset = spectrum->startChan() - spectrum->firstChan();
   size_t end = spectrum->endChan() - spectrum->firstChan();
   size_t allPossibleChans = dsb->legalEndChan() - dsb->legalStartChan() + 1;
   spectrum->setNoticedChannels(BoolArray(allPossibleChans, false));
   for (size_t j=offset; j<=end; ++j)
   {
      spectrum->noticedChannels(j, true);
   }

   string keyval;
   float  floatval;
   std::vector<RealArray >   floatValueFromCol(1);
   std::vector<string> keyValueFromCol(1,"");
   // copy keywords
   static const string BCK   = ".bck";
   static const string COR   = ".cor";
   static const string PULSEHEIGHT = "PHA";

   spectrum->spectrumNumber(spectrumNumber);

   string errmsg("");

   try
   {
        keyval = ext.keyWord(CHANNELTYPE()).value(keyval);         
        spectrum->channelType(keyval);
   }
   catch (CCfits::HDU::NoSuchKeyword)
   {
        spectrum->channelType(PULSEHEIGHT);       
   }


   try
   {
        // setAncilliary file catches filename read errors and sets them
        // to empty output. It changes "none" to empty, and matches "%match%"
        // to suffix. It throws exceptions that are not caused by keyword errors.
        string auxFileName("");
        // various data description keys are read: EXPOSURE, TELESCOPE
        // INSTRUMENT, BACKFILE, CORRFILE, BACKSCALE, CORRSCALE.
        // The latest amended (and as yet unpublished) OGIP standard
        // allows the BACKSCALE and AREASCALE to be a vector for each 
        // spectrum, thus a column vector needs to be tried for.
        // on the other hand, try to be easy on user files that don't
        // reference background / correction files or backscale/corrscal data.
        // Even though both are required by the standard.

        setAncillaryFileName(auxFileName, dataName(), BACKFILE(), BCK, row);
	// if the background filename is the same as the source filename
	// then auto-look for the background extension
	if ( auxFileName == dataName() ) {
	  spectrum->backgroundFile((auxFileName+"[back]"));
	} else {
	  spectrum->backgroundFile(auxFileName);
	}

        setAncillaryFileName(auxFileName, dataName(), CORRFILE(), COR, row);
        spectrum->correctionFile(auxFileName);

        errmsg = TELESCOPE();
        spectrum->telescope(ext.keyWord(TELESCOPE()).value(keyval));
        errmsg = INSTRUMENT();
        spectrum->instrument(ext.keyWord(INSTRUMENT()).value(keyval));


        // exposure time is an absolute requirement.
        Real expTime (1.);
        errmsg = EXPOSURE();
        try
        {
             RealArray valueFromCol;
             ext.column(EXPOSURE()).read(valueFromCol,row,row);
             expTime = valueFromCol[0];
        }
        catch ( CCfits::Table::NoSuchColumn )
        {
             expTime = ext.keyWord(EXPOSURE()).value(floatval);     
        }

        if (expTime <= 0)
        {
           std::ostringstream oss;
           oss << "XSPEC will not load a data set with exposure time <= 0.0: "
               << dsb->dataName();
           if (row)
              oss << "{" << row << "}";           
           oss << "\n\n";
           throw YellowAlert(oss.str());
        }
        spectrum->exposureTime(expTime);

        int last (spectrum->endChan() - spectrum->startChan());
        int first (0);

        errmsg = AREASCALE();
        spectrum->setAreaScale(scaleVector(AREASCALE(),row,first,last));

        errmsg = BACKSCALE(); 
        spectrum->setBackgroundScale(scaleVector(BACKSCALE(),row,first,last));               
        // read correction scale factor. Don't penalize user if it's not there
        // and there isn't a correction file, which is the general case. 
        // (But should still read CORRSCALE if it exists, even if no CORRFILE.)
        Real corrScale = 1.;

        bool isCorrScale = false;
        try
        {
             RealArray valueFromCol;
             errmsg = CORRSCALE();
             ext.column(CORRSCALE()).read(valueFromCol,row,row);
             corrScale = valueFromCol[0];
             isCorrScale = true;
        }
        catch ( CCfits::Table::NoSuchColumn&)
        {
        }  
        if (!isCorrScale)
        {
           try
           {
              errmsg = CORRSCALE();
              corrScale = ext.keyWord(CORRSCALE()).value(floatval);
              isCorrScale = true;
           }
           catch (CCfits::HDU::NoSuchKeyword&)
           {
              if ( spectrum->correctionFile().length() )
              {
                 // No corrScale is only a problem if CORRFILE exists.
                 throw;
              }
           }
        }

        // correctionScale can in principle be any real number, although 
        // uses with corrScale < 0 are unlikely. A user might want to turn
        // the correction off by editing corrScale to 0.

        spectrum->correctionScale(corrScale);       

        // channel type can either be PI or PHA. Again a required keyword
        // but if not present, use PHA. Mainly an anachronism.

   }
   catch ( CCfits::FitsException& )
   {
        string msg = "Keyword ";
        msg += errmsg;
        msg += " is missing or of improper type in file: " ;
        msg += dsb->dataName(); 
        throw XspecDataIO::RequiredDataNotPresent(msg);       
   }        

   // read filter keys into array
   setFilterKeys(row);
}

void OGIP_92aData::groupArrays (size_t row)
{
  CCfits::ExtHDU& ext = dataSource()->extension(extensionName());

  SpectralData* data = sourceData(row);
  const Real ZERO(0.);

  // these are the arrays to be modified.

  int last = data->endChan() - data->startChan();
  int first = 0;

  size_t ungroupedChannels = (size_t)(last - first + 1);
  int chans (data->channels());
  RealArray outputSpectrum(0.0,chans);
  RealArray outputVariance(0.0,chans);
  RealArray outputArea(1.0,chans);
  RealArray outputBackground(1.0,chans);
  RealArray ungroupedArea(data->areaScale());
  RealArray ungroupedBackground(data->backgroundScale());
  // recall: RealArray implemented as std::valarray which has vector-like
  // operation syntax. As in the next source line...

  outputVariance = 0;


  CCfits::Column* dataColumn = 0;

  // COUNTS or RATE Column. Grab a pointer to whichever is there.
  // defaults to RATE if someone is silly enough to put in both.
  if (isCounts())
  {
        dataColumn = &ext.column(COUNTS());       
  }
  else
  {
        dataColumn = &ext.column(RATE());                 
  }
  // note that up to this point no allocations have been done, only
  // reference-finding.

  RealArray src(0.,ungroupedChannels);
  if ( row == 0)
  {
        dataColumn->read(src,first,last);
  }
  else
  {
        RealArray srcTmp;
        dataColumn->read(srcTmp,row);
        src = RealArray(srcTmp[std::slice(first,last - first + 1,1)]);       
  }


  // Get statistical errors.  Patch fix to v12.2.0: to make compatible
  // w/ v11, Poisserr = true will override presence of STAT_ERR col.
  // Even if Poisserr = true, still check for STAT_ERR col and
  // issue warning if it exists.

  bool poissonStatistics   = false;
  bool poissonKey = false;
  bool systematicsPresent = false;
  RealArray stat(0.,ungroupedChannels);
  RealArray syst(0.,ungroupedChannels);
  bool isFirst = (row == 0 || dataSetBase()->multiSpectralData().size() == 1);

  try
  {
        ext.readKey(POISSERR(),poissonKey);
        if (poissonKey)
        {
                stat.resize(ungroupedChannels,0.0);
                poissonStatistics = true;
        }
  }
  catch (CCfits::FitsException&) 
  {
     if (isFirst)
        tcerr << "***Warning: " << POISSERR() 
              << " keyword is missing or of wrong format, assuming FALSE." <<std::endl;
  }

  try 
  {
          CCfits::Column& statCol = ext.column(STATISTICAL());
          if (!poissonStatistics)
          {
             if ( row == 0)
             {
                   statCol.read(stat,first,last);
             }
             else
             {
                   RealArray statTmp;
                   statCol.read(statTmp,row);
                   stat = RealArray(statTmp[std::slice(first,last - first + 1,1)]);       
             }
          }
          else
          {
             // Poisserr=T and STAT_ERR exists.  Issue warning,
             // but only for first spectrum in data set.
             if (isFirst)
             {
                tcout << xsverbose(10)<< "***Warning: Data file " << dataSetBase()->dataName()
                     << " has both POISSERR key set to 'true' and a "
                     << STATISTICAL() << " column.\n"
                     << "   XSPEC will assume Poisson errors." << std::endl
                     << xsverbose();
             }
          }
  }
  catch (CCfits::Table::NoSuchColumn)
  {
     if (!poissonStatistics)
     {
          tcout << xsverbose(10);
          tcout << "\nWarning: Statistics not present and Poisson Error Key ";
          tcout << "not set to 'true': setting errors to zero"<<std::endl; 
          tcout << xsverbose();
     }    
  }  
  catch ( ... ) { throw; }


  try 
  {
     CCfits::Column& sysCol = ext.column(SYSTEMATIC());
     systematicsPresent = true;
     if ( row == 0)
     {
        sysCol.read(syst,first,last);
     }
     else
     {
        if (sysCol.repeat() > 1)
        {
           RealArray systTmp;
           sysCol.read(systTmp, row);
           syst = RealArray(systTmp[std::slice(first,last - first + 1,1)]);       
        }
        else
        {
           RealArray tmp;
           sysCol.read(tmp, row, row);
           syst = tmp[0];
        }                        
     }
  }
  catch (CCfits::Table::NoSuchColumn)
  {
       Real sysVal=.0;
       try
       {
          ext.keyWord(SYSTEMATIC()).value(sysVal); 
          if (std::fabs(sysVal) > SMALL)
          {
             systematicsPresent = true;
             syst = sysVal;
          }

       }
       catch (CCfits::HDU::NoSuchKeyword&)
       {
       }  
  }         


  // does the spectrum represent net flux? If so the area and background 
  // scale will be treated differently. If the flux is not NET, areas will be
  // flux-averaged, but will be straight averaged if it is.

  bool netFlux (isNet(src));

  if (groupingStorage() == NO_STORE)
  {
     // Case I: simplest case: no quality or grouping information.
     // This should be a little more efficient than setting qArray to 0
     // everywhere and executing Case II - which tactic is adopted for
     // the general case, III.
     if (qualityStorage() == NO_STORE)
     {
        outputSpectrum = src;  
        outputArea = ungroupedArea;
        outputBackground = ungroupedBackground;             
        if (poissonStatistics)
        {
           if (poissonKey)
           {
              for (size_t j = 0; j < src.size(); ++j)
                      outputVariance[j] = std::max(src[j],ZERO);
              if (!isCounts())
              {
                 // If input is in RATES, to keep things consistent in later 
                 // scaleArrays,variance needs to be in units of cts/sec^2 
                 // at this point.
                 outputVariance /= data->exposureTime(); 
              }
           }
        }
        else   outputVariance = stat*stat;

        if (systematicsPresent)
        {
           outputVariance += (syst*src)*(syst*src);
        }
     }
     // Case II: no grouping information, but quality information.
     else
     {
        IntegerArray outputQuality(chans);
        const IntegerArray& quality = data->qualityInfo();
        using std::setw;
        // in this case 
        // # channels =  # ungroupedChannels -  # of (quality = 1) entries.
        size_t k = 0;
        size_t j = 0;
        while (k < ungroupedChannels )
        {
           if ( quality[k] != 1)
           {
              outputQuality[j]        = quality[k];
              outputSpectrum[j]       = src[k];
              outputArea[j]           = ungroupedArea[k];
              outputBackground[j]     = ungroupedBackground[k];
              if (!poissonStatistics)         
              {
                 outputVariance[j]    =  stat[k]*stat[k];
              }
              else
              {
                 if (poissonKey) 
                 {
                    outputVariance[j] = std::max(src[k],ZERO);
                    if (!isCounts())
                       outputVariance[j] /= data->exposureTime();
                 }
              }
              if (systematicsPresent) 
              {
                 outputVariance[j] += 
                          (src[j]*syst[k])*(src[j]*syst[k]);
              }
              ++j;       
           }
           ++k;   
        }
        data->setQuality(outputQuality);
     }
  }
  else
  {
     // Case III General case: there is grouping information.
     // the complication arises primarily because of the statistics.
     // Since the correct thing to do seems to be to add channel
     // variances/systematics in quadrature the sums come out wrong unless 
     // this is executed in two stages.


     const IntegerArray& grouping = data->groupingInfo();

     // in this case 
     // # channels =  # channels with GC=0 && # channels with GC = 1.
     //                              -  # of (quality = 1) entries.
     // channels with GC=1 followed by GC=-1 are summed until the next
     // GC = 1 channel is reached.
     if (!poissonStatistics)
     {
        RealArray __tmp0(stat*stat);
        stat = __tmp0;
     }
     else
     {
        if (poissonKey) 
        {
           for (size_t l = 0; l < ungroupedChannels; ++l)
           {
              src[l] >= 0 ? stat[l] = src[l] : stat[l] = 0;
           }
           if (!isCounts())
              stat /= data->exposureTime();
        }
     }

     // k refers to individual channels, j is the GROUPED bin
     size_t k = 0;
     int j = -1;

     if (qualityStorage() != NO_STORE)
     {
        IntegerArray outputQuality(chans);
        const IntegerArray& quality = data->qualityInfo();
        Real binSum = 0.;
        Real binSumA = 0.; 
        Real binSumB = 0.;      
        Real invSumA = 0.;
        Real invSumB = 0.; 
        size_t binsInChannel(0);
        // If there's a systematic fractional error, it's allowed to
        // vary per channel.  The group will use the maximum syst error
        // of all its channels.
        Real maxSyst = 0.;
        while (k < ungroupedChannels )
        {
           if ( quality[k] != 1)
           {
                      // "bad" - user ignorable data is GC =  0
              if ( grouping[k] != -1 )
              {        
                 // startbin channel is GC = 1 ('+')
                 // we already checked that grouping flag is (1,0,-1)
                 // we also already know that ungroupedArea != 0.
                 ++j; 
                 outputQuality[j]  = quality[k];
                 if ( j > 0)
                 {
                    // Finish the previous grouped bin.
                    outputSpectrum[j - 1] = binSum;
                    if (systematicsPresent)
                       outputVariance[j-1] += maxSyst*maxSyst*binSum*binSum;
                    if ( !netFlux )
                    {
                       outputArea[j-1]  
                           = (binSumA != 0
                               ? binSum/binSumA : binsInChannel/invSumA);
                       outputBackground[j-1] 
                           = (binSumB != 0 
                               ? binSum/binSumB : binsInChannel/invSumB);
                    }
                    else
                    {
                       outputArea[j-1]=binSumA / binsInChannel;
                       outputBackground[j-1]=binSumB/binsInChannel;
                    }
                 }

                 binSum = src[k];
                 if (systematicsPresent)
                    maxSyst = syst[k];
                 if (!netFlux)
                 {
                    invSumA = 1.0/ungroupedArea[k];
                    invSumB = 1.0/ungroupedBackground[k];
                    binSumA = binSum*invSumA;  
                    binSumB = binSum*invSumB;
                 }
                 else
                 {
                    binSumA = ungroupedArea[k];
                    binSumB = ungroupedBackground[k];  
                 }
                 binsInChannel = 1;  
                 outputVariance[j] = stat[k];
              }
              else
              {
                 // GC = -1 ('-')
                 // new: if the quality array differs among grouped bins
                 // set the quality for that bin equal to the worst value
                 // found
                 outputQuality[j] = std::max(quality[k],outputQuality[j]);
                 outputVariance[j] += stat[k];       
                 binSum  += src[k];
                 if (systematicsPresent)
                 {
                    // Systematic error will be added to outputVariance
                    // only after group is completed.
                    maxSyst = std::max(maxSyst, syst[k]);
                 }
                 if (!netFlux)
                 {
                    invSumA += 1.0/ungroupedArea[k];
                    invSumB += 1.0/ungroupedBackground[k];
                    binSumA += src[k]/ungroupedArea[k];
                    binSumB += src[k]/ungroupedBackground[k];
                 }
                 else
                 {
                    binSumA += ungroupedArea[k];
                    binSumB += ungroupedBackground[k];  
                 }
                 ++binsInChannel;

              }        
           }
           // QC = 1 (GC = ' ')
           ++k;
           if (k == ungroupedChannels && j >= 0)
           {
              outputSpectrum[j] = binSum;
              if (systematicsPresent)
                 outputVariance[j] += maxSyst*maxSyst*binSum*binSum;
              if ( !netFlux )
              {
                 outputArea[j] =  (binSumA != 0) ? binSum/binSumA : 
                        binsInChannel/invSumA;
                 outputBackground[j] = (binSumB != 0) ? binSum/binSumB :
                        binsInChannel/invSumB;
              }
              else
              {
                 outputArea[j] = binSumA / binsInChannel;
                 outputBackground[j] = binSumB/binsInChannel;
              }
           }

        }
        data->setQuality(outputQuality);
     } // end if quality set
     else
     {
        Real binSum = 0.;
        Real binSumA = 0.; 
        Real binSumB = 0.;               
        Real invSumA = 0.;
        Real invSumB = 0.; 
        size_t binsInChannel(0); 
        Real maxSyst = 0.;        
        while (k < ungroupedChannels )
        {
           // "bad" - user ignorable data is GC =  0
           if ( grouping[k] != -1 )
           {        
              // startbin channel is GC = 1 ('+')
              // we already checked that grouping flag is (1,0,-1)
              // we also already know that ungroupedArea != 0.
              ++j; 
              if ( j > 0)
              {
                 outputSpectrum[j - 1] = binSum;
                 if (systematicsPresent)
                    outputVariance[j-1] += maxSyst*maxSyst*binSum*binSum;
                 if ( !netFlux )
                 {
                    outputArea[j-1]  
                        = (binSumA != 0
                            ? binSum/binSumA : binsInChannel/invSumA);
                    outputBackground[j-1] 
                        = (binSumB != 0 
                            ? binSum/binSumB : binsInChannel/invSumB);
                 }
                 else
                 {
                    outputArea[j-1]=binSumA / binsInChannel;
                    outputBackground[j-1]=binSumB/binsInChannel;
                 }
              }

              binSum = src[k];
              if (!netFlux)
              {
                 invSumA = 1.0/ungroupedArea[k];
                 invSumB = 1.0/ungroupedBackground[k];
                 binSumA = binSum*invSumA;  
                 binSumB = binSum*invSumB;
              }
              else
              {
                 binSumA = ungroupedArea[k];
                 binSumB = ungroupedBackground[k];  
              }
              binsInChannel = 1;  

              outputVariance[j] = stat[k];
              if (systematicsPresent)
                 maxSyst = syst[k];
           }
           else
           {
              // GC = -1 ('-')
              // new: if the quality array differs among grouped bins
              // set the quality for that bin equal to the worst value
              // found
              outputVariance[j] += stat[k];       
              binSum  += src[k];
              if (systematicsPresent)
              {
                 // Systematic error will be added to outputVariance
                 // only after group is completed.
                 maxSyst = std::max(maxSyst, syst[k]);
              }
              if (!netFlux)
              {
                 invSumA += 1.0/ungroupedArea[k];
                 invSumB += 1.0/ungroupedBackground[k];
                 binSumA += src[k]/ungroupedArea[k];
                 binSumB += src[k]/ungroupedBackground[k];
              }
              else
              {
                 binSumA += ungroupedArea[k];
                 binSumB += ungroupedBackground[k];  
              }
              ++binsInChannel;

           }        
           // QC = 1 (GC = ' ')
           ++k;
           if (k == ungroupedChannels)
           {
              outputSpectrum[j] = binSum;
              if (systematicsPresent)
                 outputVariance[j] += maxSyst*maxSyst*binSum*binSum;
              if ( !netFlux )
              {
                 outputArea[j] =  (binSumA != 0) ? binSum/binSumA : 
                                binsInChannel/invSumA;
                 outputBackground[j] = (binSumB != 0) ? binSum/binSumB : 
                                binsInChannel/invSumB;
              }
              else
              {
                 outputArea[j] = binSumA / binsInChannel;
                 outputBackground[j] = binSumB/binsInChannel;
              }
           }  
        }

     }
     assert ( j == chans  - 1);
  }
  data->setSpectrum(outputSpectrum);
  data->setAreaScale(outputArea);
  data->setBackgroundScale(outputBackground);
  data->setRawVariance(outputVariance);
  // Mostly for regression testing: only for very high chatter level 
  // output to log.
  tpout.setVerbose(9999, 50);
  if (tpout.logChatterLevel() >= tpout.logVerbose())
  {
     tcout << "\nGrouped channel:     cts(/sec):" << std::endl;
     for (int i=0; i<chans; ++i)
     {
        tcout << i << "      " << outputSpectrum[i]<<std::endl;
     }
     tcout << std::endl;
  }
  tpout.setVerbose();
}

void OGIP_92aData::initialize (DataPrototype* proto, DataInputRecord& record)
{
  //int dot = record.fileName().find_last_of('.');
  //const string& name = record.fileName().substr(0, dot < 0 ? record.fileName().length() : dot );
  bool readFlag(record.spectrumRange().size() == 1 && record.spectrumRange()[0] == 0);

  dataSetBase() = new DataSetBase(record.fileName(), DataSet::count(),proto);
  setRunPath();

  size_t numberOfSpectra(1);
  // read returns an integer parameter denoting the number of spectra in the
  // file. This can be checked against the ranges requested and that is interpreted
  // if it contains a wildcard, or corrected if the user requested more spectra
  // than exist in the file.  read also determines type of data, COUNTS or RATE.
  numberOfSpectra = read(record.fileName(),readFlag);


  getNetType();
  // Determine whether quality and grouping exist as keywords, cols, 
  // or neither.
  getQualGroupStorage(OGIP_92aIO::QUALITY());
  getQualGroupStorage(OGIP_92aIO::GROUPING());


  // Early detection of Type 1 vs 2 mismatch based on presence or lack
  // of SPEC_NUM keyword.
  CCfits::ExtHDU& ext = dataSource()->extension(extensionName());
  if (record.spectrumRange(0)==0)
  {
     // The user expects file to be Type 1.  Is it?
     if (specNum(ext))
     {
        string msg = record.fileName() + '\n';
	msg += "Detected to be of Type 2 format.  ";
	msg += "Row specifier is needed.\n";
	throw XspecDataIO::UnspecifiedSpectrumNumber(msg);
     }
  }
  else
  {
     // Type 2 file is expected
     if (!specNum(ext))
     {
        string msg = '\n'+record.fileName();
	msg += " is detected to be of Type 1 format.  ";
	msg += "Row specifiers do not apply.\n";
	throw XspecDataIO::SingleSpectrumOnly(msg);
     }     
  }


  record.updateSpectrumCounts(numberOfSpectra); 
  if (!record.spectrumRange().size() || !record.spectrumNumber().size())
  {
     string msg = "All rows out of range of file "+record.fileName()+'\n';     
     throw YellowAlert(msg);
  }

}

void OGIP_92aData::setAncillaryFileName (string& outFileName, const string& matchName, const string& key, const string& suffix, size_t row)
{
   static const string MATCH = "%match%";
   static const string SAME = "[]";
   static const string NONE  = "none";


   outFileName = "";
   bool fromCol = false;
   try
   {
        CCfits::ExtHDU& ext = dataSource()->extension(extensionName());
        string fileName("");

        try 
        {
                ext.keyWord(key).value(fileName);
        }
        catch (CCfits::HDU::NoSuchKeyword)
        {
           if (row == 0)
           {
              // Relax the keyword requirements of the OGIP standard
              // only for CORRFILE. (and also RESPFILE - 01/09)
              if (key != CORRFILE() && key != RESPFILE())
              {
                 throw XspecDataIO::RequiredDataNotPresent(key);
              }
              if (key == RESPFILE())
              {
                 tcout<<"\n***Warning: Missing RESPFILE keyword, assuming \"none\""
                    << std::endl;
              }
              fileName = NONE;
           }
           else
           {
              std::vector<string> fileNameCol(1,"");
              ext.column(key).read(fileNameCol,row,row); 
              fileName = fileNameCol[0];
              fromCol = true;
           }      
        }

        const string testFileName(XSutility::lowerCase(fileName));
        if ( testFileName == MATCH ) 
        {
                outFileName = matchName.substr(0, matchName.find_last_of('.')) + suffix;
        }
	else if ( testFileName == SAME )
	{
                outFileName = matchName;
	}
        else
        {
                if (testFileName.substr(0,4)  == NONE || testFileName.find_first_not_of(" \t\n")
                              == XSparse::NOTFOUND() ) outFileName = "";
                else if (fromCol && (key != RESPFILE())) 
                {
                   // This block is a patch kluge to allow a way for type-II data files
                   // to get at type-I back/corr/arf files.  They can't currently do it
                   // through keyword, only from column.  We'll explicitly add the
                   // unstandard {0} specifier so this will be recognized down the
                   // road in setAncillaryData for back/corr and setResponse for arf.

                   outFileName = fileName;
                   if (fileName.find("{") == string::npos)
                   {
                      outFileName += string("{0}");
                   }
                } 
                else outFileName = fileName;
        }
   }
   catch (CCfits::FitsException&)
   {
        // should only throw a HDU::NoSuchKeyword, but the required action is:
        // if anything goes wrong set the ancilliary data filename to blank.

        // Again, relax the standard for the CORRFILE keyword.
        // (and RESPFILE 01/09)
        if (key != CORRFILE() && key != RESPFILE())
        {
           throw XspecDataIO::RequiredDataNotPresent(key);
        }
        if (key == RESPFILE())
        {
           tcout<<"\n***Warning: Missing RESPFILE keyword, assuming \"none\""
              << std::endl;
        }
        outFileName = "";       
   }
   catch (...) 
   {
           throw; // probably only std::bad_alloc
   }             
}

bool OGIP_92aData::setAncillaryData (size_t row, int ancRow)
{

  // there are four potential ancillary files, Background, Correction, Response,
  // and AuxResponse. Current plan is to delegate responsibility for AuxResponse
  // to the Response class since they are in 1:1 correspondence. However, we 
  // can instantiate and read both here perhaps.

  // these will be set here if the filenames have  been obtained successfully
  // from the data file (after processing by setAncilliaryFileName).

  // the dataSetBase structure should contain enough information to instantiatiate
  // and initialize the BackCorr files using their setData method.

  // the row parameter is redundant for OGIP-I. 
  bool isSuccess = false;    
  SpectralData* data = sourceData(row);
  const DataPrototype* proto = dataSetBase()->protoType();
  int backAncRow = ancRow, corrAncRow = ancRow;
  // If ancRow = the default value of -1, then we got here during initialization
  // of spectral data object, NOT by way of backgrnd and corfile commands.
  if (ancRow == -1)
  {
     // Patch fix: Check for possibility that filenames include {} specifiers.
     // These must be parsed and handled.  Backgnd and Corr commands would have 
     // already done this.
     if (data->backgroundFile().length())
     {
        const string totBackFileSpec(data->backgroundFile());
        string::size_type pos = totBackFileSpec.find("{");
        if (pos != string::npos)
        {
           // Note that setAncillaryFileName may have stuck a "{0}" at
           // the end, it's telling us it wants a type-I back/corr
           // for a type-II file.  
           string actualFileName;
           // this may throw
           backAncRow = XSparse::getCurlyBracketInt(totBackFileSpec,actualFileName);

           // This odd bit of functionality is merely for backwards 
           // compatibility.  The old XSparse processStringToken function
           // seemed to add a default suffix for files without '.', but
           // only for non-zero row numbers.
           string::size_type dummySquare = string::npos;
           string::size_type dummyPlus = string::npos;
           if (XSutility::lowerCase(actualFileName) != XSparse::NONE() &&
                backAncRow > 0 && 
                !XSparse::checkExtendedSyntax(actualFileName,dummySquare,dummyPlus))
              actualFileName = XSutility::addSuffix(actualFileName,XSutility::PHA);

           data->backgroundFile(actualFileName);
        }
        else if (row)
        {
           // Coming from type-II data, assume user wants 1-to-1 match up with type-II 
           // background file.  Will need to catch later if this is not the case.
           backAncRow = row;
        }
        else
        {
           // Type-I data, no row specifier, this one's easy.
           backAncRow = 0;
        }
     }
     if (data->correctionFile().length())
     {
        const string totCorrFileSpec(data->correctionFile());
        string::size_type pos = totCorrFileSpec.find("{");
        if (pos != string::npos)
        {
           // Note that setAncillaryFileName may have stuck a "{0}" at
           // the end, it's telling us it wants a type-I back/corr
           // for a type-II file.  
           string actualFileName;
           // this may throw
           corrAncRow = XSparse::getCurlyBracketInt(totCorrFileSpec,actualFileName);

           string::size_type dummySquare = string::npos;
           string::size_type dummyPlus = string::npos;
           if (XSutility::lowerCase(actualFileName) != XSparse::NONE() &&
                corrAncRow > 0 && 
                !XSparse::checkExtendedSyntax(actualFileName,dummySquare,dummyPlus))
              actualFileName = XSutility::addSuffix(actualFileName,XSutility::PHA);

           data->correctionFile(actualFileName);
        }
        else if (row)
        {
           // Coming from type-II data, assume user wants 1-to-1 match up with type-II 
           // correction file.  Will need to catch later if this is not the case.
           corrAncRow = row;
        }
        else
        {
           // Type-I data, no row specifier, this one's easy.
           corrAncRow = 0;
        }
     }
  }

  if ( data->backgroundFile().length() != 0)
  {
     std::auto_ptr<Background> bkg(proto->MakeBackground());
     bool formatCheck = false;
     try
     {
        formatCheck = bkg->fileFormat(data->backgroundFile(),XspecDataIO::SpectrumType); 

     }               
     catch ( XspecDataIO::CannotOpen )
     {
        string newFile; 
        data->backgroundFile("");                
        try
        {
           XSparse::getFileNameFromUser(data->backgroundFile(),newFile,XSutility::BCK);
           if (newFile.length() != 0)
           {
              formatCheck = bkg->fileFormat(newFile,XspecDataIO::SpectrumType); 
              data->backgroundFile(newFile);                                
           }
        }
        catch (XspecDataIO::CannotOpen)
        {
           tcout <<"\n*** Cannot open file - skipped "<<std::endl;                   
        }
	catch (XSparse::SkipThis)
	{
	}
     }
     if (formatCheck)
     {
        bkg->initialize(this, row, data->backgroundFile(), 
	        static_cast<size_t>(backAncRow));
        bkg->setData(data->spectrumNumber(), backAncRow);
        bkg->closeSourceFiles();
        data->background(bkg.release());
        isSuccess = true;
     }
     else
     {
	tcout <<std::endl;
        if (data->backgroundFile().length() > 0)
        {
           tcout << "*** Background file has inconsistent format with data: ";
        }
        tcout << " background file " << data->backgroundFile() << " read skipped";
	tcout <<std::endl;
	data->backgroundFile("");
     }
  }

  if ( data->correctionFile().length() != 0)
  {
     isSuccess = false;
     std::auto_ptr<Correction> cor(proto->MakeCorrection());
     bool formatCheck = false;
     try
     {
        formatCheck = cor->fileFormat(data->correctionFile(),XspecDataIO::SpectrumType);                             
     }               
     catch ( XspecDataIO::CannotOpen )
     {
        string newFile;
        try
        {
           data->correctionFile("");                             
           XSparse::getFileNameFromUser(data->correctionFile(),newFile,XSutility::COR);
           if (newFile.length() != 0)
           {
              formatCheck  = cor->fileFormat(newFile,XspecDataIO::SpectrumType); 
              data->correctionFile(newFile);

           }
        }
        catch (XspecDataIO::CannotOpen)
        {
           tcout << "\n*** Cannot open file - skipped " <<std::endl;
        }
	catch (XSparse::SkipThis)
	{
	}                
     }        
     if (formatCheck)
     {
	cor->initialize(this, row, data->correctionFile(), 
		static_cast<size_t>(corrAncRow));
        cor->setData(data->spectrumNumber(), corrAncRow, true);
        cor->closeSourceFiles();
        data->correction(cor.release());
	isSuccess = true;
     }
     else
     {
	tcout <<std::endl;
        if (data->correctionFile().length() > 0)
        {
           tcout << "*** Correction file has inconsistent format with data: ";
        }
        tcout << " correction file " << data->correctionFile() << " read skipped";
	tcout <<std::endl;
	data->correctionFile("");
     }
  }

  return isSuccess;     
}

bool OGIP_92aData::isCounts () const
{

  return OGIP_92aIO::isCounts();
}

void OGIP_92aData::closeSourceFiles ()
{
  OGIP_92aIO::closeFile();

}

void OGIP_92aData::setResponse (size_t spectrumNumber, size_t row)
{


  static const string RSP   = ".rsp";
  static const string ARF   = ".arf";

  SpectralData* sourceSpectrum = sourceData(row);
  string responseName;
  setAncillaryFileName(responseName, dataName(), RESPFILE(), RSP, row);

  string arfName;
  setAncillaryFileName(arfName, dataName(), ANCRFILE(), ARF, row);

  if (responseName.length() == 0) 
     return;

  setResponse(sourceSpectrum, spectrumNumber, 1, responseName, arfName);

}

bool OGIP_92aData::match (const IntegerArray& srcValue, const IntegerArray& value)
{

   bool  valueSet = !srcValue.empty();
   bool  vSet = !value.empty();


   if (valueSet == vSet)
   {
      if (!valueSet)
	 return true;

      if (value == srcValue)
	 return true;
   }

   return false;
}

void OGIP_92aData::reportResponse (size_t row) const
{
   using namespace std;
   const std::vector<Response*>& dets = sourceData(row)->detector();
   bool noRespLoaded = true;
   for (size_t i=0; i<dets.size(); ++i)
   {
      const Response* rsp = dets[i];
      if (rsp)
      {
         const RealResponse* rrsp = rsp->toRealResponse();
         const UserDummyResponse* drsp = rsp->toUserDummyResponse();
         noRespLoaded = false;
         if (rrsp)
         {
            if ( rrsp->rmfName().size() >  0)
            {
               tcout << left << setw(38) 
                    << " Using Response (RMF) File " << rrsp->rmfName()
                    << " for Source " << i+1 << std::endl;
               if (rrsp->isGainApplied())
               {
                  const std::vector<Real>& gainFactors = rrsp->gainFactor();
                  tcout << "   With applied gain:  slope = " << gainFactors[1]
                        << "  offset = " << gainFactors[0] << std::endl;
               }
            }
            if (rrsp->arfName().size() >  0 )
            {
               tcout << left << setw(38) 
                          << " Using Auxiliary Response (ARF) File " 
                          <<  rrsp->arfName();
               if (rrsp->arfRow())
                  tcout << '{' << rrsp->arfRow() << '}';
               tcout << std::endl;         
            }
         }
         else if (drsp)
         {
	    tcout << left << setw(38) << " Using Dummy Response for Source"
                << i+1 << std::endl;
            if (drsp->diagRspMode() && drsp->arfNames().size() 
                        && drsp->arfNames()[0].length())
            {
	       tcout  << left << setw(38)
	      	       << " Using Auxiliary Response (ARF) File "
		       << drsp->arfNames()[0] << std::endl;               
            }
         }
      }
   }
   if (noRespLoaded)
   {
      tcout << " No response loaded." << std::endl;
   }
}

bool OGIP_92aData::setResponse (SpectralData* sourceSpectrum, size_t spectrumNumber, size_t sourceNum, const string& responseName, const string& arfName)
{
    using XSContainer::responses; 

    const DataPrototype *proto = dataSetBase()->protoType();

    // OGIP has an OGIP response object so the cast is safe here.
    std::auto_ptr<OGIP_92aResponse> rsp(static_cast<OGIP_92aResponse*>(proto->MakeResponse()));
    rsp->sourceNumber(sourceNum);
    rsp->rmfName(responseName);
    rsp->rspRunPath(XSutility::getRunPath());

    // arfName may actually contain a row specifier which must be parsed.
    string arfFileName;

    // If setAncillaryFileName stuck a "{0}" at the end, it's 
    // telling us it wants a type-I arf file for a type-II data set.
    string::size_type pos = arfName.find("{");
    if (pos != string::npos)
    {
       // this may throw
       size_t rowNum = XSparse::getCurlyBracketInt(arfName,arfFileName);

	// Only set the arf rowNum at this point if it has been explicitly
	// entered inside of brackets in either the ancrfile keyword
	// or column of the DataSet file.  The "response" command also
	// calls this function but it leaves the arfName parameter empty,
	// so it can't get in here.  If no brackets have been entered
	// that doesn't MEAN that rowNum=0 (ie. Type 1) is being specified.
	// It could also be a Type 2 file where the arf rows correspond
	// 1 to 1 with the spectra.  That will get determined later
	// on when the response object's readAuxResponse function is called.
	rsp->arfRow(rowNum);

       // This odd bit of functionality is merely for backwards 
       // compatibility.  The old XSparse processStringToken function
       // seemed to add a default suffix for files without '.', but
       // only for non-zero row numbers.
       string::size_type dummySquare = string::npos;
       string::size_type dummyPlus = string::npos;
       if (XSutility::lowerCase(arfFileName) != XSparse::NONE() &&
            rowNum > 0 && 
            !XSparse::checkExtendedSyntax(arfFileName,dummySquare,dummyPlus))
          arfFileName = XSutility::addSuffix(arfFileName,XSutility::ARF);

    }
    else if (arfName.length())
    {
       // No row specifier in arfName, which is coming from keyword.
       // Should not get in here from response command.
       arfFileName = arfName;
       if (sourceSpectrum->rowNumber())
       {
          // This is the case of a type-II data file asking for the 1 to 1
          // match-up with rows in a type-II arf file.
          // Let OGIP-Response::readAuxResponse know this by setting to 
          // npos for now.
          rsp->arfRow(string::npos);
       }
    }
    rsp->arfName(arfFileName);
    rsp->arfRunPath(XSutility::getRunPath());

    bool responseCheck = false;
    bool isSuccess = false;

    try
    {
	responseCheck = rsp->fileFormat(responseName, XspecDataIO::ResponseType); 
    }               
    catch ( XspecDataIO::CannotOpen )
    {
	string newFile("");
	try
	{
	    responseCheck = false;
	    // throws AbortLoop or SkipThis if user types "none" or "/*"
	    XSparse::getFileNameFromUser(responseName, newFile, XSutility::RSP);
	    if (newFile.length() != 0)
	    {
		responseCheck = rsp->fileFormat(newFile,XspecDataIO::ResponseType); 
		if (responseCheck)
		{
		    rsp->rmfName(newFile);
		}
	    }
	}
	catch (XSparse::SkipThis) { }
	catch (XspecDataIO::CannotOpen) { }
	catch (XSparse::AbortLoop)
	{ 
	    throw;      
	}

    }

    if ( responseCheck )
    {
        try
        {
	    rsp->source(sourceSpectrum);
	    rsp->setData(spectrumNumber,dataSetBase()->dataGroup());
	    rsp->setEnergies();
	    RealResponse* rspSaved = rsp.release();
	    if (sourceSpectrum->detector(sourceNum-1))
	    {
		// This also removes response from response container.
		sourceSpectrum->removeResponses(sourceNum);
	    }
	    responses->addToList(responseName,rspSaved);


	    // for a one source file.


	    sourceSpectrum->attachDetector(responses->responseList(responseName,
                                rspSaved->index()),sourceNum-1);
	    isSuccess = true;
        }
	catch (XSparse::AbortLoop)
	{
	    throw;
	}
        catch (...)
        {
	    tcout << "\n*** Error processing response data - file skipped"<<std::endl;
	    rsp->closeSourceFiles(); 
        }
    }
    else
    {
        tcout << std::endl;
        if (responseName.length() > 0)
        {
	    tcout <<std::endl <<"*** Response file has format inconsistent with data: ";
        }
        tcout << " response file " << rsp->rmfName() 
              << " read skipped" <<std::endl;   
    }
    return isSuccess;
}

bool OGIP_92aData::setBackgroundData (size_t row, int bckRow)
{

   bool status = false;
   string tmp = sourceData(row)->correctionFile();
   // switch off the Corr file half of setAncillaryData
   sourceData(row)->correctionFile("");
   try
   {
      status = setAncillaryData(row, bckRow);
   }
   catch (...)
   {
      sourceData(row)->correctionFile(tmp);
      throw;
   }
   sourceData(row)->correctionFile(tmp);

   return status;
}

bool OGIP_92aData::setCorrectionData (size_t row, int corRow)
{

   bool status = false;
   string tmp = sourceData(row)->backgroundFile();
   // switch off the Background file half of setAncillaryData
   sourceData(row)->backgroundFile("");
   try
   {
      status = setAncillaryData(row, corRow);
   }
   catch (...)
   {
      sourceData(row)->backgroundFile(tmp);
      throw;
   }
   sourceData(row)->backgroundFile(tmp);

   return status;
}

void OGIP_92aData::initializeFake (DataPrototype* proto, FakeDataInputRecord& record)
{
  //int dot = record.fileName().find_last_of('.');
  //const string& name = record.fileName().substr(0, dot < 0 ? record.fileName().length() : dot );
  const std::vector<FakeDataInputRecord::BackLocator>& inBackgrounds =
  		record.inputBackgrounds();
  size_t nSpec = record.spectrumNumber().size();
  dataSetBase() = new DataSetBase(record.fileName(),DataSet::count(),proto);
  size_t sourceNums = record.numSourcesForSpectra();
  origNumSources(sourceNums);
  setRunPath();
  dataGroup(record.groupNumber());
  outputFileName(record.fileName());
  int legalStart=0, legalEnd=0;

  const DataSet* origDSet = record.data();
  if (origDSet)
  {
     if (record.isType2() && nSpec != origDSet->multiSpectralData().size())
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
  // Construct the necessary number of SpectralData objects for the DataSet,
  // but leave them empty for now.  These new spectra are inserted into the
  // DataSet object immediately after they are constructed.  Therefore, if
  // any exceptions leave here, they can (will) be destroyed by destruction
  // of the entire DataSet.  

  // Fake data sets will store grouping and quality in columns.
  // This is probably not needed since these settings are normally
  // only used when reading and processing from a file, but just
  // to keep things in a self-consistent state ...
  qualityStorage(COLSTORE);
  groupingStorage(COLSTORE);

  size_t firstDetChans = 0;
  for (size_t i=0; i<nSpec; ++i)
  {
     bool isBackSuccess=false, isRespSuccess=false, isArfSuccess=false;
     size_t outRow = record.isType2() ? i+1 : 0;
     // NOTE: firstDetChans and detChans are not used when origDSet.
     SpectralData::ChannelInfo chanInfo;
     getNChansForFake(record, i, chanInfo);
     size_t nChans = chanInfo.m_endChan - chanInfo.m_startChan + 1;
     std::auto_ptr<SpectralData> asd(new SpectralData(this, nChans, outRow, sourceNums));
     asd->spectrumNumber(record.spectrumNumber(i));
     asd->exposureTime(record.exposureTime());
     asd->correctionScale(record.correctionNorm());
     const size_t nResps = record.inputResponses(i).size();

     if (!origDSet)
     {
        // Fake data set is not based on a pre-existing data set.
	asd->initializeFake(chanInfo);
        asd->statName(record.statName());
        asd->testStatName(record.testStatName());
        if (i == 0)
        {
           firstDetChans = chanInfo.m_detChans;
           dataSetBase()->legalStartChan(chanInfo.m_firstChan);
           dataSetBase()->legalEndChan(chanInfo.m_detChans + chanInfo.m_firstChan - 1);
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
        if (record.inputResponses(i)[0].first == DUMMY_RSP)
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
        // Fake data set is based on a pre-existing data set.
	const SpectralData* origSd = origDSet->sourceData(record.origRowNums(i));
	asd->initializeFake(origSd);
        for (size_t j=0; j<nResps; ++j)
        {
           size_t iSource = record.inputResponses(i)[j].second; // 0-based
           if (record.inputResponses(i)[j].first == USR_DUMMY_RSP)
           {
              // In this case, the original spectrum has a user dummy
              // response which is to be used for the fake spectrum.
              // So, we'll just copy it here and skip the "attach response"
              // section below.
              UserDummyResponse *origUd = dynamic_cast<UserDummyResponse*>
                           (origSd->detector(iSource));
              if (!origUd)
              {
                 throw RedAlert("Fake spectrum is attempting to use non-existent dummy response.");              
              }
              asd->attachUserDummy(new UserDummyResponse(*origUd), iSource);
           }
           else if (record.enteredNone())
           {
              asd->telescope("USE_FAKEIT_RMF");
              asd->instrument("USE_FAKEIT_RMF");
              asd->channelType("USE_FAKEIT_RMF");
           }
        }
     }

     // At this point new spectrum is ready to be given over to the dataset, 
     // so the auto_ptr can relinquish ownership.
     SpectralData* sd = asd.release();
     if (record.isType2())
     {
        std::map<size_t,SpectralData*>::value_type __tmp(outRow, sd);
	dataSetBase()->multiSpectralData().insert(__tmp);
     }
     else
     {
        dataSetBase()->spectralData(sd);
     }

     // Attach background.
     // inBackgrounds refers to the B in the M*R+B used to generate
     // the fake spectrum.  record.backgndFile refers to the output
     // file that will be a possibly Poisson randomized version of B.
     // It does not yet exist at this point (see DataSet::generateFake
     // and the output functions of this class).
     sd->backgroundFile(inBackgrounds[i].first);
     try
     {
	isBackSuccess = setBackgroundData(outRow, inBackgrounds[i].second);
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
           if (record.backExposureTime() > 0.0)
              sd->getBackground()->data()->exposureTime(record.backExposureTime());
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
     // Attach corr file
     if (record.inputCorrFiles(i).first.length())
     {
        sd->correctionFile(record.inputCorrFiles(i).first);
        try
        {
	   isBackSuccess = setCorrectionData(outRow, record.inputCorrFiles(i).second);
        }
        catch (...)
        {
	   sd->correctionFile("");
	   throw; 
        }
        if (!isBackSuccess)
        { 
	   sd->correctionFile("");
	   tcout << "\nNo corr file will be applied to fake spectrum #" << 
	   	        record.spectrumNumber()[i] << std::endl;
        }     
     }

     for (size_t j=0; j<nResps; ++j)
     {
        string respFileName = record.inputResponses(i)[j].first;
        if (respFileName != USR_DUMMY_RSP && respFileName != DUMMY_RSP)
        {
           size_t iSource = record.inputResponses(i)[j].second; //0-based
           try
           {

	      // NOTE:  If it succeeds, setResponse has inserted a new response
	      //   object INTO THE GLOBAL CONTAINER.
	      // The Arf name parameter is left empty for this call.  This
	      // prevents the setSharedDescription function from automatically
	      // calling readAuxResponse with a default row number. The Arf will
	      // be created later, after the Response object is validated.
	      isRespSuccess = setResponse(sd, sd->spectrumNumber(), iSource+1, respFileName,
				      string(""));
           }
           catch (...)
           {
	      throw;
           }
           if (!isRespSuccess)
           {
	      std::ostringstream msg;
	      msg << "Spectrum #" << record.spectrumNumber()[i] 
                        << " source #" << iSource+1;
	      throw ResponseIsNeeded(msg.str());
           }

           RealResponse* rsp = static_cast<RealResponse*>(sd->detector(iSource));

           // Attach Arf.
           rsp->arfName(record.inputArfs(i)[j].first.first);
           try
           {
              // If arfName is empty, this simply returns false.
	      isArfSuccess = rsp->readAuxResponse(record.inputArfs(i)[j].first.second); 
           }
           catch (...)
           {
	      rsp->arfName("");
	      throw;
           }
           // Not sure it can return false without throwing, assuming arfName != "",
           // but just to be sure ....
           if (!isArfSuccess)
           {
	      rsp->arfName("");
	      tcout << "\nNo ARF will be applied to fake spectrum #" << 
	   	           record.spectrumNumber()[i] << " source #" << iSource+1 << std::endl;
           }
        } // end if non-dummy resp exists
        else if (respFileName == DUMMY_RSP)
        {
           // Must do a final consistency check for case of spectrum NOT based
           // on original data, and using both the default dummyrsp AND a
           // background file.  The fake spectrum will have the same nChans as
           // the background file (result of getNChansForFake call), but not
           // necessarily the same as the dummyrsp.  For real responses, this
           // conflict is caught inside the setResponse call.
           const size_t nDummyChans = sd->detector(0)->numChannels();
           const size_t nSpecChans = sd->channels();
           if (nSpecChans != nDummyChans)
           {
              std::ostringstream msg;
              msg << "Number of spectrum channels: "<< nSpecChans
                <<", does not match response channels: "<<nDummyChans<<"\n";
              throw YellowAlert(msg.str());
           }
        }
     } // end sources loop
  } // end spectra loop
}

void OGIP_92aData::outputData ()
{
  if (!numSpectra())  
  {
     tcout << "\n***Warning -- No spectra in data set, output file will not be written."
                << std::endl;
     return;
  }
  try
  {
     if (isMultiple())
     {
        outputType2Table();
     }
     else
     {
        outputType1Table();
     }
  }
  catch (YellowAlert&)
  {
     return;
  }
}

FakeDataInputRecord::Arfs OGIP_92aData::getAncillaryLocation (size_t rowNum, const FakeDataInputRecord::Detectors& respInfo) const
{
   FakeDataInputRecord::Arfs arfInfo;   
   const SpectralData* sd = 0;
   if (rowNum == 0)
   {
      sd = spectralData();
   }
   else
   {
     XSContainer::SpectralDataMapConstIt sdIt = multiSpectralData().find(rowNum);
     if (sdIt != multiSpectralData().end())
     {
        sd = sdIt->second;     
     }
   }
   // sd should always be valid since presumably it already succeeded in a preceding
   // call to getResponseName.  Still...
   if (!sd)
      throw RedAlert("Attempt to access non-existing spectrum in OGIP-9aData::getResponseName");

   for (size_t i=0; i<respInfo.size(); ++i)
   {
      const FakeDataInputRecord::ResponseID& curRespInfo = respInfo[i];
      size_t sourceNum = curRespInfo.second;
      const Response* rsp = sd->detector(sourceNum);
      string arfFile;
      size_t rowNum = 0;
      if (curRespInfo.first == USR_DUMMY_RSP)
         arfFile = USR_DUMMY_RSP;
      else
      {
         const RealResponse* rrsp = dynamic_cast<const RealResponse*>(rsp);
         arfFile = rrsp->arfName();
         if (arfFile.length())
            rowNum = rrsp->arfRow();
      }
      FakeDataInputRecord::ArfID curArfInfo;
      curArfInfo.first.first = arfFile;
      curArfInfo.first.second = rowNum;
      curArfInfo.second = sourceNum;
      arfInfo.push_back(curArfInfo);   
   }
   return arfInfo;
}

std::pair<string,size_t> OGIP_92aData::getBackCorrLocation (size_t rowNum, bool isCorr) const
{
  std::pair<string,size_t> location = std::pair<string,size_t>(string(),0);
  SpectralData* sd=0;
  if (rowNum == 0)
  {
     sd = spectralData();
  }
  else
  {
     XSContainer::SpectralDataMapConstIt sdIt = multiSpectralData().find(rowNum);
     if (sdIt == multiSpectralData().end())
     {
        throw RedAlert("Attempt to access non-existing spectrum in OGIP-9aData::getBackgroundLocation");
     }
     sd = sdIt->second;       
  }

  if (isCorr)
  {
     const Correction* cor = sd->correction();
     if (cor && sd->correctionFile().length())
     {
        location.first = sd->correctionFile();
        location.second = cor->data()->rowNumber();
     }
  }
  else
  {
     const Background* bck = sd->background();
     if (bck && sd->backgroundFile().length())
     {
        location.first = sd->backgroundFile();
        location.second = bck->data()->rowNumber();
     }
  }

  return location;
}

void OGIP_92aData::getNChansForFake (FakeDataInputRecord& record, const size_t index, SpectralData::ChannelInfo& chanInfo)
{
    const DataSet* origDset = record.data();
    if (origDset)
    {
       // When based on original data, we're actually interested
       // only in m_channels since first, start and end chans will be 
       // copied directly in initializeFake, and they don't tell
       // us about the number of GROUPED channels.   
       const SpectralData* origSd = origDset->sourceData(record.origRowNums(index));
       chanInfo.m_firstChan = chanInfo.m_startChan = 1;
       chanInfo.m_endChan = origSd->channels();
       int legalStartChan=0, legalEndChan=0;
       origDset->legalChannelBounds(legalStartChan, legalEndChan);
       chanInfo.m_detChans = legalEndChan - legalStartChan + 1;
    }
    else
    {
       // No original data, first see if there's a background file to 
       // base this on.  If not, then get from response, which can
       // ONLY come from SOURCE 1 if not from original data.
       // Also can assume endChan - startChan + 1 = m_channels
       // because there will be no ignored or grouped chans.

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
       else if (record.inputResponses(index)[0].first == DUMMY_RSP)
       {
          chanInfo.m_detChans = XSContainer::responses->responseList(DUMMY_RSP,size_t(0))->numEnergies();
          chanInfo.m_startChan = chanInfo.m_firstChan = DataSet::STD_FIRST_CHAN();
          chanInfo.m_endChan = chanInfo.m_startChan + chanInfo.m_detChans - 1;
       }
       else
       {
	  try
	  {
	    // This opens (and closes) the response file for the sole purpose of 
	    // pulling out the DETCHANS and TLMIN# value.  It does not do as rigorous 
	    // a format check as when the response is ultimately opened during a 
            // setResponse call.
             std::pair<int,int> vals = getChanInfoFromResponse(record.inputResponses(index)[0].first);
             chanInfo.m_startChan = chanInfo.m_firstChan = (vals.first >= 0 ) ?
                        static_cast<size_t>(vals.first) : DataSet::STD_FIRST_CHAN();
             chanInfo.m_detChans = static_cast<size_t>(vals.second);
             chanInfo.m_endChan = chanInfo.m_startChan + chanInfo.m_detChans - 1;
	  }
	  catch (XspecDataIO::CannotOpen)
	  {
	     string newFile;
	     XSparse::getFileNameFromUser(record.inputResponses(index)[0].first, 
	      	       newFile, XSutility::RSP);
             std::pair<int,int> vals = getChanInfoFromResponse(newFile);
             chanInfo.m_startChan = chanInfo.m_firstChan = (vals.first >= 0 ) ?
                        vals.first : DataSet::STD_FIRST_CHAN();
             chanInfo.m_detChans = static_cast<size_t>(vals.second);
             chanInfo.m_endChan = chanInfo.m_startChan + chanInfo.m_detChans - 1;
             FakeDataInputRecord::Detectors tmpDetIDs(1);
             tmpDetIDs[0].first = newFile;
             tmpDetIDs[0].second = 0;
	     record.inputResponses(index, tmpDetIDs);			
	  }
       }
    }
}

void OGIP_92aData::writeCommonKeys (CCfits::Table* tbl)
{
   //Ext. keywords common to type 1 and 2
    const SpectralData *sd1 = sourceData(isMultiple() ? 1 : 0);
    tbl->addKey(HDUCLASS(), OGIPTYPE(), "format conforms to OGIP standard");
    tbl->addKey(HDUCLAS1(), SPECTYPE(), "PHA dataset (OGIP memo OGIP-92-007)");
    tbl->addKey("HDUVERS1","1.1.0","Version of format (OGIP memo OGIP-92-007a)");
    // Needed to get around CCfits addKey const problem.
    string buffer = sd1->telescope();
    tbl->addKey(TELESCOPE(), buffer, "mission/satellite name");
    buffer = sd1->instrument();
    tbl->addKey(INSTRUMENT(), buffer, "instrument/detector name");
    buffer = sd1->channelType();
    tbl->addKey(CHANNELTYPE(), buffer, "channel type (PHA, PI etc)");
    
    // XFLT keys are used by some models
    std::map<string,Real>::const_iterator itXflt = sd1->xflt().begin();
    std::map<string,Real>::const_iterator itXfltEnd = sd1->xflt().end();
    size_t iKey = 0;
    while (itXflt != itXfltEnd) {
      iKey++;
      std::ostringstream xfltKey;
      xfltKey << FILTER() << std::setfill('0') << std::right 
	      << std::setw(4) << iKey;
      std::ostringstream xfltVal;
      xfltVal << itXflt->first << ": " << itXflt->second;
      tbl->addKey(xfltKey.str(), xfltVal.str(), string(""));
      ++itXflt;
    }
        
    tbl->writeDate();
    string history("Fake data file created by ");
    history += DataSet::xspecVersion();
    history += " \"fakeit\" command";
    tbl->writeHistory(history);
}

void OGIP_92aData::outputType1Table ()
{
  const SpectralData* sd = sourceData(0);
  std::vector<string> respNames;
  std::vector<string> arfNames;
  collectRespArfNames(sd->detector(), respNames, arfNames);

  RealArray bScaleRatio(1.0, sd->channels());
  const OutputInfo specOut(outputFileName(), sd, respNames, arfNames,
           outputBckFileName(), sd->correctionFile(), bScaleRatio,
           -sd->correctionScale(), dataSetBase()->modelNamesForFake(), 
           aScaleIsKeyword(), bScaleIsKeyword());
  outputTable1Common(specOut);

  if (outputBckFileName().length())
  {
     const SpectralData* bsd = sd->background()->data();
     const std::vector<string> bckRespNames(1,"NONE"), bckArfNames(1,"NONE");
     const string bckBckName("NONE"), bckCorrName("NONE");
     const RealArray bckScaleRatio(bsd->backgroundScale()/sd->backgroundScale());
     const OutputInfo bckOut(outputBckFileName(), bsd, bckRespNames, bckArfNames,
              bckBckName, bckCorrName, bckScaleRatio, 0.0, std::vector<string>(0),
              sd->background()->aScaleIsKeyword(), sd->background()->bScaleIsKeyword());
     outputTable1Common(bckOut);
  }
}

void OGIP_92aData::outputType2Table ()
{
  string hduName = SPECTYPE();
  size_t nMaxChans = getMaxChannels();
  size_t nSpec = numSpectra();
  write(outputFileName());

  const SpectralData *sd1 = sourceData(1);
  BoolArray optCols(OGIP_92aIO::NOPTCOLS,false);
  optCols[OGIP_92aIO::COUNTS_COL] = sd1->isPoisson();
  optCols[OGIP_92aIO::STATERR_COL] = !sd1->isPoisson();
  optCols[OGIP_92aIO::QUAL_COL] = anyQuality();
  optCols[OGIP_92aIO::GROUP_COL] = anyGrouping();
  optCols[OGIP_92aIO::ASCALE_COL] = !aScaleIsKeyword();
  optCols[OGIP_92aIO::BSCALE_COL] = !bScaleIsKeyword();

  CCfits::Table *tbl = makeType2Table(hduName, nSpec, nMaxChans, optCols);
  // Now add columns for storing the model and responses used in the 
  // fakeit generation.
  const size_t nSources = std::min(XSContainer::datasets->numSourcesForSpectra(),
                        static_cast<size_t>(999));
  if (nSources != XSContainer::datasets->numSourcesForSpectra())
  {
     tcout <<"***Warning: There are too many model sources to record all of the model\n"
           <<"     and response names used for fakeit generation.  Only the first "<<nSources
           <<"     sources will be recorded." << std::endl;
  }
  type2FakeitHistoryCols(tbl, nSources);

  writeCommonKeys(tbl);
  // sd1 is used to grab things which are the same for each spectrum,
  // and written as keyvalues rather than column data.
  tbl->addKey(CORRSCALE(),-sd1->correctionScale(), "correction file scaling factor");
  if (aScaleIsKeyword())
  {
     tbl->addKey(AREASCALE(), sd1->areaScale(0), "area scaling factor");
  }
  tbl->addKey(BACKFILE(),outputBckFileName(),"associated background filename");
  if (bScaleIsKeyword())
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
  tbl->addKey(POISSERR(), sd1->isPoisson(), "Pois. err assumed ?");
  for (size_t i=0; i<nSpec; ++i)
  {
     const SpectralData *sd = sourceData(i+1);
     std::vector<string> respNames;
     std::vector<string> arfNames;
     collectRespArfNames(sd->detector(), respNames, arfNames);

     RealArray bScaleRatio(1.0, sd->channels());
     // Note: some of the parameters sent to the OutputInfo
     // ctor will not actually be used in outputTable2Common,
     // since they correspond to keywords already set above and
     // outputTable2Common only handles things which vary by 
     // spectrum.  The outputInfo class does need all of them 
     // in the type1 case.
     const OutputInfo specOut(outputFileName(), sd, respNames, arfNames,
              outputBckFileName(), sd->correctionFile(), bScaleRatio,0.0,
              dataSetBase()->modelNamesForFake(), aScaleIsKeyword(),
              bScaleIsKeyword());
     outputTable2Common(specOut, i+1, optCols);
  }
  closeFile();

  if (outputBckFileName().length())
  {
     // We need to know prior to making the type 2 table whether or not
     // ascale and bscale columns will be required. If any of the
     // spectra background objects require a column, then all will 
     // have to.  Otherwise a simple keyword will do.
     bool aColRequested = false, bColRequested = false;
     size_t firstBck = 0;
     for (size_t i=0; i<nSpec; ++i)
     {
        const Background *bck = sourceData(i+1)->background();
        if (bck)
        {
           if (!firstBck)
           {
              firstBck = i+1;
           }
           if (!bck->aScaleIsKeyword())
           {
              aColRequested = true;
           }
           if (!bck->bScaleIsKeyword())
           {
              bColRequested = true;
           }
           if (aColRequested && bColRequested)  break;
        }
     }   
     optCols[OGIP_92aIO::ASCALE_COL] = aColRequested;
     optCols[OGIP_92aIO::BSCALE_COL] = bColRequested;
     // Set bsd1 to point to the spectrum of the first background
     // data object found in the set.  If outputBckFileName has been 
     // set, we can assume that at least 1 of the spectra in this
     // set has a valid background pointer.
     if (!firstBck)
     {
        throw RedAlert("Type 2 file output background data error.");
     }
     const SpectralData* bsd1 = sourceData(firstBck)->background()->data();
     optCols[OGIP_92aIO::COUNTS_COL] = bsd1->isPoisson();
     optCols[OGIP_92aIO::STATERR_COL] = !bsd1->isPoisson();

     write(outputBckFileName());
     CCfits::Table *tbl = makeType2Table(hduName, nSpec, nMaxChans, optCols);
     writeCommonKeys(tbl);
     tbl->addKey(CORRSCALE(),0.0, "correction file scaling factor");
     if (!aColRequested)
     {
        tbl->addKey(AREASCALE(), bsd1->areaScale(0), "area scaling factor");
     }
     const string bckBckName("NONE");
     tbl->addKey(BACKFILE(),bckBckName,"associated background filename");
     if (!bColRequested)
     {
        tbl->addKey(BACKSCALE(), bsd1->backgroundScale(0), "background file scaling factor");
     }
     tbl->addKey(EXPOSURE(),bsd1->exposureTime(), "exposure (in seconds)");
     std::ostringstream tlmin;
     std::ostringstream tlmax;
     int nChanCol = tbl->column(CHANNEL()).index();
     tlmin << "TLMIN" << nChanCol;
     tlmax << "TLMAX" << nChanCol;
     tbl->addKey(tlmin.str(),dataSetBase()->legalStartChan(),"Lowest legal channel number");
     tbl->addKey(tlmax.str(),dataSetBase()->legalEndChan(),"Highest legal channel number");  
     tbl->addKey(DETCHANS(),dataSetBase()->legalEndChan() -
           dataSetBase()->legalStartChan() + 1,"total number possible channels");
     tbl->addKey(POISSERR(), bsd1->isPoisson(), "Pois. err assumed ?");
     for (size_t i=0; i<nSpec; ++i)
     {
        const SpectralData *sd = sourceData(i+1);
        const SpectralData *actualBsd = sd->background() ? 
                        sd->background()->data() : 0;
        SpectralData dummyBsd(this, sd->channels(),i+1, origNumSources());
        if (!actualBsd)
        {
           dummyBsd.initializeFake(sd);
           RealArray tmp(.0,dummyBsd.channels());
           dummyBsd.setSpectrum(tmp);
           tmp = 1.0;
           dummyBsd.setAreaScale(tmp);
           dummyBsd.setBackgroundScale(tmp);           
        }
        const SpectralData *bsd = actualBsd ? actualBsd : &dummyBsd;
        const std::vector<string> bckRespNames(1), bckArfNames(1);
        const string bckCorrName("");
        RealArray bckScaleRatio = bsd->backgroundScale()/
                        sourceData(i+1)->backgroundScale();
        const OutputInfo bckOut(outputBckFileName(), bsd, bckRespNames, bckArfNames,
              bckBckName, bckCorrName, bckScaleRatio, 0.0, std::vector<string>(0), 
              !aColRequested, !bColRequested);
        outputTable2Common(bckOut, i+1, optCols);
     }
     closeFile();     
  }
}

void OGIP_92aData::outputTable1Common (const OutputInfo& info)
{
  int nl=-999;
  write(info.fileName());
  string hduName = SPECTYPE();
  const SpectralData* sd = info.sd();
  // nChans refers to the # of ungrouped channels.
  size_t nChans = sd->endChan()-sd->startChan()+1;

  BoolArray optCols(OGIP_92aIO::NOPTCOLS, false);
  optCols[OGIP_92aIO::COUNTS_COL] = sd->isPoisson();
  optCols[OGIP_92aIO::STATERR_COL] = !sd->isPoisson();
  optCols[OGIP_92aIO::QUAL_COL] = static_cast<bool>(sd->qualityInfo().size());
  optCols[OGIP_92aIO::GROUP_COL] = static_cast<bool>(sd->groupingInfo().size());
  optCols[OGIP_92aIO::ASCALE_COL] = !info.isAScaleKeyword();
  optCols[OGIP_92aIO::BSCALE_COL] = !info.isBScaleKeyword();

  CCfits::Table *tbl = makeType1Table(hduName, nChans, optCols);
  writeCommonKeys(tbl);

  // This function will also take care of RESPFILE and ANCRFILE keys
  fakeitModRespHistory(tbl, info, 0);

  tbl->addKey(CORRFILE(),info.corrName(),"associated correction filename");
  tbl->addKey(CORRSCALE(),info.corrScale(), "correction file scaling factor");
  tbl->addKey(BACKFILE(),info.bckName(),"associated background filename");
  tbl->addKey(EXPOSURE(),sd->exposureTime(), "exposure (in seconds)");
  std::ostringstream tlmin;
  std::ostringstream tlmax;
  int nChanCol = tbl->column(CHANNEL()).index();
  tlmin << "TLMIN" << nChanCol;
  tlmax << "TLMAX" << nChanCol;
  tbl->addKey(tlmin.str(),dataSetBase()->legalStartChan(),"Lowest legal channel number");
  tbl->addKey(tlmax.str(),dataSetBase()->legalEndChan(),"Highest legal channel number");  
  tbl->addKey(DETCHANS(),dataSetBase()->legalEndChan() -
        dataSetBase()->legalStartChan() + 1,"total number possible channels");
  tbl->addKey(POISSERR(), sd->isPoisson(), "Pois. err assumed ?");
  IntegerArray chans(nChans);
  size_t startChan = sd->startChan();
  for (size_t i = 0; i < nChans; ++i) chans[i] = i + startChan;
  IntegerArray lostChans;
  DataUtility::getLostChannelNumbers(sd->qualityInfo(), sd->groupingInfo(),
                lostChans);
  tbl->column(CHANNEL()).write(chans, 1, &nl);
  // Need to do this multiplication outside the CCfits call to
  // build on Linux.
  RealArray tmp = sd->spectrum()*sd->areaScale()*info.bckScaleRatio();
  RealArray fillTmp;
  IntegerArray iFillTmp;
  Real defVal = 0.0;
  if (sd->isPoisson())
  {
     tmp *= sd->exposureTime();
     size_t nC = sd->channels();
     IntegerArray counts(nC);
     for (size_t i=0; i<nC; ++i)
     {
        counts[i] = static_cast<int>(tmp[i] + .5);
     }
     int iVal = 0;
     DataUtility::fillLostChannels(lostChans, counts, iFillTmp, iVal);
     tbl->column(COUNTS()).write(iFillTmp, 1, &nl);
  }
  else
  {
     RealArray tmpErr = sd->rawVariance()*sd->areaScale()*sd->areaScale()*
                           info.bckScaleRatio()*info.bckScaleRatio();
     DataUtility::fillLostChannels(lostChans, tmp, fillTmp, defVal);
     tbl->column(RATE()).write(fillTmp, 1);
     DataUtility::fillLostChannels(lostChans, tmpErr, fillTmp, defVal);
     // CCfits on linux won't allow this in function arg:
     RealArray stdDev(sqrt(fillTmp));
     tbl->column(STATISTICAL()).write(stdDev, 1);
  }
  defVal = -999.99;
  if (optCols[OGIP_92aIO::QUAL_COL])
  {
     tbl->column(QUALITY()).write(sd->qualityInfo(), 1, &nl);
  }
  if (optCols[OGIP_92aIO::GROUP_COL])
  {
     tbl->column(GROUPING()).write(sd->groupingInfo(), 1, &nl);
  }
  if (optCols[OGIP_92aIO::ASCALE_COL])
  {
     DataUtility::fillLostChannels(lostChans, sd->areaScale(), fillTmp, defVal);
     tbl->column(AREASCALE()).write(fillTmp,1);
  }
  else
  {
     tbl->addKey(AREASCALE(), sd->areaScale(0), "area scaling factor");
  }
  if (optCols[OGIP_92aIO::BSCALE_COL])
  {
     DataUtility::fillLostChannels(lostChans, sd->backgroundScale(), fillTmp, defVal);
     tbl->column(BACKSCALE()).write(fillTmp,1);
  }
  else
  {
     tbl->addKey(BACKSCALE(), sd->backgroundScale(0), "background file scaling factor");
  }
  closeFile();
}

void OGIP_92aData::outputTable2Common (const OutputInfo& info, size_t row, const BoolArray& optCols)
{
  // We KNOW this extension is a binary table.
  CCfits::Table* tbl = static_cast<CCfits::Table*>(&dataSource()->extension(SPECTYPE()));
  // NOTE: Current implementation will either output a statistical error
  // column for all spectra in table or none of them, in which case 
  // Poisserr keyword has already been set to true in header.  
  const bool isPoisson = !optCols[OGIP_92aIO::STATERR_COL];
  const SpectralData *sd = info.sd();
  if (sd)
  {
     int nl=-9999;
     IntegerArray specNum(1, row);
     tbl->column(NUMKEY()).write(specNum, row);
     std::vector<std::valarray<int> > chans(1);
     std::vector<std::valarray<int> > quality(1);
     std::vector<std::valarray<int> > grouping(1);
     size_t nChans = sd->endChan() - sd->startChan() + 1;
     chans[0].resize(nChans);  
     std::valarray<int>& chans0 = chans[0];
     size_t startChan = sd->startChan();  
     for (size_t i = 0; i  < nChans; ++i)
     {
        chans0[i] = i + startChan;
     }

     std::valarray<int>& quality0 = quality[0];
     std::valarray<int>& grouping0 = grouping[0];
     // sd's quality and grouping are vector<int> not valarrays, hence
     // the element by element copy.
     if (sd->qualityInfo().size())
     {
        quality0.resize(nChans);
        for (size_t i=0; i<nChans; ++i)
        {
           quality0[i] = sd->qualityInfo(i);
        }
     }
     else if (optCols[OGIP_92aIO::QUAL_COL])
     {
        // The table has a quality column and is expecting something 
        // even though spectrum has no quality info.  So, make a dummy
        // quality array of all 0's.
        quality0.resize(nChans,0);
     }
     if (sd->groupingInfo().size())
     {
        grouping0.resize(nChans);
        for (size_t i=0; i<nChans; ++i)
        {
           grouping0[i] = sd->groupingInfo(i);
        }
     }
     else if (optCols[OGIP_92aIO::GROUP_COL])
     {
        // Make a dummy gouping array of all 1's.
        grouping0.resize(nChans,1);
     }
     IntegerArray lostChans;
     DataUtility::getLostChannelNumbers(sd->qualityInfo(), sd->groupingInfo(),
                   lostChans);
     std::vector<RealArray> fillTmp(1);
     Real defVal = 0.0;
     if (isPoisson)
     {
        // output counts and no stat_err column.
        std::vector<std::valarray<int> > iFillTmp(1);
        const RealArray rcounts = sd->spectrum()*sd->exposureTime()*sd->areaScale()
                                *info.bckScaleRatio();
        size_t nC = sd->channels();
        std::valarray<int> counts(nC);
        for (size_t i=0; i<nC; ++i)
        {
           counts[i] = static_cast<int>(rcounts[i] + .5);
        }
        int iVal = 0;
        DataUtility::fillLostChannels(lostChans, counts, iFillTmp[0], iVal);
        tbl->column(COUNTS()).writeArrays(iFillTmp, row, &nl);
     }
     else
     {
        // output rate (cts/sec) and stat_err column.
        RealArray rate(sd->channels());
        RealArray areaAndRatio(sd->areaScale()*info.bckScaleRatio());
        rate = sd->spectrum()*areaAndRatio;
        DataUtility::fillLostChannels(lostChans, rate, fillTmp[0], defVal);
        tbl->column(RATE()).writeArrays(fillTmp, row);
        RealArray tmpErr(sd->rawVariance()*areaAndRatio*areaAndRatio);
        DataUtility::fillLostChannels(lostChans, tmpErr, fillTmp[0], defVal);
        // output stdDev:
        fillTmp[0] = sqrt(fillTmp[0]);
        tbl->column(STATISTICAL()).writeArrays(fillTmp, row);        
     }
     defVal = -999.0;
     tbl->column(CHANNEL()).writeArrays(chans, row, &nl);
     if (optCols[OGIP_92aIO::QUAL_COL])
     {
        tbl->column(QUALITY()).writeArrays(quality, row, &nl);
     }
     if (optCols[OGIP_92aIO::GROUP_COL])
     {
        tbl->column(GROUPING()).writeArrays(grouping, row, &nl);
     }
     if (optCols[OGIP_92aIO::ASCALE_COL])
     {
        DataUtility::fillLostChannels(lostChans, sd->areaScale(), fillTmp[0], defVal);
        tbl->column(AREASCALE()).writeArrays(fillTmp, row);
     }
     if (optCols[OGIP_92aIO::BSCALE_COL])
     {
        DataUtility::fillLostChannels(lostChans, sd->backgroundScale(), 
                           fillTmp[0], defVal);
        tbl->column(BACKSCALE()).writeArrays(fillTmp, row);
     }
     StringArray corr(1, info.corrName());
     tbl->column(CORRFILE()).write(corr,row);

     fakeitModRespHistory(tbl, info, row);
  }
}

void OGIP_92aData::setFilterKeys (size_t row)
{

  CCfits::ExtHDU& ext = dataSource()->extension(extensionName());
  SpectralData* spectrum = sourceData(row);
  using namespace std;

  bool done = false;
  size_t j = 0;
  while (!done) {
    j++;

    // set filter keyword name
    std::ostringstream xfltKey;
    xfltKey << FILTER() << setfill('0') << right << setw(4) <<  j;
    string fitskey(xfltKey.str());

    // look for the XFLTnnnn input. If these are strings then check
    // whether they are of the correct "key: value" format and if not
    // ignore them
    // first the case where the filterKeys are Keywords.
    try {        
      const CCfits::Keyword& keyword = ext.keyWord(fitskey);
      if (typeid(keyword) == typeid(CCfits::KeyData<int>) ||
	  typeid(keyword) == typeid(CCfits::KeyData<float>) ||
	  typeid(keyword) == typeid(CCfits::KeyData<double>)) {
	double fval;
	keyword.value(fval);
	spectrum->setxflt(j, (Real)fval);
      } else if (typeid(keyword) == typeid(CCfits::KeyData<string>)) {
	string filterstring;
	keyword.value(filterstring);
	if ( filterstring.find(":") != string::npos ) {
	  spectrum->setxflt(filterstring);
	}
        else 
        {
           // May be a string representation of double.
           std::istringstream iss(filterstring);
           double tmpdbl=0.0;
           if (iss >> tmpdbl)
              spectrum->setxflt(j, tmpdbl);
        }
      } else {
	throw IncorrectDataType(fitskey);
      }
    } catch (CCfits::HDU::NoSuchKeyword) {
      // If we reached here, XFLT---- is not a keyword.
      // Check if it exists as a column
      
      // throws a Table::NoSuchColumn exception
      try {
	CCfits::Column& col = ext.column(fitskey);
	if (typeid(col) == typeid(CCfits::ColumnData<int>) ||
	    typeid(col) == typeid(CCfits::ColumnData<float>) ||
	    typeid(col) == typeid(CCfits::ColumnData<double>)) {
	  std::vector<double> fvals;
	  col.read(fvals,row,row);
	  spectrum->setxflt(j, (Real)fvals[0]);
	} else if (typeid(col) == typeid(CCfits::ColumnData<string>)) {
	  std::vector<string> svals;
	  col.read(svals,row,row);
	  if ( svals[0].find(":") != string::npos ) {
	    spectrum->setxflt(svals[0]);
	  }
          else 
          {
             // May be a string representation of double.
             std::istringstream iss(svals[0]);
             double tmpdbl=0.0;
             if (iss >> tmpdbl)
                spectrum->setxflt(j, tmpdbl);
          }
	} else {
	  throw IncorrectDataType(fitskey);
	}

      } catch (CCfits::BinTable::NoSuchColumn) {
	done = true;
      }
    } catch ( CCfits::FitsException& ) {
      done = true;
    }

  }
}

RealArray OGIP_92aData::scaleVector (const string& key, int row, int first, int last)
{
  CCfits::ExtHDU& ext = dataSource()->extension(extensionName());
  int ungroupedChannels (last - first  + 1);
  RealArray scale(1.,ungroupedChannels);
  const SpectralData* spec = sourceData(row);
  try
  {       
     if ( row == 0 )
     {
        ext.column(key).read(scale,first,last);
     }
     else
     {
        // type II file
        CCfits::Column& col = ext.column(key);
        if (col.repeat() > 1)
        {
           col.read(scale, row);
        }
        else
        {
           RealArray tmp;
           col.read(tmp, row, row);
           scale = tmp[0];
        }                        
     }
     bool badScaleVal = false;
     for (int j = 0; j < ungroupedChannels; ++j) 
     {
        if (scale[j] == .0 && spec->qualityInfo()[j] != 1)
        {
           badScaleVal = true;
           scale[j] = 1.;
        } 
     }
     if (badScaleVal)
        tcout <<"***Warning: Data file " << dataName() <<" contains 1 or more\n"
           <<"    good quality channels with invalid "<<key<<" value = 0.0.\n"
           <<"    XSPEC will instead use "<<key<<" = 1.0 for these channels."
           <<std::endl;
   }
  catch (CCfits::Table::NoSuchColumn)
  {
       float floatval;
       Real scalarScale(ext.keyWord(key).value(floatval));   
       if (scalarScale == 0.0)
       {
          tcout <<"***Warning: Data file " <<dataName()<<" has invalid "
             <<key<<" value = 0.0.\n    XSPEC will instead use 1.0."<<std::endl;             
       } 
       else
          scale = scalarScale;
       if (key == AREASCALE())
       {
          dataSetBase()->aScaleIsKeyword(true);
       }
       else if (key == BACKSCALE())
       {
          dataSetBase()->bScaleIsKeyword(true);
       }
  }         
  return scale;       
}

bool OGIP_92aData::isNet (const RealArray& spectrum)
{
  // if the "NET" flag hasn't been set already, check that there are
  // no negative values and set it. If no negative values are found,
  // then we assume that the file is not background subtracted if
  // it's Type I, but can assume this if it's Type II.
  if ( !netIsSet() ) 
  {
        size_t N(spectrum.size());
        size_t i(0);
        bool negative(false);
        while ( i < N && !negative)
        {
                negative = spectrum[i] < 0;
                ++i;
        }
        if ( negative )
        {
                netIsSet(true);
                net(true);
        }
        else
        {
                if ( !isMultiple()) netIsSet(true);
        }
  }
  return OGIP_92aIO::isNet();
}

FakeDataInputRecord::Detectors OGIP_92aData::getResponseName (size_t rowNum) const
{
  FakeDataInputRecord::Detectors respIDs;
  const SpectralData* sd = 0;
  if (rowNum == 0)
  {
     sd = spectralData();
  }
  else
  {
     XSContainer::SpectralDataMapConstIt sdIt = multiSpectralData().find(rowNum);
     if (sdIt != multiSpectralData().end())
     {
        sd = sdIt->second;     
     }
  }
  if (!sd)
     throw RedAlert("Attempt to access non-existing spectrum in OGIP-9aData::getResponseName");

  const std::vector<Response*>& dets = sd->detector();
  for (size_t i=0; i<dets.size(); ++i)
  {
     const Response* rsp = dets[i];
     if (rsp)
     {
        string fName;
        if (const UserDummyResponse* udr = dynamic_cast<const UserDummyResponse*>(rsp))
        {
           fName = udr->usingChannels() ? USR_DUMMY_RSP : string("");
        }
        else
        {
           fName = (dynamic_cast<const RealResponse*>(rsp))->rmfName();
        } 
        if (fName.length())
        {
           FakeDataInputRecord::ResponseID respID(fName, i);
           respIDs.push_back(respID);
        }            
     }
  }

  return respIDs;
}

void OGIP_92aData::fakeitModRespHistory (CCfits::Table* tbl, const OutputInfo& info, size_t rowNum)
{
  using namespace std;
  // modelNames vector should either be size 0 or numSourcesForSpectra.
  // If modelName is filled in, we know there's a corresponding response,
  // either dummy or real.
  size_t nSources = info.modelNames().size();
  if (nSources > 999)
  {
     // Is this ever going to happen?  Doubtful.
     tcout <<"***Warning: There are too many model sources to record all of fakeit history."
        << std::endl;
     nSources = 999;
  }
  // There could be responses without associated models, which
  // should not be printed as part of history.
  // Judgement Call:  Which response to eventually use for RESPFILE?
  //   Let's use the first non-dummy that's also associated with 
  //   a model.  It's possible there's an unassociated response,
  //   but then it wouldn't have been part of the fakeit generation
  //   and so seems irrelevant.  Note that there also may be NO
  //   non-dummy associated responses.
  string forRespfile, forAncrfile;
  bool issueNameLengthWarning = false;
  for (size_t i=0; i<nSources; ++i)
  {
     if (info.modelNames()[i].length())
     {
        std::ostringstream keyNum;
        keyNum << setfill('0') << right << setw(3) << i+1;
        // Model keys only need to be written for the first spectrum in the set.
        if (rowNum == 0 || rowNum == 1)
        {
           string modKey = s_fkModRoot + keyNum.str();
           if (info.modelNames()[i].length() <= s_fkKeyValLen)
              tbl->addKey(modKey, info.modelNames()[i], string(""));
           else
           {
              // We're going to go behind CCfits' back to write long string
              // keywords.  This shouldn't cause problems though, particularly
              // since we aren't going to need to access these keys once they
              // are written.
              int status=0;
              fits_write_key_longwarn(tbl->fitsPointer(), &status);
              fits_write_key_longstr(tbl->fitsPointer(), const_cast<char*>(modKey.c_str()),
                     const_cast<char*>(info.modelNames()[i].c_str()), 0, &status);
           }
        }

        string respName = info.respNames()[i];
        string arfName = info.arfNames()[i];
        if (respName.length() <= s_fkKeyValLen &&
            arfName.length() <= s_fkKeyValLen)
        {
           if (forRespfile.empty() && respName != string("dummyrsp"))
           {
              forRespfile = respName;
              forAncrfile = arfName; // this may be empty
           }
        }
        else
        {
           if (respName.length() > s_fkKeyValLen)
           {
              // Only keep the first s_fkKeyValLen chars.  Do NOT
              // let this abbreviated name be stored by the RESPFILE
              // keyword since it will cause a file loading failure.
              respName = respName.substr(0,s_fkKeyValLen);
              issueNameLengthWarning = true;
           }
           if (arfName.length() > s_fkKeyValLen)
           {
              arfName = arfName.substr(0,s_fkKeyValLen);
              issueNameLengthWarning = true;
           }
        }

        string rspKey = s_fkRspRoot + keyNum.str();
        if (rowNum == 0)
        {
           // type 1 file
           tbl->addKey(rspKey, respName, string(""));
           if (arfName.length())
           {
              string arfKey = s_fkArfRoot + keyNum.str();
              tbl->addKey(arfKey, arfName, string(""));
           }
        }
        else
        {
           // type 2 file
           std::vector<string> names(1);
           // Could just simply write empty names, but then it appears in
           // table as a blank rather than "NULL".  So to give things
           // a more uniform appearance, only write if strings are not
           // empty.
           if (respName.length())
           {
             names[0] = respName;
             tbl->column(rspKey).write(names, rowNum);
           }
           if (arfName.length())
           {
              names[0] = arfName;
              string arfKey = s_fkArfRoot + keyNum.str();
              tbl->column(arfKey).write(names, rowNum);
           }
        }

     } // end if model 
  } // end sources loop
  if (rowNum == 0)
  {
     string respComment("associated redistrib matrix filename");
     string arfComment("associated ancillary response filename");
     int availCommentLen = std::max(0, (int)s_fkKeyValLen - (int)forRespfile.length() - 3);
     respComment = respComment.substr(0, static_cast<size_t>(availCommentLen));
     availCommentLen = std::max(0, (int)s_fkKeyValLen - (int)forAncrfile.length() - 3);
     arfComment = arfComment.substr(0, static_cast<size_t>(availCommentLen)); 
     tbl->addKey(RESPFILE(),forRespfile,respComment);
     tbl->addKey(ANCRFILE(),forAncrfile,arfComment);
  }
  else
  {
     std::vector<string> names(1,forRespfile);
     tbl->column(RESPFILE()).write(names, rowNum);
     names[0] = forAncrfile;
     tbl->column(ANCRFILE()).write(names, rowNum);
  }

  if (issueNameLengthWarning)
  {
     tcout << "***Warning: 1 or more RMF or ARF filenames exceeds " << s_fkKeyValLen << " chars.\n"
           << "   It will not be used for the fakeit file's RESPFILE or ANCRFILE entry,\n" 
           << "     and will appear abbreviated in the FKRSP or FKARF history listing." << std::endl; 
  }
}

void OGIP_92aData::collectRespArfNames (const std::vector<Response*>& detectors, std::vector<string>& respNames, std::vector<string>& arfNames)
{
  // Intended for gathering names of responses used in generating a fakeit
  // spectrum.
  const size_t nSources = detectors.size();
  respNames.resize(nSources);
  arfNames.resize(nSources);
  for (size_t i=0; i<nSources; ++i)
  {
     if (detectors[i])
     {
        const RealResponse* rr = detectors[i]->toRealResponse();
        // could be a dummy response
        if (rr)
        {
           respNames[i] = rr->rmfName();
           arfNames[i] = rr->arfName();
           if (rr->arfRow() != 0)
           {
              std::ostringstream tmp;
              tmp << arfNames[i] << "{" << rr->arfRow() << "}";
              arfNames[i] = tmp.str();
           }
        }
        else
           respNames[i] = "dummyrsp";
     }
  }
}

void OGIP_92aData::type2FakeitHistoryCols (CCfits::Table* tbl, size_t nSources)
{
   using namespace std;
   const size_t nS = min(nSources,static_cast<size_t>(999));
   for (size_t iSource=0; iSource<nS; ++iSource)
   {
      ostringstream keyNum;
      keyNum << setfill('0') << right << setw(3) << iSource+1;
      // Model names don't need a column since they aren't going to
      // vary by spectra.
      string colName = s_fkRspRoot + keyNum.str();
      tbl->addColumn(CCfits::Tstring, colName, static_cast<long>(s_fkKeyValLen));
      colName = s_fkArfRoot + keyNum.str();
      tbl->addColumn(CCfits::Tstring, colName, static_cast<long>(s_fkKeyValLen));
   }
}

// Additional Declarations
