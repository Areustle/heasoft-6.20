//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// DataSet
#include <XSModel/Data/DataSet.h>
// OGIP-92aBackground
#include <XSModel/Data/BackCorr/OGIP-92aBackground.h>
#include <CCfits/CCfits>
#include <iomanip>
#include <fstream>
#include <cassert>
#include "XSstreams.h"
#include "XSsymbol.h"
#include <unistd.h>
#include <cmath>
#include <sstream>


// Class OGIP_92aBackCorr 

OGIP_92aBackCorr::OGIP_92aBackCorr()
{
}

OGIP_92aBackCorr::OGIP_92aBackCorr(const OGIP_92aBackCorr &right)
  : BackCorr(right), OGIP_92aIO(right)
{
}


OGIP_92aBackCorr::~OGIP_92aBackCorr()
{
}


bool OGIP_92aBackCorr::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  return OGIP_92aIO::fileFormat(fileName,type);
}

OGIP_92aBackCorr* OGIP_92aBackCorr::clone () const
{

  return new OGIP_92aBackCorr(*this);
}

void OGIP_92aBackCorr::setDescription (size_t spectrumNumber)
{
  static const string PULSEHEIGHT("PHA");
  CCfits::ExtHDU& ext = dataSource()->extension(extensionName());

  SpectralData* Source = source()->sourceData(sourceRow());
  size_t row = data()->rowNumber();

  data()->spectrumNumber(spectrumNumber);  

  string keyval;
  float floatval;

  try
  {

        ext.keyWord(CHANNELTYPE()).value(keyval);
        // should  print warnings if there are mismatches.
        data()->channelType(keyval);
        data()->telescope(ext.keyWord(TELESCOPE()).value(keyval)); 
        data()->instrument(ext.keyWord(INSTRUMENT()).value(keyval)); 
  }
  catch (CCfits::FitsException&)
  {
        tcout  <<  "*** Warning: Channel Type, Telescope or Instrument Keyword not read "
                <<  " from Background/Correction file: using values from Spectrum File "
                << std::endl;
        data()->channelType(Source->channelType());
        data()->telescope(Source->telescope());
        data()->instrument(Source->instrument());
  }    


  const string fakeitPlaceholder("USE_FAKEIT_RMF");
  if (Source->channelType() != fakeitPlaceholder &&
        data()->channelType() != Source->channelType())
  {
        tcout << "***Warning: Type mismatch between spectrum and background/correction files "
              << " using type from spectrum file:" << Source->channelType() << std::endl;
        data()->channelType(Source->channelType());
  }

  if (Source->telescope() != fakeitPlaceholder &&
        data()->telescope() !=  Source->telescope())
  {
        tcout << "***Warning: Telescope name mismatch between spectrum and "
              << " background/correction files \n"
              << " Source: " << Source->telescope()
              << " Background/Correction " << keyval << std::endl;
  }

  if (Source->instrument() != fakeitPlaceholder &&
        data()->instrument() !=  Source->instrument())
  {
        tcout << "***Warning: Instrument name mismatch between spectrum and "
              << " background/correction files \n"
              << " Source: " << Source->instrument()
              << " Background/Correction " << keyval <<std::endl;
  }

  string attemptToRead(EXPOSURE());
  try
  {  
        std::vector<Real> floatValueFromCol;
        Real expTime = 1.;
        try
        {
             ext.column(EXPOSURE()).read(floatValueFromCol,row,row);
             expTime = floatValueFromCol[0];
        }
        catch ( CCfits::Table::NoSuchColumn )
        {
             expTime = ext.keyWord(EXPOSURE()).value(floatval);     
        }

        if (expTime <= 0)
        {
           std::ostringstream oss;           
           oss << "XSPEC will not load a background spectrum with exposure time <= 0.0: "
               <<  Source->backgroundFile();
           if (row)
              oss << "{" << row << "}";
           oss << "\n\n";
           throw YellowAlert(oss.str());
        }
        data()->exposureTime(expTime);
        attemptToRead = CORRSCALE();


        Real corrScale = 1.;
        try
        {
             ext.column(CORRSCALE()).read(floatValueFromCol,row,row);
             corrScale = floatValueFromCol[0];
        }
        catch ( CCfits::Table::NoSuchColumn )
        {
             corrScale = ext.keyWord(CORRSCALE()).value(floatval);
        }    
        // correctionScale can in principle be any real number, although 
        // uses with corrScale < 0 are unlikely. A user might want to turn
        // the correction off by editing corrScale to 0.

        data()->correctionScale(corrScale);
        attemptToRead = AREASCALE();

        int last = data()->endChan() - data()->startChan();
        int first = 0;
        int ungroupedChannels = last - first  + 1;
        try
        {       
                RealArray area;
                if ( row == 0 )
                {
                        ext.column(AREASCALE()).read(area,first,last);
                }
                else
                {
                        // type II file
                        CCfits::Column& col = ext.column(AREASCALE());
                        if (col.repeat() > 1)
                        {
                           col.read(area,row);
                        }
                        else
                        {
                           RealArray tmp;
                           col.read(tmp,row,row);
                           area.resize(ungroupedChannels,tmp[0]);
                        }

                }
                for (int j = 0; j < ungroupedChannels; ++j) 
                {

                        if (std::fabs(area[j]) <=   SMALL) area[j] = 1.; 
                } 
                data()->setAreaScale(area);
        }
        catch (CCfits::Table::NoSuchColumn)
        {
             Real area = 1.;
             area = ext.keyWord(AREASCALE()).value(floatval);     
             if (std::fabs(area) > SMALL ) 
             {
                     data()->setAreaScale(RealArray(area,ungroupedChannels));
             }
             else 
             {
                     data()->setAreaScale(RealArray(1.,ungroupedChannels));       
             }
             aScaleIsKeyword(true);
        }         
        attemptToRead = BACKSCALE();

        try
        {       
                RealArray bkgScale;
                if ( row == 0 )
                {
                        ext.column(BACKSCALE()).read(bkgScale,first,last);
                }
                else
                {
                        // type II file
                        CCfits::Column& col = ext.column(BACKSCALE());
                        if (col.repeat() > 1)
                        {
                           col.read(bkgScale,row);
                        }
                        else
                        {
                           RealArray tmp;
                           col.read(tmp,row,row);
                           bkgScale.resize(ungroupedChannels,tmp[0]);
                        }                        
                }
                bool badBscaleVal = false;                        
                for (int j = 0; j < ungroupedChannels; ++j) 
                {
                   if (bkgScale[j] == .0 && Source->qualityInfo()[j] != 1) 
                   {
                      badBscaleVal = true;
                      bkgScale[j] = 1.;
                   }
                }
                if (badBscaleVal)
                   tcout <<"***Warning: Background file " <<Source->backgroundFile()
                      <<" contains 1 or more good\n    quality channels with invalid BACKSCAL "
                      <<"value = 0.0.\n    XSPEC will instead use BACKSCAL = 1.0 for these channels."
                      <<std::endl; 
                data()->setBackgroundScale(bkgScale);
        }
        catch (CCfits::Table::NoSuchColumn)
        {
             Real bkgScale = 1.;
             bkgScale = ext.keyWord(BACKSCALE()).value(floatval);     
             if (bkgScale == 0.0 )
             {
                tcout <<"***Warning: Background file " << Source->backgroundFile()
                   <<" has invalid BACKSCAL value = .0, XSPEC will assume 1.0."
                   << std::endl;
                data()->setBackgroundScale(RealArray(1.,ungroupedChannels));       
             }
             else 
             {
                data()->setBackgroundScale(RealArray(bkgScale,ungroupedChannels));
             }
             bScaleIsKeyword(true);
        }  


  }
  catch (CCfits::FitsException&)
  {

        string msg = "Keyword ";
        msg += attemptToRead;
        msg += " is missing or of improper type in Background File: ";
        msg += dataSource()->name();
        throw XspecDataIO::RequiredDataNotPresent(msg);       
  }     

}

void OGIP_92aBackCorr::setArrays (size_t backgrndRow, bool correction)
{

  // prerequisite: source() != 0;

   data(new SpectralData(source(), source()->channels(sourceRow()), backgrndRow));

   int start (0);
   int last (0);
   size_t legalStart (0);
   size_t legalEnd (0);
   channelBounds(start, last, backgrndRow);
   size_t lowDefault = start ? 1 : 0;
   channelLimits(lowDefault, legalStart, legalEnd);
   if (start < (int)legalStart || last > (int)legalEnd)
   {
      const string& fileName = dataSource()->name();
      std::ostringstream msg;
      msg <<"BackCorr file " << fileName << " row " << backgrndRow
        << "\n   has channel numbers outside legal limits.\n";
      throw YellowAlert(msg.str());
   }
   // get all of the required data on rows, grouping, etc. from the 
   // DataSet object 
   SpectralData*  Source = source()->sourceData(sourceRow());
// Check that the number of BackCorr channels (prior to grouping
// or ignore/notice) matches the number in the owning spectral data.
   int parentLStart=0, parentLEnd=0;
   source()->legalChannelBounds(parentLStart, parentLEnd);
   if (((int)legalEnd-(int)legalStart) != (parentLEnd-parentLStart))
   {
      const string& fileName = dataSource()->name();
      std::ostringstream msg;
      msg <<"Background file " << fileName << " DETCHANS do not match data file.\n";
      throw YellowAlert(msg.str());
   }
   if ((start - legalStart) != (Source->startChan()-Source->firstChan()))
   {
      const string& fileName = dataSource()->name();
      std::ostringstream msg;
      msg <<"Channel numbering mismatch between data file and"
         <<"\n   background file " << fileName <<"\n";
      throw YellowAlert(msg.str());
   }
   if ((last-start+1) != (int)Source->qualityInfo().size())
   {
      const string& filename = dataSource()->name();
      std::ostringstream msg;
      msg <<'\n'<<"Wrong number of channels in BackCorr file: "
      		<<filename<<'\n';
      throw YellowAlert(msg.str());      
   } 

   data()->startChan(start);
   data()->endChan(last);
   data()->firstChan(legalStart);
   data()->setGroupingInfo(Source->groupingInfo());
   data()->setQualityInfo(Source->qualityInfo());
   data()->setNoticedChannels(Source->noticedChannels());

}

void OGIP_92aBackCorr::groupArrays (bool correction)
{
  using namespace CCfits;
  ExtHDU& ext = dataSource()->extension(extensionName());
  const Real ZERO (0);  
  int last(data()->endChan() - data()->startChan());
  int first(0);
  size_t row(data()->rowNumber());

  bool groupingSet  = source()->groupingSet(sourceRow());
  bool qualitySet   = source()->qualitySet(sourceRow());

  // these are the arrays to be modified.
  //RealArray& outputSpectrum    =  data()->spectrum();
  //IntegerArray& outputQuality  =  data()->quality();

  size_t ungroupedChannels = (size_t)(last - first + 1);
  size_t chans = data()->channels();

  // statistical arrays are ignored for correction files.

  // But IntegerArray is a std::vector (because we don't often do int calculations).
  // set quality vector to zeros (good everywhere).  
  IntegerArray outputQuality(chans,0);
  RealArray outputSpectrum(0.,chans);
  RealArray outputArea(1.,chans);
  RealArray outputBackground(1., chans);
  RealArray outputVariance(0.,chans);
  RealArray ungroupedArea(data()->areaScale());
  RealArray ungroupedBackground(data()->backgroundScale());
  Column* sourceArray = 0;

  // COUNTS or RATE Column. Grab a pointer to whichever is there.
  // defaults to RATE if someone is silly enough to put in both.

  if (isCounts())
  {
        sourceArray = &ext.column(COUNTS());       
  }
  else
  {
        sourceArray = &ext.column(RATE());                 
  }
  // note that up to this point no allocations have been done, only
  // reference-finding.


  RealArray src(0.,ungroupedChannels);

  if (row == 0)
  {
        sourceArray->read(src,first,last);
  }
  else
  {
        RealArray srcTmp;
        sourceArray->read(srcTmp,row);
        src = RealArray(srcTmp[std::slice(first,last-first+1,1)]);       
  }

  // Get statistical errors.  Patch fix to v12.2.0: to make compatible
  // w/ v11, Poisserr = true will override presence of STAT_ERR col.
  // Even if Poisserr = true, still check for STAT_ERR col and
  // issue warning if it exists.

  RealArray stat(0.0,ungroupedChannels);
  RealArray syst(0.0,ungroupedChannels);

  bool poissonStatistics   = false;
  bool poissonKey = false;
  bool systematicsPresent = false;
  if (!correction)
  {  
     try
     {
        ext.readKey(POISSERR(),poissonKey);
        if (poissonKey)
        {
           poissonStatistics = true;
        }        
     }
     catch (CCfits::FitsException&) 
     {
        tcerr << "***Warning: Background file " << POISSERR() 
           << " keyword is missing or of wrong format, assuming FALSE."
               <<std::endl;
     }       
     try 
     {
        Column& statCol = ext.column(STATISTICAL());
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
              stat = RealArray(statTmp[std::slice(first,last-first+1,1)]);       
           }
        }
        else
        {
           // Poisserr=T and STAT_ERR exists.  Issue warning.
           tcout << xsverbose(10) <<"***Warning: Background file "
                << dataSource()->name()
                << " has both POISSERR key set to 'true' and a "
                << STATISTICAL() << " column.\n"
                << "   XSPEC will use assume Poisson errors." 
                << std::endl << xsverbose();
        }

     }
     catch (Table::NoSuchColumn)
     {
        if (!poissonStatistics)
        {
           tcout << xsverbose(10)<<"\nWarning: Statistics not present and Poisson Error Key ";
           tcout << "not set to 'true': setting errors to zero"
           <<std::endl << xsverbose(); 
        }    
     }  

     try 
     {
        Column& sysCol = ext.column(SYSTEMATIC());
        systematicsPresent = true;
        if ( row == 0)
        {
           sysCol.read(syst,first,last);
        }
        else
        {
           RealArray systTmp;
           sysCol.read(systTmp,row);
           syst = systTmp[std::slice(first,last-first+1,1)];       
        }
     }
     catch (Table::NoSuchColumn) 
     {
        // absorb and continue
     }

  }
  else
  {
     // correction file.
     outputVariance.resize(0);
     data()->setVariance(outputVariance);
     data()->setRawVariance(outputVariance);        
  }

  if (!groupingSet)
  {
     // Case I: simplest case: no quality or grouping information.
     // This should be a little more efficient than setting qArray to 0
     // everywhere and executing Case II - which tactic is adopted for
     // the general case, III.
     if (!(correction || qualitySet))
     {
        outputSpectrum = src;    
        outputArea     = ungroupedArea; 
        outputBackground = ungroupedBackground;          
        if (poissonStatistics)
        {

           if (poissonKey)
           {
              for (size_t j = 0; j < ungroupedChannels; ++j)
                      outputVariance[j] = std::max(src[j],ZERO);
              if (!isCounts())
                   outputVariance /= data()->exposureTime();
           }
        }
        else    outputVariance = stat*stat;

        if (systematicsPresent)
        {
           outputVariance += (syst*src)*(syst*src);
        }
     }
     // Case II: no grouping information, but quality information.
     else 
     {
        if (qualitySet)
        {
           const IntegerArray& quality = source()->qualityInfo(sourceRow());


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
                 if (!correction)
                 {
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
                             outputVariance[j] /= data()->exposureTime();                          
                       }
                    }
                    if (systematicsPresent) 
                    {
                       outputVariance[j] += 
                               (src[k]*syst[k])*(src[k]*syst[k]);
                    }
                 }       
                 ++j;       
              }
              ++k;       
           }
        }
        if (correction) outputVariance.resize(0);
     }
     data()->setRawVariance(outputVariance);

  }
  else
  {
     // Case III General case: there is grouping information.
     // the complication arises primarily because of the statistics.
     // Since the correct thing to do seems to be to add channel
     // variances/systematics in quadrature the sums come out wrong unless 
     // this is executed in two stages.

     const IntegerArray& grouping = source()->groupingInfo(sourceRow());


     // in this case 
     // # channels =  # channels with GC=0 && # channels with GC = 1.
     //                              -  # of (quality = 1) entries.
     // channels with GC=1 followed by GC=-1 are summed until the next
     // GC = 1 channel is reached.
     if (!correction)
     {
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
                 src[l] >= 0 ? stat[l] = src[l]  : stat[l] = 0;
              }
              if (!isCounts())
                 stat /= data()->exposureTime();
           }
        }
     }

     size_t k = 0;
     int j = -1;
     Real binSum = 0.;
     Real binSumA = 0.; 
     Real binSumB = 0.; 
     Real invSumA = 0.;
     Real invSumB = 0.; 
     int nInGroup = 0;     
     // the code checks for the case binSumA, binSumB > 0. For backgrounds,
     // only by error in the file can these be violated.        

     if ( qualitySet )
     {
        const IntegerArray& quality  = source()->qualityInfo(sourceRow());
        // If there's a systematic fractional error, it's allowed to
        // vary per channel.  The group will use the maximum syst error
        // of all its channels.
        Real maxSyst = 0.;
        while (k < ungroupedChannels )
        {
           if ( quality[k] != 1)
           {
              if ( grouping[k] != -1 )
              {        
                 // "bad" - user ignorable data is GC =  0 (*)
                 // startbin channel is GC = 1 ('+')
                 // we already checked that grouping flag is (1,0,-1)
                 ++j;                                
                 outputQuality[j]  = quality[k];
                 if ( j > 0)
                 {
                    outputSpectrum[j-1] = binSum;
                    if (systematicsPresent && !correction)
                       outputVariance[j-1] += maxSyst*maxSyst*binSum*binSum;
                    outputArea[j-1] = (binSumA > 0) 
                       ? binSum/binSumA : nInGroup/invSumA;
                    outputBackground[j-1] = (binSumB > 0) 
                       ? binSum/binSumB : nInGroup/invSumB;
                 }
                 binSum  = src[k];
                 if (systematicsPresent && !correction)
                    maxSyst = syst[k];
                 invSumA = 1.0/ungroupedArea[k];
                 invSumB = 1.0/ungroupedBackground[k];
                 binSumA = binSum*invSumA;
                 binSumB = binSum*invSumB;
                 nInGroup = 1;
                 if (!correction) outputVariance[j] = stat[k];

              }
              else
              {
                 // GC = -1 ('-')
                 // new: if the quality array differs among grouped bins
                 // set the quality for that bin equal to the worst value
                 // found
                 outputQuality[j] = std::max(quality[k],outputQuality[j]);
                 binSum  += src[k];
                 if (systematicsPresent && !correction)
                 {
                    // Systematic error will be added to outputVariance
                    // only after group is completed.
                    maxSyst = std::max(maxSyst, syst[k]);
                 }
                 invSumA += 1.0/ungroupedArea[k];
                 invSumB += 1.0/ungroupedBackground[k];
                 binSumA += src[k]/ungroupedArea[k];
                 binSumB += src[k]/ungroupedBackground[k];
                 ++nInGroup;
                 if (!correction) outputVariance[j] += stat[k];       
              }        
           }
           // QC = 1 (GC = ' ')
           ++k;
           if (k == ungroupedChannels && j >= 0)
           {
              outputSpectrum[j] = binSum;
              if (systematicsPresent && !correction)
                 outputVariance[j] += maxSyst*maxSyst*binSum*binSum;
              outputArea[j] =  (binSumA > 0) 
                              ? binSum/binSumA : nInGroup/invSumA;
              outputBackground[j] =  (binSumB > 0) 
                              ? binSum/binSumB : nInGroup/invSumB;
           }

        } // end while
        data()->setQuality(outputQuality);

     } // end if quality set
     else
     {
        Real maxSyst = 0.;
        while (k < ungroupedChannels )
        {
           if ( grouping[k] != -1)
           {        
              // "bad" - user ignorable data is GC =  0 (*)
              // startbin channel is GC = 1 ('+')
              // we already checked that grouping flag is (1,0,-1)
              ++j;                                
              if ( j > 0)
              {
                   outputSpectrum[j-1] = binSum;
                   if (systematicsPresent && !correction)
                      outputVariance[j-1] += maxSyst*maxSyst*binSum*binSum;
                   outputArea[j-1] = (binSumA > 0) ? binSum/binSumA : 
                                              nInGroup/invSumA;
                   outputBackground[j-1] = (binSumB > 0) ? binSum/binSumB : 
                                              nInGroup/invSumB;
              }
              binSum  = src[k];
              invSumA = 1.0/ungroupedArea[k];
              invSumB = 1.0/ungroupedBackground[k];
              binSumA = binSum*invSumA;
              binSumB = binSum*invSumB;
              nInGroup = 1;
              if (!correction) outputVariance[j] = stat[k];
              if (systematicsPresent && !correction)
                 maxSyst = syst[k];
           }
           else
           {
              // GC = -1 ('-')
              binSum  += src[k];
              binSumA += src[k]/ungroupedArea[k];
              binSumB += src[k]/ungroupedBackground[k];
              invSumA += 1.0/ungroupedArea[k];
              invSumB += 1.0/ungroupedBackground[k];
              ++nInGroup;
              if (!correction) outputVariance[j] += stat[k];       
              if (systematicsPresent && !correction)
              {
                 // Systematic error will be added to outputVariance
                 // only after group is completed.
                 maxSyst = std::max(maxSyst, syst[k]);
              }
           }        
           ++k;
           if (k == ungroupedChannels)
           {
              outputSpectrum[j] = binSum;
              if (systematicsPresent && !correction)
                 outputVariance[j] += maxSyst*maxSyst*binSum*binSum;
              outputArea[j] =  (binSumA > 0) ? binSum/binSumA : 
                      nInGroup/invSumA;
              outputBackground[j] =  (binSumB > 0) ? binSum/binSumB :
                      nInGroup/invSumB;
           }

        } // end while

     }  // end if !quality set
     assert ( static_cast<size_t>(j) == chans - 1 );

  } // end case III     
  data()->setSpectrum(outputSpectrum);
  data()->setAreaScale(outputArea);
  data()->setBackgroundScale(outputBackground);
  if (!correction) data()->setRawVariance(outputVariance);
}

size_t OGIP_92aBackCorr::read (const string& fileName, bool readFlag)
{
  return OGIP_92aIO::read(fileName,readFlag);
}

void OGIP_92aBackCorr::closeSourceFiles ()
{
  OGIP_92aIO::closeFile();
}

void OGIP_92aBackCorr::initialize (DataSet* parentData, size_t row, const string& fileName, size_t backCorRow)
{
  source(parentData);
  sourceRow(row);

  runPath(XSutility::getRunPath());

  int numberOfSpectra = read(fileName);
  CCfits::ExtHDU& ext = dataSource()->extension(extensionName());

// Test #1, check that the BackCorr file is the type the user expects
   if (backCorRow == 0)
   {
     // The user expects file to be Type 1.  Is it?
     if (specNum(ext))
     {
        string msg = fileName + '\n';
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
         string msg = '\n'+ fileName;
	 msg += " is detected to be of Type 1 format.  ";
	 msg += "Row specifiers do not apply.\n";
	 throw XspecDataIO::SingleSpectrumOnly(msg);
      }
// Test #2, for type2 file, check that the user specified row actually
//  exists in the file.
      if (backCorRow > (size_t)numberOfSpectra)
      {
         std::ostringstream msg;
	 msg << "Row number " << backCorRow << "is out of range of\n"
	 	<< "file:  " << fileName << '\n';
	 throw YellowAlert(msg.str());
      }     
   }
}

// Additional Declarations
