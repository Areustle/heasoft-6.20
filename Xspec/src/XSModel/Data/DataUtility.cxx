//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include "DataFactory/OGIP-92aIO.h"

// SpectralData
#include <XSModel/Data/SpectralData.h>
// Response
#include <XSModel/Data/Detector/Response.h>
// DataSet
#include <XSModel/Data/DataSet.h>
// Background
#include <XSModel/Data/BackCorr/Background.h>
// DataUtility
#include <XSModel/Data/DataUtility.h>

#include "Parse/XSparse.h"
#include "Data/Detector/ResponseMatrix.h"
#include "xsTypes.h"
#include "XSstreams.h"
#include <CCfits/CCfits>
#include "Utils/XSstream.h"
#include "DataFactory/XspecRegistry.h"
#include "XSContainer.h"
#include "XSsymbol.h"


// Class Utility DataUtility 
XSContainer::Weight* DataUtility::s_weightScheme;


void DataUtility::readArrays (ResponseMatrix& rmf, const CCfits::ExtHDU& fitsData)
{
  using namespace std;
  size_t  nE = fitsData.rows();
  static const size_t ONE(1); // kill boring compiler warnings;

  string stringval("");

  //fitsData.readKey(OGIP_92aIO::TELESCOPE(),stringval);

  try {  
     rmf.telescope(fitsData.keyWord(OGIP_92aIO::TELESCOPE()).value(stringval));
  } catch (...) {
    tcout << xsverbose(25)<< "Failed to read TELESCOPE keyword" <<endl<<xsverbose();
    throw;
  }

  //rmf.telescope(stringval);
  //fitsData.readKey(OGIP_92aIO::INSTRUMENT(),stringval);
  //rmf.instrument(stringval);
  //fitsData.readKey(OGIP_92aIO::CHANNELTYPE(),stringval);
  //rmf.channelType(stringval == "PHA" ? PHA : PI);


  // Load the energy ranges and grouping columns into RMF object
  try {
     fitsData.column(OGIP_92aIO::ENERGYLO()).read(rmf.energyLow(), ONE, nE);
  } catch(...) {
    tcout << xsverbose(25)<<"Failed to read ENERGYLO column"<<endl<<xsverbose();
    throw;
  }
  try {
     fitsData.column(OGIP_92aIO::ENERGYHI()).read(rmf.energyHigh(), ONE, nE);
  } catch(...) {
    tcout << xsverbose(25)<<"Failed to read ENERGYHI column"<<endl<<xsverbose();
    throw;
  }
  try {
     fitsData.column(OGIP_92aIO::NGROUP()).read(rmf.binResponseGroups(), ONE, nE);
  } catch(...) {
    tcout << xsverbose(25)<<"Failed to read NGROUP column"<<endl<<xsverbose();
    throw;
  }

  // Prevent energy = 0 (or neg).
  RealArray& eLow = rmf.energyLow();
  RealArray& eHigh = rmf.energyHigh();
  bool smallEWarn = false;
  for (size_t i=0; i<nE; ++i)
  {
     if (eHigh[i] < SMALL)
     {
        eHigh[i] = SMALL;
        smallEWarn = true;
     }
     if (eLow[i] < SMALL)
     {  
        eLow[i] = std::max(SMALL, eHigh[i]/1000.0);
        smallEWarn = true;
     }
  }
  if (smallEWarn)
  {
     tcout <<"***Warning: Detected response matrix energy bin value = 0 (or neg)." 
         << "\n     XSPEC will instead use small finite value (response file will not be altered)."
         << std::endl;
  }
  // Store the RMF matrix channel offset, given by the TLMIN keyword.
  CCfits::Column& channelColumn = fitsData.column(OGIP_92aIO::FCHANNEL());
  size_t channelNo = channelColumn.index();
  std::ostringstream chanKey;
  chanKey << "TLMIN" << channelNo;
  int offset(0);
  try
  {
     const_cast<CCfits::ExtHDU*>(&fitsData)->readKey(chanKey.str(),offset);
  }
  catch (CCfits::HDU::NoSuchKeyword)
  {
     tcout <<"***Warning: No TLMIN keyword value for response matrix FCHAN column."
         <<"\n            Will assume TLMIN = 1."<<std::endl;
     offset = 1;
  }

  try   // loading F_CHAN as vector column
  {
     std::vector<std::valarray<int> > fchan;
     fitsData.column(OGIP_92aIO::FCHANNEL()).readArrays(fchan, ONE, nE);
     MatrixIndex& binStart = rmf.binStart();

     binStart.resize(nE);

     for (size_t i=0; i<nE; i++)
     {
        size_t  numElements = fchan[i].size();
	binStart[i].resize(numElements);

	// This loop subtracts offset from the fchan values, as read from the 
	// .rmf files, so that the binStart values are always zero based.
	for (size_t k=0; k<numElements; k++)
	{
	   binStart[i][k] = fchan[i][k] - offset;
	}
     }
  }

  catch (CCfits::Column::WrongColumnType)   // try loading it as scalar column
  {
     IntegerArray  fchan;
     fitsData.column(OGIP_92aIO::FCHANNEL()).read(fchan, 1, nE);
     MatrixIndex&  binStart = rmf.binStart();
     size_t  numElements = fchan.size();
     binStart.resize(numElements);

     for (size_t j = 0; j < numElements; ++j)
     {
        // Again, subtract offset from fchan, for reason given above.
        binStart[j]  = IntegerArray(1,fchan[j] - offset);
     }
  }

  catch ( ... )
  {
     tcout << xsverbose(25)<<"Unexpected failure loading F_CHAN"<<endl<<xsverbose();
     throw;
  }

  try   // loading N_CHAN as vector column
  {
     std::vector<std::valarray<int> > nchan;
     fitsData.column(OGIP_92aIO::NCHANNEL()).readArrays(nchan, 1, nE);
     MatrixIndex&  binRunLengths = rmf.binRunLengths();

     binRunLengths.resize(nE);

     for (size_t i=0; i<nE; i++)
     {
        size_t  numElements = nchan[i].size();
	binRunLengths[i].resize(numElements);
	std::copy(&nchan[i][0], &nchan[i][0]+numElements, binRunLengths[i].begin());
     }
  }

  catch (CCfits::Column::WrongColumnType)   // try loading it as scalar column
  {
     IntegerArray  nchan;
     fitsData.column(OGIP_92aIO::NCHANNEL()).read(nchan, 1, nE);
     MatrixIndex&  binRunLengths = rmf.binRunLengths();
     size_t  numElements = nchan.size();
     binRunLengths.resize(numElements);
     for (size_t j = 0; j < numElements; ++j)
     {
        binRunLengths[j]  = IntegerArray(1,nchan[j]);
     }
  }

  catch ( ... )
  {
     tcout <<xsverbose(25)<< "Unexpected failure loading N_CHAN"<<endl<<xsverbose();
     throw;
  }

  // read MATRIX
  try 
  {
     fitsData.column(OGIP_92aIO::MATRIX()).readArrays(rmf.matrixValue(), ONE, nE);
  } 
  catch (CCfits::FitsException&)
  {
     try
     {
        // 2nd attempt, perhaps this is a scalar Matrix column.
        std::vector<Real> tmpScalarCol;
        fitsData.column(OGIP_92aIO::MATRIX()).read(tmpScalarCol, ONE, nE);
        MatrixValue& rmfVals = rmf.matrixValue();
        rmfVals.clear();
        size_t sz = tmpScalarCol.size();
        for (size_t i=0; i<sz; ++i)
        {
           RealArray tmpElem(tmpScalarCol[i], 1);
           rmfVals.push_back(tmpElem);
        }
     }
     catch (CCfits::FitsException&) 
     {
        tcout <<xsverbose(25)<< "Failed to read response element array"
           << endl <<xsverbose();
        throw;
     }
  }

  rmf.normalizeRMF();
}

void DataUtility::groupArrays (ResponseMatrix& rmf, const size_t startChan, const size_t endChan)
{
  IntegerArray grouping;
  IntegerArray quality;  

  rmf.decodeGQ(grouping, quality);
  int ungroupedSize = endChan - startChan + 1;

  XSutility::auto_array_ptr<int> apChan2bin(new int[ungroupedSize]);
  int* chan2bin(apChan2bin.get());
  // chan2bin is maps chans (which may be offset by startChan) to
  // 0-based post-grouping bin number.  Its size should
  // always be ungroupedSize, which we'll assume is non-zero by
  // this point.
  int firstGood = 0;
  while (firstGood < ungroupedSize)
  {
     if (quality[firstGood] != 1) break;
     chan2bin[firstGood] = 0;
     ++firstGood;
  }
  if (firstGood == ungroupedSize) throw YellowAlert("All channels marked bad quality, cannot group RMF values.\n"); 
  int currBin = -1;
  for (int i=firstGood; i<ungroupedSize; ++i)
  {
     if (grouping[i] != -1 && quality[i] != 1)
     {
        ++currBin;
     }
     // currBin should never be -1 at this point.  If it is,
     // then something during patchGrouping func went wrong.
     if (currBin < 0) 
     {
        string msg("Patch grouping error detected in DataUtility::groupArrays");
        throw RedAlert(msg);
     }
     chan2bin[i] = currBin;
  } 
  int nGroupedBins = currBin + 1;

  int nE = rmf.energyLow().size();
  XSutility::auto_array_ptr<Real> apGroupedRMFrow(new Real[nGroupedBins]);
  Real* groupedRMFrow(apGroupedRMFrow.get());
  MatrixIndex& allFChans = rmf.binStart();
  MatrixIndex& allNChans = rmf.binRunLengths();
  MatrixValue& allRmfVals = rmf.matrixValue();
  for (int iRow=0; iRow<nE; ++iRow)
  {
     int nGrp = rmf.binResponseGroups(iRow);
     if (nGrp)
     {
        IntegerArray& fChan = allFChans[iRow];
        IntegerArray& nChan = allNChans[iRow];
        RealArray& rmfVals = allRmfVals[iRow];
        // We know that grouping is never going to INCREASE the
        // channel numbers corresponding to non-zero matrix elements.
        // Therefore to save time, only bother to initialize the
        // groupedRMFrow elements that might possibly be used.
        int endWorkspace = std::max(fChan[nGrp-1] + nChan[nGrp-1] - (int)startChan, 0);
        endWorkspace = std::min(endWorkspace, nGroupedBins);
        // initial guess for startWorkspace:
        int startWorkspace = std::max(fChan[0] - (int)startChan, 0);
        // Now modify it due to grouping:
        if (startWorkspace >= ungroupedSize)
        {
           // No need for any groupedRMFrow workspace:
           startWorkspace = endWorkspace;
        }
        else
        {
           startWorkspace = chan2bin[startWorkspace];
        }
        for (int i=startWorkspace; i<endWorkspace; ++i) groupedRMFrow[i] = .0;
        // We also know that whatever funny stuff grouping might
        // do, increasing the total number of non-zero channels
        // isn't going to be one of 'em. 
        int sumOrgNChans = 0;
        for (int i=0; i<nGrp; ++i) sumOrgNChans += nChan[i];
        IntegerArray newElemIndices;
        newElemIndices.reserve(sumOrgNChans+1);
        // Make initial entry a dummy, simply so we know size
        // is always at least 1 and therefore don't have to
        // check in inner for loop.
        newElemIndices.push_back(-1);

        int iCompressed = 0;
        bool beyondSpec = false;
        for (int iGrp=0; iGrp<nGrp && !beyondSpec; ++iGrp)
        {
           int fChan_i = fChan[iGrp];
           int nChan_i = nChan[iGrp];
           for (int j=0; j<nChan_i; ++j)
           {
              int zeroBasedChan = fChan_i + j - startChan;
              if (zeroBasedChan >= ungroupedSize)
              {
                 beyondSpec = true;
              }
              else if (zeroBasedChan >= 0)
              {
                 if (quality[zeroBasedChan] != 1)
                 {
                    int newIndex = chan2bin[zeroBasedChan];
                    groupedRMFrow[newIndex] += rmfVals[iCompressed];
                    // newIndex is always being accessed in
                    // increasing order.
                    if (newIndex != newElemIndices.back())
                       newElemIndices.push_back(newIndex);
                 }
                 ++iCompressed;
              }
              else
              {
                 ++iCompressed;
              }
           }
        }

        size_t  ngroups=0;
        fChan.clear();
        nChan.clear();
        size_t  rowSize = newElemIndices.size() - 1;
        rmfVals.resize(rowSize);

        int prevChan = -999;
        int chanCount = 0;
        for (size_t i=1; i<=rowSize; ++i)
        {
           int currChan = newElemIndices[i];
           if (currChan != prevChan+1)
           {
              // start of new group, possibly the end
              // of a previous one.
              if (ngroups)  nChan.push_back(chanCount);
              ngroups++;
              fChan.push_back(currChan);
              chanCount = 0;              
           }
           rmfVals[i-1] = groupedRMFrow[currChan];
           prevChan = currChan;
           ++chanCount;
        }
        // Fill in nChan for last group (if there are any).
        if (chanCount) nChan.push_back(chanCount);

        rmf.binResponseGroups(iRow,ngroups);
     } // end if nGrp
  } // end nE loop

  rmf.createConvolutionArray();
}

void DataUtility::setSource (SpectralData* spectrum, Response* response, size_t sourceNum)
{
  response->source(spectrum);
  // there's always at lead one 'slot' in the detector array.
  // it's the job of the calling routine to ensure an index position
  // exists.
  spectrum->attachDetector(response,sourceNum);
}

const CodeContainer& DataUtility::encodeGQ (const IntegerArray& group, const IntegerArray& quality, CodeContainer& coded)
{
   encode(group, coded);
   coded.push_back(static_cast<CodeField>(7));
   encode(quality, coded);

   return coded;
}

void DataUtility::decodeGQ (const CodeContainer& coded, IntegerArray& group, IntegerArray& quality)
{
   const CodeField delim = 7;
   CodeContainer::const_iterator boundary = 
        std::find(coded.begin(), coded.end(), delim);

   CodeContainer gString(coded.begin(), boundary);
   decode(gString, group);
   if (boundary != coded.end())
   {
      ++boundary;
      CodeContainer qString(boundary, coded.end());
      decode(qString, quality);
   }
}

void DataUtility::encode (const IntegerArray& inputArray, CodeContainer& coded)
{

  bitCard b;

  size_t  nvalues = inputArray.size();
  const CodeField MAXNTIMES = 268435456; // = 2^28
  size_t  i=1;

  // Patch fix for unusual case of only 1 channel.
  if (nvalues == 1)
  {
     // inputArray[0] can't be -1.  It would have been corrected by now.
     coded.push_back((1 << 3) + inputArray[0]);
     return;
  }  

  while (i < nvalues)
  {
     CodeField  n=1;
     while ((i < nvalues) && (inputArray[i] == inputArray[i-1])) ++i, ++n;
     if (n > MAXNTIMES || n<0)
     {
        throw YellowAlert("Cannot process qual/group columnn: Too many equivalent consecutive values.");
     }
     b.ntimes = n;
     b.flag = ((inputArray[i-1]==-1) ? 6 : inputArray[i-1]);
     // b.flag setting has already been verified to be within
     // correct limits.
     coded.push_back((b.ntimes << 3) + b.flag);

     if (i == nvalues-1)
     {
        n = 1;

	b.ntimes = n;
	b.flag = ((inputArray[i]==-1) ? 6 : inputArray[i]);
	coded.push_back((b.ntimes << 3) + b.flag);
     }

     ++i;
  }
}

void DataUtility::decode (const CodeContainer&  coded, IntegerArray& inputArray)
{
  size_t sz = coded.size();

  for (size_t  i=0; i<sz; ++i)
  {
     CodeField  w = coded[i];

     CodeField num = w >> 3;
     int  j = static_cast<int>(w & 7);
     j = ((j == 6) ? -1 : j);

     std::fill_n(back_inserter(inputArray), num, j);
  }
}

void DataUtility::fixSequence (DataUtility::recordList& records, size_t spectraDefined)
{
        // we need to check the following:
        // 1) no dataset defined by the input command overwrites the spectrum
        //    numbers of an earlier dataset referenced on the line (the recordList
        //    has been stable-sorted by the time this is called, so we can simply
        //    iterate once through the list.
        // 2) the largest spectrum number referenced, or the first of 
        //    a set of spectrum numbers referenced is no larger than spectraDefined+1,
        //    where spectraDefined is the number of spectra defined prior to the
        //    current command.
        // 3) [New Rule in XSPEC12] data group number may not exceed spectrum number
        //    for any spectrum. If additionally datagroups are renumbered with spectra,
        //    this simple rule ensures that there are no datagroups with no spectra
        //    assigned to them, and thus no models with dangling parameters.

        // it's possible to get here with an empty input record, for example
        // if the user was prompted for replacement input and typed "none".

   fixOverlap(records);
   fixSpectrumGaps(records, spectraDefined);
   fixDataGroupsVsSpecNum(records);
}

void DataUtility::makeInputRecords (size_t firstSpectrumNumber, size_t groupNumber, const string& inputString, recordList& inputRecordList, XSutility::fileType defaultSuffix)
{

  // function for parsing filename specification with range

  string  inString(inputString);

  // range is the row number of spectra in a vector Data file, e.g. OGIP-II

  IntegerArray range;
  string fName("");

  fName = XSparse::returnDelimitedArgument(inString, "{");
  inString = XSparse::returnDelimitedArgument(inString, "}");

  // fName now contains the filename and inString the contents of the {}

  const string rangeChars = "0123456789-*,";
  if ( inString.find_first_not_of(rangeChars) == string::npos ) {

    string strSingleRange;
    int begin, end;

    static int sLastLeft = -2, sLastRight = -2;

    while(!(strSingleRange = 
	  XSparse::returnDelimitedArgument(inString, ",")).empty()) {

      XSparse::oneRange(strSingleRange, begin, end);

      begin = (begin == -1 ? sLastLeft : begin);
      end   = (end == -1 ? sLastRight : end);
      range.push_back(begin);
      range.push_back(end);
      sLastLeft = begin;
      sLastRight = end;
    }

  } else {

    // if the {} does not contain a range then attach its contents to the
    // filename within []

    fName += "[" + inString + "]";

  }

  //other functions expect this
  if(range.size() == 0) range = IntegerArray(1, 0);

  if (!(XSutility::lowerCase(fName) == XSparse::NONE() && range[0] == 0))
    fName = XSutility::addSuffix(fName, defaultSuffix);

  //inString = XSparse::processStringToken(inString,fName,range, defaultSuffix);
#ifndef STD_COUNT_DEFECT
  //was counting -1's, not sure why
  int wild(std::count(range.begin(),range.end(),-2));
#else
  int wild(0);
  std::count(range.begin(),range.end(),-2,wild);
#endif
  IntegerArray spectraNumbers(1,firstSpectrumNumber);
  //Call XSparse::expandRange if no wild card values were found. Otherwise,
  //delay calling this until DataInputRecord::updateSpectrumCounts, when we
  //know the total number of spectra
  if (!wild) {
    range = XSparse::expandRange(range);
    
    size_t N(range.size());
    spectraNumbers.resize(N);

    for (size_t j = 1; j < N; ++j ) 
      spectraNumbers[j] = firstSpectrumNumber + j;
  }
  inputRecordList.push_back(DataInputRecord(fName,spectraNumbers,groupNumber,range));
}

DataUtility::recordList DataUtility::parseDataCommandString (StringArray& args, XSutility::fileType defaultSuffix)
{

    int first = -1;
    int second = -1;
    size_t spectrumNumber = 1;
    size_t groupNumber = 1;
    size_t saveGroupNumber = 1;
    size_t trailingCommas = 0; 
    enum entryType {comma, name, intPair} previous = comma;
    const string WS(" \t");

    recordList inputRecords;

    // CheckBrackets will make sure brackets are properly paired off,
    // and will remove any unnecessary spaces inside the brackets.  
    // In doing so, it could end up modifying the size of args.  It also
    // prevents nested brackets (of the same type only).
    XSparse::checkBrackets(args,'{');
    XSparse::checkBrackets(args,'[');
    size_t N(args.size());  

    for (size_t iArg=1; iArg<N; ++iArg)
    {
       const string& curArg = args[iArg];
       const string::size_type argLength = curArg.length();
       // Could have any empty string or only whitespace if user entered
       // "" or "  ".  
       if (!argLength || curArg.find_first_not_of(" \t") == string::npos)
          continue;

       bool foundCurl = false;
       bool foundSquare = false;
       bool betweenCurl = false;
       bool betweenSquare = false;
       bool startOfRecord = true;
       bool endOfRecord = false;
       string commaDelimitedStr;
       string::size_type iPos=0;   
       while (iPos < argLength)
       {
          // Analyze each comma-delimited substring of curArg separately.
          // However there's a complication: commas enclosed within
          // brackets ("{}" or "[]") must NOT be considered delimiters.
          char curChar = curArg[iPos];
          bool addToStr = true;
          switch (curChar)
          {
             case '{':
                if (foundCurl)
                   throw XSparse::SyntaxError("Multiple '{}' brackets not allowed.");
                if (startOfRecord)
                   throw XSparse::SyntaxError("Cannot begin an entry with '{'");
                if (betweenSquare)
                   throw XSparse::SyntaxError("Nesting of brackets not allowed.");
                foundCurl = true;
                betweenCurl = true;
                break;
             case '}':
                betweenCurl = false;
                break;
             case '[':
                if (foundSquare)
                   throw XSparse::SyntaxError("Multiple '[]' brackets not allowed.");
                if (startOfRecord)
                   throw XSparse::SyntaxError("Cannot begin an entry with '['");
                if (foundCurl)
                   throw XSparse::SyntaxError("'[]' brackets may not follow '{}' brackets.");
                foundSquare = true;
                betweenSquare = true;
                break;
             case ']':
                betweenSquare = false;
                break;
             case ',':
                if (!betweenCurl && !betweenSquare)
                {
                   // We found the un-enclosed comma delimiter.
                   addToStr = false;
                   endOfRecord = true;
                }
                break;
             default:
                break;
          }
          startOfRecord = false;

          if (addToStr)
             commaDelimitedStr += curChar;

          // If delimiting comma was found, endOfRecord is already set
          // to true.
          if (iPos+1 == argLength)
             endOfRecord = true;

          if (endOfRecord)
          {
             // Comma-delimited entry is now complete (though it may be
             // empty) and ready for processing.
             if (commaDelimitedStr.find_first_not_of(WS) == string::npos)
             {
                // processing just a comma
                if (previous == comma)
                {
                   ++spectrumNumber;
                   ++trailingCommas;
                }
                else
                {
                   // comma follows a name or intPair
                   ++trailingCommas;
                }
                previous = comma;
             }
             else
             {
	        if (XSparse::integerPair(commaDelimitedStr,first,second))
	        {
	           if (previous == intPair)
	           {
		      string msg("\n***Warning:  Int or int-pair is missing a file name");
		      msg += "\n        It will be skipped.";
		      tcout << msg <<std::endl;
		      // Case of "n n", the previous intPair is now invalidated.
		      groupNumber = saveGroupNumber;
	           }
	           saveGroupNumber = groupNumber;
	           spectrumNumber = first;
	           if (second != -1)
	           {
		      spectrumNumber = second;
		      groupNumber = first;
	           }
                   if (groupNumber == 0 || spectrumNumber == 0)
                   {
                      throw YellowAlert("0 is not a valid spectrum or data group number.\n");
                   }
	           previous = intPair;
	        }
	        else
	        {
	           // Assume we are dealing with fileName{..} or "none"
                   // and that there may be a trailing comma.
                   if (addToStr)
                   {
                      previous = name;
                      trailingCommas = 0;
                   }
                   else
                   {
		      previous = comma;
		      trailingCommas = 1;
                   }
      	           // There should be no commas in the string sent to this 
                   // function, except inside of brackets.
	           makeInputRecords(spectrumNumber, groupNumber, 
                                   commaDelimitedStr, inputRecords, defaultSuffix);
	           ++spectrumNumber;
	        }
             } // end if commaDelimitedStr is not blank

             // reset things for next entry in arg
             commaDelimitedStr.clear();
             foundCurl = false;
             foundSquare = false;
             betweenCurl = false;
             betweenSquare = false;
             startOfRecord = true;
             endOfRecord = false;
          } // end processing of commaDelimitedStr
          ++iPos;

       } // end while loop processing a single string.             
    } // end for loop processing all input arguments.

    size_t nRecs = inputRecords.size();
    if (nRecs)
    {
       int backNum = inputRecords.back().spectrumNumber(0);       
       // DataInputRecord objects are ordered by spectrum number.
       inputRecords.sort();
       // Deal with trailing commas.  If just 1 trailing comma, don't do 
       // anything.
       if (trailingCommas > 1)
       {
	  // After the sort, does the last record still have the same
	  // spectrum number? (This isn't fool-proof, but it's close.)
	  recordListConstIter rCheck = inputRecords.end();
	  --rCheck;
          if (backNum != rCheck->spectrumNumber(0))
	  {
	     string msg("\n***Warning:  The trailing commas do not belong to the set");
	     msg += "\n       with the highest spectrum number.  They will be ignored.";
	     tcout << msg <<std::endl;
	  }
	  else if (nRecs > 1 && (--rCheck)->spectrumNumber(0) == backNum)
	  {
	     string msg("\n***Warning:  Multiple sets assigned highest spectrum number");
	     msg += "\n       trailing commas will be ignored.";
	     tcout << msg <<std::endl;	     
	  }
	  else
	  {
	     // Add to last record .
	     inputRecords.back().numTrailCommas(trailingCommas);
	  }
       }  

    }
    return inputRecords;  
}

size_t DataUtility::dataToProcess (const recordList& records)
{

  // compute total number of data sets referred to by command line.
  // as of 9/2001 this function is not called anywhere, but we are keeping
  // as it might prove useful someday.      

  size_t total(0);

  if (records.size() == 0) return total;

  recordListConstIter rl = records.begin();
  recordListConstIter endRl = records.end();

  while (rl != endRl)
  {
        total += rl->spectrumNumber().size();       
        ++rl;
  }
  return total; 
}

const XSContainer::Weight& DataUtility::statWeight ()
{
      if (s_weightScheme) 
      {
                return *s_weightScheme;       
      }
      else 
      {
                throw RedAlert("Global weight scheme not set");       
      }
}

void DataUtility::fixOverlap (DataUtility::recordList& records)
{

  // Fix any errors from the user input to the data command of the kind
  // where one record would overwrite the spectra of another.  For
  // type 1, this is simply cases such as:
  //   >data 1 file1 2 file2 1 file3
  // For type 2, this could also be cases like:
  //   >data 1 file1{3-6} 3 file2{7-9}
  // (ie. specNums 3 and 4 from the second record overwrite the last 2
  // of the first record).   The correction is made by renumbering the
  // spectra of the offending record starting at 1 above the previously
  // highest spectrum number.

  // This works on the assumption that by the time this is called, the
  // records will be in sorted order (sorted by the first element in
  // the spectrumNumber array).

  recordListIter rl = records.begin();
  recordListIter rlEnd = records.end();
  size_t previousHigh = 0;
  if (!records.empty())
  {
     previousHigh = rl->spectrumNumber(rl->spectrumNumber().size()-1);
     ++rl;
  }
  while (rl != rlEnd)
  {
     size_t startSpec = rl->spectrumNumber(0);
     if (startSpec <= previousHigh)
     {
        tcout << "\n*** Warning: coincident spectrum numbers requested: file ";
	tcout << rl->fileName() << std::endl;
	tcout << "*** Correcting from " << startSpec << " to "
	      << previousHigh+1 <<std::endl;
	rl->renumber(previousHigh+1);
     }
     previousHigh = rl->spectrumNumber(rl->spectrumNumber().size()-1);
     ++rl;
  }

}

void DataUtility::fixSpectrumGaps (DataUtility::recordList& records, size_t spectraDefined)
{

  // This function will ensure that 1) the first record's first spectrum
  // begins at a number <= spectraDefined+1, and 2) each following record's
  // first spectrum begins at a number <= spectraDefined+1 or the maximum
  // spectrum number from the input list so far + 1, whichever is the larger.
  // At this point, it DOES NOT take into account the result of entering
  // "none" in any of the records.
   if (records.empty())  return;

   size_t maxSpectra = spectraDefined + 1;
   recordListIter rl = records.begin();
   recordListIter rlEnd = records.end();
   while (rl != rlEnd)
   {
      size_t currentNumber = rl->spectrumNumber(0);
      if (currentNumber > maxSpectra)
      {
         tcout << "\n***Warning: invalid input spectrum number:";
	 tcout << "\n            Correcting from " << currentNumber <<
	       " to " << maxSpectra <<std::endl;
	 rl->renumber(maxSpectra);
      }
      size_t rlHigh = rl->spectrumNumber(rl->spectrumNumber().size()-1);
      maxSpectra = std::max(rlHigh+1, maxSpectra);
      ++rl;
   }
}

void DataUtility::fixDataGroupsVsSpecNum (DataUtility::recordList& records)
{

  // This will correct for the case where an input record contains
  // a data group number higher than its spectrum number.  It only
  // looks for the obvious cases -- it does not check for entries of
  // "none".  It presumes the effects of a "none" entry will be 
  // corrected for at some later stage.

  if (records.empty()) return;

  recordListIter rl = records.begin();
  recordListIter rlEnd = records.end();
  while (rl != rlEnd)
  {
     if (rl->groupNumber() > (size_t)rl->spectrumNumber(0))
     {
        tcout << "\n***Warning:  Data group number for a spectrum in file "
	      << rl->fileName()
	      << "\n    must be no greater than the spectrum number: "
	      << "correcting " << rl->groupNumber() << " to " 
	      << rl->spectrumNumber(0) <<std::endl;
        size_t newGroupNum = static_cast<size_t>(rl->spectrumNumber(0));

        // Can still get in here with a "none" entry, in which case the
        // record will not have a dataset pointer.
        if (rl->data())
           rl->data()->dataGroup(newGroupNum);
     }
     ++rl;
  }
}

bool DataUtility::searchRecordListSpecNums (const recordList& records, const size_t specNum, recordListConstIter& currRec, size_t& index)
{

  // This is designed for use by xsFakeit.  Given a specNum, it will search
  // a recordList starting at the input currRec and determine if any of the
  // records contain a spectrum number == specNum. If found, it returns 
  // "true", currRec will now point the record containing the spectrum, and
  // index will tell the position of that spectrum within the spectrumNumber 
  // array .

  // Assumptions:  The input recordList is in order sorted by each record's
  // spectrumNumber(0) value, and within each record the specNums are 
  // consecutive and increasing.  Though, there may be gaps in spectrum number 
  // between the records.
  const recordListConstIter rlEnd = records.end();
  bool isFound = false;
  index = 0;
  while (currRec != rlEnd)
  {
     size_t low = currRec->spectrumNumber(0);
     size_t high = currRec->spectrumNumber().back();
     if (specNum < low)
     {
        return isFound;
     }
     else if (specNum <= high)
     {
        index = specNum - low;
	isFound = true;
	break;
     }
     ++currRec;
  }
  return isFound;
}

DataPrototype* DataUtility::prototypeFromResponse (string& responseName)
{
  DataPrototype *proto=0;
  bool isOk = false;
  try
  {
     proto = XSContainer::xsRegistry->returnPrototype(responseName, 
     				XspecDataIO::ResponseType);
  }
  catch (YellowAlert& excep)
  {
     string errMsg = dynamic_cast<XspecDataIO::CannotOpen*>(&excep) ?
        string("Unable to open response file: ") :
        string("Unable to determine data type from response name: ");
     
     if (!XSparse::executingScript())
     {
        while (!isOk)
        {
           tcout <<"\n***Error:  " << errMsg << responseName << std::endl;
	   try
	   {
	      XSparse::basicPrompt("New response name?: ", responseName);
              proto = XSContainer::xsRegistry->returnPrototype(responseName, 
	   				   XspecDataIO::ResponseType);
	      isOk = true;
	   }
	   catch (XSparse::AbortLoop)
	   {
	      throw;
	   }
	   catch (YellowAlert& excep2)
	   {	
              errMsg = dynamic_cast<XspecDataIO::CannotOpen*>(&excep2) ?
                 string("Unable to open response file: ") :
                 string("Unable to determine data type from response name: ");
	   }
        }
     }
     else
     {
        tcerr <<"\n***Error:  " << errMsg << responseName << std::endl;
        throw;
     }
  }
  return proto;
}

void DataUtility::getLostChannelNumbers (const IntegerArray& quality, const IntegerArray& grouping, IntegerArray& lostChannels)
{

  // This assumes that the input quality and grouping arrays have
  // been verified elsewhere.  No error checking is performed on them.
  // Quality array should consist of ints [0-5], grouping should have
  // ints {-1, 0, 1}, and there should be no anomalies such as the
  // first bin = -1.  If they both exist, quality and grouping arrays 
  // must also be the same size

  // The idea here is to check each channel for bad quality = 1 OR
  // grouping = -1.  Each time this condition is met, increment by 1
  // the element of lostChannels which corresponds to the element
  // of the spectrum AFTER to where this lost channel would have
  // been inserted. ie. if grouping = {1,-1,-1}, lostChannels[0] = 0,
  // lostChannels[1] = 2 (lostChannels.size() = sd->channels.size()+1.)
  size_t qsz = quality.size();
  size_t gsz = grouping.size();
  if (qsz && gsz && (qsz != gsz))
  {
     throw RedAlert("DataUtility: size mismatch between quality and grouping arrays.");
  }
  if (!qsz && !gsz)
  {
     lostChannels.clear();
     return;
  }

  lostChannels.resize(1,0); 
  if (qsz && gsz)
  { 
     for (size_t i=0; i<qsz; ++i)
     {
        if (quality[i] == 1 || grouping[i] == -1)
        {
           ++lostChannels.back();
        }
        else
        {
           lostChannels.push_back(0);
        }
     }
  }
  else if (qsz)
  {
     for (size_t i=0; i<qsz; ++i)
     {
        if (quality[i] == 1)
        {
           ++lostChannels.back();
        }
        else
        {
           lostChannels.push_back(0);
        }
     }
  }
  else 
  {
     for (size_t i=0; i<gsz; ++i)
     {
        if (grouping[i] == -1)
        {
           ++lostChannels.back();
        }
        else
        {
           lostChannels.push_back(0);
        }
     }
  }  
}

void DataUtility::patchGrouping (const IntegerArray& quality, IntegerArray& grouping)
{
  size_t sz  (grouping.size());

  if (sz && grouping[0] == -1)
  {
	tcout << "***Warning: first channel has grouping = -1, will be reset to +1 " << std::endl;
	grouping[0] = 1;
  }



  if (quality.size())
  {
     if (quality.size() != sz)  throw RedAlert("Improper quality and grouping data");
     size_t i=0;
     bool firstGood = true;
     while (i < sz)
     {
        // conditions under which we need to modify the grouping vector arise if there
        // are poor quality channels marked at the start of bins that will be treated
        // separately.

        // always step past quality == 1     
        if ( quality[i] != 1)
        {
           if (firstGood)
           {
              if (grouping[i] == -1)
              {
                 tcout << "***Warning: first good channel has grouping = -1, will be reset to +1"
                      << std::endl;
                 grouping[i] = 1;
              }
              firstGood = false;
           }
           bool enterRun = (quality[i] == 0 && grouping[i] == 1);
           if ( enterRun ) 
           {
   //                        tcout << " Enter : " << i << " " << grouping[i] ;
              while (enterRun )
              {
                 ++i;
                 if (i >= sz)  break;
                 if (quality[i] != 1)
                 {
                     enterRun = (quality[i] == 0 && grouping[i] == -1);
                    //     if (!enterRun)
                    //     {
                    //        tcout << " End Run " << i <<  " q " << quality[i]
                    //        << " g " << grouping[i] << std::endl;  
                    //     }
                 }
              } 
           } // end if enterRun
           else
           {
              if ( quality[i] > 1)
              {
                 int lastQuality ( quality[i] );
             //                tcout << " poor quality: " << i << "  " << lastQuality ;
                 grouping[i] = 1;
                 // NOTE: following loop always iterates at least once.
                 while (i<sz && (quality[i] == lastQuality || quality[i] == 1))
                       ++i;
             //            tcout << " End " << i << std::endl;
                 if (i<sz && grouping[i] == -1) grouping[i] = +1;
              }
	      else   ++i;

           }
        } // end if quality != 1
        else 
        {
           ++i;
        }
     } // end sz loop 
  }

//  tcout << " Patched Grouping/Quality \n";
//  for (size_t j = 0; j < sz ; ++j)
//  {
//        tcout << j << "  " << quality[j] << " " << grouping[j]  
//                << "  "<< oldGrouping[j] << std::endl;
//  }
}

void DataUtility::weightScheme (XSContainer::Weight* value)
{
  s_weightScheme = value;
}

// Additional Declarations
