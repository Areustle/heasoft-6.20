//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <fstream>
#include <algorithm>

// ResponseMatrix
#include <XSModel/Data/Detector/ResponseMatrix.h>
#include "XSstreams.h"
#include <XSModel/Data/DataUtility.h>
#include <XSUtil/Utils/XSstream.h>
#include <cstring>


// Class ResponseMatrix::ChanRangeIndicators 

// Class ResponseMatrix 

ResponseMatrix::ResponseMatrix(const ResponseMatrix &right)
      : m_eMin(right.m_eMin),
        m_eMax(right.m_eMax),
        m_name(right.m_name),
	m_gqString(right.m_gqString),
        m_telescope(right.m_telescope),
        m_instrument(right.m_instrument),
        m_channelType(right.m_channelType),
        m_retainPosition(right.m_retainPosition),
        m_convArraySize(right.m_convArraySize),
        m_detectorChannels(right.m_detectorChannels),
        m_chanRangeIndicator(right.m_chanRangeIndicator),
        m_energyHigh(right.m_energyHigh),
        m_energyLow(right.m_energyLow),
        m_binStart(right.m_binStart),
        m_binRunLengths(right.m_binRunLengths),
        m_eboundsMin(right.m_eboundsMin),
        m_eboundsMax(right.m_eboundsMax),
        m_matrixValue(right.m_matrixValue),
	m_normFactor(right.m_normFactor),
        m_convArray(0),
        m_binResponseGroups(right.m_binResponseGroups),
        m_channelForRun(right.m_channelForRun),
        m_energyRunLengths(right.m_energyRunLengths),
        m_energyStart(right.m_energyStart),
        m_positions(right.m_positions)
{
  m_convArray.reset(new Real[m_convArraySize]);
  memcpy(m_convArray.get(),right.m_convArray.get(),m_convArraySize);
}

ResponseMatrix::ResponseMatrix (const ResponseMatrix::ChanRangeIndicators& chanLimits, size_t nE, size_t nC, const string& id, const CodeContainer& gqString, bool retainPositionArray)
      : m_eMin(0),
        m_eMax(0),
        m_name(id),
	m_gqString(gqString),
        m_telescope(),
        m_instrument(),
        m_channelType(),
        m_retainPosition(retainPositionArray),
        m_convArraySize(0),
        m_detectorChannels(nC),
        m_chanRangeIndicator(chanLimits),
        m_energyHigh(nE),
        m_energyLow(nE),
        m_binStart(nE),
        m_binRunLengths(nE),
        m_eboundsMin(nC),
        m_eboundsMax(nC),
        m_matrixValue(nE),
	m_normFactor(),
        m_convArray(0),
        m_binResponseGroups(nE),
        m_channelForRun(),
        m_energyRunLengths(),
        m_energyStart(),
        m_positions()
{
}


ResponseMatrix::~ResponseMatrix()
{
}



int ResponseMatrix::operator==(const ResponseMatrix &right) const
{
  return compare(right);
}

int ResponseMatrix::operator!=(const ResponseMatrix &right) const
{
  return !compare(right);
}


ResponseMatrix* ResponseMatrix::clone () const
{

  return new ResponseMatrix(*this);
}

bool ResponseMatrix::compare (const ResponseMatrix& right) const
{
  if (m_name != right.m_name) return false;
  if (m_gqString != right.m_gqString) return false;
  return true;
}

void ResponseMatrix::createConvolutionArray ()
{
   int nE = static_cast<int>(m_energyLow.size());
   RMFLONG lnE = static_cast<RMFLONG>(m_energyLow.size());
   std::vector<RMFLONG> positions;

   for (int i = 0; i < nE; i++)
   {
        int ngroups_i (m_binResponseGroups[i]);
        for (int j = 0; j<ngroups_i; j++)
        {
             int fchan_ij (m_binStart[i][j]);
	     int nchan_ij (m_binRunLengths[i][j]);
	     for (int k = 0; k <nchan_ij; k++)
                positions.push_back(lnE*(fchan_ij+k)+i);
        }
   }
   std::sort(positions.begin(), positions.end());

   size_t cvsz (positions.size());
   if (cvsz)
   {
        m_channelForRun.push_back(static_cast<int>(positions[0]/lnE)); //determine row
        m_energyStart.push_back(static_cast<int>(positions[0]%lnE));  //determine col
        m_energyRunLengths.push_back(1);
   }
   for (size_t i = 1; i<cvsz; i++)
   {
        RMFLONG pos_i (positions[i]);
        if (pos_i - positions[i-1] != 1 || pos_i % lnE == 0) //start of new group
        {
           m_channelForRun.push_back(static_cast<int>(pos_i/lnE)); 
           m_energyStart.push_back(static_cast<int>(pos_i%lnE));  
           m_energyRunLengths.push_back(1);
        }
        else
        {
           ++m_energyRunLengths.back();
        }
   }

   IntegerArray ecount(nE, 0);

   m_convArraySize = cvsz;

   m_convArray.reset(new Real[m_convArraySize]);
   size_t ncrg (m_channelForRun.size());
   size_t k(0);
   for (size_t i = 0; i < ncrg; i++)
   {
        int f_i (m_energyStart[i]);
        int n_i (m_energyRunLengths[i]);
        for (int j = 0; j < n_i; j++)
        {
             int ebin (f_i+j);
	     m_convArray[k++] = m_matrixValue[ebin][ecount[ebin]++];
        }
   }

   if (m_retainPosition) m_positions = positions;

   tpout << xsverbose(60);
   if (tpout.maxChatter() >= 60)
   {
        int  nchannels (0);
        tcout << "ORIGINAL\n";
        for (int i = 0; i < nE; i++)
        {
           for (int j = 0; j < m_binResponseGroups[i]; j++)
	   {
	      tcout << "Energy: " << i+1 << " nGroups: " << m_binResponseGroups[i] << 
	        " fchan: " << m_binStart[i][j] << " nchan: " << m_binRunLengths[i][j] << std::endl;
	      nchannels += m_binRunLengths[i][j];
           }

        }
        k = 0;
        for (size_t i = 0; i < m_matrixValue.size(); i++)
        {
	   for (size_t  j = 0; j < m_matrixValue[i].size(); j++)
	   {
	      tcout << "i: " << i << " j: " << j << " matrix: " 
                                << m_matrixValue[i][j] << std::endl;
              k++;
	   }
        }
        tcout << "total number of matrix elements: " << k << '\n';

        tcout << "\nFINAL\n";
        int  sz (m_channelForRun.size());

        for (int i = 0; i<sz; i++)
        {
	   tcout << "i: " << i << " channel: " 
                    << m_channelForRun[i] << " energyStart: " 
                    << m_energyStart[i] << " energyRunLen: " 
                    << m_energyRunLengths[i] << std::endl;
        }

        k = 0;
        RealArray exprow;
        size_t maxch (0);
        size_t numch (0);
        sz = m_energyRunLengths.size(); 
        bool okflag (1);
        tcout << "m_convArray.size: " << m_convArraySize << '\n';
        tcout << "i  " << "convArray   " << "matrix_ij" << '\n'; 
        for (int i = 0; i < sz; i++)
        {
           int  n_i (m_energyRunLengths[i]);
	   int  f_i (m_energyStart[i]);
	   int  m_i (m_channelForRun[i]);
           for (int j = 0; j < n_i; j++)
	   {
	      numch=0;
	      expandRmfRow(f_i+j,maxch,numch,exprow);
	      if (fabs(m_convArray[k] - exprow[m_i]) > 1.0e-15)
	      {
	              okflag = false;
	              tcout << "ERROR" <<std::endl;
	      }
	      tcout << k << "  " << m_convArray[k] << "  " 
                        << exprow[m_i] << std::endl;
	      k++;
	   }
        }
        if (okflag) tcout << "All elements match";
        else tcout << "There are errors";

        tcout << std::endl;

   }
   tpout << xsverbose();

   m_matrixValue.resize(0);
}

void ResponseMatrix::compressRmfRow (const size_t row, const size_t groupedResponseChannels, const RealArray& rmfRow)
{
  IntegerArray&   fchan=m_binStart[row];
  IntegerArray&   nchan=m_binRunLengths[row];
  RealArray&   matrixVal=m_matrixValue[row];

  size_t  ngroups=0;
  fchan.clear();
  nchan.clear();
  matrixVal.resize(0);

  size_t  rowSize(0);
  size_t  ch (0);
  size_t  startBin (0);

  while (ch < groupedResponseChannels)
  {
     while ((ch < groupedResponseChannels) && (rmfRow[ch] == 0))
        ch++;

     startBin = ch;   // index to the first non-zero bin

     while ((ch < groupedResponseChannels) && (rmfRow[ch] != 0))
        ch++;   // index to the last non-zero bin


     if (ch > startBin)
     {
        ngroups++;
	fchan.push_back(startBin);
	nchan.push_back(ch-startBin);		 
	rowSize += ch - startBin;
     }
  }

  binResponseGroups(row,ngroups);

  if (ngroups > 0)
  {
     matrixVal.resize(rowSize);
     size_t iCompressed = 0;
     for (size_t i=0; i<ngroups; ++i)
     {
        int nchan_i = nchan[i];
        int fchan_i = fchan[i];
        for (int j=0; j<nchan_i; ++j)
        {
           matrixVal[iCompressed] = rmfRow[fchan_i+j];
           ++iCompressed;
        }
     }
  }
}

void ResponseMatrix::expandRmfRow (const size_t row, size_t& maxChans, size_t& numChannels, RealArray &rmfRow)
{
  const size_t  ngroups=binResponseGroups(row);
  const IntegerArray&  fchan=binStart(row);
  const IntegerArray&  nchan=binRunLengths(row);
  const MatrixValue&   matrixVal=matrixValue();

  maxChans = fchan[ngroups-1] + nchan[ngroups-1];
  rmfRow.resize(maxChans, 0.0);

  for (size_t groupIndex=0; groupIndex<ngroups; groupIndex++)
  {
     RealArray temp = 
       matrixVal[row][std::slice(numChannels,nchan[groupIndex],1)];
     rmfRow[std::slice(fchan[groupIndex],nchan[groupIndex],1)] = temp;
     numChannels += nchan[groupIndex];
  }
}

void ResponseMatrix::normalizeRMF ()
{
  size_t  nE=energyLow().size();
  m_normFactor.resize(nE,0);
  for (size_t i=0; i<nE; ++i)
  {

     Real sum(0);
     if (m_matrixValue[i].size()) 
     {
         sum = m_matrixValue[i].sum();
         if (sum == 0.0)
         {
            // All entries in row are 0.0
            m_binResponseGroups[i] = 0;
            m_binStart[i].clear();
            m_binRunLengths[i].clear();
            m_matrixValue[i].resize(0);
         }
         else
         {
            m_matrixValue[i] /= sum;
            m_normFactor[i] = sum;
         }
     }
 }
}

bool ResponseMatrix::gqMatch (const CodeContainer& value) const
{
  return  m_gqString==value;
}

void ResponseMatrix::decodeGQ (IntegerArray& group, IntegerArray& quality)
{
  DataUtility::decodeGQ(m_gqString, group, quality);
}

Real* ResponseMatrix::convArray ()
{
  return m_convArray.get();
}

Real ResponseMatrix::convArray (size_t index)
{
  return *(m_convArray.get()+index);
}

bool ResponseMatrix::compareChanRange (int firstChan, int startChan, int endChan) const
{
   return (firstChan == m_chanRangeIndicator.firstChan &&
      startChan == m_chanRangeIndicator.startChan &&
      endChan == m_chanRangeIndicator.endChan);
}

// Additional Declarations
