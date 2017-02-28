//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Response
#include <XSModel/Data/Detector/Response.h>
// Background
#include <XSModel/Data/BackCorr/Background.h>
// DataSet
#include <XSModel/Data/DataSet.h>
// DataInputRecord
#include <XSModel/Data/DataInputRecord.h>

#include <XSModel/Data/SpectralData.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <sstream>
#include <iomanip>
#include <iterator>


// Class DataInputRecord 

DataInputRecord::DataInputRecord(const DataInputRecord &right)
      : m_fileName(right.m_fileName),
        m_groupNumber(right.m_groupNumber),
	m_numTrailCommas(right.m_numTrailCommas),
        m_spectrumNumber(right.m_spectrumNumber),
        m_spectrumRange(right.m_spectrumRange),
        m_response(right.m_response),
        m_backcorr(right.m_backcorr),
        m_data(right.m_data)
{
  // NOTE: We only want SHALLOW COPIES of m_data, m_backcorr, and m_response
  // pointers.  DataInputRecord is not intended to create or own any of these objects.
}

DataInputRecord::DataInputRecord (const string& fName, const IntegerArray& specNums, std::size_t groupNum, const IntegerArray& specRange, size_t nCommas)
      : m_fileName(fName),
        m_groupNumber(groupNum),
	m_numTrailCommas(nCommas),
        m_spectrumNumber(specNums),
        m_spectrumRange(specRange),
        m_response(0),
        m_backcorr(0),
        m_data(0)
{
}


DataInputRecord::~DataInputRecord()
{
  // only one of these will be nonzero but by design we don't want to delete them
  // since their addresses will be passed to a global data structure for persistent 
  // storage.
  // delete m_data;
  // delete m_response;
  // delete m_backcorr;
}


DataInputRecord & DataInputRecord::operator=(const DataInputRecord &right)
{
  DataInputRecord __temp(right);
  swap(__temp);
  return *this;
}


void DataInputRecord::updateSpectrumCounts (size_t numberOfSpectra)
{

    IntegerArray::iterator
	i_search = m_spectrumRange.begin(),
	i_begRange, i_endRange = m_spectrumRange.end();

    i_begRange = i_search;

    bool wildFound = false;

    //if we still have wilds in our row ranges, we can handle them here
    //because we now know the total number of spectra in our data file
    while((i_search = std::find(i_search, i_endRange, -2)) != i_endRange) 
    {
	wildFound = true;
	*i_search = ((i_search - i_begRange) % 2 ? numberOfSpectra : 1);
	++i_search;
    }

    //if we didn't have any wilds to begin with, then our rows have already been
    //expanded out. Also means that m_spectrumNumber hasn't been initialized
    if(wildFound) 
    {
	m_spectrumRange = XSparse::expandRange(m_spectrumRange);

	int size = m_spectrumRange.size(), firstSpecNum = m_spectrumNumber[0];

	m_spectrumNumber.resize(size);

	for(int i = 1 ; i < size; ++i)
	    m_spectrumNumber[i] = firstSpecNum + i;
    }

    correctSpectraRows(numberOfSpectra);
}

void DataInputRecord::report ()
{
    using namespace std;
    XSstream& xscout = static_cast<XSstream&>(tcout);
    if ( xscout.logChatterLevel() <= 25 ) return;
    xscout.setVerbose(0,25);
    tcout << "\nFileName: " << fileName();
    tcout << "\nSpectrum numbers to be created: ";
    ostream_iterator<size_t> su(tcout,"\t");
    ostream_iterator<int> ss(tcout,"\t");
    copy(m_spectrumNumber.begin(),m_spectrumNumber.end(),su);
    tcout << "\nData group number " << groupNumber();
    if (m_spectrumRange[0] != 0)
    {
        tcout << "\nRanges in file: ";
        copy(spectrumRange().begin(),spectrumRange().end(),ss);
        tcout << '\n';
    }

    if (m_numTrailCommas > 0 )
    {
            tcout << "\nNumber of trailing commas: " << m_numTrailCommas;
    }  
    tcout << endl;  
    xscout.setVerbose();
}

const string& DataInputRecord::fileName () const
{

  return m_fileName;
}

void DataInputRecord::swap (DataInputRecord& right)
{
      std::swap(m_fileName,right.m_fileName);
      std::swap(m_spectrumRange,right.m_spectrumRange);
      std::swap(m_groupNumber,right.m_groupNumber);
      std::swap(m_spectrumNumber,right.m_spectrumNumber);
      std::swap(m_numTrailCommas,right.m_numTrailCommas);
      std::swap(m_response,right.m_response);
      std::swap(m_data,right.m_data);
      std::swap(m_backcorr,right.m_backcorr);
}

void DataInputRecord::renumber (size_t newIndex)
{
  size_t n = m_spectrumNumber.size();

  for (size_t j = 0; j < n; ++j)
  {
          m_spectrumNumber[j] = newIndex++;  
          size_t currentRow = m_spectrumRange[j];

          if (m_data != 0)
          {
                   m_data->sourceData(currentRow)->renumber(m_spectrumNumber[j]);  
          }

  }
}

void DataInputRecord::correctSpectraRows (size_t numberOfSpectra)
{

    IntegerArray inBounds;
    int nEntered = m_spectrumRange.size();
    for (int i=0; i<nEntered; ++i)
    {
       if (m_spectrumRange[i] <= static_cast<int>(numberOfSpectra))
          inBounds.push_back(i);
    }
    int nRemaining = inBounds.size();
    int nRemove = nEntered - nRemaining;
    if (nRemove)
    {
       tcout <<"\n***Warning: One or more row numbers are out of bounds, they will be ignored." 
                << std::endl;
       // this can't be < 0
       if (!nRemaining)
       {
          m_spectrumRange.clear();
          m_spectrumNumber.clear();
       }
       else
       {
          // Note: this back and forth array copying should still be more efficient
          // (and safer) than doing many erase operations over large vectors.  
          // Perhaps m_spectrumRange and m_spectrumNumber should be lists instead.
          IntegerArray tmpRange;
          IntegerArray tmpNumber;
          tmpRange.reserve(nRemaining);
          tmpNumber.reserve(nRemaining);
          for (int i=0; i<nRemaining; ++i)
          {
             tmpRange.push_back(m_spectrumRange[inBounds[i]]);
             tmpNumber.push_back(m_spectrumNumber[inBounds[i]]);
          }
          m_spectrumRange = tmpRange;
          m_spectrumNumber = tmpNumber;
       }
    }
}

// Additional Declarations
bool operator < (const DataInputRecord& left, const DataInputRecord& right)
{
        return left.m_spectrumNumber[0] < right.m_spectrumNumber[0];
}
