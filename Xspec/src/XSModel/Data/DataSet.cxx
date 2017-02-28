//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSModel/GlobalContainer/TrashPtr.h>
#include <XSModel/GlobalContainer/TrashAdapter.h>

// DataContainer
#include <XSModel/GlobalContainer/DataContainer.h>
// DataSetBase
#include <XSModel/Data/DataSetBase.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// DataSetTypes
#include <XSModel/GlobalContainer/DataSetTypes.h>
// DataSet
#include <XSModel/Data/DataSet.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>

#include <XSModel/DataFactory/DataFactory.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Numerics/RandomGenerator.h>
#include "XSstreams.h"
#include "XSContainer.h"
#include <cassert>
#include <fstream>
#include <iomanip>
#include <unistd.h>
#include <limits.h>

using namespace XSContainer;


// Class DataSet::AbortDataLoop 

DataSet::AbortDataLoop::AbortDataLoop (const string& msg)
  : YellowAlert(msg)
{
}


// Class DataSet::ResponseIsNeeded 

DataSet::ResponseIsNeeded::ResponseIsNeeded (const string& msg)
  : YellowAlert("Valid response file is needed\n")
{
   tcerr << msg << std::endl;
}


// Class DataSet 
int DataSet::s_count;
bool DataSet::s_useFakeCountingStat;
string DataSet::s_xspecVersion;
const size_t DataSet::s_STD_FIRST_CHAN = 1;

DataSet::DataSet()
  : m_dataSetBase(0)
{
  ++s_count;
}

DataSet::DataSet(const DataSet &right)
      : m_dataSetBase(0)
{
  if (right.m_dataSetBase) 
  {
        m_dataSetBase = new DataSetBase(*right.m_dataSetBase);
        m_dataSetBase->index(++s_count);
  }
}


DataSet::~DataSet()
{
  // calls DataSetBase's dtor which calls DataSetBase::destroy() nothrow().
  destroy();
}



void DataSet::plot (const string& args)
{
}

const std::vector<int>& DataSet::groupingInfo (size_t row) const
{

  return sourceData(row)->groupingInfo();      
}

const std::vector<int>& DataSet::qualityInfo (size_t row) const
{
  return sourceData(row)->qualityInfo();      
}

size_t DataSet::channels (size_t row) const
{
  return sourceData(row)->channels();      
}

const string& DataSet::dataName () const
{
  return m_dataSetBase->dataName();
}

SpectralData*& DataSet::sourceData (size_t row)
{
  return m_dataSetBase->multiSpectralData(row);
}

void DataSet::setData (size_t spectrumNumber, size_t row)
{
  setArrays(row);

  setDescription(spectrumNumber,row); 

  groupArrays(row);      

  setResponse(spectrumNumber,row);       

  setAncillaryData(row);

  scaleArrays(row);  

  SpectralData& spectrum = *sourceData(row);

  // Deliberately setting spectrum's isPoisson flag PRIOR to
  // to any variance modifications due to weighting.  The
  // flag refers to the state as originally read in from the file.
  spectrum.checkForPoisson(0);

  DataUtility::statWeight()(spectrum,spectrum.areaScale()*spectrum.exposureTime());

  computeTotals(row);
}

void DataSet::setResponse (size_t spectrumNumber, size_t row)
{
}

void DataSet::scaleArrays (size_t row)
{
  SpectralData* data = sourceData(row);

  const  Real ONE = 1.;  

  RealArray dataNorm = ONE/data->areaScale();

  if ( isCounts() ) dataNorm   /= data->exposureTime();

  RealArray var(data->rawVariance());
  RealArray spectrum(data->spectrum());

  var *= dataNorm*dataNorm;
  spectrum *= dataNorm;

  data->setRawVariance(var);
  data->setSpectrum(spectrum);

  // store the variance immediately after grouping for
  // use in resetting variance after different weighting schemes are applied.

  data->debugPrint(static_cast<XSstream&>(tcout),"Spectrum");
}

void DataSet::ignoreBadChannels ()
{
  if (!isMultiple())
  {
          m_dataSetBase->spectralData()->setBadChannels();
  }
  else
  {
          SpectralDataMapIt ig(m_dataSetBase->multiSpectralData().begin());
          SpectralDataMapIt igEnd(m_dataSetBase->multiSpectralData().end());

          while ( ig != igEnd )
          {
                (*ig).second->setBadChannels();
                ++ig;       
          }
  }
}

bool DataSet::isCounts () const
{

  return true;
}

void DataSet::report (size_t row) const
{
   const SpectralData* spec = sourceData(row);
   spec->report();
}

void DataSet::destroy () throw ()
{
  delete m_dataSetBase;
}

void DataSet::reportAll ()
{
  if (dataSetBase()->spectralData()) report(0);
  else
  {
          const SpectralDataMap& mSD = dataSetBase()->multiSpectralData();
          SpectralDataMapConstIt endList = mSD.end();
          for (SpectralDataMapConstIt i = mSD.begin(); i != endList; ++i)
          {
                  report(i->first);
          }                

  }
  tcout << std::flush;
}

void DataSet::computeTotals (size_t row)
{
  SpectralData* data = sourceData(row);
  data->computeTotals();      
}

const SpectralData* DataSet::sourceData (size_t row) const
{
  return m_dataSetBase->multiSpectralData(row);
}

bool DataSet::isMultiple () const
{
  if (!m_dataSetBase)
  {
     return false;
  }
  return !m_dataSetBase->multiSpectralData().empty();
}

int DataSet::numSpectra () const
{
  if (isMultiple()) return m_dataSetBase->multiSpectralData().size();
  else if (spectralData()) return 1; 
  else return 0;     
}

void DataSet::legalChannelBounds (int& startChan, int& endChan) const
{
  startChan = m_dataSetBase->legalStartChan();
  endChan = m_dataSetBase->legalEndChan();
}

bool DataSet::groupingSet (size_t row)
{

  return !groupingInfo(row).empty();
}

bool DataSet::qualitySet (size_t row)
{

  return !qualityInfo(row).empty();
}

size_t DataSet::dataGroup () const
{

  return m_dataSetBase->dataGroup();
}

void DataSet::dataGroup (size_t group)
{

  m_dataSetBase->dataGroup(group);
}

size_t DataSet::index () const
{

  return m_dataSetBase->index();
}

int DataSet::removeNumberedSpectrum (size_t index, bool remove)
{
  static const string REMOVED(" removed ");
  static const string REPLACED(" replaced ");
  int status = 0;
  if ( isMultiple() )
  {

        SpectralDataMap& sdm = m_dataSetBase->multiSpectralData();
        SpectralDataMapIt sdmi   = sdm.begin(); 
        SpectralDataMapIt sdmEnd = sdm.end();
        while (sdmi != sdmEnd)
        {
                if (index == sdmi->second->spectrumNumber())
                {
		    SpectralData* s = sdmi->second;

		    //a little different than the classic memento pattern...
		    //in this case, DataSet is requesting the memento,
		    //however, SpectralData will also hold onto it. With the 
		    //new undo scheme, the DataSet could be deleted, and the 
		    //SpectralData could still be floating around.
		    s->CreateMemento();

		    responses->removeByToken(s->detector());
		    datasets->moveToTrash(new TrashAdapter<SpectralData*>(s));

		    sdm.erase(sdmi);

		    status = sdm.empty() ? -1 : 1;
		    tcout << "\nSpectrum #: " << index ;
		    tcout << (remove ?  REMOVED :  REPLACED) ;
		    tcout << std::endl;
		    break;
                }       
                else ++sdmi;
        }         
  }
  else
  {
          SpectralData* spectrum = m_dataSetBase->spectralData();
          if (index == spectrum->spectrumNumber()) 
          {
	      spectrum->CreateMemento();

	      responses->removeByToken(spectrum->detector());
	      datasets->moveToTrash(new TrashAdapter<SpectralData*>(spectrum));

	      //this is so spectralData isn't deleted if/when the 
	      //dataset is destroyed
	      m_dataSetBase->spectralData(0);

	      tcout << "\nSpectrum #: " << index ;
	      tcout << (remove ?  REMOVED :  REPLACED) ;
	      tcout << std::endl;                          
	      status = -1;
          }
  }
  return status;
}

void DataSet::setChannels (bool value, IntegerArray& channelRange)
{


  // I believe the primary purpose of this is to support "notice all"             
  if (!isMultiple())
  {
          m_dataSetBase->spectralData()->setChannels(value,channelRange);
  }
  else
  {
          SpectralDataMapIt ig(m_dataSetBase->multiSpectralData().begin());
          SpectralDataMapIt igEnd(m_dataSetBase->multiSpectralData().end());

          while ( ig != igEnd )
          {
                (*ig).second->setChannels(value,channelRange);
                ++ig;       
          }
  }
}

void DataSet::setChannels (bool value, std::pair<Real,Real>& realRange)
{
  using namespace XSContainer; 
  // ... this one might not be needed, but could support a future command
  // syntax whose intent is "ignore/notice all files in a given dataset
  // with energy/wavelength equal to realRange".

  if (!isMultiple())
  {
          m_dataSetBase->spectralData()->setChannels(value,realRange);
  }
  else
  {
          SpectralDataMapIt  ig(m_dataSetBase->multiSpectralData().begin());
          SpectralDataMapIt  igEnd(m_dataSetBase->multiSpectralData().end());

          while ( ig != igEnd )
          {
                (*ig).second->setChannels(value,realRange);
                ++ig;       
          }
  }
}

SpectralData* DataSet::spectralData () const
{
  return m_dataSetBase->spectralData();
}

const std::map<size_t,SpectralData*>& DataSet::multiSpectralData () const
{
  return m_dataSetBase->multiSpectralData();
}

void DataSet::groupArrays (size_t row)
{
}

int DataSet::origNumSources () const
{
   return m_dataSetBase->origNumSources();
}

void DataSet::origNumSources (int value)
{
   m_dataSetBase->origNumSources(value);
}

bool DataSet::setResponse (SpectralData* sourceSpectrum, size_t spectrumNumber, size_t sourceNum, const string& responseName, const string& arfName)
{
  return false;
}

bool DataSet::setBackgroundData (size_t row, int bckRow)
{
  return false;
}

bool DataSet::setCorrectionData (size_t row, int corRow)
{
  return false;
}

bool DataSet::resetAllDetectors ()
{
  bool isChanged = false;

  if (!isMultiple())
  {
          isChanged = m_dataSetBase->spectralData()->removeAllDummies();
  }
  else
  {
          SpectralDataMapIt ig(m_dataSetBase->multiSpectralData().begin());
          SpectralDataMapIt igEnd(m_dataSetBase->multiSpectralData().end());

          while ( ig != igEnd )
          {
                if((*ig).second->removeAllDummies())
		{
		   isChanged = true;
		}
                ++ig;       
          }
  }

  return isChanged;
}

void DataSet::nullBackPointers (const IntegerArray& rows)
{
  if (!isMultiple())
  {
     m_dataSetBase->spectralData()->background(static_cast<Background*>(0));
  }
  else if (!rows.size())
  {
     SpectralDataMapIt id(m_dataSetBase->multiSpectralData().begin());
     SpectralDataMapIt idEnd(m_dataSetBase->multiSpectralData().end());
     while (id != idEnd)
     {
	id->second->background(static_cast<Background*>(0));
	++id;
     }
  }
  else
  {
    for (int i=0; i<(int)rows.size(); ++i)
     {
        SpectralDataMapIt id = m_dataSetBase->multiSpectralData().find(rows[i]);
	if (id == m_dataSetBase->multiSpectralData().end())
	{
	   throw RedAlert("Accessing non-existant spectrum in DataSet::nullBackPointers\n");
	}
	id->second->background(static_cast<Background*>(0));	
     }
  }
}

bool DataSet::specNumOrder (IntegerArray& rows) const
{

  // Basically, this function allows external objects a way to access
  // the multiple spectra in order of spectrum number, rather than by
  // row number which is how they are originally stored in the 
  // multiSpectralDataMap.  It does not modify the multiSpectralDataMap,
  // but instead fills an array of the row numbers in the order they 
  // should be called to go from lowest spectrum number to highest.

  // Return 'true' if spectrum numbers in multiDataSet are in sequential
  // order, or if this is only a single spectrum set.
  bool flag = true;
  rows.clear();
  if (isMultiple())
  {
     SpectralDataMap specNumsData;
     SpectralDataMapConstIt sdIt = m_dataSetBase->multiSpectralData().begin();
     SpectralDataMapConstIt sdEnd = m_dataSetBase->multiSpectralData().end();
     while (sdIt != sdEnd)
     {
        SpectralDataMap::value_type tmp(sdIt->second->spectrumNumber(),
					sdIt->second);
        specNumsData.insert(tmp);
        ++sdIt;
     }

     SpectralDataMapConstIt sdsIt = specNumsData.begin();
     SpectralDataMapConstIt sdsEnd = specNumsData.end();
     size_t prevNum(0);
     if (sdsIt != sdsEnd)
     {
        prevNum = sdsIt->first;
	rows.push_back(sdsIt->second->rowNumber());
        ++sdsIt;
     }
     while (sdsIt != sdsEnd)
     {
        if (sdsIt->first != (prevNum+1))
	{
	   flag = false;
	}
	rows.push_back(sdsIt->second->rowNumber());
	prevNum = sdsIt->first;
        ++sdsIt;
     }
  }
  else
  {  // type 1
     rows.push_back(0);
  }
  return flag;
}

void DataSet::generateFake ()
{
  size_t start, stop;
  start = isMultiple() ? 1 : 0;
  stop = isMultiple() ? multiSpectralData().size()+1 : 1;

  for (size_t i=start; i<stop; ++i)
  {
     // Spectrum should already contain M*R by this point.
     SpectralData* sd = sourceData(i);
     sd->simulateSpectrum(s_useFakeCountingStat);
     sd->computeTotals();
  }
}

const string& DataSet::outputFileName () const
{
   return m_dataSetBase->outputFileName();
}

void DataSet::outputFileName (const string& fileName)
{
   m_dataSetBase->outputFileName(fileName);
}

const string& DataSet::outputBckFileName () const
{
   return m_dataSetBase->outputBckFileName();
}

void DataSet::outputBckFileName (const string& fileName)
{
   m_dataSetBase->outputBckFileName(fileName);
}

size_t DataSet::getMaxChannels () const
{
  size_t maxChans = 0;
  if (!isMultiple())
  {
     maxChans = spectralData()->endChan() - spectralData()->startChan() + 1;
  }
  else
  {
     SpectralDataMapConstIt spec = m_dataSetBase->multiSpectralData().begin();
     SpectralDataMapConstIt end = m_dataSetBase->multiSpectralData().end();
     while (spec != end)
     {
        size_t currChans = spec->second->endChan() - spec->second->startChan() + 1;
        maxChans = (currChans > maxChans) ? currChans : maxChans;
        ++spec;
     }
  }
  return maxChans;
}

void DataSet::setModelNamesForFake (const string& name, size_t index)
{
  m_dataSetBase->modelNamesForFake(index, name);
}

void DataSet::setModelNamesForFake (const StringArray& names)
{
  m_dataSetBase->setModelNamesForFake(names);
}

bool DataSet::anyQuality () const
{
  if (isMultiple())
  {
     SpectralDataMap::const_iterator iSp = m_dataSetBase->multiSpectralData().begin();
     SpectralDataMap::const_iterator iEnd = m_dataSetBase->multiSpectralData().end();
     while (iSp != iEnd)
     {
        if (iSp->second->qualityInfo().size())
        {
           return true;
        }
        ++iSp;
     }
     return false; // avoid compiler warning.
  }
  else
  {
     return static_cast<bool>(m_dataSetBase->spectralData()->qualityInfo().size());
  }
  return false;
}

bool DataSet::anyGrouping () const
{
  if (isMultiple())
  {
     SpectralDataMap::const_iterator iSp = m_dataSetBase->multiSpectralData().begin();
     SpectralDataMap::const_iterator iEnd = m_dataSetBase->multiSpectralData().end();
     while (iSp != iEnd)
     {
        if (iSp->second->groupingInfo().size())
        {
           return true;
        }
        ++iSp;
     }
  }
  else
  {
     return static_cast<bool>(m_dataSetBase->spectralData()->groupingInfo().size());
  }
  return false;
}

bool DataSet::aScaleIsKeyword () const
{
  return m_dataSetBase->aScaleIsKeyword();
}

bool DataSet::bScaleIsKeyword () const
{
  return m_dataSetBase->bScaleIsKeyword();
}

string DataSet::getFullPathName () const
{

   // DataSetBase's m_dataName stores the filename exactly as the
   // user has entered it from the data command.  So it could contain
   // a leading absolute or relative path.

   string fullPathName;   
   if (m_dataSetBase->dataName().find('/') == 0)
   {
      // We already have the absolute path.
      fullPathName = m_dataSetBase->dataName();
   }
   else
   {
      fullPathName = m_dataSetBase->runPath();

      if(*(fullPathName.rbegin()) != '/')
          fullPathName += '/';

      fullPathName += m_dataSetBase->dataName();
   }
   return fullPathName;
}

void DataSet::setRunPath ()
{
    m_dataSetBase->runPath(XSutility::getRunPath());
}

bool DataSet::isNet (const RealArray& spectrum)
{

  return true;
}

const string& DataSet::getRunPath () const
{
    return m_dataSetBase->runPath();
}

void DataSet::insertSpectrum (SpectralData* s)
{
    if(s->rowNumber() == 0)
	dataSetBase()->spectralData(s);
    else
	dataSetBase()->multiSpectralData().insert(std::make_pair(s->rowNumber(), s));
}

// Additional Declarations

