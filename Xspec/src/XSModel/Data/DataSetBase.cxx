//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <sstream>

// DataFactory
#include <XSModel/DataFactory/DataFactory.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// DataSetBase
#include <XSModel/Data/DataSetBase.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>



// Class DataSetBase::NoSuchSpectrum 

DataSetBase::NoSuchSpectrum::NoSuchSpectrum (const string& diag)
  : YellowAlert(diag)
{
}


// Class DataSetBase 

DataSetBase::DataSetBase(const DataSetBase &right)
      : m_dataName(right.m_dataName),
        m_legalStartChan(right.m_legalStartChan),
        m_legalEndChan(right.m_legalEndChan), 
        m_dataGroup(right.m_dataGroup),
        m_index(right.m_index),
        m_origNumSources(right.m_origNumSources),
        m_outputFileName(right.m_outputFileName),
        m_outputBckFileName(right.m_outputBckFileName),
        m_aScaleIsKeyword(right.m_aScaleIsKeyword),
        m_bScaleIsKeyword(right.m_bScaleIsKeyword),
        m_runPath(right.m_runPath),
        m_spectralData(0),  // NOTE: spectra are NOT copied here.
        m_protoType(right.m_protoType), //shallow copy is appropriate here.
        m_multiSpectralData(), 
        m_modelNamesForFake(right.m_modelNamesForFake)
{
}

DataSetBase::DataSetBase (const string& name, size_t index, DataPrototype* proto)
      : m_dataName(name),
        m_legalStartChan(0),
        m_legalEndChan(0), 
        m_dataGroup(1),   
        m_index(0),   
        m_origNumSources(1),
        m_outputFileName(""),
        m_outputBckFileName(""),
        m_aScaleIsKeyword(false),
        m_bScaleIsKeyword(false),
        m_runPath(""),
        m_spectralData(0),
        m_protoType(proto),
        m_multiSpectralData(),
        m_modelNamesForFake()
{
}


DataSetBase::~DataSetBase()
{
  destroy();
}


void DataSetBase::destroy () throw ()
{

  // cannot throw because delete cannot throw,
  // assuming iterator operations can't throw;    
  // spectralData/multiSpectralData are the only owning pointers in DataSetBase.
  // (the DataPrototype pointer is not an owning pointer).
  // calling delete on SpectralData will invoke its dtor which will call its
  // destroy method. 

  delete m_spectralData;
  m_spectralData = 0;
  if (!m_multiSpectralData.empty())
  {
        for (std::map<size_t,SpectralData*>::iterator mi = m_multiSpectralData.begin(); 
              mi != m_multiSpectralData.end(); mi++)
        {
                delete mi->second;             
        }              
        m_multiSpectralData.erase(m_multiSpectralData.begin(),m_multiSpectralData.end());
  }  
}

void DataSetBase::closeFiles ()
{
  if (m_spectralData) m_spectralData->closeAncillaryFiles();

  if (!m_multiSpectralData.empty())
  {
        for ( std::map<size_t,SpectralData*>::iterator cl = m_multiSpectralData.begin();
                cl !=  m_multiSpectralData.end();    ++cl)
        {
                cl->second->closeAncillaryFiles();  
        }        
  }        
}

std::map<size_t,SpectralData*>& DataSetBase::multiSpectralData ()
{

  return m_multiSpectralData;
}

SpectralData* DataSetBase::spectralData ()
{

  return m_spectralData;
}

void DataSetBase::dataGroup (size_t value)
{
  m_dataGroup = value;

  // If there are already spectral with responses attached (ie.
  // if this is NOT reached from the first call from
  // the xsData handler), make sure that the response data group
  // values are changed as well.  One place where this situation
  // can occur is due to data group renumbering from the
  // DataUtility::fixSequence function. 
  if (!m_multiSpectralData.empty())
  {
     std::map<size_t,SpectralData*>::iterator it = m_multiSpectralData.begin();
     std::map<size_t,SpectralData*>::iterator itEnd = m_multiSpectralData.end();
     while (it != itEnd)
     {
        SpectralData* sd = it->second;
        sd->resetDetsDataGroupNums(value);
        ++it;
     }
  }
  else if (m_spectralData)
  {
     m_spectralData->resetDetsDataGroupNums(value);
  }
}

SpectralData*& DataSetBase::multiSpectralData (size_t index)
{
  // Return an entry that already exists so that it can be manipulated.
  // Does NOT create new entries.
  if (index == 0 || m_multiSpectralData.empty())
  {
        return m_spectralData;
  }
  else
  {
        std::map<size_t,SpectralData*>::iterator seek = m_multiSpectralData.find(index);
        if ( seek != m_multiSpectralData.end() ) return seek->second;
        else
        {
                std::ostringstream msg;
                msg     << "Error: spectrum from row # " << index 
                        << " not found in DataSet "  << dataName();
                throw NoSuchSpectrum(msg.str());
        }  
  }      
}

// Additional Declarations
