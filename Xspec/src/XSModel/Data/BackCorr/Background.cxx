//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <iostream>

// SpectralData
#include <XSModel/Data/SpectralData.h>
// DataSet
#include <XSModel/Data/DataSet.h>
// Background
#include <XSModel/Data/BackCorr/Background.h>

#include <XSModel/GlobalContainer/Weight.h>
#include <XSModel/Data/DataUtility.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Utils/XSutility.h>
#include "XSContainer.h"
#include "XSstreams.h"
#include <fstream>
#include <iomanip>
#include <cassert>


// Class BackCorr::IncorrectGrouping 

BackCorr::IncorrectGrouping::IncorrectGrouping (const string& message)
  : YellowAlert(string("Error: Background/Correction Grouping Cards do not match source"))
{
  tcerr << message << std::endl;
}


// Class BackCorr 

BackCorr::BackCorr()
  : m_sourceRow(0), 
    m_aScaleIsKeyword(false),
    m_bScaleIsKeyword(false),
    m_data(0), 
    m_source(0)
{
}

BackCorr::BackCorr(const BackCorr &right)
        : m_sourceRow(right.m_sourceRow), 
          m_aScaleIsKeyword(right.m_aScaleIsKeyword),
          m_bScaleIsKeyword(right.m_bScaleIsKeyword),
          m_data(0), // NOTE: Spectrum is NOT copied
          m_source(right.m_source)
{
}


BackCorr::~BackCorr()
{
  destroy();
}



void BackCorr::destroy () throw ()
{
  m_source = 0;
  delete m_data; 
  m_data = 0;    
}

void BackCorr::setData (size_t spectrumNumber, size_t backgrndRow, bool correction)
{

  setArrays(backgrndRow, correction);

  setDescription(spectrumNumber);

  groupArrays(correction);

  scaleArrays(correction);        

  SpectralData* parentSpec = m_source->sourceData(m_sourceRow);

  // Deliberately setting spectrum's isPoisson flag PRIOR to
  // to any variance modifications due to weighting.  The
  // flag refers to the state as originally read in from the file.
  m_data->checkForPoisson(parentSpec);

  RealArray norm  = m_data->exposureTime()*m_data->areaScale();
  norm           *= (m_data->backgroundScale()/parentSpec->backgroundScale());

  weightVariance(norm,correction);
}

void BackCorr::scaleArrays (bool correction)
{
  RealArray dataNorm = 1.0/m_data->areaScale();

  if ( isCounts() ) dataNorm   /= m_data->exposureTime();


  size_t n = m_data->channels();
  SpectralData* parent = source()->sourceData(m_sourceRow);
  if (parent->backgroundScale(0) < .0)
  {
     // If neg backgroundScale, assume this is coming from a
     // fakeit spectrum not based on original data.  In this
     // case the background's backScale will be used for the
     // fake spectrum as well.
     parent->setBackgroundScale(m_data->backgroundScale());
  }
  RealArray bkgScaleFactor = m_data->backgroundScale()/
  			parent->backgroundScale();
  dataNorm /= bkgScaleFactor;

  if (!correction)
  {
     RealArray var(m_data->rawVariance());
     var *= (dataNorm*dataNorm);  
     m_data->setRawVariance(var);
  }
  else
  {
        m_data->setRawVariance(RealArray(0.0,n));       
  }
  RealArray spectrum(m_data->spectrum());
  spectrum *= dataNorm;

  m_data->setSpectrum(spectrum);

  m_data->debugPrint(static_cast<XSstream&>(tcout),"Background");
}

bool BackCorr::isCounts () const
{

  return true;
}

void BackCorr::renumber (size_t newIndex)
{
  m_data->spectrumNumber(newIndex);
}

void BackCorr::groupArrays (bool correction)
{
}

void BackCorr::weightVariance (const RealArray& norm, bool correction)
{
  if (!correction) DataUtility::statWeight()(*m_data,norm);            
}

const RealArray& BackCorr::spectrum () const
{

  return m_data->spectrum();
}

const RealArray& BackCorr::variance () const
{

  return m_data->variance();
}

void BackCorr::setSpectrum (const RealArray& value)
{

  m_data->setSpectrum(value);
}

void BackCorr::setVariance (const RealArray& value)
{

  m_data->setVariance(value);
}

void BackCorr::setRawVariance (const RealArray& value)
{

  m_data->setRawVariance(value);
}

// Additional Declarations
