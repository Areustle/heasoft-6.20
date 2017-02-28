//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSstreams.h>
#include <iostream>

// FakeDataInputRecord
#include <XSModel/Data/FakeDataInputRecord.h>


// Class FakeDataInputRecord 

FakeDataInputRecord::FakeDataInputRecord (size_t nSpec)
  : DataInputRecord(""),
    m_backgndFile(""),
    m_useCountingStat(false),
    m_isType2(false),
    m_exposureTime(.0),
    m_backExposureTime(-1.0), // Negative val indicates "don't use".
    m_correctionNorm(.0),
    m_enteredNone(false),
    m_numSourcesForSpectra(1),
    m_statName("chi"), // Only matters for fake spectra NOT based on original.
    m_origRowNums(),
    m_inputCorrFiles(std::vector<BackLocator>(nSpec)),   
    m_inputBackgrounds(std::vector<BackLocator>(nSpec)),
    m_inputResponses(std::vector<Detectors>(nSpec)),
    m_inputArfs(std::vector<Arfs>(nSpec))
{
   IntegerArray tmp(nSpec);
   setSpectrumNumber(tmp);
   setSpectrumRange(tmp);
}


FakeDataInputRecord::~FakeDataInputRecord()
{
}


void FakeDataInputRecord::printDiagnostics () const
{
  tcout << "\nFake file name: " << fileName();
  tcout << "\nBased on original data?: " << (data() ? "yes" : "no");
  tcout << "\nType: " << (m_isType2 ? 2 : 1);
  tcout << "\nData group number: " << groupNumber();

  size_t sz = spectrumNumber().size();
  tcout << "\nSpectrum numbers: ";
  for (size_t i=0; i<sz; ++i)
  {
     tcout << spectrumNumber(i) << " ";
  }
  tcout << "\nRow numbers: ";
  for (size_t i=0; i<sz; ++i)
  {
     tcout << spectrumRange(i) << " ";
  }
  tcout << "\nOutput background file: " << m_backgndFile;
  tcout << "\nInput responses: "<<std::endl;
  sz = m_inputResponses.size();
  for (size_t i=0; i<sz; ++i)
  {
     const Detectors& dets = inputResponses(i);     
     for (size_t j=0; j<dets.size(); ++j)
        tcout <<"   "<<dets[j].first<<","<<dets[j].second; 
     tcout <<std::endl;
  }
  tcout << "\nInput arfs: "<<std::endl;
  for (size_t i=0; i<sz; ++i)
  {
     const Arfs& arfs = inputArfs(i);
     for (size_t j=0; j<arfs.size(); ++j)
        tcout <<"  "<<arfs[j].first.first<<","<<arfs[j].first.second<<","<<arfs[j].second;
     tcout <<std::endl;
  }
  tcout << "\nInput backgrounds: ";
  for (size_t i=0; i<sz; ++i)
  {
     tcout <<"\n  " << m_inputBackgrounds[i].first << "  " << m_inputBackgrounds[i].second;
  }
  tcout << "\nInput corr files: ";
  for (size_t i=0; i<sz; ++i)
  {
     tcout <<"\n  " << m_inputCorrFiles[i].first << "  " << m_inputCorrFiles[i].second;
  }
  tcout << "\nExposure time: " << m_exposureTime;
  tcout << "\nBack Exposure time: " << m_backExposureTime;
  tcout << "\nCorrection norm: " << m_correctionNorm;
  tcout << "\nUse counting stat: " << m_useCountingStat;
  tcout << "\n" <<std::endl;
}

// Additional Declarations
