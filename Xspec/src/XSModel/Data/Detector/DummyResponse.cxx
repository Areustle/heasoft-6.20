//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <sstream>
#include <limits>
#include <iostream>
#include <iomanip>

// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// DummyResponse
#include <XSModel/Data/Detector/DummyResponse.h>
#include "XSsymbol.h"
// This is the name that will be the key for the DummyResponse when
// it is inserted into the global response container.
const std::string DUMMY_RSP("$#aw46gXS_DUMMY");


// Class DummyResponse 
DummyResponse* DummyResponse::s_instance = 0;

DummyResponse::DummyResponse (Real eMin, Real eMax, size_t nE, bool ln)
  //: Response(), m_energyArray(nE+1,0.), 
      : UserDummyResponse()
{

//        IntegerArray id(nE,1);
//        setBinResponseGroups(id);
//        MatrixIndex  im(nE,0);
//        for (int i = 0; i < nE; ++i) im[i].push_back(i);

//        setBinRunLengths(im);
//        setBinStartChannels(im);
        isLog(ln);

        // set the active flag to false. This is used to tell the model that
        // the response is the default dummy response which cannot be used for
        // fitting data. 
        //  
        setActive(false);
        std::numeric_limits<Real> RealInfo;
        eMin  = std::max(eMin,RealInfo.min());

	if (eMax <= eMin || nE == 0)
	{
	   std::ostringstream errMsg;
	   errMsg << "\nError in user-defined dummy response specification."
	      << "\nNo. Energies: " << nE << "\n Low Energy: " << eMin 
	      << "\n High Energy: " << eMax << 
	      "\n...using predefined values, N=40, min=0.5, max=20 keV"<<std::endl;
	   nE = 40;
	   eMin = .5;
	   eMax = 20.0;
	}
	numEnergies(nE);
        numChannels(nE);	
	eLow(eMin);
	eHigh(eMax);
        setEnergies();
        channels(energies());
        generateResponse();
}


DummyResponse::~DummyResponse()
{
}


DummyResponse* DummyResponse::Instance (Real eMin, Real eMax, size_t nE, bool ln)
{
  if (s_instance == 0) {s_instance = new DummyResponse(eMin,eMax,nE,ln);} return s_instance;

}

void DummyResponse::resetEnergies (Real eMin, Real eMax, size_t nE, bool ln)
{
  // This assumes input parameters have been validate before they get here.
        isLog(ln);
        numEnergies(nE);
        numChannels(nE);
	eLow(eMin);
	eHigh(eMax);
	setEnergies();
        channels(energies());
        generateResponse();
}

Model& DummyResponse::operator * (Model& model) const
{
  // this, the "folding operator" for dummy responses, simply returns
  // its argument. 
  return model; 
}

void DummyResponse::source (SpectralData* value)
{
  // never to be called.
}

SpectralData* DummyResponse::source () const
{

  return 0;
}

void DummyResponse::operator * (SumComponent& source) const
{
  int iData(spectrumNumber());
  // one of those rare uses for const_cast which seem to be acceptable.
  // source is cast to const so it can call the const versions of
  // photonArray(), etc ... the non-const versions are protected.
  const SumComponent& csource = const_cast<SumComponent&>(source);
  source.foldedPhotonFlux(iData).resize(csource.photonArray(iData).size());
  source.foldedPhotonFluxErr(iData).resize(csource.photonErrArray(iData).size());
  source.foldedPhotonFlux(iData) = csource.photonArray(iData);
  source.foldedPhotonFluxErr(iData) = csource.photonErrArray(iData);
}

void DummyResponse::generateResponse ()
{
  MatrixValue& diagMatrix = matrix();
  std::vector<std::size_t>& startEngs = dmyStartEngs();
  size_t nC = numChannels();
  diagMatrix.resize(nC);
  startEngs.resize(nC);
  for (size_t i=0; i<nC; ++i)
  {
     startEngs[i] = i;
     diagMatrix[i].resize(1, 1.0);
  }
}

// Additional Declarations
