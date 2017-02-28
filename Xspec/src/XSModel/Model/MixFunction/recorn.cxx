// Model to do recornrms
//
// Parameters are     specnum      Spectrum number
//                    cornorm      New correction norm

#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSContainer.h>        
#include <XSstreams.h>
#include "xsTypes.h"

extern "C" void 
recorn (const RealArray& energyArray, 
	const RealArray& params, 
	int spectrumNumber,
	RealArray& fluxArray, 
	RealArray& fluxErrArray,
	const string& initString)
{

  size_t specnum = static_cast<size_t>(params[0]+.5);
  Real cornorm = params[1];

  // Reset the correction norm for the requested spectrum, but only
  // if the requested spectrum matches the spectrumNumber 
  // currently being evaluated.  Note that for recorn, each spectrum
  // is always assigned a UniqueEnergy obj.

  if (specnum == static_cast<size_t>(spectrumNumber))
  {

     const size_t nAllSpectra = XSContainer::datasets->numberOfSpectra();

     if ( specnum > nAllSpectra ) {
       tcout << "Spectrum " << specnum << " has not been loaded." << std::endl;
     }

     SpectralData* sd = XSContainer::datasets->lookup(specnum);
     if (sd) {
       sd->correctionScale(cornorm);
       sd->computeTotals();
       //    tcout << " Spectrum " << specnum << " correction norm set to "
       //	  << cornorm << std::endl;
     }                     
  }
  size_t N(energyArray.size());
  fluxArray.resize(N-1);
  fluxErrArray.resize(0);

  fluxArray = 1.0;

  return;
}
