//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SpectralData
#include <XSModel/Data/SpectralData.h>
// Response
#include <XSModel/Data/Detector/Response.h>
// ACNComponent
#include <XSModel/Model/Component/ACNComponent.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSstreams.h>
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/UniqueEnergy.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>


// Class ACNComponent 

ACNComponent::ACNComponent(const ACNComponent &right)
  : ConComponent(right)
{
}

ACNComponent::ACNComponent (ComponentGroup* p)
  : ConComponent(p)
{
}


ACNComponent::~ACNComponent()
{
}


ACNComponent* ACNComponent::clone (ComponentGroup* p) const
{
  ACNComponent* cloned = new ACNComponent(*this);
  cloned->setParent(p);
  return cloned;    
}

void ACNComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  ACNComponent __temp(static_cast<const ACNComponent&>(right));
  swap(__temp);   
}

const Response* ACNComponent::detector (size_t spectrumNumber) const
{
  return root()->detector(spectrumNumber);
}

SumComponent& ACNComponent::operator * (SumComponent& right) const
{
  const std::set<UniqueEnergy*>& energies = getUniqueEnergies();
  std::set<UniqueEnergy*>::const_iterator en   = energies.begin();
  std::set<UniqueEnergy*>::const_iterator enEnd = energies.end();

  size_t  numParams(parameterSet().size());
  RealArray  startValues(numParams);
  for (size_t  i=0; i<numParams; i++)
  {
     startValues[i] = parameterSet()[i]->value();
  }

  while (en != enEnd)
  {
     // NOTE: If model.dat entry was properly flagged, a UniqueEnergy obj 
     // should have been created for every spectrum.  We can double check 
     // for that by the size of the clientSpectra set.
     const UniqueEnergy* uniqueEng = *en;
     const std::set<size_t>& associatedSpectra = uniqueEng->clientSpectra();
     if (associatedSpectra.size() > 1)
     {
        string errMsg(name());
        errMsg += " component is attempting to share single energy array among multiple spectra.";
        errMsg += "Spectrum dependency flag in model description file needs to be set to 1 (true).\n";
        throw YellowAlert(errMsg);        
     }
     else if (associatedSpectra.empty())
     {
        // This is a more serious error, indicating a programming mistake.
        string errMsg("No spectrum assigned to unique energy array for ");
        errMsg += name() + " component.";
        throw RedAlert(errMsg);
     }         
     size_t spectrumNumber = *associatedSpectra.begin();
     size_t N (right.uniquePhotonArray(uniqueEng).size());
     RealArray efficiency(.0, N);

     // obtain efficiency array from Spectrum.  efficiency function will 
     // throw if it is not applicable (or virtual override doesn't 
     // exist).  In that case, turn off error message and set efficiency
     // array to zeros.
     tperr << xsverbose(9999);
     try
     {
        efficiency = detector(spectrumNumber)->efficiency();
     }
     catch (...)
     {
     }
     tperr << xsverbose();

     if (N != 0) 
     {
        RealArray convErr;
        RealArray& flux = right.uniquePhotonArray(uniqueEng);
        flux *= efficiency;

        (*generator())(uniqueEng->energy(), startValues, spectrumNumber, flux, 
                        convErr,initString());
        // divide checks possible...
        for (size_t k =0; k < N; ++k)
        {
           flux[k] = efficiency[k] > 0 ? flux[k]/efficiency[k] : 0.;
        }
        // warning: no convolution model has errors to propagate as yet. But, this
        // looks like the method to use. If there is error in the additive model
        // then fluxErrorArray has finite size. The error output from the generator
        // should be the error in the convolution convolved with the fluxArray. 
        if ( right.isError() || isError())
        {
           RealArray& rErr = right.uniquePhotonErrArray(uniqueEng);
           if (right.isError())
           {
              rErr *= efficiency;
              RealArray convolvedFluxErr(sqrt(rErr));
              RealArray secondOrderError;  // dummy.
              (*generator())(uniqueEng->energy(), startValues, spectrumNumber, 
                       convolvedFluxErr, secondOrderError, 
                       initString());
              rErr = convolvedFluxErr*convolvedFluxErr;
              for (size_t k =0; k < N; ++k)
              {
                 rErr[k] = efficiency[k] > 0 ? rErr[k]/efficiency[k] : 0.;
              }
           }
        }
     }  // end if N != 0

     ++en;
  }
  return right;  
}

// Additional Declarations
