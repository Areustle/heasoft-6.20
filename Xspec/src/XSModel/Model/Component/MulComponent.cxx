//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// ComponentGroup
#include <XSModel/Model/Component/ComponentGroup.h>
// typeinfo
#include <typeinfo>
// MulComponent
#include <XSModel/Model/Component/MulComponent.h>

#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSModel/Model/UniqueEnergy.h>
#include "XSstreams.h"


// Class MulComponent 

MulComponent::MulComponent(const MulComponent &right)
      : Component(right)
{
  cloneParameters(*this,right);
}

MulComponent::MulComponent (ComponentGroup* p)
         : Component(p)
{
}


MulComponent::~MulComponent()
{
  deleteParameters();
}


SumComponent& MulComponent::operator * (SumComponent& right) const
{
  std::set<UniqueEnergy*>::const_iterator rp = getUniqueEnergies().begin();
  std::set<UniqueEnergy*>::const_iterator rpEnd = getUniqueEnergies().end();

  bool isErrorIntroduced=false;
  while (rp != rpEnd)
  {
     const UniqueEnergy* uniqueEng = *rp;
     const RealArray& model = uniquePhotonArray(uniqueEng);
     if (model.size())
     {      
        RealArray& flux = right.uniquePhotonArray(uniqueEng);
        // flux may be size zero if this is only calculating the
        // components with frozen norms during a renorm operation.
        if (flux.size())
        {
           flux *= model;

           if ( right.isError() || isError())
           {
              RealArray& fluxError = right.uniquePhotonErrArray(uniqueEng);
              const RealArray& mulFactor = model;

              if ( right.isError())
              {
                 fluxError *= (mulFactor * mulFactor);          
              }
              if (isError())
              {
                 if ( !right.isError())
                 {
                    fluxError.resize(flux.size(),0.);
                    isErrorIntroduced = true;       
                 }
                 fluxError += flux*flux*uniquePhotonErrArray(uniqueEng);         
              }

           }
        }
     }
     ++rp;
  }
  if (isErrorIntroduced)
     right.isError(true);
     
   return right;
}

MulComponent* MulComponent::clone (ComponentGroup* p) const
{
  MulComponent* cloned = new MulComponent(*this);
  cloned->setParent(p);
  return cloned;    
}

void MulComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  MulComponent __temp(static_cast<const MulComponent&>(right));
  swap(__temp);   
}

void MulComponent::calculate (bool saveComponentFlux, bool frozen)
{
  if (saveComponentFlux) saveUniquePhotonArray(saveComponentFlux);


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
     const UniqueEnergy* uniqueEng = *en;      
     size_t spectrumNumber = *uniqueEng->clientSpectra().begin();
     RealArray& fluxArray = uniquePhotonArray(uniqueEng);
     RealArray& fluxErrArray = uniquePhotonErrArray(uniqueEng);

     (*generator()) (uniqueEng->energy(), startValues, spectrumNumber,
              fluxArray, fluxErrArray,initString());


     // fluxErrArray is initialized to zero in generator
     // and then resized to zero if it wasn't used.
     fluxErrArray.size() > 0  ? isError(true) : isError(false);

     debugPrint(tcout,"\nModel Output MulComponent::calculate() ");

     ++en;
  }
}

// Additional Declarations
