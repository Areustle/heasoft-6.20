//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// ConComponent
#include <XSModel/Model/Component/ConComponent.h>
#include <XSModel/Model/Component/ComponentGroup.h>

#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSModel/Model/UniqueEnergy.h>
#include <typeinfo>


// Class ConComponent 

ConComponent::ConComponent(const ConComponent &right)
   : Component(right)
{
  cloneParameters(*this,right);
}

ConComponent::ConComponent (ComponentGroup* p)
        : Component(p)
{
}


ConComponent::~ConComponent()
{
  deleteParameters();
}


ConComponent* ConComponent::clone (ComponentGroup* p) const
{
  ConComponent* cloned = new ConComponent(*this);
  cloned->setParent(p);
  return cloned;    
}

void ConComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  ConComponent __temp(static_cast<const ConComponent&>(right));
  swap(__temp);   
}

SumComponent& ConComponent::operator * (SumComponent& right) const
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

  bool isErrorIntroduced=false;
  while (en != enEnd)
  {
     const UniqueEnergy* uniqueEng = *en;      
     if (right.uniquePhotonArray(uniqueEng).size()) 
     {
        RealArray convErr;
        RealArray& flux = right.uniquePhotonArray(uniqueEng);
        size_t spectrumNumber = *uniqueEng->clientSpectra().begin();

        (*generator())(uniqueEng->energy(), startValues, spectrumNumber, flux, 
                        convErr,initString());
                        
        // An ugly workaround just to set the isError flag.  This is done
        // here since ConComponents can't do anything in the calculate()
        // function (where this->isError() would normally be set).  It
        // doesn't appear necessary to even set this->isError() since
        // it's only used in this function, but we do want to keep the
        // data members up-to-date in any case.
        ConComponent* mutableThis = const_cast<ConComponent*>(this);
        convErr.size() > 0 ? mutableThis->isError(true) : mutableThis->isError(false);
        
        // If SumComponent already has an error array, it must be convolved
        //   in the same way as the flux, then added in quadtrature to
        //   the error originating in the convolution component (if any).
        if ( right.isError() || isError())
        {
          RealArray& rErr = right.uniquePhotonErrArray(uniqueEng);
          if (right.isError())
          {
             RealArray convolvedFluxErr(sqrt(rErr));
             RealArray secondOrderError;  // dummy.
             (*generator())(uniqueEng->energy(), startValues, spectrumNumber, convolvedFluxErr, 
                           secondOrderError, initString());
             rErr = convolvedFluxErr*convolvedFluxErr;
          }
          else
          {
             rErr.resize(flux.size(), 0.);
             isErrorIntroduced=true;
          }
          
          if (isError())
             rErr += convErr;
        }
     } 
     ++en;
  }
  if (isErrorIntroduced)
     right.isError(true);
     
   return right;
}

void ConComponent::calculate (bool saveComponentFlux, bool frozen)
{
  if (saveComponentFlux) saveUniquePhotonArray(saveComponentFlux);


  // do nothing else - computations done in *= operator.      
}

// Additional Declarations
