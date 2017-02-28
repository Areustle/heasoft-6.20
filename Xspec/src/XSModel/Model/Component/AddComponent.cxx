//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// typeinfo
#include <typeinfo>
// AddComponent
#include <XSModel/Model/Component/AddComponent.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSModel/Model/UniqueEnergy.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <XSstreams.h>


// Class AddComponent 

AddComponent::AddComponent(const AddComponent &right)
      :Component(right), m_normParNum(right.m_normParNum)
{
  cloneParameters(*this,right);
}

AddComponent::AddComponent (ComponentGroup* p)
      : Component(p), m_normParNum(0)
{
}


AddComponent::~AddComponent()
{
  deleteParameters();
}


int AddComponent::read ()
{
  int status = Component::read();
  // the argument is unnecessary, but clarifies here.
  addParam("norm");        
  m_normParNum = numParams() - 1;
  return status;  
}

AddComponent* AddComponent::clone (ComponentGroup* p) const
{
  AddComponent* cloned = new AddComponent(*this);
  cloned->setParent(p);
  return cloned;  
}

Real AddComponent::norm () const
{
  return parameterSet(m_normParNum)->value();
}

void AddComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  AddComponent __temp(static_cast<const AddComponent&>(right));
  swap(__temp);   
}

void AddComponent::swap (Component& right)
{
  Component::swap(right);
  AddComponent& that = static_cast<AddComponent&>(right);
  std::swap(m_normParNum,that.m_normParNum);  
}

void AddComponent::calculate (bool saveComponentFlux, bool frozen)
{

  typedef std::set<UniqueEnergy*> UniqueEnergyContainer;
  if (saveComponentFlux) saveUniquePhotonArray(saveComponentFlux);

  size_t  numParams = parameterSet().size();
  RealArray  startValues(numParams);

  bool proceed = true;
  if (frozen)
  {
        // if the 'frozen' flag is set, only compute this component if
        // the normalization is either frozen or linked only to frozen.
        size_t j = 0;
        Parameter* normalization = 0;
        while ( j < numParams )
        {
                if (parameterSet()[j]->name() == "norm")
                {
                        normalization = parameterSet()[j];
                        break;
                }  
                ++j;

        }
        if (!normalization->isFrozen() && !Parameter::isLinkedToFrozen(normalization)) 
                        proceed = false;
  }

  if (proceed)
  {
     const UniqueEnergyContainer& energies = getUniqueEnergies();
     for (size_t  i=0; i<numParams; i++)
     {
        startValues[i] = parameterSet()[i]->value();
     }
     UniqueEnergyContainer::const_iterator en   = energies.begin();
     UniqueEnergyContainer::const_iterator enEnd = energies.end();

     while (en != enEnd)
     {
        // Passing a spectrumNumber to generator only makes sense
        // if component was flagged to enforce one UniqueEnergy
        // obj for each spectrum.  Otherwise we'll assume the
        // model function will just ignore it.
        const UniqueEnergy* uniqueEng = *en;
        size_t spectrumNumber = *uniqueEng->clientSpectra().begin();
        RealArray& fluxArray = uniquePhotonArray(uniqueEng);
        RealArray& fluxErrArray = uniquePhotonErrArray(uniqueEng);
        (*generator())(uniqueEng->energy(), startValues, spectrumNumber, fluxArray,
                        fluxErrArray,initString());

        // fluxErrArray is initialized to zero in generator
        // and then resized to zero if it wasn't used.
        fluxErrArray.size() > 0  ? isError(true) : isError(false);

        // output arrays if in debug mode (harmlessly returns if log chatter
        // is < 30)
        debugPrint(tcout,"\nModel Output AddComponent::calculate() -");
        ++en;
      }
  }  
  else
  {
          const UniqueEnergyContainer& energies = getUniqueEnergies();
          UniqueEnergyContainer::const_iterator en   = energies.begin();
          UniqueEnergyContainer::const_iterator enEnd = energies.end();
          while (en != enEnd)
          {
                uniquePhotonArray(*en).resize(0);
                uniquePhotonErrArray(*en).resize(0);
                ++en;
          }
  }
}

// Additional Declarations
