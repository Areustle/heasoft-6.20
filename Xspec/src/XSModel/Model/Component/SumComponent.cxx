//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSModel/Model/Component/AddComponent.h>
#include <XSModel/Model/Component/MulComponent.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/UniqueEnergy.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSUtil/Error/Error.h>
#include <XSstreams.h>
#include <sstream>
#include <typeinfo>


// Class SumComponent 
const string SumComponent::NORM = "norm";

SumComponent::SumComponent(const SumComponent &right)
  : Component(right), 
    m_foldedPhotonFlux(right.m_foldedPhotonFlux), 
    m_foldedPhotonFluxErr(right.m_foldedPhotonFluxErr)
{
  Parameter* p (right.parameter(NORM));      
  if (p) parameterSet().push_back(p);
}

SumComponent::SumComponent (ComponentGroup* p)
  : Component(p), 
    m_foldedPhotonFlux(), 
    m_foldedPhotonFluxErr()
{
  index(0);
}

SumComponent::SumComponent (const AddComponent& right)
  : Component(right), 
    m_foldedPhotonFlux(), 
    m_foldedPhotonFluxErr()
{

  // don't want the deep copy of the AddComponent's parameters - only want a 
  // reference to the AddComponent's norm parameter.               
  // Parameter* p (right.parameter("norm"));      
  //p->setParent(componentBase()); 
  parameterSet().push_back(right.parameter(NORM));
  // DON'T assume the index number of the generating component.
  // (But, there should still be some relation between the SumComponent
  // and the AddComponent it's built on, to assist things that
  // want to use the SumComponents built by Model::makeSourceComponents
  // as opposed to the SumComponents generated during the fit
  // calculations.  Therefore, lets just multiply by minus one.
  index(-right.index());
}

SumComponent::SumComponent (const MulComponent& right)
  : Component(right), 
    m_foldedPhotonFlux(), 
    m_foldedPhotonFluxErr()
{
   // Don't take on the index of the MulComponent.  The SumComponents
   // generated during the combine algorithm should never have positive
   // indices, otherwise it can break ModelBase::bundleComponents.
   // However this CAN pick up a negative index (and set a norm parameter)
   // if/when an AddComponent (as opposed to a nested group) contributes 
   // to this subgroup.
   index(0);
}


SumComponent::~SumComponent()
{
}


SumComponent* SumComponent::clone (ComponentGroup* p) const
{
  SumComponent* cloned = new SumComponent(*this);
  cloned->setParent(p);
  return cloned;    
}

void SumComponent::calculate (bool saveComponentFlux, bool frozen)
{
  const XSModExpTree<ComponentGroup*>& master  = root()->compGroupTree();
  XSModExpTree<ComponentGroup*>::const_iterator a(master.begin());
  XSModExpTree<ComponentGroup*>::const_iterator b(master.end());

  // find the ComponentGroup with the right name.
  while (a != b && (*a)->groupFlux()->name() != name()) ++a;

  (*a)->calculate(saveComponentFlux,frozen);

  debugPrint(tcout,"\nModel Output SumComponent::calculate() ");
}

SumComponent& SumComponent::operator += (const SumComponent& right)
{
  UniquePhotonContainer::const_iterator rp = right.uniquePhotonArray().begin();
  UniquePhotonContainer::const_iterator rpEnd = right.uniquePhotonArray().end();

  bool isErrorIntroduced=false;
  while (rp != rpEnd)
  {
     const UniqueEnergy* uniqueEng = rp->first;
     const RealArray& rightHandFlux = right.uniquePhotonArray(uniqueEng);
     RealArray& leftHandFlux = uniquePhotonArray(uniqueEng);

     // rightHandFlux could be empty if for example it's coming from
     // a renorm calculation and it has a non-frozen norm param.
     if (rightHandFlux.size())
     {
        if (leftHandFlux.size() != rightHandFlux.size())
        {
              leftHandFlux.resize(rightHandFlux.size(),0.);
        }
        leftHandFlux += rightHandFlux;

        if (right.isError())
        {
              RealArray&  lErr  = uniquePhotonErrArray(uniqueEng);
              if (!isError())
              {
                  lErr.resize(rightHandFlux.size(),0.);
                  isErrorIntroduced=true;    
              }
              if (lErr.size() != right.uniquePhotonErrArray(uniqueEng).size())
              {
                 throw RedAlert("SumComponent: Error array size mismatch");
              }
              lErr += right.uniquePhotonErrArray(uniqueEng) ;
        }
        else
        {
	      // make sure photonErrArray exists even if there is
	      // no error vector, or just leave alone if there is
	      // already one
	      if (!isError()) uniquePhotonErrArray(uniqueEng).resize(0);
        }
     }
     ++rp;
  }
  if (isErrorIntroduced)
     isError(true);

  return *this;   
}

void SumComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  SumComponent __temp(static_cast<const SumComponent&>(right));
  swap(__temp);   
}

bool SumComponent::isNested () const
{
  return parent()->isNested();
}

void SumComponent::normalize ()
{

  // SumComponents with the name "SUM" are temporaries.
  static const string SUM("SUM");
  if ( name() != SUM )
  {      
     UniquePhotonContainer normFlux(uniquePhotonArray());       
     Real x (norm());
     UniquePhotonContainer::iterator i (normFlux.begin());
     UniquePhotonContainer::iterator iEnd (normFlux.end());
     while ( i != iEnd)  
     {
        (i->second) *= x;
        ++i;
     }
     setUniquePhotonArray(normFlux);

     if ( isError())
     {
        UniquePhotonContainer normFluxError (uniquePhotonErrArray());       
        UniquePhotonContainer::iterator i (normFluxError.begin());
        UniquePhotonContainer::iterator iEnd (normFluxError.end());
        Real x2 (x*x);
        while ( i != iEnd)  
        {
           (i->second) *= x2;
           ++i;
        }
        setUniquePhotonErrArray( normFluxError );
     }
  }
}

Real SumComponent::norm () const
{
  return parameter(NORM)->value();
}

void SumComponent::savePhotonArray (bool setSaveFlag)
{

  // switch this operation off for nested subgroups. Will be done in the 'children'.      
}

void SumComponent::restorePhotonArray ()
{
  // switch this operation off for nested subgroups. Will be done in the 'children'.      
}

void SumComponent::swap (Component& right)
{
  Component::swap(right);
  SumComponent& that = static_cast<SumComponent&>(right);
  std::swap(m_foldedPhotonFlux,that.m_foldedPhotonFlux);
  std::swap(m_foldedPhotonFluxErr,that.m_foldedPhotonFluxErr);
}

void SumComponent::registerSelf () const
{
  // do nothing in this version -- base class version registers parameters
  // in the Global  ModelContainer list.
}

void SumComponent::saveUniquePhotonArray (bool setSaveFlag)
{
  // switch this operation off for nested subgroups. Will be done in the 'children'.      
}

void SumComponent::restoreUniquePhotonArray ()
{
  // switch this operation off for nested subgroups. Will be done in the 'children'.      
}

void SumComponent::fillPhotonArrays ()
{
   // The purpose of this function is to copy the unique photon arrays
   // into a container where they are now 1-to-1 with spectra.  This
   // means creating duplicate arrays when more than 1 spectra make use
   // of a UniqueEnergy.

   clearPhotonArrays();

   const std::set<UniqueEnergy*>& energies = getUniqueEnergies();
   std::set<UniqueEnergy*>::const_iterator itEngs = energies.begin();
   std::set<UniqueEnergy*>::const_iterator itEngsEnd = energies.end();
   while (itEngs != itEngsEnd)
   {
      const UniqueEnergy* uniqueEng = *itEngs;
      const RealArray& uniqueFlux = uniquePhotonArray(uniqueEng);
      const RealArray& uniqueFluxErr = uniquePhotonErrArray(uniqueEng);
      std::set<size_t>::const_iterator itSpec = 
                uniqueEng->clientSpectra().begin();
      std::set<size_t>::const_iterator itSpecEnd = 
                uniqueEng->clientSpectra().end();
      while (itSpec != itSpecEnd)
      {
         size_t specNum = *itSpec;
         // Remember, this does an insert into m_photonArray.
         RealArray& insertedArray = m_photonArray[specNum];
         RealArray& insertedErrArray = m_photonErrArray[specNum];
         insertedArray.resize(uniqueFlux.size());
         insertedErrArray.resize(uniqueFluxErr.size());
         insertedArray = uniqueFlux;
         insertedErrArray = uniqueFluxErr;
         ++itSpec;
      }
      ++itEngs;
   }
}

void SumComponent::clearPhotonArrays ()
{
   m_photonArray.clear();
   m_photonErrArray.clear();
}

SumComponent& SumComponent::operator *= (const Component& right)
{
   return right*(*this);
}

SumComponent& SumComponent::operator * (SumComponent& right) const
{
  // One of these SumComponents (but not both) can have a norm 
  // parameter already set.
  if (parameter(NORM))
  {
     if (right.parameter(NORM))
       throw RedAlert("Attempting to multiply 2 AddComponents in SumComponent::operator*");
     right.parameterSet().push_back(parameter(NORM));
     right.index(index());
  }


  UniquePhotonContainer::const_iterator p    =  right.uniquePhotonArray().begin();
  UniquePhotonContainer::const_iterator pEnd =  right.uniquePhotonArray().end();

  bool isErrorIntroduced=false;
  while (p != pEnd)
  {
     // mulFactor might have size zero in some instances, such as during
     // computation of the frozen norm elements of a model during renorm.
     const UniqueEnergy* uniqueEng = p->first;
     RealArray& flux = right.uniquePhotonArray(uniqueEng);
     const RealArray& mulFactor = uniquePhotonArray(uniqueEng);

     if (flux.size() && mulFactor.size())
     {
        flux *= mulFactor;
        if ( right.isError() )
        {
           right.uniquePhotonErrArray(uniqueEng) *= (mulFactor * mulFactor);          
        }
        if (isError())
        {
           if ( !right.isError())
           {
              right.uniquePhotonErrArray(uniqueEng)
                              .resize(flux.size(),0.);
              isErrorIntroduced = true;       
           }
           right.uniquePhotonErrArray(uniqueEng) += 
                     flux*flux*uniquePhotonErrArray(uniqueEng);         
        }  
     }
     ++p;
  }
  if (isErrorIntroduced)
     right.isError(true);
  return right;
}

const RealArray& SumComponent::photonArray (size_t spectrumNumber) const
{
  ArrayContainer::const_iterator it = m_photonArray.find(spectrumNumber);
  if (it != m_photonArray.end())
  {
     return it->second;
  }
  else
  {
     std::ostringstream oss;
     oss << "SumComponent: Attempting to access non-existing spectrum " 
       << spectrumNumber << " in photon array" ;
     throw RedAlert(oss.str());
  }
}

const RealArray& SumComponent::photonErrArray (size_t spectrumNumber) const
{
  ArrayContainer::const_iterator it = m_photonErrArray.find(spectrumNumber);
  if (it != m_photonErrArray.end())
  {
     return it->second;
  }
  else
  {
     std::ostringstream oss;
     oss << "SumComponent: Attempting to access non-existing spectrum " 
       << spectrumNumber << " in photon error array" ;
     throw RedAlert(oss.str());
  }
}

// Additional Declarations
