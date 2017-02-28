//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

#include <XSContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/UniqueEnergy.h>
#include <XSModel/Model/Component/MixComponent.h>
#include <XSstreams.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSModel/Model/Component/MixUtility.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <typeinfo>
#include <memory>
#include <limits>


// Class MixComponent 

// MixComponents must never share shallow copies of m_mixUtility.
MixComponent::MixComponent(const MixComponent &right)
  : Component(right),
   m_mixUtility(0),
   // The isBad flag can't be set until the m_mixUtility for THIS object
   //  is created and tested.  Therefore don't copy from 'right'.
   m_isBad(false)
{
  cloneParameters(*this,right);
}

MixComponent::MixComponent (ComponentGroup* p)
     : Component(p),
      m_mixUtility(0),
      m_isBad(false)
{
}


MixComponent::~MixComponent()
{
  deleteParameters();
  if (m_mixUtility)
     delete m_mixUtility; 
}


MixComponent* MixComponent::clone (ComponentGroup* p) const
{
  MixComponent* cloned = new MixComponent(*this);
  cloned->setParent(p);
  return cloned;    
}

void MixComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  MixComponent __temp(static_cast<const MixComponent&>(right));
  swap(__temp);    
}

void MixComponent::calculate (bool saveComponentFlux, bool frozen)
{
  if (saveComponentFlux) saveUniquePhotonArray(saveComponentFlux);


  // do nothing else. calculate at the 'multiply' stage.      
}

void MixComponent::initialize (const IntegerArray& specNums)
{
  // initialize method to be called once per model container update.  
  if (m_mixUtility)
  {
     try
     {
          size_t  numParams = parameterSet().size();
          std::vector<Real>  startValues(numParams);
          for (size_t j = 0; j < numParams; ++j)
          {
                  startValues[j] = parameterSet()[j]->value();
          }

          const string& modelName = parent()->parentName();
          m_mixUtility->initialize(startValues, specNums, modelName);
     }
     catch (YellowAlert&)
     {
        delete m_mixUtility;
        m_mixUtility = 0;
        m_isBad = true;
     }
  }
}


void MixComponent::initializeForFit ()
{
  // called once per fit and only for lowest data group.
  if (root()->dataGroupNumber() == 
        XSContainer::datasets->getLowestGroupForSource(root()->sourceNumber()))
  {
     if (m_mixUtility)
     {
        try
        {
        size_t  numParams = parameterSet().size();
        bool parametersAreFrozen = true;
        std::vector<Real>  startValues(numParams);
        for (size_t j = 0; j < numParams; ++j)
        {
                startValues[j] = parameterSet()[j]->value();
                if (!parameterSet()[j]->isFrozen())
                {
                   parametersAreFrozen = false;
                }
        }

        m_mixUtility->initializeForFit(startValues, 
                              parametersAreFrozen);
        }
        catch(YellowAlert&)
        {
           delete m_mixUtility;
           m_mixUtility=0;
           m_isBad=true;
        }
     }
  }
}

void MixComponent::operator () (const std::vector<SumComponent*>& sumComps)
{
   EnergyPointer energyEnsemble;
   GroupFluxContainer allFluxes;
   GroupFluxContainer allFluxErrors;
   bool isErrors = false;
   for (size_t iSc=0; iSc<sumComps.size(); ++iSc)
   {
      SumComponent* sumComp = sumComps[iSc];
      const ModelBase* parentModel = sumComp->parent()->parent();
      energyEnsemble[parentModel->dataGroupNumber()] = &parentModel->energy();
      sumComp->fillPhotonArrays();
      allFluxes[parentModel->dataGroupNumber()] = sumComp->photonArray();
      // If empty fluxError array is detected in first spectrum of
      //   first sumComp, assume ALL are empty.
      if (iSc == 0)
      {
         isErrors = sumComp->photonErrArray().begin()->second.size();
      }
      if (isErrors)
      {
         allFluxErrors[parentModel->dataGroupNumber()] = sumComp->photonErrArray();
      }
   }
   
   size_t  numParams = parameterSet().size();
   std::vector<Real>  startValues(numParams);
   for (size_t j = 0; j < numParams; ++j)
   {
      startValues[j] = parameterSet()[j]->value();
   }

   // The m_mixUtility pointer is of type MixUtility and passes an instance
   // of a model specific class implementation to the generator function.
   const string& modelName = parent()->parentName();
   if (!m_isBad)
   {
      // If in a 'bad' state, do NOT call XSModelFunction's 'operator()()'.
      // It can ASSUME it has a valid mixUtility object, so performs no checking.
      try
      {
         (*generator())(energyEnsemble, startValues, allFluxes, allFluxErrors, m_mixUtility, modelName);
      }
      catch (YellowAlert&)
      {
         if (m_mixUtility)
         {
            delete m_mixUtility;
            m_mixUtility = 0;
            m_isBad=true;
            makeBadFlux(allFluxes);
         }
      }
   }
   else
   {
      makeBadFlux(allFluxes);
   }
   
   // Refill the SumComps with the results of the mixing operation.
   // This is similar to doing SumComponent::fillPhotonArrays in reverse.
   // NOTE: There should only ever be one spectrum per UniqueEnergy object
   //   when mix models are involved.
   for (size_t iSc=0; iSc<sumComps.size(); ++iSc)
   {
      SumComponent* sumComp = sumComps[iSc];
      const ModelBase* parentModel = sumComp->parent()->parent();
      // For each UniqueEnergy in SumComp, must reset with appropriate flux array
      //  found in allFluxes.
      UniquePhotonContainer mixedFluxes(sumComp->uniquePhotonArray());
      UniquePhotonContainer::iterator itMixed = mixedFluxes.begin();
      UniquePhotonContainer::iterator itMixedEnd = mixedFluxes.end();
      while (itMixed != itMixedEnd)
      {
         const UniqueEnergy* uniqueEng = itMixed->first;
         if (uniqueEng->clientSpectra().size() > 1)
            throw RedAlert("Energy array assignment error in MixComponent.");
         const size_t specNum = *(uniqueEng->clientSpectra().begin());
         // Naturally assume that size of flux array cannot have 
         //   changed during mix operation.
         itMixed->second = allFluxes[parentModel->dataGroupNumber()][specNum];
         ++itMixed;
      }
      sumComp->setUniquePhotonArray(mixedFluxes);
      if (isErrors)
      {
         UniquePhotonContainer mixedFluxErrors(sumComp->uniquePhotonErrArray());
         UniquePhotonContainer::iterator itMixed = mixedFluxErrors.begin();
         UniquePhotonContainer::iterator itMixedEnd = mixedFluxErrors.end();
         while (itMixed != itMixedEnd)
         {
            const UniqueEnergy* uniqueEng = itMixed->first;
            const size_t specNum = *(uniqueEng->clientSpectra().begin());
            itMixed->second = allFluxErrors[parentModel->dataGroupNumber()][specNum];
            ++itMixed;
         }
         sumComp->setUniquePhotonErrArray(mixedFluxErrors);
      }
      // UniquePhotonContainer has been reset with mixed values,
      //   now do the same for the complete photonArray container(s).
      sumComp->photonArray() = allFluxes[parentModel->dataGroupNumber()];
      if (isErrors)
      {
         sumComp->photonErrArray() = allFluxErrors[parentModel->dataGroupNumber()];
      }
   } // end SumComponent loop
}

void MixComponent::addMixUtility()
{
   // Do nothing if mix utility already exists.  It should only be removed
   //  by removeUtility or destructor.
   if (!m_mixUtility)
   {
      m_mixUtility = generator()->getUtilityObject();
      // Reset the isBad flag. Won't know if this new case is bad until
      //  data validation is performed.
      m_isBad=false;
   }
}

void MixComponent::removeMixUtility()
{
   if (m_mixUtility)
   {
      delete m_mixUtility;
      m_mixUtility = 0;
   }
}

void MixComponent::makeBadFlux(GroupFluxContainer& allFluxes)
{
   GroupFluxContainer::iterator itGroups = allFluxes.begin();
   GroupFluxContainer::iterator itGroupsEnd = allFluxes.end();
   while (itGroups != itGroupsEnd)
   {
      ArrayContainer::iterator itArrays = itGroups->second.begin();
      ArrayContainer::iterator itArraysEnd = itGroups->second.end();
      while (itArrays != itArraysEnd)
      {
         itArrays->second = std::numeric_limits<Real>::quiet_NaN();
         ++itArrays;
      }
      ++itGroups;
   }
}

// Additional Declarations
