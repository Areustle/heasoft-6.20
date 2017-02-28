//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/Data/Detector/MultiResponse.h>
#include <XSModel/Data/Detector/RealResponse.h>
#include <XSModel/Data/Detector/UserDummyResponse.h>
#include <XSUtil/Error/Error.h>
#include <cmath>
#include <sstream>

// UniqueEnergy
#include <XSModel/Model/UniqueEnergy.h>
// UniqueEnergyManager
#include <XSModel/Model/UniqueEnergyManager.h>



// Class UniqueEnergyManager 

UniqueEnergyManager::UniqueEnergyManager ()
  : m_uniqueEnergies(),
    m_specNumLookup(),
    m_rmfNameLookup()
{
}


UniqueEnergyManager::~UniqueEnergyManager()
{
   clearAll();
}


bool UniqueEnergyManager::addRespEnergy (const Response* response, bool isSpecDependent)
{
   StringArray rmfNames;
   findRmfNames(response, rmfNames);
   const std::vector<Real>& gain = response->gainFactor();
   bool isGainFit = response->getConstGain() || response->getLinearGain();
   UniqueEnergy* uniqueEnergy = 0;
   // If isSpecDependent is true, 1 or more model components
   // require that every spectrum be treated individually, and
   // therefore ALL will have uniqueEnergies created here.
   // If ifGainFit is true, this response will get its own
   // UniqueEnergy and it won't ever share it with others.
   if (!isSpecDependent && !isGainFit)
   {
      uniqueEnergy = checkIfUnique(response->energies(), rmfNames, gain);
   }
   bool isUnique = !uniqueEnergy;
   if (isUnique)
   {      
      const RealArray& energy = response->energies();
      uniqueEnergy = new UniqueEnergy(energy);
      uniqueEnergy->associatedGain(gain);
      m_uniqueEnergies.insert(uniqueEnergy);
      // This is the easier case.  We know we're adding a new
      // UniqueEnergy obj, so each rmf name will become a new
      // entry into the m_rmfNameLookup table.  This is true even 
      // if the same rmf name is already in there, pointing to 
      // another UniqueEnergy obj.  That's why it's a multimap.
      NameLookup::mapped_type newEntry(1, uniqueEnergy);
      for (size_t i=0; i<rmfNames.size(); ++i)
      {
         m_rmfNameLookup.insert(NameLookup::value_type(rmfNames[i],
                newEntry));
      }      
   }
   else
   {
      // Even though there's already a UniqueEnergy obj for this
      // energy, one or more of its rmf names may still be new 
      // to the m_rmfNameLookup table.  Need to check each one.
      for (size_t i=0; i<rmfNames.size(); ++i)
      {
         bool isNewNameEntry = true;
         std::pair<NameLookup::iterator, NameLookup::iterator> rmfRange =
                m_rmfNameLookup.equal_range(rmfNames[i]);
         NameLookup::iterator itRmfName = rmfRange.first;
         while (itRmfName != rmfRange.second)
         {
            if (itRmfName->second.second == uniqueEnergy)
            {
               // increment the count for this name and UniqueEnergy*.
               ++itRmfName->second.first;
               isNewNameEntry = false;
               break;
            }
            ++itRmfName;
         }
         if (isNewNameEntry)
         {
            NameLookup::mapped_type newEntry(1, uniqueEnergy);
            m_rmfNameLookup.insert(NameLookup::value_type(rmfNames[i],
                   newEntry));
         }         
      }
   }
   size_t specNum = response->spectrumNumber();
   uniqueEnergy->addClient(specNum);
   if (isGainFit)
      uniqueEnergy->dontShare(true);
   SpecNumMap::value_type newLookup(specNum, uniqueEnergy);
   std::pair<SpecNumMap::iterator,bool> testInsert = 
        m_specNumLookup.insert(newLookup);
   if (!testInsert.second)
   {
      // There should never be an already existing specNum key in
      // map if we're about to add a UniqueEnergy* to it.  It should
      // have been removed prior to this with removeRespEnergy.
      std::ostringstream msg;
      msg << "Duplicate entries of spectrum " << specNum 
        << " in UniqueEnergyManager map.";
      throw RedAlert(msg.str());
   }     
   return isUnique;
}

void UniqueEnergyManager::removeRespEnergy (const Response* response)
{
   size_t specNum = response->spectrumNumber();
   SpecNumMap::iterator doomed = 
        m_specNumLookup.find(specNum);
   if (doomed == m_specNumLookup.end())
   {
      std::ostringstream msg;
      msg <<"Attempting to remove non-existing spectrum " << specNum
         << " from UniqueEnergyManager map.";
      throw RedAlert(msg.str());
   }
   UniqueEnergy* uniqueEnergy = doomed->second;
   // Erasing doomed from m_specNumLookup doesn't necessarily
   // mean we're going to delete the UniqueEnergy obj that 
   // doomed->second points to.  That will be done below only 
   // if this was the last specNum using it.
   m_specNumLookup.erase(doomed);
   bool isLast = uniqueEnergy->removeClient(specNum);

   StringArray rmfNames;
   findRmfNames(response, rmfNames);
   for (size_t i=0; i<rmfNames.size(); ++i)
   {
      std::pair<NameLookup::iterator, NameLookup::iterator> rmfRange =
             m_rmfNameLookup.equal_range(rmfNames[i]);
      NameLookup::iterator itRmfName = rmfRange.first;
      while (itRmfName != rmfRange.second)
      {
         if (itRmfName->second.second == uniqueEnergy)
         {
            --itRmfName->second.first;
            if (itRmfName->second.first == 0)
            {
               // This of course invalidates itRmfName, but 
               // that's OK since we're breaking from the loop
               // without using it again.
               m_rmfNameLookup.erase(itRmfName);
            }
            else if (isLast)
            {
               // If this was last spec using UniqueEnergy, it had
               // better also be the last using this rmfName entry.
               // Otherwise something is seriously broken.
               string errMsg("Programmer error: Dangling rmfNameLookup pointer.");
               throw RedAlert(errMsg);
            }
            break;
         }
         ++itRmfName;
      }
   }

   if (isLast)
   {
      m_uniqueEnergies.erase(uniqueEnergy);
      delete uniqueEnergy;
   } 
}

UniqueEnergy* UniqueEnergyManager::checkIfUnique (const RealArray& energies, const StringArray& rmfNames, const std::vector<Real>& gainFactor)
{
   static const size_t CUTOFF = 500;
   static const Real INSIGNF = 1.0e-35;
   UniqueEnergy* uniqueEnergy = 0;
   std::set<UniqueEnergy*>::iterator itUEnergy = m_uniqueEnergies.begin();
   std::set<UniqueEnergy*>::iterator itUEnergyEnd = m_uniqueEnergies.end();
   // First try a shortcut: if ANY of rmfNames matches any rmf names already
   // stored, AND gain settings are the same, then ASSUME energies are the same.
   bool isFound = false;
   for (size_t i=0; i<rmfNames.size() && !isFound; ++i)
   {
      std::pair<NameLookup::iterator, NameLookup::iterator> rmfRange =
             m_rmfNameLookup.equal_range(rmfNames[i]);
      NameLookup::iterator itRmfName = rmfRange.first;
      while (itRmfName != rmfRange.second)
      {
         UniqueEnergy* testEnergy = itRmfName->second.second;
         if (!testEnergy->dontShare())
         {
            if (gainFactor.size() == testEnergy->associatedGain().size())
            {
               bool gainMatch = true;
               for (size_t j=0; j<gainFactor.size(); ++j)
               {
                  if (std::fabs(gainFactor[j] - testEnergy->associatedGain()[j]) 
                           > INSIGNF)
                  {
                     gainMatch = false;
                     break;
                  } 
               }
               if (gainMatch)
               {
                  uniqueEnergy = testEnergy;
                  isFound = true;
                  break;
               }
            }
         }
         ++itRmfName;
      }
   }

   if (!isFound && m_uniqueEnergies.size() < CUTOFF)
   {
      itUEnergy = m_uniqueEnergies.begin();
      size_t nEngs = energies.size();
      while (!isFound && itUEnergy != itUEnergyEnd)
      {
         if (!(*itUEnergy)->dontShare())
         {
            const RealArray& storedEngs = (*itUEnergy)->energy();
            if (nEngs == storedEngs.size())
            {
               size_t i=0;
               while (i<nEngs)
               {
                  if (std::fabs(energies[i] - storedEngs[i]) > INSIGNF)
                     break;
                  ++i;
               }
               if (i == nEngs)
               {
                  isFound = true;
                  uniqueEnergy = *itUEnergy;
               }
            }
         }
         ++itUEnergy;
      }
   }
   return uniqueEnergy;
}

void UniqueEnergyManager::findRmfNames (const Response* response, StringArray& rmfNames)
{
   rmfNames.clear();
   // If response is Real/Multi or diagrsp UserDummy, we can use a
   // shortcut to detect duplicate by comparing RMF file names.
   // Otherwise, no such luck.
   if (const RealResponse* rResponse = response->toRealResponse())
   {
      rmfNames.push_back(rResponse->rmfName());
   }
   else if (const MultiResponse* mResponse = response->toMultiResponse())
   {
      rmfNames = mResponse->rmfNames();
   }
   else if (const UserDummyResponse* dResponse = 
                response->toUserDummyResponse())
   {
      if (dResponse->diagRspMode())
      {
         if (const RealResponse* rResponse = response->toRealResponse())
         {
            rmfNames.push_back(rResponse->rmfName());
         }
         else if (const MultiResponse* mResponse = response->toMultiResponse())
         {
            rmfNames = mResponse->rmfNames();
         }
      }
   }
}

void UniqueEnergyManager::clearAll ()
{
   m_specNumLookup.clear();
   m_rmfNameLookup.clear();
   std::set<UniqueEnergy*>::iterator itEnergy = m_uniqueEnergies.begin();
   std::set<UniqueEnergy*>::iterator itEnergyEnd = m_uniqueEnergies.end();
   while (itEnergy != itEnergyEnd)
   {
      delete *itEnergy;
      ++itEnergy;
   }
   m_uniqueEnergies.clear();
}

void UniqueEnergyManager::updateEnergyFromResponse (const Response* response)
{
   size_t specNum = response->spectrumNumber();
   SpecNumMap::iterator itEnergy = m_specNumLookup.find(specNum);
   if (itEnergy == m_specNumLookup.end())
      throw RedAlert("Missing unique energy in manager container.");
   // Careful when calling this.  It better have only 1 client
   // spectrum or it will throw.
   itEnergy->second->changeEnergy(response->energies());
}

// Additional Declarations
