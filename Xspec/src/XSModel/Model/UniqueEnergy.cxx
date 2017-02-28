//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// UniqueEnergy
#include <XSModel/Model/UniqueEnergy.h>
#include <XSUtil/Error/Error.h>


// Class UniqueEnergy 

UniqueEnergy::UniqueEnergy (const RealArray& energies)
  :m_energy(energies),
   m_clientSpectra(),
   m_associatedGain(),
   m_dontShare(false)
{
}


UniqueEnergy::~UniqueEnergy()
{
}


void UniqueEnergy::addClient (size_t specNum)
{
   if (m_dontShare)
   {
      throw RedAlert("Attempting to add client to a no-share UniqueEnergy.");
   }
   m_clientSpectra.insert(specNum);
}

bool UniqueEnergy::removeClient (size_t specNum)
{
   m_clientSpectra.erase(specNum);
   return m_clientSpectra.empty();
}

void UniqueEnergy::changeEnergy (const RealArray& newEnergy)
{
   // One should be very careful about using this function.  It's
   // original intended use is for shifting gain parameters during
   // a fit, and where clientSpectra should only be size of one.
   if (m_clientSpectra.size() > 1)
   {
      throw RedAlert("Cannot modify UniqueEnergy array that has more than 1 client spectrum.");      
   }
   if (m_energy.size() != newEnergy.size())
      m_energy.resize(newEnergy.size());
   m_energy = newEnergy;
}

// Additional Declarations
