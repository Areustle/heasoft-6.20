//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// TableComponent
#include <XSModel/Model/Component/TableComponent.h>
// MulTableComponent
#include <XSModel/Model/Component/MulTableComponent.h>
#include <XSModel/Model/Component/ComponentGroup.h>

#include <XSModel/Model/UniqueEnergy.h>
#include <XSUtil/Parse/XSparse.h>
#include "XSstreams.h"
#include <typeinfo>


// Class MulTableComponent 

MulTableComponent::MulTableComponent(const MulTableComponent &right)
      : MulComponent(right), 
        m_tableFile(right.m_tableFile), 
        m_tableType(right.m_tableType),
        m_table(0),
        m_expTable(right.m_expTable)
{
   if (right.m_table) m_table = right.m_table->clone(this);
}

MulTableComponent::MulTableComponent (ComponentGroup* p)
      : MulComponent(p), 
        m_tableFile(""), 
        m_tableType(""), 
        m_table(0),
        m_expTable(false)
{
}


MulTableComponent::~MulTableComponent()
{
  delete m_table;
}


void MulTableComponent::calculate (bool saveComponentFlux, bool frozen)
{
  if (saveComponentFlux) saveUniquePhotonArray(saveComponentFlux);


  const std::set<UniqueEnergy*>& energies = getUniqueEnergies();
  std::set<UniqueEnergy*>::const_iterator en   = energies.begin();
  std::set<UniqueEnergy*>::const_iterator enEnd = energies.end();
  // this flag is true for additive models - it means accumulate the
  // total flux in the energy bins as defined by uniqueEng. The converse
  // is where we want to "interpolate" the model, producing an array which
  // is scaled to the bin width - for multiplicative models.  
  const static bool interpolateSpectrum(false);
  while (en != enEnd)
  {
     const UniqueEnergy* uniqueEng = *en;
     const RealArray& energy = uniqueEng->energy();      
     uniquePhotonArray(uniqueEng).resize(energy.size()-1);
     if ( m_table->isError()) 
     {
        uniquePhotonErrArray(uniqueEng).resize(energy.size()-1);
        isError(true);
     }


     m_table->interpolate(uniqueEng,uniquePhotonArray(uniqueEng),
                 uniquePhotonErrArray(uniqueEng),interpolateSpectrum, m_expTable);

     // output arrays if in debug mode (harmlessly returns if log chatter
     // is < 30)

     debugPrint(tcout,"\nModel Output MulTableComponent::calculate() -");
     ++en;

  }
}

int MulTableComponent::read ()
{
    string modelName (Component::currentModelName());

    // set exponential table flag, used in calculate.    
    m_expTable = (modelName[0] == 'e');
    XSparse::returnDelimitedArgument(modelName," {");
    m_tableFile = XSparse::returnDelimitedArgument(modelName,"}");
    m_table = TableComponent::returnTable(m_tableFile);
    m_table->filename(m_tableFile);

    bool readNow(true);    
    m_table->read(readNow,this);
    name(m_table->name());
    const std::vector<Parameter*>& tableParams = m_table->parameters();
    parameterSet().reserve(tableParams.size());
    std::vector<Parameter*>::const_iterator p = tableParams.begin();
    std::vector<Parameter*>::const_iterator pEnd = tableParams.end();
    while ( p != pEnd )
    {
            (*p)->setParent(this);
            (*p)->index(incrementModelParameterCount());
            parameterSet().push_back(*p);
            ++p;                     
    }
    numParams(tableParams.size());

    return 0;
}

void MulTableComponent::swap (Component& right)
{
  Component::swap(right);
  MulTableComponent& that = static_cast<MulTableComponent&>(right);
  std::swap(m_tableFile,that.m_tableFile);
  std::swap(m_tableType,that.m_tableType);
  std::swap(m_table,that.m_table);    
}

MulTableComponent* MulTableComponent::clone (ComponentGroup* p) const
{
  MulTableComponent* cloned = new MulTableComponent(*this);
  cloned->setParent(p);
  return cloned;    
}

void MulTableComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  MulTableComponent __temp(static_cast<const MulTableComponent&>(right));
  swap(__temp);   
}

void MulTableComponent::clearArrays (const std::set<UniqueEnergy*>& currentEngs)
{
  Component::clearArrays(currentEngs);
  m_table->clearArrays(currentEngs);
}

void MulTableComponent::initializeForFit ()
{
   const std::set<UniqueEnergy*>& energies = getUniqueEnergies();
   std::set<UniqueEnergy*>::const_iterator itEng   = energies.begin();
   std::set<UniqueEnergy*>::const_iterator itEngEnd = energies.end();
   while (itEng != itEngEnd)
   {
      const UniqueEnergy* uniqueEng = *itEng;
      m_table->energyWeights(uniqueEng);
      ++itEng;
   }
}

// Additional Declarations
