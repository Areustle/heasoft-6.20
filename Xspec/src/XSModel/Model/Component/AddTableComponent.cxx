//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// TableComponent
#include <XSModel/Model/Component/TableComponent.h>
// AddTableComponent
#include <XSModel/Model/Component/AddTableComponent.h>
#include <XSModel/Model/Component/ComponentGroup.h>

#include <XSModel/Model/UniqueEnergy.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSstreams.h>
#include <XSUtil/Error/Error.h>
#include <iostream>
#include <typeinfo>


// Class AddTableComponent 

AddTableComponent::AddTableComponent(const AddTableComponent &right)
      : AddComponent(right), 
        m_tableFile(right.m_tableFile), 
        m_tableType(right.m_tableType),
        m_table(0)
{
  if (right.m_table) m_table = right.m_table->clone(this);
}

AddTableComponent::AddTableComponent (ComponentGroup* p)
      : AddComponent(p), 
        m_tableFile(""), 
        m_tableType(""), 
        m_table(0)
{
}


AddTableComponent::~AddTableComponent()
{
  delete m_table;
}


void AddTableComponent::swap (Component& right)
{
  AddComponent::swap(right);
  AddTableComponent& that = static_cast<AddTableComponent&>(right);
  std::swap(m_tableFile,that.m_tableFile);
  std::swap(m_tableType,that.m_tableType);
  std::swap(m_table,that.m_table);        
}

int AddTableComponent::read ()
{

    string modelName = Component::currentModelName();
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
    addParam("norm");
    numParams(tableParams.size()+1);
    normParNum(numParams()-1);

    return 0;
}

void AddTableComponent::calculate (bool saveComponentFlux, bool frozen)
{

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
          // this flag is true for additive models - it means accumulate the
          // total flux in the energy bins as defined by uniqueEng. The converse
          // is where we want to "interpolate" the model, producing an array which
          // is scaled to the bin width - for multiplicative models.
          const static bool rebinSpectrum(true);
          const std::set<UniqueEnergy*>& energies = getUniqueEnergies();
          std::set<UniqueEnergy*>::const_iterator en   = energies.begin();
          std::set<UniqueEnergy*>::const_iterator enEnd = energies.end();

          while (en != enEnd)
          {
                const UniqueEnergy* uniqueEng = *en;
                const RealArray& energy = uniqueEng->energy();
                uniquePhotonArray(uniqueEng).resize(energy.size()-1);
                if ( m_table->isError()) 
                {
                        isError(true);
                        uniquePhotonErrArray(uniqueEng).resize(energy.size()-1);
                }
                else
                {
                        uniquePhotonErrArray(uniqueEng).resize(0);       
                }
                m_table->interpolate(uniqueEng,uniquePhotonArray(uniqueEng),
                                                      uniquePhotonErrArray(uniqueEng),rebinSpectrum);

                // time dilation. Check for a redshift parameter and apply redshifts to
                // the energy and variance array.

                XSutility::MatchPtrName<Parameter> matchName;

                std::vector<Parameter*>::const_iterator r =
                        find_if(parameterSet().begin(),parameterSet().end(),bind2nd(matchName,"z"));

                Real z (0);      
                if ( r != parameterSet().end() ) z = (*r)->value();

                if ( z > -1.0 ) 
                {
                        Real zf (1/(1+z));
                        uniquePhotonArray(uniqueEng) *= zf;
                        uniquePhotonErrArray(uniqueEng) *= zf*zf;
                }

                // output arrays if in debug mode (harmlessly returns if log chatter
                // is < 30)

                debugPrint(tcout,"\nModel Output AddTableComponent::calculate() -");
                ++en;
           }




  }  
  else
  {
          const std::set<UniqueEnergy*>& energies = getUniqueEnergies();
          std::set<UniqueEnergy*>::const_iterator en   = energies.begin();
          std::set<UniqueEnergy*>::const_iterator enEnd = energies.end();
          while (en != enEnd)
          {
                uniquePhotonArray(*en).resize(0);
                uniquePhotonErrArray(*en).resize(0);
                ++en;
          }
  }
}

void AddTableComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  AddTableComponent __temp(static_cast<const AddTableComponent&>(right));
  swap(__temp);   
}

AddTableComponent* AddTableComponent::clone (ComponentGroup* p) const
{
  AddTableComponent* cloned = new AddTableComponent(*this);
  cloned->setParent(p);
  return cloned;    
}

void AddTableComponent::clearArrays (const std::set<UniqueEnergy*>& currentEngs)
{
  Component::clearArrays(currentEngs);
  m_table->clearArrays(currentEngs);
}

void AddTableComponent::initializeForFit ()
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
