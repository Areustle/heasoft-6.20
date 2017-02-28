//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Utils/IosHolder.h>

// Component
#include <XSModel/Model/Component/Component.h>
// UniqueEnergy
#include <XSModel/Model/UniqueEnergy.h>
// Parameter
#include <XSModel/Parameter/Parameter.h>
// TableComponent
#include <XSModel/Model/Component/TableComponent.h>

#include <CCfits/CCfits>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <typeinfo>


// Class TableComponent::ParameterRangeError 

TableComponent::ParameterRangeError::ParameterRangeError (const string& diag)
: YellowAlert(" Table Parameter Value out of range")
{
        *IosHolder::errHolder() << diag << '\n';
}


// Class TableComponent::WrongTableType 

TableComponent::WrongTableType::WrongTableType (const string& diag)
   : YellowAlert(" Table Model Type incorrect ")             
{
  *IosHolder::errHolder() << diag << '\n';
}


// Class TableComponent 

std::map<const char*,TableComponent*> TableComponent::s_formats;

TableComponent::TableComponent(const TableComponent &right)
      : m_filename(right.m_filename),
        m_name(right.m_name),
        m_numEngVals(right.m_numEngVals),
        m_numIntPar(right.m_numIntPar),
        m_numAddPar(right.m_numAddPar),
        m_isError(right.m_isError),
        m_engLow(right.m_engLow),
        m_engHigh(right.m_engHigh),
        m_params(),
        m_parent(0)
{
   // m_params and m_parent must be set from the subclass clone function.
}

TableComponent::TableComponent (const string& nameString, Component* p)
      : m_filename(""),
        m_name(""),
        m_numEngVals(0),
        m_numIntPar(0),
        m_numAddPar(0),
        m_isError(false),
        m_engLow(),
        m_engHigh(),
        m_params(),
        m_parent(p)
{
}


TableComponent::~TableComponent()
{
  // If this has a Component parent, that will be responsible for
  // cleaning up Parameters.  If not, then this is a stand-alone table
  // object presumably created by a TableModel wrapper function, and
  // it is the owner of the Parameter objects.
   if (!m_parent)
   {
      std::vector<Parameter*>::iterator itP (m_params.begin());
      std::vector<Parameter*>::iterator itEnd(m_params.end());
      while (itP != itEnd)
      {
              delete *itP;
              ++itP;
      } 
   }
}


void TableComponent::interpolate (const UniqueEnergy* uniqueEng, RealArray& spectrum, RealArray& variance, bool rebin, bool exponential)
{
  RealArray componentSpectrum;
  RealArray componentVariance;

  // interpolate in parameter space and return the arrays for spectrum and variance.

  getInterpolant(componentSpectrum, componentVariance);            

  // Set up arrays for interpolating in energy.  For most cases 
  // energyWeights only needs to be performed during the 
  // Data/Model/Fit update chain, which is why it is called from 
  // owning Component's initializeForFit function.  However if 
  // energy arrays can be altered during fit iterations (ie. from 
  // gain parameters or the redshift z param), it must be performed 
  // here as well.

  // We know that ALL uniqueEnergies coming from responses with gain params
  // are flagged with dontShare=true.  Also all energies flagged
  // true must come from gain params, though this may not always be
  // the case in the future. 
  if (uniqueEng->dontShare())
           energyWeights(uniqueEng);
  else
  {
     // Note that we still call energyWeights even if the redshift z
     // happens to be a frozen parameter -- it could be coming
     // from a steppar run.  Steppar freezes the parameter as it
     // iterates through its range, but it doesn't trigger the entire
     // model/fit update chain each time it increments the parameter value
     // (that would be overkill in all other cases).
     XSutility::MatchPtrName<Parameter> matchName;
     std::vector<Parameter*>::iterator itRed =
                      std::find_if(params().begin(),params().end(),bind2nd(matchName,"z"));
     if (itRed != params().end())
     { 
        energyWeights(uniqueEng);
     }
  }

  // rebin is for additive components, interpolate for multiplicative.

  if ( rebin )
  {          
        rebinComponent(uniqueEng,componentSpectrum,spectrum);
        if ( m_isError ) 
        {
                rebinComponent(uniqueEng,componentVariance,variance,m_isError); 
        } 
  }      
  else
  {
        interpolateComponent(uniqueEng,componentSpectrum,spectrum,exponential);
        if ( m_isError ) 
        {
                interpolateComponent(uniqueEng,componentVariance,variance); 

                // complete variance computation for exponential tables.

                if ( exponential ) variance *= spectrum*spectrum;              
        } 
  }
}

TableComponent* TableComponent::returnTable (string& fileName)
{
   TableComponent* tablePtr (0);

   // First simply check whether the file even exists, and re-prompt
   // if it doesn't.
   try 
   {
      CCfits::FITS fileTest(fileName);
   }
   catch (CCfits::FitsException&)
   {
      string newName;
      XSparse::getFileNameFromUser(fileName, newName, XSutility::NONE);
      fileName = newName;
   }

   std::map<const char*, TableComponent*>::iterator it = s_formats.begin();
   std::map<const char*, TableComponent*>::iterator itEnd = s_formats.end();
   while(it != itEnd ) 
   {
       TableComponent* ptr = it->second;

       if ( ptr->formatCheck(fileName) ) 
       {
	   tablePtr = ptr;
           break;
       }
       ++it;
   }
   if ( it == itEnd )
   {
       throw YellowAlert("Unknown table format: check identifiers\n");        
   }

   // tablePtr's Component parent will be set at a later stage.
   return tablePtr->clone(0);
}

void TableComponent::registerTableFormat (TableComponent* format)
{
    s_formats[typeid(*format).name()] = format;
}

const std::vector<Parameter*>& TableComponent::parameters ()
{

  return m_params;
}

void TableComponent::energyWeights (const UniqueEnergy* uniqueEng)
{
}

void TableComponent::isError (bool value)
{

  m_isError = value;
}

const std::vector<Parameter*>& TableComponent::params () const
{

  return m_params;
}

void TableComponent::clearArrays (const std::set<UniqueEnergy*>& currentEngs)
{
}

void TableComponent::clearTableFormats ()
{
   std::map<const char*,TableComponent*>::iterator itTf = s_formats.begin();
   std::map<const char*,TableComponent*>::iterator itEnd = s_formats.end();
   while (itTf != itEnd)
   {
      delete itTf->second;
      ++itTf;
   }
   s_formats.clear();
}

void TableComponent::setParamPointersFromCopy ()
{
   // A helper function needed during copy construction only.  For
   // the original construction (or copy from prototype), 
   // the read function collects th owning Component object pointers.
   // This ASSUMES all of m_parent's parameters come from the table
   // except the norm parameter, and that they have already been
   // cloned by this point.
   // Still need to check m_parent.  If the calling clone function
   // is coming from returnTable and not a copy ctor, this will be 0.
   if (m_parent)
   {
      std::vector<Parameter*>::const_iterator itParam = 
           const_cast<const Component*>(m_parent)->parameterSet().begin();
      std::vector<Parameter*>::const_iterator itEnd = 
           const_cast<const Component*>(m_parent)->parameterSet().end();
      while (itParam != itEnd)
      {
         Parameter* par = *itParam;
         if (par->name() != "norm")
            m_params.push_back(par);
         ++itParam;
      }
   }
}

// Additional Declarations
