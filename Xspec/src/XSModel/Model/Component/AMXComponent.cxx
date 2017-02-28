//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SpectralData
#include <XSModel/Data/SpectralData.h>
// Response
#include <XSModel/Data/Detector/Response.h>
// AMXComponent
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/Component/AMXComponent.h>
#include <XSstreams.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSModel/Model/Component/MixUtility.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <typeinfo>
#include <memory>


// Class AMXComponent 

// AMXComponents must never share shallow copies of m_mixUtility.
AMXComponent::AMXComponent(const AMXComponent &right)
  : Component(right),
   m_mixUtility(0)
{
  cloneParameters(*this,right);
}

AMXComponent::AMXComponent (ComponentGroup* p)
  : Component(p),
   m_mixUtility(0)
{
}


AMXComponent::~AMXComponent()
{
  deleteParameters();
  if (m_mixUtility)
     delete m_mixUtility;
}


AMXComponent* AMXComponent::clone (ComponentGroup* p) const
{
  AMXComponent* cloned = new AMXComponent(*this);
  cloned->setParent(p);
  return cloned;    
}

void AMXComponent::copy (const Component& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  AMXComponent __temp(static_cast<const AMXComponent&>(right));
  swap(__temp);    
}

const Response* AMXComponent::detector (size_t spectrumNumber) const
{
  return root()->detector(spectrumNumber);
}

void AMXComponent::calculate (bool saveComponentFlux, bool frozen)
{
  if (saveComponentFlux) saveUniquePhotonArray(saveComponentFlux);


  // do nothing else. calculate at the 'multiply' stage.      
}

void AMXComponent::initialize ()
{
  // initialize method to be called once per model container update.  
  if (m_mixUtility)
  {
     IntegerArray specNums;
     size_t  numParams = parameterSet().size();
     std::vector<Real>  startValues(numParams);
     for (size_t j = 0; j < numParams; ++j)
     {
        startValues[j] = parameterSet()[j]->value();
     }

     const string& modelName = parent()->parentName();
     m_mixUtility->initialize(startValues, specNums, modelName);
  }
}

void AMXComponent::perform (const EnergyPointer& energy, GroupFluxContainer& flux, GroupFluxContainer& fluxError)
{

   // this perform method must pass information from the SumComponent to be massaged
   // by the implementation of the mixing model component.

   // parameterValues
   size_t  numParams = parameterSet().size();
   std::vector<Real>  startValues(numParams);
   for (size_t j = 0; j < numParams; ++j)
   {
      startValues[j] = parameterSet()[j]->value();
   }

   const string& modelName = parent()->parentName();
   try
   {
     // need to multiply flux and fluxError by the efficiency. Define an iterator
     // f with f->second being an ArrayContainer of all the flux arrays for the data
     // group f->first.
     GroupFluxContainer::iterator f(flux.begin());
     GroupFluxContainer::iterator fEnd(flux.end());
     while ( f != fEnd ) {
       ArrayContainer::iterator s(f->second.begin());
       ArrayContainer::iterator sEnd(f->second.end());
       while ( s != sEnd ) {
	 size_t specNum = s->first;
	 s->second *= (detector(specNum)->efficiency());
	 s++;
       }
       f++;
     }
     // now repeat for the errors if they have been calculated
     f = fluxError.begin();
     fEnd = fluxError.end();
     while ( f != fEnd ) {
       ArrayContainer::iterator s(f->second.begin());
       ArrayContainer::iterator sEnd(f->second.end());
       while ( s != sEnd ) {
	 size_t specNum = s->first;
	 if ( s->second.size() > 0 ) s->second *= (detector(specNum)->efficiency());
	 s++;
       }
       f++;
     }

     // this is the bit that actually runs the mixing model

     (*generator())(energy, startValues, flux, fluxError, m_mixUtility, modelName);

     // now divide the result by the efficiency
     f= flux.begin();
     fEnd = flux.end();
     while ( f != fEnd ) {
       ArrayContainer::iterator s(f->second.begin());
       ArrayContainer::iterator sEnd(f->second.end());
       while ( s != sEnd ) {
	 size_t specNum = s->first;
	 RealArray efficiency(s->second.size());
	 efficiency = detector(specNum)->efficiency();
	 for (size_t i=0; i<efficiency.size(); i++) {
	   s->second[i] = efficiency[i] > 0 ? s->second[i]/efficiency[i] : 0.0;
	 }
	 s++;
       }
       f++;
     }
     // and repeat for the errors if they have been calculated
     f = fluxError.begin();
     fEnd = fluxError.end();
     while ( f != fEnd ) {
       ArrayContainer::iterator s(f->second.begin());
       ArrayContainer::iterator sEnd(f->second.end());
       while ( s != sEnd ) {
	 size_t specNum = s->first;
	 if ( s->second.size() > 0 ) {
	   RealArray efficiency(s->second.size());
	   efficiency = detector(specNum)->efficiency();
	   for (size_t i=0; i<efficiency.size(); i++) {
	     s->second[i] = efficiency[i] > 0 ? s->second[i]/efficiency[i] : 0.0;
	   }
	 }
	 s++;
       }
       f++;
     }

   }
   catch (...)
   {
      if (m_mixUtility)
      {
         delete m_mixUtility;
         m_mixUtility = 0;
      }
      throw;
   }
}

void AMXComponent::initializeForFit ()
{
  // called once per fit and only for first data group.
  if (root()->dataGroupNumber() == 1)
  {
     if (m_mixUtility)
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
  }
}

// Additional Declarations
