//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Model
#include <XSModel/Model/Model.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// ResponseParam
#include <XSModel/Parameter/ResponseParam.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>
// Response
#include <XSModel/Data/Detector/Response.h>

#include <XSsymbol.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Parameter/ParamLinkList.h>
#include <limits>


// Class Response::InvalidBins 

Response::InvalidBins::InvalidBins (const string& message)
  : YellowAlert(" invalid bins - ")
{
    tcerr << message << std::endl;
}


// Class Response::InvalidRequest 

Response::InvalidRequest::InvalidRequest()
  : YellowAlert(" request not supported by this  response type.\n")
{
}


// Class Response::GainError 

Response::GainError::GainError (const string& msg)
  : YellowAlert(" Gain setting error.\n")
{
  tcerr << msg << std::endl;
}


// Class Response 
int Response::s_count = 0;
const Real Response::s_NO_KEYVAL = std::numeric_limits<Real>::min();

Response::Response()
      : m_numEnergies(0),m_numChannels(0),m_spectrumNumber(0),
        m_sourceNumber(1),
        m_dataGroup(0),m_index(s_count++),m_eboundsExtName(""),m_active(true),
        m_constGain(0), m_linearGain(0),
        m_slopeKeyLimits(s_NO_KEYVAL,s_NO_KEYVAL),
        m_offsetKeyLimits(s_NO_KEYVAL,s_NO_KEYVAL), 
        m_energies(), m_gainFactor(2)
{
  m_gainFactor[0] = .0;
  m_gainFactor[1] = 1.0;
}

Response::Response(const Response &right)
      : m_numEnergies(right.m_numEnergies),
        m_numChannels(right.m_numChannels),
        m_spectrumNumber(right.m_spectrumNumber),
        m_sourceNumber(right.m_sourceNumber),
        m_dataGroup(right.m_dataGroup),
        m_index(s_count++),
	m_eboundsExtName(right.m_eboundsExtName),
        m_active(right.m_active),
        m_constGain(0), // Owning pointers, need deep copies made below.
        m_linearGain(0),
        m_slopeKeyLimits(right.m_slopeKeyLimits),
        m_offsetKeyLimits(right.m_offsetKeyLimits),
        m_energies(right.m_energies),
        m_gainFactor(right.m_gainFactor)
{
  if (right.m_constGain) m_constGain = right.m_constGain->clone();
  if (right.m_linearGain) m_linearGain = right.m_linearGain->clone();
}


Response::~Response()
{
  removeGainParams();
}


void Response::renumber (size_t newIndex)
{
  m_spectrumNumber = newIndex;
}

bool Response::fileFormat (const string& fileName, XspecDataIO::DataType type)
{

  return false;
}

void Response::checkRMFdata () throw ()
{
        // this would look a little less pointless if there wasn't
        // an empty base class default implementation.

        RMFremove();
}

RealArray Response::sensitivity (const SpectralData* data)
{
  throw InvalidRequest();
}

void Response::RMFremove ()
{
}

bool Response::order (const Response& right) const
{
  return true;
}

const RealArray& Response::lowerBoundNominalEnergy () const
{
  return eboundsMin();
}

const RealArray& Response::upperBoundNominalEnergy () const
{
  return eboundsMax();
}

const RealArray& Response::eboundsMin () const
{

  return m_energies;
}

const RealArray& Response::eboundsMax () const
{

  return m_energies;
}

void Response::convolve (const RealArray& flux, const RealArray& fluxErr, RealArray& foldFlux, RealArray& foldFluxErr) const
{
}

void Response::prepareForFit ()
{
}

const RealArray& Response::efficiency () const
{
  throw InvalidRequest();
}

void Response::setActive (bool activeStatus)
{
  m_active = activeStatus;
}

void Response::applyGainFromPrompt (Real slope, Real intercept)
{
  // Perhaps this isn't possible by this point.  Still 
  // worth checking ...
  if (!m_energies.size())
  {
     throw GainError("Response contains no energy bins.");
  }
  RealArray oldEnergies(m_energies);
  std::vector<Real> oldGainFactor(m_gainFactor);
  // shiftEffectiveArea function, called below, will only work
  // properly if m_energies has its no-gain value at the time
  // of the call, which is in-synch with m_savedEffectiveArea.
  if (isGainApplied())
  {
     // Undo previous gain
     m_energies += m_gainFactor[0];
     m_energies *= m_gainFactor[1];
  }
  RealArray newEnergies(m_energies);
  m_gainFactor[1] = slope;
  m_gainFactor[0] = intercept;  
  newEnergies /= m_gainFactor[1];
  newEnergies -= m_gainFactor[0];
  try
  {
     // Check for negative energy, assume newEnergies[0] is lowest.
     if (newEnergies[0] < .0)
     {
        string msg("WARNING: This gain will make negative energies, you may have too large an offset.");
	tcout << xsverbose(10) << msg << std::endl;
	tcout << xsverbose();
     }
     shiftEffectiveArea(newEnergies);
  }
  catch (...)
  {
     m_gainFactor = oldGainFactor;
     m_energies = oldEnergies;
     if (m_linearGain)
     {  
        m_linearGain->isPrompt(true);
        m_linearGain->setValue(oldGainFactor[1]);
        m_linearGain->isPrompt(false);
     }
     if (m_constGain)  
     {
        m_constGain->isPrompt(true);
        m_constGain->setValue(oldGainFactor[0]);
        m_constGain->isPrompt(false);
     }
     throw;
  }
  m_energies = newEnergies;
  if (m_linearGain)  
  {
     m_linearGain->isPrompt(true);
     m_linearGain->setValue(slope);
     m_linearGain->isPrompt(false);
     ParamLinkList::Instance()->updateResponseParamLinkers(m_linearGain);
  }
  if (m_constGain)  
  {
     m_constGain->isPrompt(true);
     m_constGain->setValue(intercept);
     m_constGain->isPrompt(false);
     ParamLinkList::Instance()->updateResponseParamLinkers(m_constGain);
  }
}

void Response::removeGain ()
{
  removeGainParams();
  if (isGainApplied())
  {
     m_energies += m_gainFactor[0];
     m_energies *= m_gainFactor[1];
     m_gainFactor[1] = 1.0;
     m_gainFactor[0] = 0.0;
     restoreEffectiveArea();
  }
}

bool Response::isGainApplied () const
{
  const Real FUZZ=1.0e-16;
  return (fabs(m_gainFactor[1]-1.0) > FUZZ || fabs(m_gainFactor[0]) > SMALL);
}

void Response::shiftEffectiveArea (const RealArray& newEnergies)
{
}

void Response::restoreEffectiveArea ()
{
}

bool Response::makeGainParams (const string& slopeParamVals, const string& offsetParamVals)
{
  bool deletionOccurred = false;

  // If one gain fit parameter already exists, then both must.
  if (m_constGain)
  {
     XSContainer::responses->removeFromRespParContainer(this, 0, 2);
     delete m_constGain;
     m_constGain = 0;
     delete m_linearGain;
     m_linearGain = 0;
     deletionOccurred = true;
  }

  // These constructors may throw, and we CANNOT leave here if
  // one has been created without the other.  Calling functions must
  // be able to assume that unless this function has properly returned,
  // neither parameter exists and nothing has been entered into 
  // the global parameter map.
  m_constGain = new ResponseParam(offsetParamVals, this, OFFSET);
  if (m_offsetKeyLimits.first != NO_KEYVAL())
  {
     m_constGain->setValue(m_offsetKeyLimits.first,'l');
     m_constGain->setValue(m_offsetKeyLimits.first,'b');
  }
  if (m_offsetKeyLimits.second != NO_KEYVAL())
  {
     m_constGain->setValue(m_offsetKeyLimits.second,'h');
     m_constGain->setValue(m_offsetKeyLimits.second,'t');
  }
  try
  {
     m_linearGain = new ResponseParam(slopeParamVals, this, LINEAR);
     if (m_slopeKeyLimits.first != NO_KEYVAL())
     {
        m_linearGain->setValue(m_slopeKeyLimits.first,'l');
        m_linearGain->setValue(m_slopeKeyLimits.first,'b');
     }
     if (m_slopeKeyLimits.second != NO_KEYVAL())
     {
        m_linearGain->setValue(m_slopeKeyLimits.second,'h');
        m_linearGain->setValue(m_slopeKeyLimits.second,'t');
     }
  }
  catch (...)
  {
     delete m_constGain;
     m_constGain = 0;
     throw;
  }

  std::vector<ResponseParam*> parsToAdd;
  parsToAdd.push_back(m_linearGain);
  parsToAdd.push_back(m_constGain);
  XSContainer::responses->addToRespParContainer(this, 0, parsToAdd); 

  return deletionOccurred;
}

void Response::removeGainParams ()
{

  // Again, either both or neither of these parameters exist 
  // and are in the global container.

  if (m_constGain)
  {
     XSContainer::responses->removeFromRespParContainer(this, 0, 2);     
     delete m_constGain;
     m_constGain = 0;
     delete m_linearGain;
     m_linearGain = 0;
  }
}

string Response::makeParamPromptString () const
{
  std::ostringstream respString;
  respString << "Resp source_" << m_sourceNumber << "_spec_"
             << m_spectrumNumber << ": ";
  return respString.str();
}

void Response::promptParamValues (string& paramVals, Response::ResponseParType parType) const
{
  // Assumes only two parTypes for now...
  ResponseParam* pGain = (parType == LINEAR) ? m_linearGain : m_constGain;
  if (pGain)
  {
     pGain->rePrompt(paramVals);
  }
}

void Response::setParamValues (const string& paramVals, Response::ResponseParType parType)
{
  // Assumes only two parTypes for now...
  ResponseParam* pGain = (parType == LINEAR) ? m_linearGain : m_constGain;
  if (!pGain)
  {
     throw GainError("Attempted to set values for non-existent response parameter.");
  }
  pGain->setValuesFromString(paramVals);
}

void Response::applyGainFromFit (Response::ResponseParType parType)
{
  // If we're here, this should mean one of the response params
  // has just been reset and is notifying Response parent to
  // reapply the corresponding gain.
  RealArray oldEnergies(m_energies);
  std::vector<Real> oldGainFactor(m_gainFactor);
  // NOTE: Unlike applyGainFromPrompt, we don't test for negative
  // energies, which might pop up here during intermediate stages
  // of a fit.  The shiftEffectiveArea function though is still
  // capable of throwing, if new energies do not overlap the
  // previous ones at all.  If this happens, we must set the
  // response params back to their old values to keep things in 
  // a consistent state.
  m_energies += m_gainFactor[0];
  m_energies *= m_gainFactor[1];
  RealArray newEnergies(m_energies);
  try
  {
     if (parType == LINEAR)
     {        
        m_gainFactor[1] = m_linearGain->value();        
     }
     else if (parType == OFFSET)
     {
        m_gainFactor[0] = m_constGain->value();
     }
     newEnergies /= m_gainFactor[1];
     newEnergies -= m_gainFactor[0];
     shiftEffectiveArea(newEnergies);
     if (tpout.logChatterLevel() >= 30)
     {
        tcout << "\nGain adjust:  Previous energies     New energies" << std::endl;
        for (size_t i=0; i<m_energies.size(); ++i)
        {
           tcout << i << "  "<<oldEnergies[i] << "    " << newEnergies[i] <<std::endl;
        }
     }
  }
  catch (...)
  {
     m_gainFactor = oldGainFactor;
     m_energies = oldEnergies;
     if (parType == LINEAR)
     {
        // To prevent setValue from calling back to this function
        // and getting infinite recursion...
        m_linearGain->isPrompt(true);
        m_linearGain->setValue(oldGainFactor[1],'v');
        m_linearGain->isPrompt(false);
     }
     else if (parType == OFFSET)
     {
        m_constGain->isPrompt(true);
        m_constGain->setValue(oldGainFactor[0],'v');
        m_constGain->isPrompt(false);
     }
     throw;
  }   
  m_energies = newEnergies;
  ParamLinkList::Instance()->updateResponseParamLinkers(parType == LINEAR ?
                m_linearGain : m_constGain);

  // Don't assume models container even exists.  This function may be
  // called during shut-down if a response parameter link is being
  // removed.
  if (XSContainer::models)
  {
     Model* affectedModel = XSContainer::models->lookup(this);
     if (affectedModel)
     {
        affectedModel->updateNewGainFromFit(this);
        affectedModel->fillEnergyContainer();
     }
  }
}

void Response::calcEffAreaPerChan (RealArray& effArea)
{
   throw InvalidRequest();
}

void Response::untieParamLinks () const
{
   // untie function does nothing if parameter is not linked.
   if (m_constGain)
      m_constGain->untie(true);
   if (m_linearGain)
      m_linearGain->untie(true);
}

void Response::registerResponseParams () const
{
   // For now response parameters are limited to gain parameters.
   // If one exists then both do.
   if (m_constGain)
   {
      std::vector<ResponseParam*> parsToAdd;
      parsToAdd.push_back(m_linearGain);
      parsToAdd.push_back(m_constGain);
      XSContainer::responses->addToRespParContainer(this, 0, parsToAdd); 
   }
}

void Response::deregisterResponseParams () const
{
   if (m_constGain)
   {
     XSContainer::responses->removeFromRespParContainer(this, 0, 2); 
     untieParamLinks();    
   }
}

// Additional Declarations
