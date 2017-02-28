//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// ChainManager
#include <XSFit/MCMC/ChainManager.h>
// FitMethod
#include <XSFit/Fit/FitMethod.h>
// ModParam
#include <XSModel/Parameter/ModParam.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// RandomizerBase
#include <XSFit/Randomizer/RandomizerBase.h>
// ModelContainer
#include <XSModel/GlobalContainer/ModelContainer.h>
// DataContainer
#include <XSModel/GlobalContainer/DataContainer.h>
// Fit
#include <XSFit/Fit/Fit.h>

#include <XSFit/Fit/FitErrorCalc.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUtil/Numerics/Numerics.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <iomanip>
#include <algorithm>

Fit*   XSContainer::fit = 0;
using namespace XSContainer;


// Class Fit::NoSuchFitMethod 

Fit::NoSuchFitMethod::NoSuchFitMethod (const string& name)
  : YellowAlert(" Fitting Algorithm not loaded: ")
{
  tcerr << name << '\n';
}


// Class Fit::NoSuchStatMethod 

Fit::NoSuchStatMethod::NoSuchStatMethod (const string& name)
  : YellowAlert(" Statistic not recognized: ")
{
  tcerr << name << std::endl;;
}


// Class Fit::FitOverDetermined 

Fit::FitOverDetermined::FitOverDetermined()
  : YellowAlert() 
{
  tcerr << "***Warning: Ill-formed Fit problem - number of variable parameters exceeds number of bins"
     << std::endl;
}


// Class Fit::ParameterLookupError 

Fit::ParameterLookupError::ParameterLookupError()
  : RedAlert(" parameter indexing error ")               
{
}


// Class Fit::NotVariable 

Fit::NotVariable::NotVariable()
  : YellowAlert(" No variable parameters for fit ")
{
}


// Class Fit::CantInitialize 

Fit::CantInitialize::CantInitialize (const string& diag)
  : YellowAlert(" initializing statistic: ")
{
  tcerr << diag << '\n';
}


// Class Fit::HoldSpecVar 

Fit::HoldSpecVar::HoldSpecVar()
  : m_spectrum(), m_rawVariance(), m_variance(), m_bckSpectrum(), 
    m_bckRawVariance(), m_bckVariance()
{
}


Fit::HoldSpecVar::~HoldSpecVar()
{
}


void Fit::HoldSpecVar::saveSpecVar (const SpectralData& data)
{
  size_t nChans = data.channels();
  m_spectrum.resize(nChans);
  m_rawVariance.resize(nChans);
  m_variance.resize(nChans);
  m_spectrum = data.spectrum();
  m_rawVariance = data.rawVariance();
  m_variance = data.variance();
  const Background* bck = data.background();
  if (bck)
  {
     m_bckSpectrum.resize(nChans);
     m_bckRawVariance.resize(nChans);
     m_bckVariance.resize(nChans);
     m_bckSpectrum = bck->spectrum();
     m_bckRawVariance = bck->data()->rawVariance();
     m_bckVariance = bck->variance();
  }
}

void Fit::HoldSpecVar::restoreSpecVar (SpectralData& data) const
{
  data.setSpectrum(m_spectrum);
  data.setRawVariance(m_rawVariance);
  data.setVariance(m_variance);
  Background* bck = data.getBackground();
  if (bck)
  {
     data.setBackgroundSpectrum(m_bckSpectrum);
     bck->setRawVariance(m_bckRawVariance);
     bck->setVariance(m_bckVariance);
  } 
}

// Additional Declarations

// Class Fit::FitInterrupt 

Fit::FitInterrupt::FitInterrupt ()
  : YellowAlert()
{
  tcerr << "\n*** Warning: User interrupted fit, fit not valid."
        << std::endl;
}


// Class Fit 
Fit* Fit::s_instance = 0;
bool Fit::s_errorCalc = false;
Real Fit::s_deltaStat = 2.706;
int Fit::s_errorTry = 20;
Real Fit::s_tolerance = 0.01;
Real Fit::s_chiMax = 2.0;
bool Fit::s_doNewMinRecalc = true;
int Fit::s_MCrealizations = 100;
Real Fit::s_lastFtest = .0;
const int Fit::s_RESPAR_INDEX = (1 << (XSContainer::ModelContainer::SHIFT()-1)) - 1;
Real Fit::s_lastGoodness = .0;
RealArray Fit::s_goodnessSims;

Fit::Fit (const string& fitMethodName)
  : m_isStillValid(false),
    m_chainManager(ChainManager::Instance()),
    m_statManager(StatManager::Instance()),
    m_nativeRandomizerNames(),
    m_useNumericalDifferentiation(false),
    m_fitMethod(0),
    m_oldParameterValues(),
    m_variableParameters(),
    m_spectraToFit(),
    m_queryMode(ON),
    m_stepGrid(0),
    m_activeModels(),
    m_randomizingStrategies(),
    m_renormType(PREFIT)
{
  m_fitMethod.reset(FitMethod::get(fitMethodName));

  // Register Fit as an observer of ModelContainer.  If
  // ModelContainer doesn't already exist, it will be created here.
  ModelContainer::Instance()->Attach(this); 
  // StatManager singleton actually exists before Fit in Xspec's
  // current start-up sequence.  So we can't register StatManager
  // as an observer from StatManager's ctor.
  Attach(m_statManager); 
}


Fit::~Fit()
{
  // let's deallocate the resources anyway, but actually
  // Fit's dtor is only called by program exit.
  // Fit's fitMethod is an auto_ptr thus will die automatically.
  deleteSavedParams();
  delete m_chainManager; 
  delete m_statManager;
  clearRandomizingStrategies();     
}


void Fit::renormalize ()
{

  m_statManager->renormalizeStats();
}

void Fit::reinitialize (bool saveParameters)
{
  cleanup();

  initializeFitModels();

  if (m_activeModels.empty()) 
  {
     throw YellowAlert("Cannot fit: No models are currently active.\n");
  }

  initializeFitParameters();      

  if ( saveParameters)     
  {      
        // now create an (owned) copy of the parameters for resetting
        // purposes.
        std::map<int,ModParam*>::const_iterator vp    = m_variableParameters.begin();
        std::map<int,ModParam*>::const_iterator vpEnd = m_variableParameters.end();
        for ( ; vp != vpEnd; ++vp )
        {
                ModParam* savePar = vp->second->clone();
                m_oldParameterValues.insert(std::map<int,ModParam*>::value_type(vp->first,savePar));

        }
  }     

  std::vector<size_t> missingModels;   
  initializeFitSpectra(missingModels);
  if (missingModels.size())
  {
     std::ostringstream oss;
     oss << "Spectra with responses but no models: ";
     for (size_t i=0; i<missingModels.size(); ++i)
        oss << missingModels[i] << " ";
     throw CantInitialize(oss.str());
  }

  if ( m_spectraToFit.empty() )  throw CantInitialize(" no data defined ");

  // initialize the statistical calculation, allocating for the arrays
  // for intermediate results.
  m_statManager->initializeStats();
}

FitMethod* Fit::fitMethod ()
{
  return m_fitMethod.get();
}

void Fit::fitMethod (FitMethod* method)
{
  bool wasStillValid = m_isStillValid;
  m_fitMethod.reset(method);
  Update();
  // Update will automatically mark fit as invalid, but if
  // it was previously valid and all we did is change the
  // fit method, let's still call it valid.
  m_isStillValid = wasStillValid;    
}

Fit* Fit::Instance (const string& fitMethodName)
{
  if (s_instance == 0) {s_instance = new Fit(fitMethodName);} return s_instance;
}

void Fit::Update (Subject* changed)
{
  m_isStillValid = false;     
  cleanup();
  initializeFitModels();
  initializeFitParameters();
  m_chainManager->isSynchedWithFitParams(checkChainsForSynch());
  // In this context, we don't care if some spectra have responses
  // but no models since they aren't trying to do a fit at this
  // point.
  std::vector<size_t> dummyMissingMods;
  initializeFitSpectra(dummyMissingMods);

  // If anything throws prior to here, it must be some sort of
  // bookkeeping error and something is seriously wrong.  Don't
  // bother catching it.

  try
  {
     calculateModel();

  }
  catch (YellowAlert&)
  {
     // Do NOT propagate exceptions through the notify/update chain.
  }
  Notify();
}

void Fit::deleteSavedParams ()
{
  if (!m_oldParameterValues.empty())      
  {      
        std::map<int,ModParam*>::const_iterator op    = m_oldParameterValues.begin();
        std::map<int,ModParam*>::const_iterator opEnd = m_oldParameterValues.end();
        for ( ; op != opEnd; ++op )
        {
                delete  op->second;
        }    
        m_oldParameterValues.clear();
  }    
}

void Fit::cleanup ()
{
  deleteSavedParams();

  m_spectraToFit.clear();

  m_variableParameters.clear();
}

void Fit::resetParameters ()
{
  std::map<int,ModParam*>::const_iterator p (m_oldParameterValues.begin());
  std::map<int,ModParam*>::const_iterator pEnd (m_oldParameterValues.end());
  std::map<int,ModParam*>::iterator varEnd(m_variableParameters.end());
  while (p != pEnd )
  {
     // Note: m_variableParameters may not have all the ModParam
     // objects that were originally stored in m_oldParameterValues
     // (see for example its usage in FitErrorCalc::calcUncertainty).
     // In this case, simply do nothing.

     std::map<int,ModParam*>::iterator itVar = 
                m_variableParameters.find(p->first);
     if (itVar != varEnd)
     {
        itVar->second->setValue(p->second->value(), 'v');
     }
     ++p;
  }
}

void Fit::reportParameters (std::ostream& s, bool header)
{
  if (s_errorCalc) return;
  using namespace std;
  ios_base::fmtflags saveFormat(s.flags());
  const int savePrecision(s.precision());
  int nvpar = m_variableParameters.size();
  std::map<int,ModParam*>::const_iterator vp = m_variableParameters.begin();
  if (header)
  {  
     // write header for output
     s << setw(45) << "Parameters" << std::endl;
     const StatMethod* stat = m_statManager->usingSingleStat();
     const string statName = stat ? stat->fullName() : string("Total Stat");
     s << setw(13) << left << statName << right << "|beta|/N    Lvl";

     int j (0);
     while ( j < nvpar)
     {
        string parHeader(vp->second->getParameterLabel());
        parHeader += string(":") + vp->second->name();
        parHeader = parHeader.substr(0,13);
        s  << setw(14) << right << parHeader;
        ++j, ++vp;
     }
     s << std::endl;

  }      
  else
  {
     m_fitMethod->reportProgress(s,this); 
     s.precision(6);
     int j (0);  

     while (j < nvpar)
     {
        // Showpoint is necessary for printing trailing zeros.
        s << right << showpoint << setw(14) << vp->second->value('v');
        ++j, ++vp;
     }
     s << std::endl;

  }
  s.flags(saveFormat);
  s.precision(savePrecision);      
}

void Fit::report ()
{
  if (s_errorCalc) return;
  m_fitMethod->reportCovariance();
  // Only want to report active/on models.
  std::map<string,bool>::const_iterator itModNames = models->activeModelNames().begin();
  std::map<string,bool>::const_iterator itNamesEnd = models->activeModelNames().end();
  while (itModNames != itNamesEnd)
  {
     if (itModNames->second)
     {
        // It's active, is it on?
        std::vector<Model*> mods = models->lookupModelGroup(itModNames->first);
        if (mods.size() && mods[0]->isActive())
        {
           // It's on.
           mods[0]->printHeading();
           for (size_t i=0; i<mods.size(); ++i)
           {
              tcout << *mods[i];
           }
           mods[0]->printMixComp();           
           tcout << string(72,'_') << '\n' << std::endl;
        }
     }
     ++itModNames;
  }

  if (responses->totalResponseParams())
  {
     tcout << "\nResponse Parameters:" <<std::endl;
     responses->reportResponseParams();
  }                
  m_statManager->reportStats();

  if (m_useNumericalDifferentiation)
  {
     tcout << "\nNote that fit is using the full (slower) numerical differentiation method."
        <<"\nThis setting may be changed in the user's ~/.xspec/Xspec.init start-up file.\n"
        << std::endl;
  }      
}

void Fit::simulate (std::vector<Real>& trialValues, bool simulateParams, bool doFit, int statMode)
{
   using namespace std;
   
   size_t nProcs=1;
   map<string,int>::iterator itMax = ProcessManager::maxProcs().find("goodness");
   if (itMax != ProcessManager::maxProcs().end())
   {
      nProcs = itMax->second;
   }
   ProcessManager procs(new ParallelSim(), "goodness");
   procs.createProcesses(nProcs);
   const size_t nRealize = trialValues.size();
   // truncation is intentional
   const size_t nSimsPerProc = nRealize/nProcs;
   const size_t nRemainder = nRealize % nProcs;
   vector<TransferStruct> parallelInput;
   for (size_t i=0; i<nProcs; ++i)
   {
      TransferStruct inputStruct;
      vector<int> inputVec(3);
      if (i < nRemainder)
         inputVec[0] = static_cast<int>(nSimsPerProc) + 1;
      else
         inputVec[0] = static_cast<int>(nSimsPerProc);
      inputVec[1] = static_cast<int>(simulateParams);
      inputVec[2] = static_cast<int>(doFit);
      inputStruct.iValues.push_back(inputVec);
      parallelInput.push_back(inputStruct);
   }
   ProcessManager::ParallelResults results;
   // This will not throw, and we can assume that upon return models
   //  will have been restored to their original states.
   procs.run(parallelInput, results);
   ProcessManager::ParallelResults::const_iterator itResults = results.begin();
   size_t iElem=0;
   while (itResults != results.end())
   {
      const TransferStruct& output = itResults->second;
      if (output.status < 0)
      {
         procs.killProcesses();
         throw YellowAlert("Unable to perform simulations\n.");
      }
      else
      {
         // The order in which the processes are received is irrelevant.
         const vector<double>& procStats = output.dValues[0];
         std::copy(procStats.begin(),procStats.end(),trialValues.begin()+iElem);
         iElem += procStats.size();
      }
      ++itResults;
   }

   procs.killProcesses();

}

void Fit::goodness (int realizations, bool simulateParams, bool doFit, int statMode)
{
  using namespace std; 
  map<size_t,HoldSpecVar*> saveData;

  map<size_t,SpectralData*>::iterator f = m_spectraToFit.begin();
  map<size_t,SpectralData*>::iterator fEnd = m_spectraToFit.end();

  // Save the spectrum and variance (raw and processed) of every 
  // spectral data object that is fit.
  // We are going to need to replace them temporarily with simulated
  // realizations. 

  Real origStat = m_statManager->totalTestStatistic();

  while ( f != fEnd)
  {
     HoldSpecVar *specVar = new HoldSpecVar();
     specVar->saveSpecVar(*f->second);
     saveData.insert(map<size_t,HoldSpecVar*>::value_type(f->first, specVar));
     ++f;
  }

  std::vector<Real> simResults(realizations,0.);     

  try
  {  
     simulate(simResults,simulateParams,doFit,statMode);
     sort(simResults.begin(),simResults.end());
  }
  catch (...)
  {
     // Assume any changes to models have been restored at 
     // lower levels.  Restore the spectra here.
     f = m_spectraToFit.begin();
     while ( f != fEnd )
     {
        HoldSpecVar *specVar = saveData[f->first];
        specVar->restoreSpecVar(*f->second);
        delete specVar;
        ++f; 
     }
     m_statManager->performStats();
     throw;
  }
#ifndef STD_COUNT_DEFECT
        int n (count_if(simResults.begin(),simResults.end(),bind2nd(less<Real>(),origStat)));
#else

        int n (0);
        count_if(simResults.begin(),simResults.end(),bind2nd(less<Real>(),origStat),n);
#endif
  std::ios_base::fmtflags currentSetting(tcout.flags());
  size_t savePrecision = tcout.precision();
  Real pct = Real(n)/realizations*100.;
  tcout.precision(2);
  tcout <<  fixed << pct << "% of realizations are < best fit statistic " << origStat;
  tcout.precision(7);
  simulateParams ? tcout << "  (sim)" : tcout << "  (nosim)";
  doFit ? tcout << "  (fit)" : tcout << "  (nofit)";
  tcout << std::endl;
  s_lastGoodness = pct;
  s_goodnessSims.resize(simResults.size());
  for (size_t i=0; i<s_goodnessSims.size(); i++) s_goodnessSims[i] = simResults[i];
  tcout.flags(currentSetting);
  tcout.precision(savePrecision);
  // replace the original data. Got to ensure exception safety here.

  f = m_spectraToFit.begin();
  while ( f != fEnd )
  {
     HoldSpecVar *specVar = saveData[f->first];
     specVar->restoreSpecVar(*f->second);
     delete specVar;
     ++f; 
  }
  m_statManager->performStats();
}

void Fit::simulateModel (ArrayContainer& simSpec, int randomizeIndicator)
{
  // prepare one model spectrum for each real spectrum. Sum all the relevant
  // models, scale by exposure time and area, and add in the background
  // and any correction factor.

  switch (randomizeIndicator)
  {
     case 0:
        break;
     case 1:
        randomizeModelParameters(false);
        break;
     case 2:
     default:
        randomizeModelParameters(true);
        break;
  }

  calculateModel();  

  std::map<size_t,SpectralData*>::const_iterator f = m_spectraToFit.begin();
  std::map<size_t,SpectralData*>::const_iterator fEnd = m_spectraToFit.end();
  while ( f != fEnd )
  {
        SpectralData* data = f->second;
        const size_t NC (data->spectrum().size());
        RealArray sim(0.,NC);
        ModelMapConstIter mm (models->modelSet().begin());
        ModelMapConstIter mmEnd (models->modelSet().end());
        // sum all the folded models that are fit to a single spectrum
        // ( the normal case is one model per spectrum, but the following
        //   code supports the multisource case).
        while ( mm != mmEnd )
        {
                Model* m (mm->second);

                if (!m->isActive()) 
                {
                   ++mm;
                   continue;
                }
                ArrayContainer::const_iterator s = m->foldedModel().find(f->first);

                if ( s != m->foldedModel().end())
                {
                        sim += s->second;       
                }
                ++mm;

        }

        simSpec.insert(ArrayContainer::value_type(f->first,sim));
        ++f;      
  }  
}

void Fit::initializeFitModels ()
{
  // are there any models that have attached responses? 

  ModelMapConstIter mi (models->modelSet().begin());
  ModelMapConstIter miEnd (models->modelSet().end());
  m_activeModels.clear();
  for ( ; mi != miEnd; ++mi)
  {
        if (mi->second->isActive()) 
        {
                // active models is a set, will ignore duplicates.
                m_activeModels.insert(std::set<string>::value_type(mi->first));
                mi->second->prepareForFit();  
        }    
  }  
}

void Fit::initializeFitSpectra (std::vector<size_t>& missingModels)
{
  missingModels.clear();
  size_t N = datasets->numberOfSpectra();
  size_t nSources = datasets->numSourcesForSpectra();
  bool allHaveResp = true;
  bool thisHasResp = false;
  bool respHasMod = false;
  for ( size_t j = 1; j <= N; ++j)
  {
        thisHasResp = false;
        respHasMod = false;
        SpectralData* sp (datasets->lookup(j));
        // Requirement for spectrum to be added to fit:  at
        // least 1 detector slot must be filled with an actual
        // response suitable for fitting, and that slot must have
        // a model associated with it.  

        // If a spectrum has no responses, we'll let that go with just
        // a warning and won't add it to the missingModels container.
        // If it has 1 or more responses but no models, it gets added
        // to missingModels.
	for (size_t i=0; i<nSources; ++i)
	{
           if (sp->responseLoaded(i) && !sp->isDummyrspMode2(i)) 
           {
	      thisHasResp = true;
              if (models->lookupModelForSource(i+1).size())
              {
                 respHasMod = true;
                 break;
              }
           }
	}
	if (thisHasResp)
	{
           if (respHasMod)
           {
              if (!sp->spectrumIsZeroed())
              {
                 m_spectraToFit.insert(std::map<size_t,SpectralData*>::
		   				              value_type(j,sp));
              }
              sp->prepareForFit(); 
           }
           else
              missingModels.push_back(j);
	}
        else
           allHaveResp = false;      
  }
  if (!allHaveResp)
  {
     tcout << "***Warning!  One or more spectra are missing responses,\n";
     tcout << "               and are not suitable for fit.\n";
     tcout.flush();
  }
}

void Fit::initializeFitParameters ()
{
  // second, get an array of variable parameters, if saveParameters
  // is true (and therefore we are fitting rather than renormalizing).
  const ParamMap& pars = models->parameterList();
  ParamMapConstIter gp = pars.begin();
  ParamMapConstIter gpEnd = pars.end();

  for ( ; gp != gpEnd ; ++gp)
  {
     const string modelName = gp->first.substr(0,gp->first.find_first_of(':'));
     const std::vector<Model*> mods = models->lookupModelGroup(modelName);
     const size_t nParsInMod = mods[0]->numberOfParameters();
     const size_t groupNum = (gp->second->index()-1)/nParsInMod;     
     const Model* mod = mods[groupNum];

     if (mod && mod->isActive())
     {
          ModParam* mp = dynamic_cast<ModParam*>(gp->second);
          if (mp && !(mp->isFrozen() || mp->isLinked()) )
          {
             // create a map entry with the full index as
             // key and ModParam* value
             int mpIndex = models->keyToIndex(gp->first);
             // shallow copy
             variableParameters(mpIndex,mp); 
             //  tcout << " Parameter: " << (gp->first) << " index " << mpIndex 
             //        << " val: " << mp->value() << " address " << mp << std::endl;
          }
      }
  }

  // Now add any response parameters, give index a high enough
  // value so that it is extremely unlikely to ever conflict with
  // a model index.
  // This scheme should allow a max of 2^16 response params.
  if (responses->totalResponseParams())
  {
     const size_t MAX = 1 << (31 - ModelContainer::SHIFT() + 1);
     const size_t nSources = responses->respParContainer().size();
     if (responses->totalResponseParams() >= MAX)
     {
        std::ostringstream msg;
        msg << "Number of response parameters has exceeded the maximum of " << MAX;
        throw RedAlert(msg.str());
     }
     int k=1;
     for (size_t i=0; i<nSources; ++i)
     {
        const std::vector<ResponseParam*>& parsForSource = 
                responses->respParContainer()[i];
        for (size_t j=0; j<parsForSource.size(); ++j)
        {
           ResponseParam* rp = parsForSource[j];
           if (rp && !(rp->isFrozen() || rp->isLinked()))
           {
              // This puts the respFloor at (2^15 - 1)*2^16 = 2^31 - 2^16
              int respIndex = (s_RESPAR_INDEX << ModelContainer::SHIFT()) + k;
              // shallow copy
              variableParameters(respIndex, rp);
           }
           ++k;
        }
     }

  }
}

void Fit::calculateModel ()
{
  m_isStillValid = false;
  std::set<string>::iterator ss (m_activeModels.begin());
  std::set<string>::iterator ssEnd (m_activeModels.end());

  while ( ss != ssEnd)
  {
        models->calculate(*ss);
        models->fold(*ss);
	++ss;
  }         
}

void Fit::randomizeModelParameters (bool callInitialize, Real fSigma)
{
  std::map<int,ModParam*>::iterator itVp = m_variableParameters.begin();
  std::map<int,ModParam*>::iterator itVpEnd = m_variableParameters.end();
  if (m_chainManager->isSynchedWithFitParams())
  {
     std::vector<Real> parVals;
     std::vector<ModParam*> parsToModify;
     if (m_activeModels.size())
     {
        // We have active/on models
        while (itVp != itVpEnd)
        {
           parsToModify.push_back(itVp->second);
           parVals.push_back(itVp->second->value('a'));
           ++itVp;
        }
     }
     else
     {
        // use active/off
        std::map<string,bool>::const_iterator itActive =
              models->activeModelNames().begin();
        std::map<string,bool>::const_iterator itActiveEnd = 
              models->activeModelNames().end();
        while (itActive != itActiveEnd)
        {
           if (itActive->second)
           {
              const Model* mod = models->lookup(itActive->first);
              std::vector<Parameter*> pars;
              mod->bundleParameters(pars);
              for (size_t iPar=0; iPar<pars.size(); ++iPar)
              {
                 ModParam* modPar = dynamic_cast<ModParam*>(pars[iPar]);
                 if (modPar && !(modPar->isFrozen() || modPar->isLinked()))
                 {
                    parsToModify.push_back(modPar);
                    parVals.push_back(modPar->value('a'));
                 }
              }
           }
           ++itActive;
        }

     } // end if using active/off
     const size_t nPars = parVals.size();
     RealArray vaParVals(.0, nPars);
     for (size_t i=0; i<nPars; ++i)
        vaParVals[i] = parVals[i];
     m_chainManager->getRandomPoint(vaParVals);
     for (size_t i=0; i<parsToModify.size(); ++i)
     {
        parsToModify[i]->setValue(vaParVals[i],'a');
     }
  } // end if using chains for randomization
  else
  {
     RealArray vaParVals(.0, m_variableParameters.size());
     size_t i=0;
     while (itVp != itVpEnd)
     {
        vaParVals[i] = itVp->second->value('a');
        ++itVp, ++i;
     }
     RandomizerBase* rand = 
                m_randomizingStrategies.find("gaussian fit")->second;
     if (callInitialize)
        rand->initializeRun(this);

     rand->randomize(vaParVals, this);
     itVp = m_variableParameters.begin();
     i=0;
     while (itVp != itVpEnd)
     {
        itVp->second->setValue(vaParVals[i],'a');
        ++itVp, ++i;
     }    
  } // end if using fit for randomization

  // If fit was valid before, it sure isn't now.  Leave it up
  // to client to decide if it wants to return fit to valid state.
  m_isStillValid = false;      
}

void Fit::fluxes (bool lumin, bool error, int nTrials, Real level, Real eMin, Real eMax, Real redshift)
{
    using std::setw;
    using namespace XSContainer;
    static const Real LUMCON (1.07057E13);
    static std::pair<Real,Real> ZERO(0,0);
    std::ios_base::fmtflags currentSetting(tcout.flags());
    size_t savePrecision (tcout.precision());

    if (!models->modelSet().size())
    {
       string err("No models defined for flux calculation\n");
       throw YellowAlert(err);
    }
    
    // 2 modes of operation for flux/lumin command:  If ANY active/on
    // Model objects are found (and which overlap entered flux eMin 
    // and eMax), calculate the flux integration and store results with
    // the applicable SpectralData objects.  If NO active/on models,
    // perform the calculation on all active/off models and store the
    // results with the models.   
    
    // The keys in the specsAndMods map are spectrum numbers.
    std::map<size_t,std::vector<Model*> > specsAndMods;
    ModelMap::const_iterator itMod = models->modelSet().begin();
    ModelMap::const_iterator itModEnd = models->modelSet().end();
    bool isRangeError=false;
    while (itMod != itModEnd)
    {
       Model* mod = itMod->second;
       ArrayContainer::const_iterator itEng = mod->energy().begin();
       ArrayContainer::const_iterator itEngEnd = mod->energy().end();
       while (itEng != itEngEnd)
       {
          const size_t specNum = itEng->first;
	  const RealArray& energyArray = itEng->second;
          // Only interested in active/on models in this part,
          // so specNum can't be 0.
          if (specNum)
          {
	     const Real arrayMax = energyArray[energyArray.size()-1];
	     const Real arrayMin = energyArray[0];
	     if (eMin > arrayMax || eMax < arrayMin)
	     {
	         tcout << "Spectrum "<<specNum<<": No overlap between matrix range  ( "
		       << setw(12) << arrayMin << ',' << setw(12)
		       << arrayMax << " ) \nand the requested range "
		       << "( " << setw(12) << eMin << ',' 
		       << setw(12) << eMax << " )"<<std::endl;
                 isRangeError = true;
	     }
             else
             {
                std::vector<Model*>& modsForSpec = specsAndMods[specNum];
                modsForSpec.push_back(mod); 
             }
          } // end if specNum
          ++itEng;
       }
       ++itMod;
    } // end models loop
    
    if (!specsAndMods.size())
    {
       // If we're in here, either all active models (if any) are 'off'
       // (no spectra with corresponding response), or all spectra with
       // appropriate response and model have a no-overlap range error.
       // If it's the first case, we want to calculate flux for the 
       // active/off models and store the results with them.  In the
       // second case the user almost certainly entered the wrong range,
       // so let's throw.
       if (isRangeError)
       {
          string err("All active model energy arrays are out of range of");
          err +=" entered flux energies.\n";
          throw YellowAlert(err);
       }
       // Grab all active/off models, but must check for no-overlap
       // once again.  Ugh.
       std::map<string,bool>::const_iterator itModName =
                 models->activeModelNames().begin();
       std::map<string,bool>::const_iterator itModNameEnd =
                 models->activeModelNames().end();
       while (itModName != itModNameEnd)
       {
          if (itModName->second)
          {
             Model* mod = models->lookup(itModName->first);
             ArrayContainer::const_iterator itEng = mod->energy().find(0);
             if (itEng == mod->energy().end())
             {
                throw RedAlert("Energy array error in Fit::fluxes");
             }
	     const RealArray& energyArray = itEng->second;
	     const Real arrayMax = energyArray[energyArray.size()-1];
	     const Real arrayMin = energyArray[0];
	     if (eMin > arrayMax || eMax < arrayMin)
	     {
	         tcout << " No overlap between matrix range  ( "
		       << setw(12) << arrayMin << ',' << setw(12)
		       << arrayMax << " ) \nand the requested range "
		       << "( " << setw(12) << eMin << ',' 
		       << setw(12) << eMax << " )"<<std::endl;
	     }
             else
             {
                std::vector<Model*>& modsForSpec = specsAndMods[0];
                modsForSpec.push_back(mod); 
             }
          }
          ++itModName;
       }
       // If we're STILL empty, time to give up.
       if (!specsAndMods.size())
       {
          string err("No model has energies within requested range\n");
          throw YellowAlert(err);
       }
    } // end if no suitable spectra
    
    Numerics::FZSQ fzsq;
    Real H0 (models->cosmo().H0/100.);
    Real q0 (models->cosmo().q0);
    Real lambda0 = (models->cosmo().lambda0);
    if  ( H0 <= 0 ) 
    {
	H0 = 0.50;
	tcout << "  Warning: stored H0 value is zero, taken as 50 km/s/Mpc " << std::endl;
    }

    std::map<size_t,RealArray> prevEngsForSpec;
    size_t prevDataGroupNum = 0;

    std::map<size_t,std::vector<Model*> >::const_iterator itSpec =
                specsAndMods.begin();
    std::map<size_t,std::vector<Model*> >::const_iterator itSpecEnd =
                specsAndMods.end();
    while (itSpec != itSpecEnd)
    {
       const size_t spectrumNumber = itSpec->first;
       const std::vector<Model*>& modsForSpec = itSpec->second;       
       size_t currDataGroupNum = 0;
       std::vector<SpectralData::FluxCalc> modelFluxCalc;
       bool diffFound = false;
       // This returns 0 when spectrumNumber is 0
       SpectralData *sd = datasets->lookup(spectrumNumber);
       if (sd)
       {
          currDataGroupNum = sd->parent()->dataGroup();
       }
       // Keys in this map are sourceNums
       std::map<size_t,const RealArray*> engsForSpec;
       for (size_t i=0; i<modsForSpec.size(); ++i)
       {
          Model* mod = modsForSpec[i];
          ArrayContainer::const_iterator itEng = mod->energy().find(spectrumNumber);
          if (itEng != mod->energy().end())
          {
             // Should always get in here.
             engsForSpec[mod->sourceNumber()] = &itEng->second;
          }
       }

       // The only reason we bother looking for differences between the
       // current spectrum's energies and the previous is to avoid redundant
       // display of the flux output.
       const size_t nSources = engsForSpec.size();
       if (nSources == prevEngsForSpec.size())
       {
          std::map<size_t,RealArray>::const_iterator itPrev = prevEngsForSpec.begin();
          std::map<size_t,RealArray>::const_iterator itPrevEnd = prevEngsForSpec.end();
          std::map<size_t,const RealArray*>::const_iterator itEngsEnd = engsForSpec.end();
          while (itPrev != itPrevEnd && !diffFound)
          {
             size_t iSource = itPrev->first;
             std::map<size_t,const RealArray*>::const_iterator itEngs = 
                        engsForSpec.find(iSource);
             if (itEngs != itEngsEnd)
             {
                const RealArray& eng_iSource = *itEngs->second;
                const RealArray& prevEng_iSource = itPrev->second;
                const size_t engSize = eng_iSource.size();
                if (engSize != prevEng_iSource.size())
                   diffFound = true;
                else
                   for (size_t i=0; i<engSize; ++i)
                   {
                      if (eng_iSource[i] != prevEng_iSource[i])
                      {
                         diffFound = true;
                         break;
                      }
                   }
             }
             else
                diffFound = true;
             ++itPrev;
          }
       }
       else
	   diffFound = true;
       if (diffFound)
       {
	   prevEngsForSpec.clear();
           std::map<size_t,const RealArray*>::const_iterator itEngs =
                engsForSpec.begin();
           std::map<size_t,const RealArray*>::const_iterator itEngsEnd =
                engsForSpec.end();
           while (itEngs != itEngsEnd)
           {
              prevEngsForSpec.insert(std::map<size_t,RealArray>::value_type
                        (itEngs->first, *itEngs->second));
              ++itEngs;
           }
       }

       const bool showOutput = diffFound || (currDataGroupNum != prevDataGroupNum);
       prevDataGroupNum = currDataGroupNum;
       for (size_t i=0; i<nSources; ++i)
       {
	   Model* m = modsForSpec[i];
           Real energyLow = eMin;
           Real energyHigh = eMax;
           const RealArray& energyArray = m->energy().find(spectrumNumber)->second;
           const Real arrayMin = energyArray[0];
           const Real arrayMax = energyArray[energyArray.size()-1];
	   if (energyLow < arrayMin) 
	   {
               if (spectrumNumber)
                  tcout << "Spectrum "<<spectrumNumber<<":";
	       tcout << " Lower range bound " << setw(12) << energyLow
		     << " reset by matrix bound to " << setw(12)
		     << arrayMin << std::endl;
	       energyLow = arrayMin;
	   } 
	   if ( energyHigh > arrayMax ) 
	   {
               if (spectrumNumber)
                  tcout << "Spectrum "<<spectrumNumber<<":";
	       tcout << " Upper range bound " << setw(12) << energyHigh
		     << " reset by matrix bound to " << setw(12)
		     << arrayMax << std::endl;
	       energyHigh = arrayMax;
	   }

	   Real kFlux (0);
	   Real eFlux (0);

	   m->integrateFlux(spectrumNumber,energyLow, energyHigh, kFlux, eFlux);
	   m->keVFlux(spectrumNumber,kFlux);
	   if (lumin)
	   {
	       Real lumNorm  = LUMCON*fzsq(redshift,q0,lambda0)/H0/H0;
	       eFlux *= lumNorm;
	   }
	   m->ergFlux(spectrumNumber,eFlux);

	   modelFluxCalc.push_back(SpectralData::FluxCalc(eFlux,.0,.0,kFlux));
           // If fit is not valid, don't go into the error block later on
           // regardless if error flag is set to true.
	   if (!error || !m_isStillValid)
	   {
	       if (spectrumNumber && datasets->numberOfSpectra() > 1 && showOutput)
	       {
		   tcout << " Spectrum Number: " << spectrumNumber << std::endl;
                   tcout << " Data Group Number: " << currDataGroupNum << std::endl;
	       }
	       m->keVFluxRange(spectrumNumber,ZERO);
	       m->ergFluxRange(spectrumNumber,ZERO);
	       if (showOutput)
	       {
		   m->reportFluxes(spectrumNumber,1.+redshift,
				   lumin,energyLow,energyHigh);
	       }
	   }                               
       } // end mods for spectrum loop

       if(sd)
       {
          if (lumin)
	      sd->setLastModelLuminCalc(modelFluxCalc);
          else
	      sd->setLastModelFluxCalc(modelFluxCalc);
       }
       else
       {
	   for(size_t i = 0; i < nSources; ++i)
	   {
	       Model* m = modsForSpec[i];

	       if(lumin)
		   m->lastModelLuminCalc(modelFluxCalc[i]);
	       else
		   m->lastModelFluxCalc(modelFluxCalc[i]);
	   }
       }

       ++itSpec;
    } // end spectra loop

    if ( error && m_isStillValid)
    {
	// one vector per trial containing a vector of
	// size #spectra in fit containing a vector of # sources
	// being simultaneously fit.
	std::vector<std::vector< std::vector<Real> > > evFF(nTrials);
	std::vector<std::vector< std::vector<Real> > > ergFF(nTrials);    
	size_t N (m_variableParameters.size());
	size_t nFluxSpecs = specsAndMods.size();
	std::vector<Real> saveParameterValues(N);
	std::map<int,ModParam*>::const_iterator vp 
	    = m_variableParameters.begin();
	for ( size_t k = 0; k < N; ++k, ++vp)
	{
	    saveParameterValues[k] =    vp->second->value('a');    
	}

        reportErrorSimulationMethod();
        if (m_chainManager->isSynchedWithFitParams())
        {
           const std::vector<size_t>& chainLengths = m_chainManager->accumulatedLengths();
           size_t totLength = chainLengths[chainLengths.size()-1];
           if (static_cast<size_t>(nTrials) > totLength)
           {
              tcout<<"***Warning: The number of error samples: "<< nTrials 
                 <<"\n    is larger than the total lengths of loaded chains: "<< totLength
                 << std::endl;
           }
        }
	for (int j = 0; j < nTrials; ++j)
	{
	    std::vector<std::vector<Real> >& evf = evFF[j];  
	    std::vector<std::vector<Real> >& ergf = ergFF[j];  

	    evf.resize(nFluxSpecs);  
	    ergf.resize(nFluxSpecs);

            bool firstTime = !j;               
	    randomizeModelParameters(firstTime);

	    calculateModel();

	    // iterate through datasets/models  again.
            size_t iSp=0;
            itSpec = specsAndMods.begin();
	    while (itSpec != itSpecEnd)
	    {
	       const size_t spectrumNumber = itSpec->first;
               const std::vector<Model*>& modsForSpec = itSpec->second;
               const size_t nMods = modsForSpec.size();
	       std::vector<Real>& evff = evf[iSp];
	       std::vector<Real>& ergff = ergf[iSp];
	       evff.resize(nMods,0);
	       ergff.resize(nMods,0);
               for (size_t iMod=0; iMod<nMods; ++iMod)
	       {
		  Model* m = modsForSpec[iMod];

		  Real& evt = evff[iMod];
		  Real& ergt = ergff[iMod];
		  ArrayContainer::const_iterator itEng = 
		      m->energy().find(spectrumNumber);
		  ArrayContainer::const_iterator itEngEnd = m->energy().end();
		  if (itEng != itEngEnd)
		  {
		     const RealArray& energyArray = itEng->second;
		     const size_t N (energyArray.size());
		     const Real& arrayMax = energyArray[N-1];
		     const Real& arrayMin = energyArray[0];
		     Real energyLow = std::max(eMin,arrayMin);
		     Real energyHigh = std::min(eMax,arrayMax);
		     m->integrateFlux(spectrumNumber,energyLow, 
				      energyHigh, evt, ergt);
		     if (lumin)
		     {
		        Real lumNorm  
			    = LUMCON*fzsq(redshift,q0,lambda0)/H0/H0;
		        ergt *= lumNorm;
		     }
		  }
	       }  // foreach model fitting the spectrum
               ++iSp;
               ++itSpec;
	    } // foreach spectrum

	    vp = m_variableParameters.begin();
	    for ( size_t k = 0; k < N; ++k, ++vp)
	    {
		vp->second->setValue
		    (saveParameterValues[k],'a');       
	    }
	} // foreach trial

	// collect results and find confidence ranges
        try
        {
           size_t iSp=0;
           itSpec = specsAndMods.begin();
	   while (itSpec != itSpecEnd)
	   {
	       const size_t spectrumNumber = itSpec->first;
               const std::vector<Model*>& modsForSpec = itSpec->second;
               const size_t nMods = modsForSpec.size();
	       for (size_t iMod=0; iMod<nMods; ++iMod)
	       {
	          Model* m = modsForSpec[iMod];
	          ArrayContainer::const_iterator s 
		      (m->energy().find(spectrumNumber));
	          ArrayContainer::const_iterator sEnd (m->energy().end());
	          if ( s != sEnd )
	          {
		      const RealArray& energyArray = s->second;
		      std::vector<Real> teV(nTrials);
		      std::vector<Real> terg(nTrials);
		      for (int j = 0; j < nTrials; ++j)
		      {
		          teV[j] = evFF[j][iSp][iMod];
		          terg[j] = ergFF[j][iSp][iMod];
		      } 
                      // This can throw.
		      std::pair<Real,Real> ceVRange = 
		          XSutility::confidenceRange(level,teV,true);
		      std::pair<Real,Real> cergRange  =
		          XSutility::confidenceRange(level,terg,true);
		      m->keVFluxRange(spectrumNumber,ceVRange);
		      m->ergFluxRange(spectrumNumber,cergRange);

		      SpectralData::FluxCalc tmp; 
		      tmp.errLow = cergRange.first;
		      tmp.errHigh = cergRange.second;
                      tmp.photonLow = ceVRange.first;
                      tmp.photonHigh = ceVRange.second;
                      tmp.errorTrialVals = teV;
                      
		      if(spectrumNumber)
		      {
		          SpectralData* spec = datasets->lookup(spectrumNumber);

		          if (lumin)
		          {
			      tmp.value = spec->lastModelLuminCalc(iMod).value;
                              tmp.photonValue = spec->lastModelLuminCalc(iMod).photonValue;
			      spec->lastModelLuminCalc(iMod,tmp);
		          }
		          else
		          {
			      tmp.value = spec->lastModelFluxCalc(iMod).value;
                              tmp.photonValue = spec->lastModelFluxCalc(iMod).photonValue;
			      spec->lastModelFluxCalc(iMod,tmp);
		          }
		      }
		      else
		      {
                         // Can't see how it can ever get here if m_isStillValid
                         //  is true.
                         throw RedAlert("Attempting flux error calc with no fit spectra.");
		      }

		      if ( spectrumNumber && datasets->numberOfSpectra() > 1)
		      {
		          tcout << " Spectrum Number: " 
			        << spectrumNumber << std::endl;
		      }
		      const Real& arrayMax = energyArray[energyArray.size()-1];
		      const Real& arrayMin = energyArray[0];
		      Real energyLow (eMin);
		      Real energyHigh (eMax);
		      energyLow = std::max(energyLow,arrayMin);
		      energyHigh = std::min(energyHigh,arrayMax);
		      m->reportFluxes(spectrumNumber, 1.+redshift, lumin,
				      energyLow, energyHigh, level);
		      tcout << std::flush;  
	          }
	       }
               ++itSpec;
               ++iSp;       
	   } // end spectra loop
        }
        catch (YellowAlert&)
        {
           calculateModel();
           m_isStillValid = true;
           throw;
        }
        // restore model
        calculateModel();
        m_isStillValid = true;
    } // end if error

    if ( lumin )
    {
	tcout.precision(3);
	tcout << "     (z = " << std::showpoint << setw(5) << redshift ;
	tcout << " H0 = "  << setw(5) << H0*100. << " q0 = "  << setw(5) << q0;
	if ( lambda0 > 0 ) tcout << " Lambda0 = " << setw(5) << lambda0 << ")" << std::endl;
    }

    tcout.flags(currentSetting);
    tcout.precision(savePrecision);
}

bool Fit::getErrors (const IntegerArray& paramNums)
{
  using namespace std;
  
  if (!m_fitMethod->getErrors(this, paramNums))
  {
     bool doAnotherCalc = true;
     while (doAnotherCalc)
     {
        doAnotherCalc = false;
        ProcessManager procs(new ErrorCmd(), "error");
        // For now, not going to allow multiple processes at both the 'error' and
        //  Jacobian calculation levels.  If user has set 'leven', we'll temporarily
        //  set it to 1 before calling run.
        int savedLevenProcs=-1;
        map<string,int>::iterator itEnd = ProcessManager::maxProcs().end();
        map<string,int>::iterator itErrors = ProcessManager::maxProcs().find("error");
        if (itErrors != itEnd && itErrors->second > 1 && paramNums.size() > 1)
        {
           map<string,int>::iterator itLeven=ProcessManager::maxProcs().find("leven");
           if (itLeven != itEnd)
           {
              savedLevenProcs = itLeven->second;
              itLeven->second = 1;
           }
        }
        procs.createProcesses(paramNums.size());
           
        vector<TransferStruct> parallelInput;
        for (size_t i=0; i<paramNums.size(); ++i)
        {
           TransferStruct parInfo;
           parInfo.iValues.push_back(vector<int>(1, paramNums[i]));
           parallelInput.push_back(parInfo);
        }
        ProcessManager::ParallelResults results;
        // This will not throw:
        procs.run(parallelInput, results);
        if (savedLevenProcs > 1)
           ProcessManager::maxProcs()["leven"] = savedLevenProcs;
        ProcessManager::ParallelResults::const_iterator itResults = results.begin();
        size_t iPar=0;
                
        bool isParallelNewMin=false;
        bool isSingleNewMin=false;
        if (procs.isParallel())
        {
           // First check for 1 or more instances of new min found.
           //   Need to find lowest min AND set current
           //   parameters (in parent process) to new values. 
           Real lowestParallelStat=9.99e39;
           std::vector<Real> lowestParallelPars;
           while (itResults != results.end())
           {
              ModParam* currentPar = variableParameters(paramNums[iPar]);
              const TransferStruct& output = itResults->second;
              const size_t nMsgs = output.sValues.size();
              // output.sValues[1...nMsgs-2] will always be diagnostic messages.
              // [nMsg-1] will be the results string if everything's OK
              //   (status == 0), else it's another diagnostic message.
              if (nMsgs)
              {
                 currentPar->lastErrorStatus(output.sValues[0]);
                 for (size_t iMsg=1; iMsg<nMsgs; ++iMsg)
                 {
                    if (output.iValues[0][iMsg] < 0)
                    {
                       // This message was generated by a FitErrorOut exception.
                       tcerr << output.sValues[iMsg] << std::endl;
                    }
                    else
                       tcout << xsverbose(output.iValues[0][iMsg]) << output.sValues[iMsg]
                           << std::endl << xsverbose();
                 }
              }
              
              if (output.status == -2)
              {
                 isParallelNewMin = true;
                 // If not parallel, ErrorCmd::execute() will not
                 //  have filled in these arrays with anything.
                 if (output.dValues[1][0] < lowestParallelStat)
                 {
                    lowestParallelStat = output.dValues[1][0];
                    lowestParallelPars = output.dValues[0];
                 }
              }
              else if (output.status == 0)
              {
                 // Assume everything's OK.
                 currentPar->setValue(output.dValues[0][0], 'm');
                 currentPar->setValue(output.dValues[0][1], 'p');
              }
              ++itResults;
              ++iPar;
           } // end parallel results loop
           if (isParallelNewMin)
           {
              // Calling this here in case anything throws during the refit.
              //   Redundant calls to killProcesses won't hurt anything.
              procs.killProcesses();

              std::map<int,ModParam*>::const_iterator itVar = variableParameters().begin();
              std::map<int,ModParam*>::const_iterator itVarEnd = variableParameters().end();
              size_t iVar=0;
              while (itVar != itVarEnd)
              {
                 itVar->second->setValue(lowestParallelPars[iVar]);
                 ++itVar;
                 ++iVar;
              }           
              calculateModel();
              initializeStatistic();
              // We want the re-fit to display, so set s_errorCalc to false.
              s_errorCalc=false;
              perform();
              report();
	      tcerr << "***Warning: New best fit found, fit parameters will be set to new values." <<endl;
           }
        } // end if parallel
        else
        {
           while (itResults != results.end())
           {
              const TransferStruct& output = itResults->second;
              if (output.status == -2)
              {
                 isSingleNewMin=true;
              }
              ++itResults;
              ++iPar;
           }
        }
        
        
        if (isParallelNewMin || isSingleNewMin)
        {
           // For a newMinFound error, add it to the error status flag for
           //  ALL of the parameters sent to the 'error' command.  This is
           //  the one exception where an error in one parameter's
           //  calculation affects another's error status string.
           
           // For the parameter(s) which triggered the newMinFound, this
           //   will redundantly add to the status flag that's already
           //   been set.  But that's OK.
           iPar=0;
           while (iPar < paramNums.size())
           {
              ModParam* par = variableParameters(paramNums[iPar]);
              // Is this check necessary?  Probably not but can't hurt.
              if (par)
              {
                 string newErrString = FitErrorCalc::errCodeToString(
                      FitErrorCalc::ErrorCalcCodes(FitErrorCalc::stringToErrCode(
                      par->lastErrorStatus()) | FitErrorCalc::NEWMIN));
                 par->lastErrorStatus(newErrString);  
              }
              ++iPar;
           }
           
           if (s_doNewMinRecalc)
           {
              doAnotherCalc = true;
           }
           // Need to replace m_oldParameters with new best fit values:
           reinitialize(true);
           if (!doAnotherCalc)
           {
              tcerr<<"***Warning: auto-repeat is off for error calculation when new best fit found.\n\n";
              procs.killProcesses();
              throw YellowAlert();
           }                 
        }
        else
        {
           // If here, then for NO param was there a new min found
           //  whether parallel or single.
           itResults = results.begin();
           iPar=0;
           while (itResults != results.end())
           {
              const TransferStruct& output = itResults->second;
              if (output.status < 0)
              {
                 procs.killProcesses();                 
                 throw YellowAlert();
              }
              ++itResults;
              ++iPar;
           } 
        }
        procs.killProcesses();
     } // end doAnotherCalc
     
  } // end if fitMethod->getErrors
  return true;
}

Step* Fit::stepGrid ()
{
  return m_stepGrid.get();
}

void Fit::stepGrid (Step* value)
{
  m_stepGrid.reset(value);
}

void Fit::initializeStatistic (bool isUpdate)
{

    std::pair<int,size_t> dofVals = m_statManager->totalDegreesOfFreedom();
    if (dofVals.first < 0) 
    {
       throw FitOverDetermined();       
    }


    if (m_renormType == AUTO || (m_renormType == PREFIT && !isUpdate)) 
    {
            // can throw "can't initialize".
            m_statManager->renormalizeStats();       
    }
    // this has the byproduct of computing the initial
    // model difference array, which we need.
    m_statManager->performStats();
}

void Fit::freezeByIndex (int index)
{

    // std::map<K,V>::erase returns 1 if erasure was successful.
    if ( !m_variableParameters.erase(index))
    {
            tcerr << "*** Warning: attempt to freeze non-fit parameter "<<std::endl;       
    }      
}

Real Fit::statistic () const
{

  return m_statManager->totalStatistic();
}

void Fit::perform ()
{
  m_fitMethod->perform(this);
}

void Fit::calcEqErrors (const XSContainer::EqWidthRecord& currComp, const std::vector<Model*>& mods, size_t iMod, size_t nIter, Real confLevel)
{
      using XSContainer::datasets;
      // Calculate errors based on gaussian distribution of parameters.
      if (!m_isStillValid)
      {
         tcout <<"\nCannot determine error of equiv width without a valid fit."<<std::endl;
         return;
      }
      if (!nIter)
      {
         return;
      }
      Model* mod = mods[iMod];
      std::vector<Real> eqWidths(nIter);
      size_t nParams = m_variableParameters.size();
      std::vector<Real> saveParameterValues(nParams);
      std::map<int,ModParam*>::const_iterator vp = m_variableParameters.begin();
      for (size_t i=0; i<nParams; ++i, ++vp)
      {
         saveParameterValues[i] = vp->second->value();
      }
      Real savLastEqWidth = .0;
      // Find the first spectrum in the first Dataset that matches mod's
      // data group.  All lastEqWidths for this group should be the same.
      DataArrayIt itDs = datasets->dataArray().begin();
      DataArrayIt itDsEnd = datasets->dataArray().end();
      while (itDs != itDsEnd)
      {
         DataSet* ds = itDs->second;
         if (ds->dataGroup() == mod->dataGroupNumber())
         {
            if (ds->isMultiple())
            {
               savLastEqWidth = ds->multiSpectralData().begin()->second->
                        lastEqWidthCalc().value;
            }
            else
            {
               savLastEqWidth = ds->spectralData()->lastEqWidthCalc().value;
            }
            break;
        } 
         ++itDs;
      }

      if (m_chainManager->isSynchedWithFitParams())
      {
         const std::vector<size_t>& chainLengths = m_chainManager->accumulatedLengths();
         size_t totLength = chainLengths[chainLengths.size()-1];
         if (nIter > totLength)
         {
            tcout<<"***Warning: The number of error samples: "<< nIter 
               <<"\n    is larger than the total lengths of loaded chains: "<< totLength
               << std::endl;
         }
      }

      bool resetVerbose=false;
      try
      {
         for (size_t i=0; i<nIter; ++i)
         {
            bool firstTime = !i;
            randomizeModelParameters(firstTime);
            calculateModel();

            // Do not print the various eqwidth output messages
            // while merely building up an errors array.
            tpout << xsverbose(14);
            resetVerbose=true;
            XSContainer::models->clearSources();
            XSContainer::models->makeSourceComponents(mods);
            eqWidths[i] = XSContainer::models->calcEqWidths(currComp, mod);
            vp = m_variableParameters.begin();
            for (size_t i=0; i<nParams; ++i, ++vp)
            {
               vp->second->setValue(saveParameterValues[i]);
            }
            // reset chatter levels
            tpout << xsverbose();
            resetVerbose=false;
         }
      }
      catch (YellowAlert&)
      {
         // calcEqWidths changes what is stored in lastEqWidthCalc, so
         // it needs to be restored.
         itDs = datasets->dataArray().begin();
         SpectralData::FluxCalc eqWidthStruct(savLastEqWidth, 0., 0.);
         while (itDs != itDsEnd)
         {
            DataSet* ds = itDs->second;
            if (ds->dataGroup() == mod->dataGroupNumber())
            {
               if (ds->isMultiple())
               {
                  SpectralDataMapConstIt itSd = ds->multiSpectralData().begin();
                  SpectralDataMapConstIt itSdEnd = ds->multiSpectralData().end();
                  while (itSd != itSdEnd)
                  {
                     itSd->second->lastEqWidthCalc(eqWidthStruct);
                     ++itSd;
                  }
               }
               else
               {
                  SpectralData* sd = ds->spectralData();
                  if (sd)
                  {
                     sd->lastEqWidthCalc(eqWidthStruct);
                  }
               }
            }
            ++itDs;
         }

         vp = m_variableParameters.begin();
         for (size_t i=0; i<nParams; ++i, ++vp)
         {
            vp->second->setValue(saveParameterValues[i]);
         }
         calculateModel();
         m_isStillValid = true;
         if (resetVerbose)
            tpout << xsverbose();
         throw;
      }
      // Note: fit statistic hasn't been changed during the errors
      // loop, so it doesn't need to be recalculated.
      calculateModel();
      m_isStillValid = true;

      std::pair<Real,Real> confRange(0.0,0.0);
      try
      {
         // This performs a full sort on eqWidths in addition to finding
         //    the confidence boundaries.  It also may throw.
         confRange = XSutility::confidenceRange(confLevel, eqWidths, true);
      }
      catch (...)
      {
         // Don't exit this function.  We still need to reset the
         // spectra's lastEqWdithCalcs below.  confRange will
         // retain its obviously wrong 0.0 values.
      }
      if (nIter < static_cast<size_t>(s_MCrealizations))
      {
         tcout << "\n***Warning: The number of simulations for error determination is low."
              <<std::endl;
      }
      tcout << "Equiv width error range:  " << confRange.first << " - "
            << confRange.second << " keV" << std::endl;

      itDs = datasets->dataArray().begin();
      SpectralData::FluxCalc eqWidthStruct(savLastEqWidth, confRange.first,
                                confRange.second);
      eqWidthStruct.errorTrialVals = eqWidths;
      while (itDs != itDsEnd)
      {
         DataSet* ds = itDs->second;
         if (ds->dataGroup() == mod->dataGroupNumber())
         {
            if (ds->isMultiple())
            {
               SpectralDataMapConstIt itSd = ds->multiSpectralData().begin();
               SpectralDataMapConstIt itSdEnd = ds->multiSpectralData().end();
               while (itSd != itSdEnd)
               {
                  itSd->second->lastEqWidthCalc(eqWidthStruct);
                  ++itSd;
               }
            }
            else
            {
               SpectralData* sd = ds->spectralData();
               if (sd)
               {
                  sd->lastEqWidthCalc(eqWidthStruct);
               }
            }
         }
         ++itDs;
      }
}

std::vector<Model*> Fit::getModsForFlux (const SpectralData* sd)
{
   // If sd exists, models vector will return with mods in order
   // sorted by sourceNum.  Else, models vector will contain
   // all active/off mods sorted alphabetically.
   std::vector<Model*> mods;
   if (sd)
   {
      const size_t dataGroup = sd->parent()->dataGroup();
      const std::vector<Response*>& detectors = sd->detector();
      for (size_t i=0; i<detectors.size(); ++i)
      {
         if (detectors[i])
         {
            const string modName(models->lookupModelForSource(i+1));
            if (modName.length())
            {
               Model* mod = models->lookup(modName, dataGroup);
               mods.push_back(mod);
            }
         }
      }
   }
   else
   {
      std::map<string,bool>::const_iterator itModName =
                models->activeModelNames().begin();
      std::map<string,bool>::const_iterator itModNameEnd =
                models->activeModelNames().end();
      while (itModName != itModNameEnd)
      {
         if (itModName->second)
         {
            Model* mod = models->lookup(itModName->first);
            mods.push_back(mod);
         }
         ++itModName;
      }
   }
   return mods;
}

const RealArray& Fit::getSVDevalue () const
{
   const FitMethod* cfitMethod = 
        const_cast<const FitMethod*>(m_fitMethod.get());
   return cfitMethod->evalue();
}

const RealArray& Fit::getSVDevector () const
{
   const FitMethod* cfitMethod = 
        const_cast<const FitMethod*>(m_fitMethod.get());
   return cfitMethod->evector();
}

const RealArray& Fit::getSVDcovariance () const
{
   const FitMethod* cfitMethod = 
        const_cast<const FitMethod*>(m_fitMethod.get());
   return cfitMethod->covariance();
}

bool Fit::checkChainsForSynch () const
{
  // Naturally this should be called only after variableParameters
  // map has been updated.  The idea is to make ChainManager's 
  // isSynchedWithFitParams flag the only thing that needs to be checked 
  // to determine whether to get the distribution from covariance or 
  // from chains.

  // If there ARE active/on models, their variable parameters (if any) will 
  // set the matching criteria.  If NO active/on models, it could mean there
  // are no loaded spectra or that no spectra have responses assigned to a 
  // source with a defined model.  In those cases, the variable parameters 
  // in all active/off models will set the matching criteria.

  // The case of no chains loaded is defined as inSync = FALSE.
  // The case of chains loaded but somehow no chainPars (if that is even possible)
  //    is defined as inSync = FALSE.  
   bool areAllParsMatched = false;
   if (m_chainManager->chains().size())
   {
      // All loaded chains must have the same pars, so just
      // grab the info from the first chain.
      const std::vector<Chain::ParamID>& chainPars = 
                m_chainManager->chains().begin()->second->paramIDs();
      if (chainPars.size())
      {
         std::map<int,ModParam*> paramsToMatch;

         if (m_activeModels.size())
         {
            paramsToMatch = m_variableParameters;
         }
         else
         {
            ParamMapConstIter itLoadedPars = models->parameterList().begin();
            ParamMapConstIter itLoadedEnd = models->parameterList().end();
            while (itLoadedPars != itLoadedEnd)
            {
               Parameter* par = itLoadedPars->second;
               const string& modName = par->modelName();
               if (models->activeModelNames(modName))
               {
                  ModParam* modPar = dynamic_cast<ModParam*>(par);
                  if (modPar && !(modPar->isFrozen() || modPar->isLinked()))
                  {
                     // Use the fullIndex for paramsToMatch key.  Want
                     // things in the same order as when coming from
                     // m_variableParameters map.
                     int fullIndex = models->keyToIndex(itLoadedPars->first);
                     paramsToMatch[fullIndex] = modPar;
                  }
               }
               ++itLoadedPars;
            }
         }

         if (chainPars.size() == paramsToMatch.size())
         {
            areAllParsMatched = true;
            std::vector<Chain::ParamID>::const_iterator itChainPar = 
                      chainPars.begin();
            std::map<int,ModParam*>::const_iterator itVar = 
                      paramsToMatch.begin();
            std::map<int,ModParam*>::const_iterator itVarEnd = 
                      paramsToMatch.end();
            while (itVar != itVarEnd)
            {
               const Chain::ParamID& chainPar = *itChainPar;
               const ModParam* fitPar = itVar->second;
               // Note that passing the test below does NOT guarantee that 
               // parameters in the chain file were actually generated from 
               // those currently stored in the variableParameters map.  
               if (chainPar.modName != fitPar->modelName() ||
                   chainPar.parName != fitPar->name() ||
                   chainPar.index != fitPar->index() ||
                   chainPar.units != fitPar->unit())
               {
                  areAllParsMatched = false;
                  break;
               }
               ++itVar, ++itChainPar;
            }
         }
      }  // end if chains have pars
   } // end if chains loaded
   return areAllParsMatched;
}

void Fit::reportErrorSimulationMethod () const
{
   tcout << "Parameter distribution is derived from ";
   if (m_chainManager->isSynchedWithFitParams())
      tcout << "loaded chain files." << std::endl;
   else
      tcout <<"fit covariance matrix." << std::endl;
}

void Fit::randomizeForChain (bool onlyInit, bool useChainPropSetting)
{
  RandomizerBase* rand=0;
  if (useChainPropSetting)
  {
     rand = m_chainManager->chainProposal();
  }
  else
  {
    // Assume chain is using something other than M-H (ie. Goodman-Weare),
    // and hardcode to use "gaussian fit".
    rand = getRandomizingStrategy("gaussian fit");
  }
  if (onlyInit)
  {
     rand->initializeRun(this);
  }
  else
  {
    // Get a new randomized set of parameter values. If they are valid then
    // use them to replace the current values. Might have to try this a few
    // times to get valid new value.
    RealArray saveParVals = variableParameterValues('v');
    RealArray parVals = saveParVals;

    bool done(false);
    size_t counter(0);
    while (!done && counter < 1000) {
      rand->randomize(parVals, this);
      if ( goodVariableParameterValues(parVals,'v') ) {
	setVariableParameterValues(parVals,'v');
	// If fit was valid before, it sure isn't now.  Leave it up
	// to client to decide if it wants to return fit to valid state.
	m_isStillValid = false; 
	done = true;
      }
      counter++;
    }
    // if we have failed to set new randomized values then give up and just
    // make sure we put back the input values.
    if (!done) {
      setVariableParameterValues(saveParVals,'v');
    }
  }
}

string Fit::getChainProposalNames () const
{
   string names;
   std::map<string,RandomizerBase*>::const_iterator itRand = m_randomizingStrategies.begin();
   std::map<string,RandomizerBase*>::const_iterator itEnd = m_randomizingStrategies.end();
   while (itRand != itEnd)
   {
      RandomizerBase* rand = itRand->second;
      if (!names.empty())
         names += " | ";
      string::size_type cmdLoc = rand->name().find("<cmdline>");
      if (cmdLoc != string::npos)
      {
         // Give a more precise description.
         // ie. "<dist> <cmdline>" becomes  
         // "<dist> diagonal <values> | <dist> matrix <values>"
         const string distName(rand->name().substr(0, cmdLoc));
         names += distName + "diagonal <values> | " + distName + "matrix <values>";
      }
      else
        names += rand->name();
      ++itRand;
   }
   return names;
}

void Fit::registerRandomizingStrategy (const string& name, RandomizerBase* strategy)
{
   const string ws(" \t\n");
   if (name.find_first_not_of(ws) == string::npos)
   {
      throw YellowAlert("No name provided for add-on randomizer class.\n");
   }
   // Assuming that the list of reserved names in m_nativeRandomizerNames
   // will be filled AFTER all native strategies have been entered.  
   // Therefore this check should really only ever apply to user-entered 
   // strategies.
   if (m_nativeRandomizerNames.size())
   {
      // User classes should only have one word names, no whitespace.
      if (name.find_first_of(ws) != string::npos)
      {
         throw YellowAlert("Whitespace is not allowed in name member of add-on randomizer classes\n");
      }
      if (m_nativeRandomizerNames.find(name) != m_nativeRandomizerNames.end())
      {
         string errMsg("Cannot add randomizing strategy named ");
         errMsg += name;
         errMsg += ".\nThe name is reserved for built-in randomizing options.\n";
         throw YellowAlert(errMsg);
      }
   }

   std::map<string,RandomizerBase*>::iterator it = m_randomizingStrategies.find(name);
   if (it != m_randomizingStrategies.end())
   {
      tcout <<"***Warning: An add-on randomizing strategy named "<< name 
        <<" already exists.  It will be replaced." <<std::endl;
      delete it->second;
      m_randomizingStrategies.erase(it);
   }
   m_randomizingStrategies[name] = strategy;
}

RandomizerBase* Fit::getRandomizingStrategy (const string& name)
{
   RandomizerBase* pStrategy = 0;
   // If strategy doesn't exist, simply return a null pointer and
   // let calling function decide if it wants to issue a message.
   // This allows for abbreviated names to match.
   std::map<string,RandomizerBase*>::iterator it =
                m_randomizingStrategies.lower_bound(name);
   if (it != m_randomizingStrategies.end() && it->first.find(name) == 0)
      pStrategy = it->second;
   return pStrategy;
}

void Fit::clearRandomizingStrategies ()
{
   std::map<string,RandomizerBase*>::iterator itRand = m_randomizingStrategies.begin();
   std::map<string,RandomizerBase*>::iterator itRandEnd = m_randomizingStrategies.end();
   while (itRand != itRandEnd)
   {
      delete itRand->second;
      ++itRand;
   }
   m_randomizingStrategies.clear();
}

void Fit::initNativeRandomizerNames ()
{
   // This should only be called once, during start-up.  If it detects
   // existing entries it will do nothing.  Only the FIRST part of
   // compound names are stored.
   if (m_nativeRandomizerNames.empty())
   {
      std::map<string,RandomizerBase*>::const_iterator itRand = 
                m_randomizingStrategies.begin();
      std::map<string,RandomizerBase*>::const_iterator itEnd =
                m_randomizingStrategies.end();
      while (itRand != itEnd)
      {
         string::size_type firstWS = itRand->first.find(' ');
         string primaryName(itRand->first.substr(0, firstWS));
         m_nativeRandomizerNames.insert(primaryName);
         ++itRand;
      }
   }
}

void Fit::reportChainAcceptance (bool isAccepted) const
{
   RealArray parVals(.0, m_variableParameters.size());
   std::map<int,ModParam*>::const_iterator itVp = m_variableParameters.begin();
   std::map<int,ModParam*>::const_iterator itVpEnd = m_variableParameters.end();
   size_t i=0;
   while (itVp != itVpEnd)
   {
      parVals[i] = itVp->second->value();
      ++itVp, ++i;
   }  
   m_chainManager->chainProposal()->acceptedRejected(parVals, isAccepted);
}

ModParam* Fit::oldParameterValues (int index) const
{
  std::map<int,ModParam*>::const_iterator p (m_oldParameterValues.find(index));
  if (p != m_oldParameterValues.end()) return p->second;
  else return 0;
}

ModParam* Fit::variableParameters (int index) const
{
  std::map<int,ModParam*>::const_iterator p (m_variableParameters.find(index));
  if (p != m_variableParameters.end()) return p->second;
  else return 0;
}

RealArray Fit::variableParameterValues(const char key)
{
  const std::map<int,ModParam*>& varPars = m_variableParameters;
  std::map<int,ModParam*>::const_iterator itVp = varPars.begin();

  size_t nPars = varPars.size();
  RealArray values(nPars);

  for (size_t i=0; i<nPars; i++, itVp++) values[i] = itVp->second->value(key);

  return values;
}

void Fit::setVariableParameterValues(const RealArray& values, const char key)
{
  const std::map<int,ModParam*>& varPars = m_variableParameters;
  std::map<int,ModParam*>::const_iterator itVp = varPars.begin();

  size_t nPars = varPars.size();

  if ( nPars != values.size() ) return;

  for (size_t i=0; i<nPars; i++, itVp++) itVp->second->setValue(values[i],key);

  return;
}

// return false if any of the input values are invalid (eg outside the hard limits)

bool Fit::goodVariableParameterValues(const RealArray& values, const char key)
{
  const std::map<int,ModParam*>& varPars = m_variableParameters;
  std::map<int,ModParam*>::const_iterator itVp = varPars.begin();

  size_t nPars = varPars.size();

  if ( nPars != values.size() ) return false;

  Real saveValue = itVp->second->value(key);
  for (size_t i=0; i<nPars; i++, itVp++) {
    int flag = itVp->second->setValue(values[i],key);
    itVp->second->setValue(saveValue,key);
    if ( flag < 0 ) return false;
  }

  return true;
}

void Fit::ErrorCmd::execute(const bool isParallel, const TransferStruct& input, TransferStruct& output)
{
   using namespace XSContainer;
   
   const int parNum = input.iValues[0][0];
   // This assumes that parNum has been verified to match an existing
   //    variable parameter.
   FitErrorCalc errCalcObj(fit, parNum, isParallel);
   try
   {
      errCalcObj.perform();
   }
   catch (FitErrorCalc::NewMinFound& nmerr)
   {
      output.status = -2;
      if (isParallel)
      {
         const ModParam* par = errCalcObj.parameter();
         output.sValues.clear();
         output.sValues.push_back(par->lastErrorStatus());
         output.dValues.clear();
         output.dValues.push_back(std::vector<double>());
         
         std::map<int,ModParam*>::const_iterator itVar = fit->variableParameters().begin();
         std::map<int,ModParam*>::const_iterator itVarEnd = fit->variableParameters().end();
         while (itVar != itVarEnd)
         {
            output.dValues[0].push_back(itVar->second->value());
            ++itVar;
         }
         output.dValues.push_back(std::vector<double>(1, fit->statistic()));
         transferMessageQueue(output, errCalcObj.msgQueue());
      }
      throw;
   }
   catch (...)
   {
      if (isParallel)
      {
         const ModParam* par = errCalcObj.parameter();
         output.sValues.clear();
         output.sValues.push_back(par->lastErrorStatus());
         transferMessageQueue(output, errCalcObj.msgQueue());
      }
      output.status = -1;
      throw;
   }
   const ModParam* par = errCalcObj.parameter();
   std::vector<double> errRange(2);
   errRange[0] = par->emn();
   errRange[1] = par->epe();
   const string& errString = par->lastErrorStatus();
   
   output.dValues.clear();
   output.dValues.push_back(errRange);
   output.sValues.clear();
   output.sValues.push_back(errString);
   if (isParallel)
      transferMessageQueue(output, errCalcObj.msgQueue());
   
}

void Fit::ErrorCmd::transferMessageQueue(TransferStruct& output, std::queue<std::pair<string,int> >& messages)
{
   // Message queue strings will fill slots 1-n of sValues array (slot 0 is already
   //  filled with the error status string).  Corresponding message verbose levels
   //  should fill slots 1-n of iValues[0] with element 0 being just a dummy placeholder.
   const size_t sz=messages.size();
   output.iValues.clear();
   output.iValues.push_back(std::vector<int>(sz+1));
   output.iValues[0][0]=0;
   for (size_t i=0; i<sz; ++i)
   {
      std::pair<string,int>& msg = messages.front();
      output.sValues.push_back(msg.first);
      output.iValues[0][i+1] = msg.second;
      messages.pop();
   }   
      
}

void Fit::ParallelSim::execute(const bool isParallel, const TransferStruct& input, TransferStruct& output)
{
   using namespace XSContainer;
   
   const size_t nSims = static_cast<size_t>(input.iValues[0][0]);
   const bool doSimulateParams = static_cast<bool>(input.iValues[0][1]);
   const bool doFit = static_cast<bool>(input.iValues[0][2]);
      
   ArrayContainer modelsSim;
   const size_t nPars = fit->variableParameters().size();
   RealArray saveParameterValues(nPars);
   
   if (!doSimulateParams)
   {
      // simulateModel will need to be called just once rather 
      //   than for every simulation.
      fit->simulateModel(modelsSim, 0);
   }
   
   if (doSimulateParams || doFit )
   {
      if (doSimulateParams && !isParallel)
         fit->reportErrorSimulationMethod();
      std::map<int,ModParam*>::const_iterator itVp = fit->variableParameters().begin();
      std::map<int,ModParam*>::const_iterator itVpEnd = fit->variableParameters().end();
      size_t k=0;
      while (itVp != itVpEnd)
      {
         saveParameterValues[k] = itVp->second->value('a');
         ++itVp, ++k;    
      }
   } 

   // Simulated spectrum is initialized for each trial by modelsSim.
   // Background spectrum though must be restored each time.
   std::map<size_t,RealArray> saveBackSpecs;
   std::map<size_t,SpectralData*>::const_iterator itCSpec = fit->spectraToFit().begin();
   std::map<size_t,SpectralData*>::const_iterator itCSpecEnd = fit->spectraToFit().end();
   while (itCSpec != itCSpecEnd)
   {
      const Background* bck = itCSpec->second->background();
      if (bck)
      {
         saveBackSpecs[itCSpec->first].resize(bck->spectrum().size());
         saveBackSpecs[itCSpec->first] = bck->spectrum();
      }
      ++itCSpec;
   } 
   
   bool resetVerbose=false;
   try
   {
      output.dValues.clear();
      output.dValues.resize(1);
      std::vector<double>& trialStats = output.dValues[0];
      trialStats.resize(nSims, 0.0);
      
      // just to stress that the operation of randomizing the model parameters
      // and randomizing the background are quite different.
      for (size_t j=0; j<nSims; ++j)
      {
         if (doSimulateParams)
         {
            // First time through, flag = 2 tells model randomizer 
            // to initialize for run.
            int randomizeFlag = j ? 1 : 2;
            fit->simulateModel(modelsSim,randomizeFlag);
            std::map<int,ModParam*>::const_iterator itVp = fit->variableParameters().begin();
            for ( size_t k = 0; k < nPars; ++k, ++itVp)
            {
               itVp->second->setValue(saveParameterValues[k],'a');       
            }
         }

         std::map<size_t,SpectralData*>::const_iterator itSpec = fit->spectraToFit().begin();
         std::map<size_t,SpectralData*>::const_iterator itSpecEnd = fit->spectraToFit().end();
         while (itSpec != itSpecEnd)
         {
            const size_t spectrumNumber = itSpec->first;
            SpectralData* data = itSpec->second;

            // Background must be restored each time, see note above.
            Background* bck = data->getBackground();
            if (bck)
            {
               bck->setSpectrum(saveBackSpecs[spectrumNumber]);
            }

            data->spectrum() = modelsSim[spectrumNumber];
            // This function will perform the randomization and store
            // the new spectra (including variances).
            data->simulateSpectrum(true);
            
            tpout << xsverbose(15);
            resetVerbose = true;
            if (!isParallel && tpout.maxChatter() >= 15 )
            {
               const RealArray& model = modelsSim[spectrumNumber];
               const RealArray& simSpec = data->spectrum();
               const RealArray& simVar = data->variance();
               const size_t nChans = simSpec.size();
               tcout << "Simulation: " << j << std::endl; 
               if (data->background())
               {
                  const RealArray& simBack = data->background()->spectrum();
                  const RealArray& bckVar = data->background()->variance();
                  tcout << " simModel " << " randSpec " << " randVar " 
                     << " rand B " << " randBVar " << std::endl; 
                  for (size_t k = 0; k < nChans ; ++k)
                  {
                     tcout << k << " " << model[k] << " " << simSpec[k] << " " 
                        << simVar[k] << "  " << simBack[k] << " " 
                        << bckVar[k] << std::endl;                                        
                  }
               }
               else
               {
                  tcout << " simModel " << " randSpec " << " randVar " << std::endl;
                  for (size_t k = 0; k < nChans ; ++k)
                  {
                     tcout << k << " " << model[k] << " " << simSpec[k] << " " 
                        << simVar[k] << std::endl;
                  }
               }
               tcout << " simvarTot " << simVar.sum() << std::endl;
            }

            ++itSpec;
         }
         if (doFit)
            fit->perform();
         if (resetVerbose)
         {
            tpout << xsverbose();
            resetVerbose = false;
         }
         fit->statManager()->performStats();
         trialStats[j] = fit->statManager()->totalTestStatistic();
         
	 // if we have done a fit then reset parameter values.
	 if (doFit) 
         {
	    std::map<int,ModParam*>::const_iterator itVp = fit->variableParameters().begin();
	    std::map<int,ModParam*>::const_iterator itEnd = fit->variableParameters().end();
	    size_t k=0;
	    while(itVp != itEnd)
            {
	       itVp->second->setValue(saveParameterValues[k],'a'); 
	       ++itVp, ++k;      
	    }
	 }
         
      } // end nSims loop         
   }
   catch (...)
   {
      if (resetVerbose)
         tpout << xsverbose();
      output.status = -1;
      // Only restore changes to the models.  The restoring of the 
      // original spectra and fit statistic will be handled
      // higher up.
      if (doSimulateParams || doFit)
      {
         std::map<int,ModParam*>::const_iterator itVp = fit->variableParameters().begin();
         std::map<int,ModParam*>::const_iterator itEnd = fit->variableParameters().end();
         size_t k=0;
         while(itVp != itEnd)
         {
            itVp->second->setValue(saveParameterValues[k],'a'); 
            ++itVp, ++k;      
         }
      }
      throw;
   } 

   
}
