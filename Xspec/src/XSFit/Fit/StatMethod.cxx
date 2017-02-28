//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Weight
#include <XSModel/GlobalContainer/Weight.h>
// StatMethod
#include <XSFit/Fit/StatMethod.h>

#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatManager.h>
#include <XSModel/Model/CompCombiner.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <cmath>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <set>
using namespace XSContainer;


// Class StatMethod::InvalidSpectrumNumber 

StatMethod::InvalidSpectrumNumber::InvalidSpectrumNumber (size_t num)
  : RedAlert(" initializing spectra for fit:\n ")  
{
  std::ostringstream message;
  message << " Statistic tried to access spectrum number " << num << " not loaded ";
  reportAndExit(message.str(),-5);
}


// Class StatMethod::InvalidParameterError 

StatMethod::InvalidParameterError::InvalidParameterError ()
  : RedAlert(" retrieving model derivative array:\n")              
{
  std::ostringstream message;
  message << " Statistic calculation attempted to retrieve parameter derivative not allocated ";
  reportAndExit(message.str(),-5);
}


// Class StatMethod::FitNotDefined 

StatMethod::FitNotDefined::FitNotDefined (const string& diag)
  : YellowAlert (" no model defined or no data read ")
{
  tcerr << diag << '\n';
}


// Class StatMethod::NoSuchSpectrum 

StatMethod::NoSuchSpectrum::NoSuchSpectrum (const string& diag)
  : YellowAlert(" spectrum not loaded ")
{
}


// Class StatMethod 


StatMethod::StatMethod (const std::string& name)
  : m_statistic(0.),
    m_isDerivAnalytic(false),
    m_spectraForStat(),
    m_name(name),
    m_protoWorkspace(),
    m_number(1)
{
}


StatMethod::~StatMethod()
{
}



bool StatMethod::checkWeight (const XSContainer::Weight* weightMethod) const
{
  //  static const string STANDARD ("standard");
  // Default checkWeight only accepts standard weighting.
  //  const string weightName = weightMethod->name();
  //  return STANDARD.find(weightName) == 0;
  // Allow all Weight methods
  return true;
}

void StatMethod::initialize (Fit* fit)
{

  doInitialize(fit);
}

void StatMethod::doInitialize (Fit* fit)
{

  // clear all and start again.
  deallocate();
  // allocate memory for derivative arrays. If derivatives are not
  // required we don't need to do anything.

  // create proto workspace

  for (size_t i=0; i<m_spectraForStat.size(); ++i)
  {
     const SpectralData* spec = m_spectraForStat[i];
     size_t N = spec->indirectNotice().size();
     m_protoWorkspace.insert(ArrayContainer::value_type(spec->spectrumNumber(),RealArray(0.,N)));  
  }

}

void StatMethod::deallocate ()
{

        doDeallocate();
}

void StatMethod::doDeallocate ()
{
        m_protoWorkspace.clear(); 
}

void StatMethod::renormalize (Fit* fit)
{

        if (  models->activeModelNames().empty() || datasets->numberOfSpectra() == 0)
        {
                throw Fit::CantInitialize(" renorm: either no model defined or no data read ");      

        }
        else
        {
           if (m_name == "cstat" || m_name == "lstat" || m_name == "pgstat")
           {
              // These stat methods aren't set up for renorming with
              // background data.  
              for (DataArrayConstIt itDs = datasets->dataArray().begin(); 
                    itDs != datasets->dataArray().end(); ++itDs)
              {
                 const DataSet* ds = itDs->second;
                 if (ds->isMultiple())
                 {
                    SpectralDataMapConstIt itSd = ds->multiSpectralData().begin();
                    while (itSd != ds->multiSpectralData().end())
                    {
                       const SpectralData* sd = itSd->second;
                       if (sd && sd->background())  return;
                       ++itSd;
                    }
                 }
                 else if (const SpectralData* sd = ds->spectralData())
                 {
                    if (sd->background())  return;
                 }
              }
           }

           // first, compute and store current model.
           std::map<Model*,bool> frozenComponents;

           size_t numberOfNorms (0);
           size_t numberOfFrozenNorms(0);
           
           // If any mixing is detected, skip renorm entirely.
           // Note:  If renorming is to be reinstated at some point for
           // mixing models, must ensure that no zero-sized SumComponent
           // arrays get passed down to mix function. (This bug arose in 
           // the past when spectra with no frozen norms were bundled for 
           // mixing with those which do have them.) 
           bool anyMixing=false;           
           ModelMapConstIter itMod (models->modelSet().begin());
           ModelMapConstIter itModEnd (models->modelSet().end());
           while ( itMod != itModEnd && !anyMixing)
           {
              Model* currentModel = itMod->second;
              frozenComponents[currentModel] = false;
              if ( currentModel->isActive() ) 
              { 
                 if (currentModel->mixingLocations().first.size() ||
                     currentModel->mixingLocations().second.size())
                    anyMixing = true;
                 else
                 {                  
                    std::list<ModParam*> norms (currentModel->normParams());
                    numberOfNorms += norms.size();
                    for (std::list<ModParam*>::const_iterator np = norms.begin();
                         np != norms.end(); ++np)
                    {
                        if ((*np)->isFrozen() || Parameter::isLinkedToFrozen(*np))
                        {
	                   frozenComponents[currentModel] = true; 
                           ++numberOfFrozenNorms;
                        }            
                    }
                 }  
              }
              ++itMod;
           }
           if (anyMixing)
              return;
              
           if (numberOfFrozenNorms == numberOfNorms )
           {
               if (!fit->errorCalc())
               {        
                   tcout << " Warning: renorm - no variable model to allow  renormalization"
                         << std::endl; 
               }   
           }
           else
           {
              std::pair<Real,Real> fSums;
              Real ofSum(0.);    // =>     fSums[0]
              Real mfSum(0.);    // =>     fSums[1]
              doReset(fit);

              std::map<Model*,ArrayContainer > saveFrozenModVals;
              std::set<string>::const_iterator itModNames = fit->activeModels().begin();
              std::set<string>::const_iterator itModNamesEnd = fit->activeModels().end();
              while (itModNames != itModNamesEnd)
              {                 
                 std::vector<Model*> modGroups = models->lookupModelGroup(*itModNames);
                 
                 // Only models with frozen norms are to be recalculated.
                 bool anyFrozenNormsInGroup=false;
                 
                 for (size_t iMod=0; iMod<modGroups.size(); ++iMod)
                 {
                    Model* mod = modGroups[iMod];
                    if (frozenComponents[mod])
                       anyFrozenNormsInGroup = true;
                 }
                 for (size_t iMod=0; iMod<modGroups.size(); ++iMod)
                 {
                    Model* mod = modGroups[iMod];
                    mod->fold();
                    if (frozenComponents[mod])
                    {
                       ArrayContainer& savModVal = saveFrozenModVals[mod];
                       ArrayContainer::const_iterator it = mod->foldedModel().begin();
                       ArrayContainer::const_iterator itEnd = mod->foldedModel().end();
                       while (it != itEnd)
                       {
                          savModVal.insert(ArrayContainer::value_type(it->first,it->second));
                          ++it;
                       }
                       mod->calcComponents(false, true);
                       std::vector<Model*> modToCombine(1, mod);
                       CompCombiner combiner(modToCombine);
                       CompCombiner::iterator itComb = combiner.begin();
                       CompCombiner::iterator itCombEnd = combiner.end();
                       while (itComb != itCombEnd)
                       {
                          ++itComb;
                       }
                       mod->fold(); 
                    }
                 }
                 ++itModNames;
              } // end modNames loop



              // Want to analyze spectra grouped by data group number.
              // DataContainer currently provides no ready-made way to
              // do this, so in order to avoid O(N^2) behavior let's
              // collate things here.
              const size_t nDgs = datasets->numberOfGroups();
              const size_t nSpecs = datasets->numberOfSpectra();
              std::vector<std::vector<const SpectralData*> > specsByGroup(nDgs);
              for (size_t iSp=1; iSp<=nSpecs; ++iSp)
              {
                 const SpectralData* spec = datasets->lookup(iSp);
                 const size_t dgNum = spec->parent()->dataGroup();
                 specsByGroup[dgNum-1].push_back(spec);
              }

              for (size_t iDg=0; iDg<nDgs; ++iDg)
              {
                 std::vector<Model*> modsForGroup(datasets->numSourcesForSpectra(),0);
                 const std::set<size_t>& sourcesForGroup =  
                        datasets->dgToSources().find(iDg+1)->second;
                 std::set<size_t>::const_iterator itSource =
                        sourcesForGroup.begin();
                 std::set<size_t>::const_iterator itSourceEnd =
                        sourcesForGroup.end();
                 while (itSource != itSourceEnd)
                 {
                    const string modName = models->lookupModelForSource(*itSource);
                    if (modName.length())
                    {
                       // Model must be active/on if in here.
                       Model* mod = models->lookup(modName, iDg+1);
                       modsForGroup[*itSource-1] = mod;
                    }
                    ++itSource;
                 }

                 const std::vector<const SpectralData*>& specsForGroup = 
                                specsByGroup[iDg];
                 for (size_t iSp=0; iSp<specsForGroup.size(); ++iSp)
                 {
                    const SpectralData* spec = specsForGroup[iSp];
                    const std::vector<Response*>& detectors = spec->detector();
                    std::vector<Model*> modsForSpec;
                    for (size_t iDet=0; iDet<detectors.size(); ++iDet)
                    {
                       if (detectors[iDet])
                       {
                          if (modsForGroup[iDet])
                             modsForSpec.push_back(modsForGroup[iDet]);
                       }
                    }
                    // adjustForFrozen functions ASSUME at least one 
                    // model is applied to spec, so filter out no model 
                    // case here.
                    if (modsForSpec.size())
                    {
                       fSums = adjustForFrozen(spec, modsForSpec, saveFrozenModVals);
                       ofSum += fSums.first;
                       mfSum += fSums.second;
                    }
                 } 
              } // end data group loop


              Real renorm(1.);
              if(std::abs(ofSum - mfSum) < SMALL)
              {
                  if (!fit->errorCalc())
                  {
                      tcout << " renorm: no renormalization necessary" <<std::endl;   
                  }
              }
              else if(std::abs(mfSum) > SMALL)
              { 
	          renorm = ofSum / mfSum;
                  itMod = models->modelSet().begin();
                  while (itMod != itModEnd)
                  {
	              Model* currentModel (itMod->second);
                      if ( currentModel->isActive()) 
                      {
	                 std::list<ModParam*> norms (currentModel->normParams());
	                 std::list<ModParam*>::iterator np = norms.begin();
	                 std::list<ModParam*>::iterator npEnd = norms.end();
	                 while (np != npEnd)
	                 {
		             if (!(*np)->isFrozen() && !(*np)->isLinked())
		             {
		                 Real newValue = (*np)->value()*renorm;
		                 newValue = std::max( (*np)->value('l'),
					      std::min(newValue,(*np)->value('h')));
		                 (*np)->setValue(newValue,'v');
		             }
		             ++np;       
	                 }
                      }
	              ++itMod;
                  }
              }
           } // end frozenNorms != numNorms

        }
}


size_t StatMethod::nBinsUsed () const
{
    size_t nBins = 0;
    const size_t nSpec = m_spectraForStat.size();
    for (size_t i=0; i<nSpec; ++i)
    {
       nBins += m_spectraForStat[i]->indirectNotice().size();
    }      
    return nBins;
}

void StatMethod::report () const
{

     if (!m_spectraForStat.empty())
        doReport();
}

void StatMethod::peakResidual (const Fit* fit, std::pair<Real,Real>& max, std::pair<Real,Real>& min, size_t spectrum, const std::pair<Real,Real> range) const
{

  ArrayContainer difference (getDifferences(fit));

  doPeakResidual(fit,difference,max,min,spectrum,range);
}

void StatMethod::doPeakResidual (const Fit* fit, const ArrayContainer& difference, std::pair<Real,Real>& max, std::pair<Real,Real>& min, size_t spectrum, const std::pair<Real,Real> range) const
{

  if ( fit->activeModels().empty() || fit->spectraToFit().empty()) 
  {          
     throw StatMethod::FitNotDefined(" : Peak Residual");
  }


  std::map<size_t,SpectralData*>::const_iterator sm = fit->spectraToFit().find(spectrum);
  if (sm == fit->spectraToFit().end()) 
  {
     throw YellowAlert("No response*model associated with spectrum\n");   
  }

  SpectralData* sp = sm->second;
  ArrayContainer::const_iterator f = difference.find(spectrum);
  const RealArray& residual  = f->second;
  const std::valarray<size_t>& IN = sp->indirectNotice();
  int M (IN.size());
  int lower(0);
  int upper(M-1);
  if ( range.first != 0 && range.second != 0)
  {
     sp->energyChannel(range.first,lower);
     sp->energyChannel(range.second,upper);
  }
  Real maxResid (-LARGE);
  Real maxEnergy (0);


  Real minResid (LARGE);
  Real minEnergy (0);
  int jmax (0),jmin (0);
  for (int j = lower; j <= upper; ++j)
  {
     Real resid = residual[j];
     if (resid > maxResid) 
     {
        maxResid = resid;
        maxEnergy = sp->channelEnergy(j);
        jmax = j;
     }         
     if (resid < minResid) 
     {
        minResid = resid;
        minEnergy = sp->channelEnergy(j);
        jmin = j;
     } 
  }

  int nResps = 0;
  Real minEffArea = 0.0;
  Real maxEffArea = 0.0;     
  for (size_t i=0; i<sp->detector().size(); ++i)
  {
     const Response* rsp = sp->detector(i);
     if (rsp)
     {
        int idx;
        XSutility::find(rsp->energies(), maxEnergy, idx);
        const RealArray& effectiveArea = rsp->efficiency();
        if (idx < 0 || static_cast<size_t>(idx) >= effectiveArea.size())
        {
           // This should never happen
           throw YellowAlert("Peak resid energy is outside effective area array\n");
        }
        maxEffArea += effectiveArea[idx];
        XSutility::find(rsp->energies(), minEnergy, idx);
        if (idx < 0 || static_cast<size_t>(idx) >= effectiveArea.size())
        {
           // This should never happen
           throw YellowAlert("Peak resid energy is outside effective area array\n");
        }
        minEffArea += effectiveArea[idx];
        ++nResps;
     }
  }
  // nResps must be > 0 or it would have been caught above.
  maxEffArea /= static_cast<Real>(nResps);
  if (maxEffArea == 0.0)
     throw YellowAlert("Zero effective area for bin: Cannot calculate peak strength.\n");
  minEffArea /= static_cast<Real>(nResps);
  if (minEffArea == 0.0)
     throw YellowAlert("Zero effective area for bin: Cannot calculate peak strength.\n");
  max.first = maxResid/maxEffArea;
  max.second = maxEnergy;
  min.first = minResid/minEffArea;
  min.second = minEnergy;
}

Real StatMethod::sumDerivs (const ArrayContainer& dFolded, int parameterIndex) const
{
   return .0;
}

Real StatMethod::plotChi (Real obs, Real mod, Real error, Real areaTime, const Real& NODATA) const
{

  return 0;
}

Real StatMethod::plotDeltaChi (Real obs, Real mod, Real error, Real areaTime, const Real& NODATA) const
{

  return 0;
}

Real StatMethod::sumSecondDerivs (const std::map<size_t,ArrayContainer>& dFolded, int pi, int pj)
{
   return .0;
}

Real StatMethod::setOutputFormat (const Real statistic, int degrees)
{
   using namespace std;
   Real deltaCrit = abs(XSContainer::fit->fitMethod()->deltaCrit());
   if (deltaCrit <= 0.0)  
      throw YellowAlert("Zero or neg critical delta detected in StatMethod::setOutputFormat.\n");
   Real truncatedStat = .0;
   Real absStatistic = statistic;
   if (statistic == 0.0)
   {
      // just print 0.0
      tcout << fixed << setprecision(1);
   }
   else
   {
      if (statistic < .0)
         absStatistic *= -1.0;
      int expDelta = static_cast<int>(floor(log10(deltaCrit)));
      int expStat = static_cast<int>(floor(log10(absStatistic)));
      int nDigits = expStat - expDelta + 1;
      if (nDigits <= 0)
      {
         // This case shouldn't really ever happen, but on the outside
         // chance, adjust expDelta to give 1 digit.
         expDelta = expStat;
         nDigits = 1;
      }
      Real stat = absStatistic;
      int lowestExp = expDelta;
      if (degrees)
      {
         // We're dealing with a reduced-chisq calc.  
         // nDigits should remain the same.
         stat /= degrees;
         expStat = static_cast<int>(floor(log10(stat)));
         lowestExp = expStat - nDigits + 1;
      }
      double truncator = pow(10.0, lowestExp);
      truncatedStat = floor(stat/truncator + .5)*truncator;
      if (expStat > 5 || expStat < -3)
      {
         if (nDigits == 1)
         {
            // use general format
            tcout.setf(ios_base::fmtflags(0), ios_base::floatfield);
            tcout.precision(1);
         }
         else
         {
            int prec = min(nDigits-1, 6);
            tcout << scientific << setprecision(prec);
         }
      }
      else
      {
         int prec = (lowestExp < 0) ? -lowestExp : 0;
         prec = min(prec, 6-expStat);
         tcout << fixed << setprecision(prec);
      }
      // Put back the '-' if there is one.
      if (statistic < .0)
         truncatedStat *= -1.0;
   }
   return truncatedStat;
}

void StatMethod::getWeightDerivs()
{
}

// Additional Declarations
