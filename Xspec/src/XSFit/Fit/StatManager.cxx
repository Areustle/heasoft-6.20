#include <XSFit/Fit/StatManager.h>

#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/Weight.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>
#include <iostream>
#include <iomanip>

StatManager* StatManager::s_instance = 0;
Bayes StatManager::s_bayes = Bayes();
std::map<string, XSContainer::Weight*> StatManager::s_weightingMethods;

StatManager::StatManager()
 :m_statMethods(),
  m_testStatMethods(),
  m_specNumToStatMethod(),
  m_totalStatistic(0.0),
  m_totalTestStatistic(0.0),
  m_bayesianPrior(0.0),
  m_usingSingleStat(0),
  m_defaultStat(0),
  m_usingSingleTestStat(0),
  m_defaultTestStat(0),
  m_alpha(),
  m_beta(),
  m_weightCmdSetting("standard"),
  m_jacobianCalc(new ParDiff(), "leven")
{
}  

StatManager::~StatManager()
{
   std::map<string,StatMethod*>::iterator itSm = m_statMethods.begin();
   std::map<string,StatMethod*>::iterator itEnd = m_statMethods.end();
   while (itSm != itEnd)
   {
      delete itSm->second;
      ++itSm;
   }
   m_statMethods.clear();

   std::map<string,StatMethod*>::iterator ittSm = m_testStatMethods.begin();
   std::map<string,StatMethod*>::iterator ittEnd = m_testStatMethods.end();
   while (ittSm != ittEnd)
   {
      delete ittSm->second;
      ++ittSm;
   }
   m_testStatMethods.clear();
}

StatManager* StatManager::Instance()
{
   if (s_instance == 0)
   {
      s_instance = new StatManager();
   }
   return s_instance;
}

void StatManager::Update (Subject* changed)
{
   using namespace XSContainer;

   m_totalStatistic = 0.0;
   m_totalTestStatistic = 0.0;
   m_bayesianPrior = 0.0;
   // In addition to the direct effects of the statistic command,
   // StatMethod configurations can be affected indirectly through
   // the data, model, and response commands.  Any of these changes
   // may also lead to a change of the "in-use" weight method (though
   // the user-requested weight method stored in m_weightCmdSetting
   // will remain the same).

   // Note that if a weight command is the cause of either creating or
   // removing a stat/weight conflict, no message will be printed from
   // here and no variance recalculations will occur.  It will have
   // already done all of this from the handler.

   const string& origWeightInUse = DataUtility::statWeight().name();
   const bool wasWeightConflict = m_weightCmdSetting != origWeightInUse;
   setSpectraForStatMethods(fit->spectraToFit());
   setSpectraForTestStatMethods(fit->spectraToFit());
   const string problemStat = checkStatsWeighting();
   const bool isWeightConflict = !problemStat.empty();
   const string newWeightInUse = isWeightConflict ? string("standard") :
                        m_weightCmdSetting;

   // Don't let exceptions propagate from anywhere in Notify/Update chain.
   try
   {
      if (newWeightInUse != origWeightInUse)
      {
         DataUtility::weightScheme(getWeightingMethod(newWeightInUse));
         recalculateWeightVariances();
      }

      if (wasWeightConflict && !isWeightConflict)
      {
         tcout << "*** Statistic and weighting function conflict has been removed."
               << "\n  Now using " << m_weightCmdSetting << " weighting.\n"
               <<std::endl;
      }
      else if (!wasWeightConflict && isWeightConflict)
      {
         tcout << "*** Warning: statistic " <<problemStat<<" does not recognize weighting"
               << "\n        function " << m_weightCmdSetting 
               << ": will use standard weighting instead.\n" <<std::endl;
      }

      if (!fit->spectraToFit().empty() && !fit->activeModels().empty())
      {
         // stat calculation may throw.
         initializeStats();
         if (fit->renormType()==Fit::AUTO)
         {
            renormalizeStats();
            performStats();           
         }
         else
         {
            // When not auto-renorming, there's no reason to perform
            // new Model calculate and fold operations.  These should
            // already have been done in Fit::Update and should still
            // be valid.
            performStats(false);
         }
         if (!Fit::errorCalc())
            reportStats();
      }
   }
   catch (YellowAlert&)
   {
      // Any exceptions should have sent out an error message, but
      // let's make it more obvious that something is wrong.
      m_totalStatistic = -1.0;
   }
} // end StatManager::Update

void StatManager::registerStatMethod(const string& name, StatMethod* method)
{
   m_statMethods.insert(std::map<string,StatMethod*>::value_type(name, method));
}
void StatManager::registerTestStatMethod(const string& name, StatMethod* method)
{
   m_testStatMethods.insert(std::map<string,StatMethod*>::value_type(name, method));
}

void StatManager::setSpectraForStatMethods(const std::map<size_t,SpectralData*>& spectraToFit)
{
   std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
   while (itStat != itStatEnd)
   {
      itStat->second->spectraForStat().clear();
      ++itStat;
   }

   std::map<size_t,SpectralData*>::const_iterator itSpec = spectraToFit.begin();
   std::map<size_t,SpectralData*>::const_iterator itEnd = spectraToFit.end();
   while (itSpec != itEnd)
   {
      const SpectralData* spec = itSpec->second;
      const string& statName = spec->statName();
      // ASSUME statName has been validated.
      itStat = m_statMethods.find(statName);
      std::vector<const SpectralData*>& specsForStat = 
                itStat->second->spectraForStat();
      specsForStat.push_back(spec);
      ++itSpec;
   }

   checkForSingleStat();
   buildSpecNumToStatMethod();
}
void StatManager::setSpectraForTestStatMethods(const std::map<size_t,SpectralData*>& spectraToFit)
{
   std::map<string,StatMethod*>::const_iterator itStat = m_testStatMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd = m_testStatMethods.end();
   while (itStat != itStatEnd)
   {
      itStat->second->spectraForStat().clear();
      ++itStat;
   }

   std::map<size_t,SpectralData*>::const_iterator itSpec = spectraToFit.begin();
   std::map<size_t,SpectralData*>::const_iterator itEnd = spectraToFit.end();
   while (itSpec != itEnd)
   {
      const SpectralData* spec = itSpec->second;
      const string& testStatName = spec->testStatName();
      // ASSUME testStatName has been validated.
      itStat = m_testStatMethods.find(testStatName);
      std::vector<const SpectralData*>& specsForTestStat = 
                itStat->second->spectraForStat();
      specsForTestStat.push_back(spec);
      ++itSpec;
   }

   checkForSingleTestStat();
   buildSpecNumToTestStatMethod();
}

void StatManager::renormalizeStats() const
{
   // Attempt a renormalization only if using a single stat.
   if (m_usingSingleStat)
   {
         m_usingSingleStat->renormalize(XSContainer::fit);
   }  
}

void StatManager::initializeStats()
{
   using namespace XSContainer;

   std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
   while (itStat != itStatEnd)
   {
      if (!itStat->second->getSpectraForStat().empty())
         itStat->second->initialize(XSContainer::fit);
      ++itStat;
   }   

   std::map<string,StatMethod*>::const_iterator ittStat = m_testStatMethods.begin();
   std::map<string,StatMethod*>::const_iterator ittStatEnd = m_testStatMethods.end();
   while (ittStat != ittStatEnd)
   {
      if (!ittStat->second->getSpectraForStat().empty())
         ittStat->second->initialize(XSContainer::fit);
      ++ittStat;
   }   

}

void StatManager::reportStats() const
{
   using namespace std;
   std::pair<int,size_t> dofVals = totalDegreesOfFreedom();
   if (dofVals.first < 0)
   {
     tcout << "Ill-formed Fit problem - number of variable parameters exceeds number of bins"
        << std::endl;
   }
   else 
   {
      if (!XSContainer::fit->activeModels().empty())
      { 
	 tcout << "\nFit statistic : ";
         std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
         std::map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
         while (itStat != itStatEnd)
         {
            itStat->second->report();
            ++itStat;
         }
         if (m_usingSingleStat)
         {
            if (m_bayesianPrior != 0.0)
               tcout << "   (Statistic includes a Bayesian contribution = "
                   <<m_bayesianPrior<<")" << std::endl;
         }
         else
         {
            ios_base::fmtflags save(tcout.flags());
            streamsize savePrec(tcout.precision());
            Real truncatedTotal = StatMethod::setOutputFormat(m_totalStatistic);
            int totalDegrees = totalDegreesOfFreedom().first;
            tcout << "Total Statistic = " << std::setw(14) << truncatedTotal
               << " with " << totalDegrees << " degrees of freedom.\n";
            if (m_bayesianPrior != 0.0)
            {
               tcout <<"   (Total includes a Bayesian contribution = "
                        <<m_bayesianPrior<<")\n";
            }
            tcout << std::endl;
            tcout.precision(savePrec);
            tcout.flags(save);
         }

	 tcout << "\nTest statistic : ";
         std::map<string,StatMethod*>::const_iterator ittStat = m_testStatMethods.begin();
         std::map<string,StatMethod*>::const_iterator ittStatEnd = m_testStatMethods.end();
         while (ittStat != ittStatEnd)
         {
            ittStat->second->report();
            ++ittStat;
         }
         if (!m_usingSingleStat)
         {
            ios_base::fmtflags save(tcout.flags());
            streamsize savePrec(tcout.precision());
            Real truncatedTotal = StatMethod::setOutputFormat(m_totalTestStatistic);
            int totalDegrees = totalDegreesOfFreedom().first;
            tcout << "nTotal Test Statistic = " << std::setw(14) << truncatedTotal
               << " with " << totalDegrees << " degrees of freedom.\n";
            tcout << std::endl;
            tcout.precision(savePrec);
            tcout.flags(save);
         }

      }
      if (!XSContainer::fit->isStillValid())
         tcout << " Current data and model not fit yet." << std::endl;         
   }   
}

void StatManager::buildSpecNumToStatMethod()
{
   using namespace XSContainer;

   // Even if spectrum isn't suitable for fit, still keep track of 
   // its StatMethod object here.
   const size_t nSpectra = datasets->numberOfSpectra();
   m_specNumToStatMethod.resize(nSpectra);
   for (size_t i=0; i<nSpectra; ++i)
   {
      const SpectralData* spec = datasets->lookup(i+1);
      StatMethod* statMethod = m_statMethods.find(spec->statName())->second;
      m_specNumToStatMethod[i] = statMethod;
   }
}
void StatManager::buildSpecNumToTestStatMethod()
{
   using namespace XSContainer;

   // Even if spectrum isn't suitable for fit, still keep track of 
   // its StatMethod object here.
   const size_t nSpectra = datasets->numberOfSpectra();
   m_specNumToTestStatMethod.resize(nSpectra);
   for (size_t i=0; i<nSpectra; ++i)
   {
      const SpectralData* spec = datasets->lookup(i+1);
      StatMethod* testStatMethod = m_testStatMethods.find(spec->testStatName())->second;
      m_specNumToTestStatMethod[i] = testStatMethod;
   }
}

void StatManager::performStats(bool doCalcFold)
{
   using namespace XSContainer;

   m_totalStatistic = 0.0;
   m_totalTestStatistic = 0.0;
   m_bayesianPrior = 0.0;
   
   if (doCalcFold)
   {
      std::set<string>::iterator ss (fit->activeModels().begin());
      std::set<string>::iterator ssEnd (fit->activeModels().end());
      while ( ss != ssEnd)
      {
         models->calculate(*ss);
         models->fold(*ss);
         ++ss;
      }
   }  

   // lPrior returns 0.0 if Bayes flag is not on.
   // For backwards compatibility reasons with the stat reporting output,
   // if only 1 stat in use, apply the Bayesian contribution directly
   // to that 1 stat.  Otherwise wait till end of stat loop and apply to total.

   // For exception safety concerns (in case StatMethod perform should throw),
   // let's not modify m_bayesianPrior till after we're out of stat loop.
   const Real bayesianPrior = -2.0* s_bayes.lPrior(fit);

   std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
   while (itStat != itStatEnd)
   {
      StatMethod* statMeth = itStat->second;
      if (!statMeth->getSpectraForStat().empty())
      {
         statMeth->perform(XSContainer::fit);
         if (m_usingSingleStat)
            statMeth->statistic(statMeth->statistic() + bayesianPrior);
         m_totalStatistic += itStat->second->statistic();
      }
      ++itStat;
   }

   std::map<string,StatMethod*>::const_iterator ittStat = m_testStatMethods.begin();
   std::map<string,StatMethod*>::const_iterator ittStatEnd = m_testStatMethods.end();
   while (ittStat != ittStatEnd)
   {
      StatMethod* testStatMeth = ittStat->second;
      if (!testStatMeth->getSpectraForStat().empty())
      {
         testStatMeth->perform(XSContainer::fit);
         m_totalTestStatistic += ittStat->second->statistic();
      }
      ++ittStat;
   }

   m_bayesianPrior = bayesianPrior;
   if (!m_usingSingleStat)
      m_totalStatistic += m_bayesianPrior;  
}

void StatManager::peakResidual(std::pair<Real,Real>&  max, std::pair<Real,Real>&  min, size_t spectrum, const std::pair<Real,Real>& range) const
{
   StatMethod* statMethod = m_specNumToStatMethod[spectrum-1];
   statMethod->peakResidual(XSContainer::fit,max,min,spectrum,range);
}

void StatManager::checkForSingleStat()
{
   size_t nInUse=0;
   std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
   while (itStat != itStatEnd)
   {
      if (itStat->second->spectraForStat().size())
      {
         m_usingSingleStat = itStat->second;
         ++nInUse;
      }
      ++itStat;
   }
   if (nInUse != 1)
      m_usingSingleStat = 0;
}

void StatManager::checkForSingleTestStat()
{
   size_t nInUse=0;
   std::map<string,StatMethod*>::const_iterator itStat = m_testStatMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd = m_testStatMethods.end();
   while (itStat != itStatEnd)
   {
      if (itStat->second->spectraForStat().size())
      {
         m_usingSingleTestStat = itStat->second;
         ++nInUse;
      }
      ++itStat;
   }
   if (nInUse != 1)
      m_usingSingleTestStat = 0;
}

void StatManager::differentiateStats(const IntegerArray& which)
{
   m_alpha.resize(0);
   m_beta.resize(0);

   bool useNumeric = XSContainer::fit->useNumericalDifferentiation();
   if (!useNumeric)
   {
      // If ANY of the stats IN USE are incapable of analytic differentiation,
      // then switch to numeric regardless of the setting in the Xspec.init file.
      std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
      std::map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
      while (!useNumeric && itStat != itStatEnd)
      {
         if (itStat->second->spectraForStat().size())
         {
            if (!itStat->second->isDerivAnalytic())
               useNumeric = true;
         }
         ++itStat;
      }
   }

   useNumeric ? numericalDifferentiate(which) : 
                analyticDifferentiate(which);
}

StatMethod* StatManager::setDefaultStat(const string& name)
{
   using namespace XSContainer;
   StatMethod* newDefault = 0;
   std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.find(name);
   if (itStat != m_statMethods.end())
   {
      m_defaultStat = itStat->second;
      newDefault = itStat->second;
      // When this is called during Xspec start-up, Fit container
      // doesn't yet exist (and naturally no spectra are loaded).
      if (fit)
      {
         const size_t nSpectra = datasets->numberOfSpectra();
         for (size_t i=0; i<nSpectra; ++i)
         {
            SpectralData* spec = datasets->lookup(i+1);
            spec->statName(newDefault->name());
         }
      }  
   }
   return newDefault;
}

StatMethod* StatManager::setDefaultTestStat(const string& name)
{
   using namespace XSContainer;
   StatMethod* newDefault = 0;
   std::map<string,StatMethod*>::const_iterator itStat = m_testStatMethods.find(name);
   if (itStat != m_testStatMethods.end())
   {
      m_defaultTestStat = itStat->second;
      newDefault = itStat->second;
      // When this is called during Xspec start-up, Fit container
      // doesn't yet exist (and naturally no spectra are loaded).
      if (fit)
      {
         const size_t nSpectra = datasets->numberOfSpectra();
         for (size_t i=0; i<nSpectra; ++i)
         {
            SpectralData* spec = datasets->lookup(i+1);
            spec->testStatName(newDefault->name());
         }
      }  
   }
   return newDefault;
}

void StatManager::setStatWeight(const string& weightName)
{
   XSContainer::Weight* weightMethod = getWeightingMethod(weightName);
   if (!weightMethod)
   {
      string err("\nNo weighting method with name: ");
      err += weightName;
      throw YellowAlert(err);
   }

   const string statName = m_usingSingleStat ? 
                   m_usingSingleStat->fullName()+string(" statistic: "):
                   string("All statistics: ");
   // First must determine if all StatMethods in use can accept the
   // requested weighting method.  If not, ALL will be set to standard
   // weighting.  But keep a record of what the user has requested.  It
   // will be substituted back if/when the weighting conflict has been
   // removed.
   m_weightCmdSetting = weightMethod->name();
   string problemStat = checkStatsWeighting();
   if (!problemStat.empty())
   {
      tcout << "*** Warning: statistic " << problemStat << " does not recognize weighting \n"
            << " scheme " << weightName << ": will use standard weighting instead."
            << std::endl;
      weightMethod = getWeightingMethod("standard");
   }
   DataUtility::weightScheme(weightMethod);
   tcout << "Variance weighted using " << weightMethod->name()
         << " weighting" << std::endl;
}

StatMethod* StatManager::getStatMethod(const string& name) const
{
   // first check for any numerals on the end of the name and split them off
   size_t ipos = name.find_first_of("0123456789");
   string statname = name.substr(0,ipos);
   int numeral(-999);
   if ( ipos != string::npos ) {
     std::istringstream num(name.substr(ipos,string::npos));
     num >> numeral;
   }

   StatMethod* stat = 0;
   bool found = false;
   std::map<string,StatMethod*>::const_iterator itStat=m_statMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd=m_statMethods.end();
   while (itStat != itStatEnd) {
     string fullname = itStat->first;
     if ( fullname.find(statname) == 0 ) {
       if ( !found ) {
	 stat = itStat->second;
	 found = true;
       } else {
	 // if another match was already made then we have an ambiguous entry so
	 // pass back a null pointer
	 return 0;
       }
     }
     itStat++;
   }

   if ( numeral != -999 ) stat->number(numeral);

   return stat;
}

StatMethod* StatManager::getTestStatMethod(const string& name) const
{
   // first check for any numerals on the end of the name and split them off
   size_t ipos = name.find_first_of("0123456789");
   string statname = name.substr(0,ipos);
   int numeral(-999);
   if ( ipos != string::npos ) {
     std::istringstream num(name.substr(ipos,string::npos));
     num >> numeral;
   }

   StatMethod* stat = 0;
   bool found = false;
   std::map<string,StatMethod*>::const_iterator itStat=m_testStatMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd=m_testStatMethods.end();
   while (itStat != itStatEnd) {
     string fullname = itStat->first;
     if ( fullname.find(name) == 0 ) {
       if ( !found ) {
	 stat = itStat->second;
	 found = true;
       } else {
	 // if another match was already made then we have an ambiguous entry so
	 // pass back a null pointer
	 return 0;
       }
     }
     itStat++;
   }

   if ( numeral != -999 ) stat->number(numeral);

   return stat;
}

std::pair<int,size_t> StatManager::totalDegreesOfFreedom() const
{
   int totDof=0;
   size_t totBins=0;
   std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
   while (itStat != itStatEnd)
   {
      totBins += itStat->second->nBinsUsed();
      ++itStat;
   }
   totDof = totBins - XSContainer::fit->variableParameters().size();
   return std::pair<int,size_t>(totDof, totBins);
}

string StatManager::statNames () const
{
  std::map<string,StatMethod*>::const_iterator sm = m_statMethods.begin();
  std::map<string,StatMethod*>::const_iterator smEnd = m_statMethods.end();
  string names;
  while ( sm != smEnd)
  {
          names += sm->first;
          ++sm;       
          if (sm != smEnd) names += " | ";
          else names += " ";
  }

  return names;      
}

string StatManager::testStatNames () const
{
  std::map<string,StatMethod*>::const_iterator sm = m_testStatMethods.begin();
  std::map<string,StatMethod*>::const_iterator smEnd = m_testStatMethods.end();
  string names;
  while ( sm != smEnd)
  {
          names += sm->first;
          ++sm;       
          if (sm != smEnd) names += " | ";
          else names += " ";
  }

  return names;      
}

void StatManager::numericalDifferentiate(const IntegerArray& parNums)
{
   using namespace XSContainer;
   using namespace std;

   const size_t nPars = parNums.size();
   RealArray dPars(.0,nPars);
   m_beta.resize(nPars,0.0);
   for (size_t i=0; i<nPars; ++i)
   {
      ModParam* par = fit->variableParameters(parNums[i]);
      Real origVal = par->value('a');
      Real delta = par->value('d');      
      par->setValue(origVal+delta,'a');      
      performStats();
      Real plusDeltaStat = m_totalStatistic;
      par->setValue(origVal-delta,'a');
      performStats();
      Real minusDeltaStat = m_totalStatistic;
      dPars[i] = delta;
      par->setValue(origVal,'a');
      m_beta[i] = -.25*(plusDeltaStat-minusDeltaStat)/delta;
   }
   if (fit->fitMethod()->secondDerivativeRequired())
   {
      size_t i=0;
      RealArray alphaElems(0.0,nPars*nPars);
      // Calculate:
      // [(S(pi+di,pj+dj)-S(pi-di,pj+dj))-(S(pi+di,pj-dj)-S(pi-di,pj-dj))]/(4di*dj)
      while (i < nPars)
      {
         ModParam* par_i = fit->variableParameters(parNums[i]);
         const Real orig_i = par_i->value('a');
         const size_t iPos = i*nPars;
         const Real dPars_i = dPars[i];
         for (int iDel=1; iDel>=-1; iDel-=2)
         {
            par_i->setValue(orig_i+iDel*dPars_i,'a');
            size_t j=0;
            while (j <= i)
            {
               ModParam* par_j = fit->variableParameters(parNums[j]);
               Real orig_j = par_j->value('a');
               for (int jDel=1; jDel>=-1; jDel-=2)
               {
                  par_j->setValue(orig_j+jDel*dPars[j],'a');
                  performStats();
                  const size_t jElem = j+iPos;
                  alphaElems[jElem] += iDel*jDel*m_totalStatistic/(dPars_i*dPars[j]);
                  if (i != j)
                     alphaElems[i + nPars*j] = alphaElems[jElem];
               }
               par_j->setValue(orig_j,'a');
               ++j;
            }
         }
         par_i->setValue(orig_i,'a');
         ++i;
      }
      alphaElems /= 8.0;
      m_alpha.resize(alphaElems.size());
      m_alpha = alphaElems;
   }
   // This will reset all model fluxes as well as statistic.
   performStats();

} // end numericalDifferentiate

void StatManager::analyticDifferentiate(const IntegerArray& parNums)
{        
    // Differentiate the statistic with respect to parameters with
    // indices specified by parNums.

    // NOTE that since each model's parameters are independent of the other
    // models, the result of all this  will be one set of derivative arrays 
    // per model per spectrum.
    using namespace XSContainer;
    using namespace std;

    const size_t N = parNums.size();
    m_beta.resize(N,0.0);

    // Calculate dsigma/dF arrays for the 2nd term of dStat/dp, which is
    // dStat/dsigma*dsigma/dF*dF/dp.  Since not every StatMethod will need
    // this (due to dStat/dsigma = 0), only do this down at the StatMethod
    // level even though dsigma/dF is itself independent of StatMethod.
    map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
    map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
    while (itStat != itStatEnd)
    {
       if (!itStat->second->getSpectraForStat().empty())
          itStat->second->getWeightDerivs();
       ++itStat;
    }
    
    // Each child process needs to know the CURRENT value of ALL the
    // variable fit parameters, not just the one it's toggling.  But it 
    // only needs to know the delta value for its own.
    vector<double> allVarParVals(N);
    for (size_t k=0; k<N; ++k)
    {
       ModParam* par = fit->variableParameters(parNums[k]);
       allVarParVals[k] = par->value('a');
    }
    
    static const int respFloor = Fit::RESPAR_INDEX() << ModelContainer::SHIFT();
    vector<TransferStruct> parallelInput;
    for (size_t k=0; k<N; ++k)
    {
       TransferStruct parInfo;
       ModParam* par = fit->variableParameters(parNums[k]);
       parInfo.iValues.push_back(vector<int>(1, parNums[k]));
       parInfo.dValues.push_back(vector<double>(2));
       parInfo.dValues[0][0] = par->value('a');
       parInfo.dValues[0][1] = par->value('d');
       parInfo.iValues.push_back(parNums);
       parInfo.dValues.push_back(allVarParVals);
       if (parNums[k] > respFloor)
       {
           // For response params, child processes will need the up-to-date
           // M flux to perform the M*dR/dP calculation.
           const ResponseParam* respPar = dynamic_cast<ResponseParam*>(par);
           if (!respPar)
           {
              throw RedAlert("Response param missing in analyticDifferentiate.\n");
           }
           const Model* modForRespPar = models->lookup(respPar->responseParent());
           const RealArray& currFlux = 
                modForRespPar->modelFlux(respPar->responseParent()->spectrumNumber());
           const size_t nBins = currFlux.size();
           parInfo.dValues.push_back(vector<double>(&currFlux[0], &currFlux[0]+nBins));
       }
       parallelInput.push_back(parInfo);
    }
    ProcessManager::ParallelResults results;
    m_jacobianCalc.run(parallelInput, results);
    
    map<size_t,ArrayContainer> dModel;
    
    for (size_t k = 0; k < N; ++k)
    {
       const int parNum = parNums[k];
       const TransferStruct& transfer = results[k];
       if ( transfer.dValues.size() != fit->spectraToFit().size())
       {
          throw RedAlert("Spectrum array size mismatch after parallel derivative calculation.");
       }
       ArrayContainer& dm = dModel[parNum];
       size_t iSpec=0;
       map<size_t,SpectralData*>::const_iterator itSpec = fit->spectraToFit().begin();
       map<size_t,SpectralData*>::const_iterator itSpecEnd = fit->spectraToFit().end();
       while (itSpec != itSpecEnd)
       {
          const size_t nChan = itSpec->second->indirectNotice().size();
          const vector<double>& transVec = transfer.dValues[iSpec];
          if (transVec.size() != nChan)
          {
             throw RedAlert("Spectrum size mismatch after parallel derivative calculation.");
          }
          // This inserts new valarray into dm.
          RealArray& resArray = dm[itSpec->first];
          resArray.resize(nChan);
          for (size_t iVal=0; iVal<nChan; ++iVal)
             resArray[iVal] = transVec[iVal];
          ++iSpec;
          ++itSpec;
       }

       Real sum = 0.0;
       map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
       map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
       while (itStat != itStatEnd)
       {
          if (!itStat->second->getSpectraForStat().empty())
          {
	     sum += itStat->second->sumDerivs(dm, parNum);
          }
          ++itStat;
       }   
       // beta_i is defined as -.5*d(stat)/d(par_i)
       m_beta[k] = -0.5*sum - s_bayes.dlPrior(fit, parNum);        
    } // end par loop

    if ( fit->fitMethod()->secondDerivativeRequired() ) 
    {
       m_alpha.resize(N*N,0.0);
       const size_t N(parNums.size());
       map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
       map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
       while (itStat != itStatEnd)
       {
          if (!itStat->second->getSpectraForStat().empty())
          {
             size_t ni=0;
             while ( ni <  N )
             {
	         int i = parNums[ni];
	         size_t nj = 0;
	         while ( nj <= ni )
	         {
		     int j = parNums[nj];
                     Real alpha_ij = itStat->second->sumSecondDerivs(dModel, i, j);
                     m_alpha[nj + N*ni] += 0.5*alpha_ij - s_bayes.d2lPrior(fit, i, j);
                     if (ni != nj)
                        m_alpha[ni + N*nj] += 0.5*alpha_ij - s_bayes.d2lPrior(fit, j, i);
		     ++nj; 
	         }  
	         ++ni; 
             }   
          }
          ++itStat;
       }   
    }

} // end analyticDifferentiate

void StatManager::registerWeightingMethod (const std::string& name, XSContainer::Weight* weightMethod)
{
  s_weightingMethods.insert(std::map<std::string, XSContainer::Weight*>::value_type(name,weightMethod));
}

XSContainer::Weight* StatManager::getWeightingMethod(const std::string& name)
{
  string lcName = XSutility::lowerCase(name);
  XSContainer::Weight* requested = 0;
  std::map<std::string,XSContainer::Weight*>::const_iterator iw = s_weightingMethods.lower_bound(lcName);
  if (iw != s_weightingMethods.end()) 
  {
     if (iw->first.find(lcName) == 0)
     {
        requested = iw->second;
     }
  }
  return requested;
}

string StatManager::weightNames ()
{
  std::map<string,XSContainer::Weight*>::const_iterator ws = s_weightingMethods.begin();
  std::map<string,XSContainer::Weight*>::const_iterator wsEnd = s_weightingMethods.end();
  string names;
  while ( ws != wsEnd)
  {
          names += ws->first;
          ++ws;       
          if (ws != wsEnd) names += " | ";
          else names += " ";
  }

  return names;      
}

void StatManager::clearWeightingMethods ()
{
   std::map<std::string, XSContainer::Weight*>::iterator itWs = s_weightingMethods.begin();
   std::map<std::string, XSContainer::Weight*>::iterator itEnd = s_weightingMethods.end();
   while (itWs != itEnd)
   {
      delete itWs->second;
      ++itWs;
   }
   s_weightingMethods.clear();
}

string StatManager::checkStatsWeighting() const
{
   XSContainer::Weight* weightMethod = getWeightingMethod(m_weightCmdSetting);
   if (!weightMethod)
   {
      throw RedAlert("Invalid weight method in checkStatsWeighting.");
   }
   string problemStat; 

   // First just check if weight is compatible with default stat.
   // If not, no need to check the rest of the stats.
   if (!m_defaultStat->checkWeight(weightMethod))
      problemStat = m_defaultStat->name();
   std::map<string,StatMethod*>::const_iterator itStat = m_statMethods.begin();
   std::map<string,StatMethod*>::const_iterator itStatEnd = m_statMethods.end();
   while (problemStat.empty() && itStat != itStatEnd)
   {
      const StatMethod* stat = itStat->second;
      if (stat->getSpectraForStat().size())
      {
         if (!stat->checkWeight(weightMethod))
            problemStat = stat->name();
      }
      ++itStat;
   }
   return problemStat;
}

void StatManager::recalculateWeightVariances()
{
   const size_t nSpec = XSContainer::datasets->numberOfSpectra();
   for (size_t i=0; i<nSpec; ++i)
   {
      SpectralData* sd = XSContainer::datasets->lookup(i+1);
      if (sd)
      {
         RealArray norm = sd->areaScale()*sd->exposureTime();
         if (Background* bkg = sd->getBackground())
         {
            RealArray bkgNorm = bkg->data()->areaScale()*bkg->data()->exposureTime();
            bkgNorm *= bkg->data()->backgroundScale()/sd->backgroundScale();
            DataUtility::statWeight()(*(bkg->data()), bkgNorm);
         }
         DataUtility::statWeight()(*sd, norm);
         sd->computeTotals();
      }
   }
}

void StatManager::ParDiff::execute(const bool isParallel, const TransferStruct& input, TransferStruct& output)
{
   using namespace XSContainer;
   using namespace std;

   const int parNum = input.iValues[0][0];
   const Real parVal = input.dValues[0][0];
   const Real delta = input.dValues[0][1];
   
   if (isParallel)
   {
      // Parallel child processes will not have the most recent fit
      // iteration parameter values.  Must set them here.
      const vector<int>& allVarParNums = input.iValues[1];
      const vector<double>& allVarParVals = input.dValues[1];
      for (size_t i=0; i<allVarParNums.size(); ++i)
      {
         ModParam* modPar = fit->variableParameters(allVarParNums[i]);
         modPar->setValue(allVarParVals[i], 'a');
      }      
   }
   
   ArrayContainer dm;   
   // Model objects contribute to dm ArrayContainers using only "+=" 
   // operations, never "=".  Therefore we must build dm to the
   // proper size here and initialize its arrays to 0.
   std::map<size_t,SpectralData*>::const_iterator itSpec = fit->spectraToFit().begin();
   std::map<size_t,SpectralData*>::const_iterator itSpecEnd = fit->spectraToFit().end();
   while (itSpec != itSpecEnd)
   {
      const size_t nChan = itSpec->second->indirectNotice().size();
      dm.insert(ArrayContainer::value_type(itSpec->first,RealArray(0.,nChan)));
      ++itSpec;
   }
   
   set<string>::const_iterator itName    (fit->activeModels().begin());
   set<string>::const_iterator itNameEnd (fit->activeModels().end());

   // Rather than do a dynamic_cast to tell if this is a
   // ResponseParam, simply rely on the fact that ResponseParams
   // are given an index above a set value.   
   const int respFloor = Fit::RESPAR_INDEX() << ModelContainer::SHIFT();
   ModParam* modp (fit->variableParameters(parNum));
   if (parNum > respFloor)
   {
      ResponseParam* respp = dynamic_cast<ResponseParam*>(modp);
      if (!respp)
      {
         throw RedAlert("Expecting but did not find response parameter in StatMethod.cxx");
      }
      
      // Will need another ArrayContainer for results of a second fold operation.
      ArrayContainer backwardsDm;
      itSpec = fit->spectraToFit().begin();
      while (itSpec != itSpecEnd)
      {
         const size_t nChan = itSpec->second->indirectNotice().size();
         backwardsDm.insert(ArrayContainer::value_type(itSpec->first,RealArray(0.,nChan)));
         ++itSpec;
      }
      
      while (itName != itNameEnd)
      {
         // forward difference  
         // Note that delta is set in unadjusted space so have to go through
         //   some contortions here
         respp->setValue(parVal,'a');
         Real tpar = respp->value('v');
         respp->setValue(tpar + delta,'v');
         // save the model and component fluxes to avoid unnecessary recalculation.
         GroupFluxContainer saveFlux, saveFluxError;
         const string& modelName = *itName;
         if (!isParallel)
         {
            models->storeFluxes(modelName, saveFlux);
            models->storeFluxErrors(modelName, saveFluxError);
         }
         models->calculate(modelName, true);
         std::pair<ModelMapConstIter,ModelMapConstIter> itRanges 
             = models->modelSet().equal_range(modelName);
         ModelMapConstIter itMod = itRanges.first;
         while (itMod != itRanges.second)
         {
            itMod->second->fold();
            itMod->second->storeDerivative(dm);
            ++itMod;
         }
         
         // backward difference
         respp->setValue(parVal,'a');
         tpar = respp->value('v');
         respp->setValue(tpar - delta,'v');
         models->calculate(modelName);
         itMod = itRanges.first;
         while (itMod != itRanges.second)
         {
            itMod->second->fold();
            itMod->second->storeDerivative(backwardsDm);
            ++itMod;
         }         
         ++itName;
         if (!isParallel)
         {
            models->restoreFluxes (modelName, saveFlux);
            models->restoreFluxErrors(modelName, saveFluxError);
            models->resetComponentFluxes(modelName);
         }
      }  // end model name loop          
      ArrayContainer::iterator itDm = dm.begin();
      ArrayContainer::iterator itBackDm = backwardsDm.begin();
      ArrayContainer::iterator itDmEnd = dm.end();
      while (itDm != itDmEnd)
      {
         itDm->second -= itBackDm->second;
         itDm->second /= (2.0*delta);
         ++itDm;
         ++itBackDm;
      }
      respp->setValue(parVal,'z');
      
   } // end if resp par
   else
   {
      while ( itName != itNameEnd )
      {
         // forward difference  
         const string& modelName = *itName;
         // note that delta is set in unadjusted space so have to go through
         // some contortions here
         modp->setValue(parVal,'a');
         Real tpar = modp->value('v');
         modp->setValue(tpar + delta,'v');
         // save the model and component fluxes to avoid unnecessary recalculation.
         GroupFluxContainer saveFlux, saveFluxError;
         if (!isParallel)
         {
            models->storeFluxes(modelName, saveFlux);
            models->storeFluxErrors(modelName, saveFluxError);
         }
         models->calculate(modelName, true);

         // save results in 'difference' container
         GroupFluxContainer forwardDifference;
         models->storeFluxes(modelName, forwardDifference);

         // backward difference
         modp->setValue(parVal,'a');
         tpar = modp->value('v');
         modp->setValue(tpar - delta,'v');
         models->calculate(modelName);

         // difference contains the forward difference and the models
         // contain the backward difference. Combine these into a derivative,
         // fold, and return the value in dm

         models->makeDerivatives(modelName, forwardDifference, delta, dm);

         // ... and put the model back straight afterwards.
         if (!isParallel)
         {
            models->restoreFluxes (modelName, saveFlux);
            models->restoreFluxErrors(modelName, saveFluxError);
            models->resetComponentFluxes(modelName);
         }

         ++itName;
      } 
      modp->setValue(parVal,'z');
   } // end if regular mod par
   
   
   
   ArrayContainer::iterator itDm = dm.begin();
   ArrayContainer::iterator itDmEnd = dm.end();
   while (itDm != itDmEnd)
   {
      const size_t nChans = itDm->second.size();
      output.dValues.push_back(std::vector<double>(&itDm->second[0],
                                     &itDm->second[0] + nChans));
      ++itDm;
   }
   
}
