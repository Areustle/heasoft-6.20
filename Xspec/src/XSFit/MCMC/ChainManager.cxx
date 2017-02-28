//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Numerics/Gamma.h>
#include <XSUtil/Parse/XSparse.h>
#include <algorithm>

// RandomizerBase
#include <XSFit/Randomizer/RandomizerBase.h>
// MarginGrid
#include <XSFit/MCMC/MarginGrid.h>
// RandomGenerator
#include <XSUtil/Numerics/RandomGenerator.h>
// ChainManager
#include <XSFit/MCMC/ChainManager.h>

int svdcmp(double* mat, int m, int n, double* wvec, double* vmat, double* rv1);

// Class ChainManager::Stats 

ChainManager::Stats::Stats()
  :means(1, .0),
   totalMean(.0),
   totalVar(.0),
   varInChains(.0),
   rubinGelman(.0),
   fracRepeats(1, .0)
{
}


// Class ChainManager 
ChainManager* ChainManager::s_instance = 0;

ChainManager::ChainManager()
  : m_length(0),
    m_width(0),
    m_allLengthsSame(true),
    m_isSynchedWithFitParams(false),
    m_accumulatedLengths(),
    m_chainProposal(0), // Non-owning.
    m_chainSVDevalue(),
    m_chainSVDevector(),
    m_chainCovariance(),
    m_lastStatCalc(),
    m_chains(),
    m_marginGrid(0),
    m_covarParams()
{
  // NOTE: m_chainProposal cannot be set from here since this is called
  // during the Fit class construction, and native simulation strategies
  // aren't registered till after Fit is constructed.  Instead, rely on
  // XSGlobal::registerNativeSimulationStrategies to set m_chainProposal.
}


ChainManager::~ChainManager()
{
  // Should only be in here during shutdown since ChainManager is a 
  // singleton.  In this context we don't want clearChains to cause an
  // output message about resetting chain proposal.  This relies on
  // XSGlobal::cleanUp() being called PRIOR to tpout destruction due
  // to Tcl_Eval call at the end of xsExit.  
  tpout << xsverbose(999);
  clearChains();
  // Don't really need to reset things since program is presumably
  // ending.  Still it's good practice...
  tpout << xsverbose();
  delete m_marginGrid;
}


ChainManager* ChainManager::Instance ()
{
  if (s_instance == 0)
  {
     s_instance = new ChainManager();
  }
  return s_instance;
}

void ChainManager::addChain (Chain* chain)
{
  // NOTE: This function should NEVER properly return without adding 
  // new Chain* to m_chains container. If it doesn't add new chain for
  // ANY reason, it must THROW. 
  const string& newFile = chain->getFileName();

  if (widthConflict(chain))
  {
     string msg(newFile);
     msg += " contains different pars than the already loaded chain(s).";
     msg += "\nTherefore, it cannot be loaded at this time.";
     throw Chain::ChainError(msg);
  }
  if (!chain->length())
  {
     throw Chain::ChainError("Chain has no length, it will not be loaded.");
  }
  if (chain->width() < 2)
  {
     throw Chain::ChainError("Chain has no parameters, it will not be loaded.");
  }
  // If loaded chains are not currently all the same length, then
  // don't bother prompting if new one may be different.
  if (!m_chains.empty() && m_allLengthsSame && chain->length() != m_length)
  {
     string prompt(" This chain has a different length than all other loaded Chains.");
     prompt += "\n Do you still want to load it? (y/n): ";
     string result;
     XSparse::basicPrompt(prompt, result);
     if (result.empty() || (result[0] != 'y'&&
        result[0] != 'Y'))  throw YellowAlert();     
  }
  removeChain(newFile);
  m_chains[newFile] = chain;
  tcout << "  New chain " << newFile << " is now loaded." << std::endl;
  if (m_chains.size() == 1)
  {
     m_width = chain->width();
     m_length = chain->length();
  }
  checkLengths();
}

bool ChainManager::removeChain (const string& fileName)
{
  ChainContainer::iterator doomed = m_chains.find(fileName);
  bool isRemoved = false;
  if (doomed != m_chains.end())
  {
     delete doomed->second;
     m_chains.erase(doomed);
     isRemoved = true;
     if (m_chains.empty())
     {
        m_width = m_length = 0;
        m_allLengthsSame = true;
        m_isSynchedWithFitParams = false;
     }
  }
  return isRemoved;
}

void ChainManager::clearChains ()
{
  ChainContainer::iterator doomed = m_chains.begin();
  while (doomed != m_chains.end())
  {
     delete doomed->second;
     m_chains.erase(doomed++);
  }
  m_width = m_length = 0;
  m_allLengthsSame = true;
  m_isSynchedWithFitParams = false; 
  m_accumulatedLengths.clear();
}

bool ChainManager::widthConflict (const Chain* newChain) const
{
  bool isConflict = false;
  // If newChain is either the first chain or it's replacing the
  // only existing chain, then no conflict.
  if (m_chains.size()>1 || (m_chains.size()==1 && 
        (newChain->getFileName() != m_chains.begin()->first)))
  {
     if (newChain->width() != m_width)
        isConflict = true;
     else
     {
        const std::vector<Chain::ParamID>& newParams = 
                        newChain->paramIDs();
        const std::vector<Chain::ParamID>& curParams = 
                        m_chains.begin()->second->paramIDs();
        // Assume nPars are same since widths are same.
        const size_t nPar = newParams.size();
        for (size_t i=0; i<nPar; ++i)
        {
           if (newParams[i] != curParams[i])
           {
              isConflict = true;
              break;
           }
        }

     }
  }
  return isConflict;
}

void ChainManager::recalc ()
{
  if (m_chains.empty())
  {
     throw Chain::ChainError("No chains loaded.");
  }
  if (!m_isSynchedWithFitParams)
  {
     string errMsg("Loaded chain(s) do not match current fit parameters.\n");
     errMsg += "No covariance recalculation will be performed.";
     throw Chain::ChainError(errMsg);
  }

  size_t nPar = m_width - 1;
  size_t nPar2 = nPar*nPar;
  // Using C-style arrays to interface with svdcmp.cxx, and 
  // anyway they're faster than valarrays.
  CollectSumVar sumVar(nPar);
  // Careful! These pointers are only valid as long as sumVar
  // stays in scope.
  double* chainMean = sumVar.getChainSum();
  double* chainVariance = sumVar.getChainVariance();

  forEachChainPoint(sumVar);

  ChainContainer::const_iterator itChain = m_chains.begin();
  ChainContainer::const_iterator itEnd = m_chains.end();
  int totalLength = 0;
  while (itChain != itEnd)
  {
     const Chain* chain = itChain->second;
     totalLength += chain->length();
     ++itChain;
  }
  if (!totalLength)
  {
     // It should be impossible to get this far with 0 length.
     throw RedAlert("Programmer error in ChainManager::recalc - No length in chain files.");
  }
  // Convert to covariance matrix.
  for (size_t i=0; i<nPar; ++i)
  {
     chainMean[i] /= static_cast<Real>(totalLength);
  } 
  for (size_t i=0; i<nPar; ++i)
  {
     const size_t iOffset = i*nPar;
     Real chainMean_i = chainMean[i];
     for (size_t j=0; j<nPar; ++j)
     {
        chainVariance[iOffset+j] /= static_cast<Real>(totalLength);
        chainVariance[iOffset+j] -= chainMean_i*chainMean[j];
     }
  }

  // Calculate the eigenvalues and eigenvectors. 
  XSutility::auto_array_ptr<double> pVmat(new double[nPar2]);
  XSutility::auto_array_ptr<double> pRv1(new double[nPar]);
  double* vmat = pVmat.get();
  double* rv1 = pRv1.get();

  m_chainCovariance.resize(nPar2);
  m_chainCovariance = RealArray(chainVariance,nPar2);
  // svdcmp will modify chainVariance, which is why we first
  // save it in m_chainCovariance.
  // Reusing chainMean as workspace to hold eigenvalues.
  int niPar = static_cast<int>(nPar);
  svdcmp(chainVariance, niPar, niPar, chainMean, vmat, rv1);

  m_chainSVDevalue.resize(nPar);
  m_chainSVDevalue = RealArray(chainMean, nPar);
  m_chainSVDevector.resize(nPar2);
  for (size_t i=0; i<nPar2; ++i)
  {
        m_chainSVDevector[i] = vmat[i];
  }
  // And now save the parameter ID information for the values
  // in the covariance matrix.
  m_covarParams = m_chains.begin()->second->paramIDs();
}

void ChainManager::calcStat (size_t iPar, const string& modName)
{
  using namespace std;
  if (!m_allLengthsSame)
  {
     string msg("Loaded chains must all be the same length in order to run stat calculations.");
     throw Chain::ChainError(msg);
  }
  size_t nChains = m_chains.size();
  if (!nChains)
  {
     string msg("No chains loaded, unable to calculate stats.");
     throw Chain::ChainError(msg);
  }
  if (m_length < 2)
  {
     throw Chain::ChainError("Chain lengths not large enough to calculate stats.");
  }
  const size_t parPos = findParPosition(iPar, modName);
  RealArray means(.0, nChains);
  std::vector<size_t> nRepeats(nChains,0);
  Real totalMean = .0;
  Real totalVar = .0;
  Real varInChains = .0;

  ChainContainer::const_iterator itChains = m_chains.begin();
  ChainContainer::const_iterator itEnd = m_chains.end();
  size_t iCh = 0;
  while (itChains != itEnd)
  {
     Chain* chain = itChains->second;
     Real thisVar=.0;
     chain->calcStats(parPos,means[iCh],thisVar,nRepeats[iCh]);
     varInChains += thisVar;
     ++iCh;
     ++itChains;
  }
  // CAUTION: This assumes all chains have the same length.
  varInChains /= m_length*m_chains.size()-1;
  totalMean = means.sum()/nChains;
  if (nChains > 1)
  {
     for (size_t i=0; i<nChains; ++i)
     {
        totalVar += (means[i]-totalMean)*(means[i]-totalMean);
     }
     totalVar /= nChains-1;
  }

  // Calculate the Rubin-Gelman convergence criterion - note that this
  // implicitly assumes that the chains are all of the same length.
  Real rhat = ((m_length-1)*varInChains/m_length +
                totalVar*(1.0+1.0/nChains))/varInChains;

  const int precision = 8;
  const int fwidth = precision + 6;
  ios_base::fmtflags saveFlags(tcout.flags());
  streamsize savePrecision = tcout.precision();
  tcout.precision(precision);  
  tcout << showpoint;
  tcout << " Means in chains : ";
  for (size_t i=0; i<nChains; ++i)
  {
     tcout << setw(fwidth) << means[i];
  }
  tcout << endl;
  if (nChains > 1)
  {
    tcout << " Mean over all chains : " << setw(fwidth) << totalMean;
    tcout << " and variance of chain means : ";
    tcout << setw(fwidth) << totalVar << endl; 
  }
  tcout << " Variance over all chains : " << setw(fwidth) << varInChains << endl;
  if ( nChains > 1 ) {
    tcout << " Rubin-Gelman convergence measure : " << setw(fwidth) << rhat << endl;
  }
  tcout.precision(savePrecision);
  tcout << " Fraction of repeated values: ";
  for (size_t i=0; i<nChains; ++i)
  {
     Real fracRepeat = static_cast<Real>(nRepeats[i])/m_length;
     tcout << fracRepeat << "  ";
  }
  tcout << endl;
  tcout << "     (rule of thumb target is 0.75)" << endl;
  tcout.flags(saveFlags);

  m_lastStatCalc.means.resize(nChains);
  m_lastStatCalc.fracRepeats.resize(nChains);
  for (size_t i=0; i<nChains; ++i)
  {
     m_lastStatCalc.means[i] = means[i];
     m_lastStatCalc.fracRepeats[i] = static_cast<Real>(nRepeats[i])/m_length;
  }
  m_lastStatCalc.totalMean = totalMean;
  m_lastStatCalc.totalVar = totalVar;
  m_lastStatCalc.varInChains = varInChains;
  m_lastStatCalc.rubinGelman = rhat;
}

void ChainManager::findBestFit (RealArray& parVals, Real& statVal)
{
  using namespace std;
  size_t nChains = m_chains.size();
  if (!nChains) {
    statVal = -1.0;
    parVals.resize(0);
    return;
  }

  // this implicitly assumes that all chains have the same width because
  // that should have been trapped out before reaching this point

  parVals.resize(m_width-1);
  ChainContainer::const_iterator itChains = m_chains.begin();
  ChainContainer::const_iterator itEnd = m_chains.end();
  while (itChains != itEnd) {
    Chain* chain = itChains->second;
    size_t lineNum;
    if ( itChains == m_chains.begin() ) {
      chain->findBestPoint(lineNum, parVals, statVal);
    } else {
      RealArray tmpParVals(parVals.size());
      Real tmpStatVal;
      chain->findBestPoint(lineNum, tmpParVals, tmpStatVal);
      if ( tmpStatVal < statVal ) {
	statVal = tmpStatVal;
	parVals = tmpParVals;
      }
    }
     ++itChains;
  }
}

void ChainManager::removeChains (const IntegerArray& chainNums)
{
  StringArray removalNames;
  ChainContainer::const_iterator itChain = m_chains.begin();
  ChainContainer::const_iterator itEnd = m_chains.end();
  // This REQUIRES chainNums to be sorted in ascending order and >= 1.
  // Note: chainNums should not contain any numbers beyond nChains,
  // but this will work even if it does.
  size_t i=0, iChain=1;
  size_t nRemove = chainNums.size();
  while (itChain != itEnd && i < nRemove)
  {
     // chainNums and iChain are 1-based
     if (iChain == static_cast<size_t>(chainNums[i]))
     {
        removalNames.push_back(itChain->first);
        ++i;
     }
     ++itChain;
     ++iChain;
  }
  // In case chainNums has numbers beyond nChains...
  nRemove = removalNames.size();
  if (nRemove) tcout << "Chains unloaded:" << std::endl;
  for (size_t j=0; j<nRemove; ++j)
  {
     if (removeChain(removalNames[j]))
     {
        tcout << "  " << removalNames[j] << std::endl;
     }
  }
  checkLengths();
}

bool ChainManager::checkLengths ()
{
  ChainContainer::const_iterator itChain = m_chains.begin();
  ChainContainer::const_iterator itEnd = m_chains.end();
  bool isFirst = true;
  m_allLengthsSame = true;
  m_accumulatedLengths.clear();
  size_t accum = 0;
  while (itChain != itEnd)
  {
     if (isFirst)
     {
        m_length = itChain->second->length();
        isFirst = false;
     }
     if (itChain->second->length() != m_length)
     {
        m_allLengthsSame = false;
     }
     accum += itChain->second->length();
     m_accumulatedLengths.push_back(accum);
     ++itChain;
  }
  return m_allLengthsSame;
}

void ChainManager::getRandomPoint (RealArray& parVals) const
{
   // Can assume parVals is already the proper size to match the number
   // of parameters in the chain files.  
   const Numerics::DefaultRandomGenerator& randGen =
                Numerics::DefaultRandomGenerator::instance();
   float randFloat=0.;
   // This will get a random number between 0 and 1 NON-inclusive.
   randGen.getRandom(&randFloat, 1);
   double randDouble = static_cast<double>(randFloat);
   // Couldn't have gotten in here if m_accumulatedLengths is empty.
   const size_t totalLengths = m_accumulatedLengths[m_accumulatedLengths.size()-1];
   // We need a random size_t from 0 to totalLengths-1 inclusive,
   // so truncation is intentional.
   const size_t randLoc = static_cast<size_t>(totalLengths*randDouble);

   size_t lineNumInChain = 0;
   const Chain* chain = getChainForRandomPoint(randLoc, lineNumInChain);
   chain->openForReadPoints();
   try
   {
      chain->readPointFromLine(lineNumInChain, parVals);
   }
   catch (YellowAlert&)
   {
      chain->closeFile();
      throw;
   }
   chain->closeFile();
}

void ChainManager::getLastPoint (RealArray& parVals) const
{
   // return a zero size array if no chains are loaded
   if ( m_chains.size() == 0 ) {
     parVals.resize(0);
     return;
   }

   // Go to the final chain
   ChainContainer::const_iterator itChainsEnd = m_chains.end();
   itChainsEnd--;
   const Chain* chain = itChainsEnd->second;
   chain->openForReadPoints();

   // and get the last point in this chain
   size_t length = chain->length();
   parVals.resize(chain->width()-1);
   try {
     chain->readPointFromLine(length-1, parVals);
   }
   catch (YellowAlert&)
   {
      chain->closeFile();
      throw;
   }
   chain->closeFile();
}

const Chain* ChainManager::getChainForRandomPoint (size_t globalLocation, size_t& locInChain) const
{
   const Chain* chain = 0;
   // m_chains and m_accumulatedLengths must be the same size.
   // (See their relation in checkLengths.)
   // We're also going to assume randLocation is some random value
   // already verified to be between 0 and nTotalLengths-1 inclusive.
   ChainContainer::const_iterator itChains = m_chains.begin();
   ChainContainer::const_iterator itChainsEnd = m_chains.end();
   std::vector<size_t>::const_iterator itAccum = m_accumulatedLengths.begin();
   size_t iCount = 0;
   while (itChains != itChainsEnd)
   {
      if (globalLocation < *itAccum)
      {
         chain = itChains->second;
         size_t chainStartLoc = !iCount ? 0 : m_accumulatedLengths[iCount-1];
         locInChain = globalLocation - chainStartLoc;
         break;
      }
      ++itChains, ++itAccum, ++iCount;
   }
   return chain;
}

void ChainManager::setChainProposal (const string& proposalName, const string& optInitArg)
{
   RandomizerBase* randStrategy = XSContainer::fit->getRandomizingStrategy(proposalName);
   if (!randStrategy)
   {
      // Should never get in here since the chain command handler outght to
      // be checking things before it calls this.  If it does get in here, 
      // it is failing on the initial attempt to set the default chain proposal
      // during start-up. 
      throw RedAlert("Unable to set default chain proposal strategy in ChainManager");
   }
   else if (randStrategy->name().find(" chain") != string::npos && !m_isSynchedWithFitParams)
   {
      string errMsg("Cannot set proposal distribution to use covariance calculated");
      errMsg += "\nfrom current chains.  Either no chains are loaded, or their parameters do not";
      errMsg += "\nmatch current variable model parameters.";
      throw Chain::ChainError(errMsg);
   }
   else
   {
      randStrategy->initString(optInitArg);
      // this may throw
      randStrategy->initializeLoad();
      // Check for m_chainProposal=0 only to prevent message output during start-up.
      if (m_chainProposal)
      {
         tcout <<"   New chains will use proposal distribution derived from: ";
         string::size_type fileTagLoc = randStrategy->name().find("<filename>");
         string name = randStrategy->name().substr(0,fileTagLoc);
         if (fileTagLoc != string::npos)
            name += optInitArg;
         tcout << name <<std::endl;
      }
      m_chainProposal = randStrategy;
   }
}

void ChainManager::reportChainProposal () const
{
   string::size_type fileTagLoc = m_chainProposal->name().find("<filename>");
   string name = m_chainProposal->name().substr(0,fileTagLoc);
   // Replace "<filename>" with actual file name.
   if (fileTagLoc != string::npos)
      name += m_chainProposal->initString();

   tcout << "Current chain proposal distribution setting: " 
         << name << std::endl;
}

bool ChainManager::checkCovarForSynch () const
{
  using namespace std;

  // An empty covariance matrix is by definition out-of-synch, even if
  // fit parameters are also empty.
  bool allInSynch = false;
  const size_t nCovarPars = m_covarParams.size();
  if (nCovarPars)
  {
     const map<int,ModParam*>& varPars = XSContainer::fit->variableParameters();
     if (varPars.size() == nCovarPars)
     {
        allInSynch = true;
        map<int,ModParam*>::const_iterator itVp = varPars.begin();
        map<int,ModParam*>::const_iterator itVpEnd = varPars.end();
        vector<Chain::ParamID>::const_iterator itCovarPar = m_covarParams.begin();
        while (itVp != itVpEnd)
        {
           const Chain::ParamID& covarPar = *itCovarPar;
           const ModParam* fitPar = itVp->second;
           // This is the same test the Fit class uses to determine if 
           // loaded chains are in synch with fit params.  Once again,
           // it doesn't guarantee that chain covariance values were
           // generated using the current fit param values.
            if (covarPar.modName != fitPar->modelName() ||
                covarPar.parName != fitPar->name() ||
                covarPar.index != fitPar->index() ||
                covarPar.units != fitPar->unit())
            {
               allInSynch = false;
               break;
            }
           ++itVp;
           ++itCovarPar;
        }
     }
  }

  return allInSynch;
}

std::pair<Real,Real> ChainManager::getParErrorRange (const Real confidence, const size_t parNum, const string& modName) const
{
   const size_t parPos(findParPosition(parNum, modName));
   // Need to convert confidence given as a delta-stat into a 
   // percentage interval.
   if (confidence <= 0.0)
   {
      throw YellowAlert("Delta-stat confidence value must be > 0.0.\n");
   }
   Numerics::GammaP gp;
   // 1 degree of freedom
   Real pctLevel = gp(.5, confidence/2.0);
   pctLevel *= 100.0;

   // This is a functor
   CollectParVals cp(parPos);
   forEachChainPoint(cp);
   std::vector<Real>& parVals = cp.parVals();

   // Get the values that lay at the boundaries of the interval which
   // covers the fraction of values given by confidence, centered 
   // about the median.  
   std::pair<Real,Real> errorRange = XSutility::confidenceRange(pctLevel, parVals);


   return errorRange;
}

size_t ChainManager::findParPosition (const size_t parNum, const string& modName) const
{
   // Only looking for 1 par here.
   std::vector<Chain::ParamID> parIDs(1);
   parIDs[0].modName = modName.length() ? modName : Model::DEFAULT();
   parIDs[0].index = parNum;
   std::vector<size_t> location;
   if (m_chains.size())
   {
      const Chain* firstChain = m_chains.begin()->second;
      firstChain->findParsInChain(parIDs, location);
   }
   if (location.empty())
   {
      std::ostringstream oss;
      oss << "Cannot find parameter ";
      if (modName.size())
         oss << modName << ":";
      oss << parNum << " in chains." 
        <<"\nIf parameter belongs to a named model, make sure model name is specified.";
      throw Chain::ChainError(oss.str());
   }
   return location[0];
}

void ChainManager::appendToChain (const string& chainName, const size_t addLength, const Real temperature, const string& format)
{
   ChainContainer::iterator itChain = m_chains.find(chainName);
   if (itChain == m_chains.end())
   {
      throw Chain::ChainError("Chain must already be loaded to append to it.");
   }
   if (!m_isSynchedWithFitParams)
   {
      throw Chain::ChainError("Chain pars must match current variable parameters to append to chain.");
   }
   Chain* chain = itChain->second;
   if (format != chain->format())
   {
      string errMsg("Current format setting: ");
      errMsg += format;
      errMsg += "\n   does not match the format of ";
      errMsg += chain->getFileName() + ": " + chain->format();
      errMsg += "\n   Modify the format setting with the \"chain filetype <format>\" option to append to this file.";
      throw Chain::ChainError(errMsg);
   }
   const size_t orgLength = chain->length();
   try
   {
      chain->runMH(addLength, temperature);
   }
   catch (YellowAlert&)
   {
      if (orgLength != chain->length())
      {
         tcout << "\nChain " << chainName << " now has length " 
            << chain->length() << std::endl;
         if (checkLengths())
            tcout <<"   All loaded chains have the same length.\n"<<std::endl;
         else
            tcout <<"   Loaded chains have varying lengths.\n"<< std::endl;
      }
      throw;
   }
   tcout << "\nChain " << chainName << " now has length " 
      << chain->length() << std::endl;
   if (checkLengths())
      tcout <<"   All loaded chains have the same length.\n"<<std::endl;
   else
      tcout <<"   Loaded chains have varying lengths.\n"<< std::endl;
}

void ChainManager::appendToChain (const string& chainName, const size_t addLength, const size_t walkers, const string& format)
{
   ChainContainer::iterator itChain = m_chains.find(chainName);
   if (itChain == m_chains.end())
   {
      throw Chain::ChainError("Chain must already be loaded to append to it.");
   }
   if (!m_isSynchedWithFitParams)
   {
      throw Chain::ChainError("Chain pars must match current variable parameters to append to chain.");
   }
   Chain* chain = itChain->second;
   if (format != chain->format())
   {
      string errMsg("Current format setting: ");
      errMsg += format;
      errMsg += "\n   does not match the format of ";
      errMsg += chain->getFileName() + ": " + chain->format();
      errMsg += "\n   Modify the format setting with the \"chain filetype <format>\" option to append to this file.";
      throw Chain::ChainError(errMsg);
   }
   const size_t orgLength = chain->length();
   try
   {
     chain->walkers(walkers);
     chain->runGW(addLength);
   }
   catch (YellowAlert&)
   {
      if (orgLength != chain->length())
      {
         tcout << "\nChain " << chainName << " now has length " 
            << chain->length() << std::endl;
         if (checkLengths())
            tcout <<"   All loaded chains have the same length.\n"<<std::endl;
         else
            tcout <<"   Loaded chains have varying lengths.\n"<< std::endl;
      }
      throw;
   }
   tcout << "\nChain " << chainName << " now has length " 
      << chain->length() << std::endl;
   if (checkLengths())
      tcout <<"   All loaded chains have the same length.\n"<<std::endl;
   else
      tcout <<"   Loaded chains have varying lengths.\n"<< std::endl;
}

// Additional Declarations

std::ostream& operator<< (std::ostream& os, const ChainManager::Stats& right) 
{
   size_t nChains = right.means.size();
   for (size_t i=0; i<nChains; ++i)
   {
      os << right.means[i] <<" ";
   }
   os << right.totalMean <<" "<< right.totalVar <<" " 
      << right.varInChains <<" "<< right.rubinGelman <<" ";
   // This size should really always be the same as the for
   // the means array.
   nChains = right.fracRepeats.size();
   for (size_t i=0; i<nChains; ++i)
   {
      os << right.fracRepeats[i] <<" ";
   }
   return os;
} 

ChainManager::CollectSumVar::CollectSumVar(size_t nPar)
:  m_nPar(nPar), 
   m_apChainSum(new double[nPar]),
   m_apChainVariance(new double[nPar*nPar])
{
   m_chainSum = m_apChainSum.get();
   m_chainVariance = m_apChainVariance.get();
   size_t nPar2 = nPar*nPar;
   for (size_t i=0; i<nPar; ++i)  m_chainSum[i] = 0.0;
   for (size_t i=0; i<nPar2; ++i)  m_chainVariance[i] = 0.0;
}
