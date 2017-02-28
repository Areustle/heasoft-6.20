//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/ParallelStats.h>
#include <XSstreams.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUtil/Numerics/RandomGenerator.h>
#include <XSUtil/Signals/SignalHandler.h>
#include <XSUtil/Utils/ProcessManager.h>
#include <cmath>
#include <sstream>
#include <fstream>
#include <utility>
#include <cstdlib>

// ChainIO
#include <XSFit/MCMC/ChainIO.h>
// Chain
#include <XSFit/MCMC/Chain.h>

using namespace XSContainer;


// Class Chain::ChainError 

Chain::ChainError::ChainError (const string& msg)
  : YellowAlert("Chain Error: ")
{
  tcerr << msg << std::endl;
}


// Class Chain::ParamID 

// Class Chain::ChainInterrupt 

Chain::ChainInterrupt::ChainInterrupt ()
  : YellowAlert()
{
}


// Class Chain 

Chain::Chain (ChainIO* pIO, const string& fileName, size_t burnLength, size_t length, bool rand)
  : m_fileName(fileName),
    m_burnLength(burnLength),
    m_length(length),
    m_rand(rand),
    m_width(0),
    m_paramIDs(),
    m_statistic(),
    m_tempering(),
    m_walkers(0),
    m_chainType("MetropolisHastings"),
    m_IO(pIO)
{
   // Nothing must throw in here, otherwise additional precautions
   // must be taken in chain run handler for deletion of m_IO.
   m_IO->setParent(this);
}

Chain::Chain (ChainIO* pIO, const string& fileName, size_t burnLength, size_t length, size_t walkers)
  : m_fileName(fileName),
    m_burnLength(burnLength),
    m_length(length),
    m_rand(false),
    m_width(0),
    m_paramIDs(),
    m_statistic(),
    m_tempering(),
    m_walkers(walkers),
    m_chainType("GoodmanWeare"),
    m_IO(pIO)
{
   // Nothing must throw in here, otherwise additional precautions
   // must be taken in chain run handler for deletion of m_IO.
   m_IO->setParent(this);
}

Chain::Chain (ChainIO* pIO, const string& fileName)
  : m_fileName(fileName),
    m_burnLength(0),
    m_length(0),
    m_rand(false),
    m_width(0),
    m_paramIDs(),
    m_statistic(),
    m_tempering(),
    m_walkers(0),
    m_chainType("MetropolisHastings"),
    m_IO(pIO)
{
  m_IO->setParent(this);
  // This may throw:
  m_IO->readFileInfo();
}


Chain::~Chain()
{
   delete m_IO;
}

void Chain::run (const size_t appendLength, const Real temperature)
{
  if ( m_chainType == "MetropolisHastings" ) {
    runMH(appendLength, temperature);
  } else if ( m_chainType == "GoodmanWeare" ) {
    runGW(appendLength);
  } else {
    string msg("Chain type is neither Metropolis-Hastings or Goodman-Weare");
    throw ChainError(msg);
  }
}

void Chain::runMH (const size_t appendLength, const Real temperature)
{
  using namespace std;
  // ASSUME temperature has already been checked != 0.0
  //  and m_length != 0.

  bool isTempChange = false;
  const Real EPS = 1.0e-15;
  if (m_tempering.size())
  {
     // Should only get in here for append.
     Real lastTemp = m_tempering.back().second;
     isTempChange = (abs(temperature-lastTemp)/lastTemp > EPS);
     if (isTempChange && !m_IO->allowsTemper())
     {
        string errMsg("Cannot append to old-style chain file with a different temperature.");
        errMsg += "\n   File does not have a temperature values field.";
        throw ChainError(errMsg);
     }
     if (isTempChange  && !m_IO->checkTemperField(temperature))
     {
        string errMsg("Cannot append another temperature to ");
        errMsg += m_fileName + "\n     Tempering info field is full.";
        throw ChainError(errMsg);
     }
  }

  const bool wasFitStillValid = fit->isStillValid();
  const map<int,ModParam*>& varPars = fit->variableParameters();
  map<int,ModParam*>::const_iterator itVp = varPars.begin();
  map<int,ModParam*>::const_iterator itVpEnd = varPars.end();

  // Until this is implemented for response params...
  while (itVp != itVpEnd)
  {
     if (dynamic_cast<ResponseParam*>(itVp->second))
     {
        string errMsg("Chain runs are not yet implemented for response parameters.");
        throw ChainError(errMsg);
     }
     ++itVp;
  }

  m_paramIDs.clear();
  const size_t nPars = varPars.size();
  if (!nPars)
  {
     throw ChainError("No variable parameters");
  }
  // This first call with flag=true will only perform Randomizer
  // initialization.  Since it may throw, let's get it out of the
  // way before we start messing around with files.
  fit->randomizeForChain(true);

  m_paramIDs.resize(nPars);
  RealArray originalParamValues(nPars);
  itVp = varPars.begin();
  for (size_t i=0; i<nPars; ++i, ++itVp)
  {
     const ModParam* param = itVp->second;
     ParamID& parID = m_paramIDs[i];
     parID.modName = param->modelName();
     parID.parName = param->name();
     parID.index = param->index();
     parID.units = param->unit();
     originalParamValues[i] = param->value();
  }

  m_width = nPars + 1;
  const size_t origLength = appendLength ? m_length : 0;
  if (appendLength)
  {
     RealArray startingVals;
     m_IO->appendFile(startingVals);
     if (varPars.size() != startingVals.size())
     {
        string errMsg("Pars in ");
        errMsg += m_fileName;
        errMsg += string(" do not match fit variable parameters.");
        throw ChainError(errMsg);
     }
     fit->setVariableParameterValues(startingVals,'v');
     fit->isStillValid(false);
     fit->statManager()->performStats();  
  }
  else
  {
     // Creating a new chain file.
     m_tempering.push_back(make_pair(size_t(1), temperature));
     m_IO->createFile(); 
     m_IO->writeFileInfo(); 
  }

  size_t i=1;
  // Initialize the chain with the first point (but not
  // if appending).
  try
  {
     if (!appendLength)
     {
        if (m_rand)
        {
           fit->randomizeForChain();
           fit->statManager()->performStats();
        }  
        if (!m_burnLength)
           m_IO->writePoint();
        ++i;
     }
  }
  catch (YellowAlert&)
  {
     cleanupRun(originalParamValues);
     fit->isStillValid(wasFitStillValid);
     throw;
  }

  RealArray saveParamValues(nPars);
  size_t totalLength = appendLength ? appendLength : m_length + m_burnLength;
  const Numerics::DefaultRandomGenerator& randGen = 
        Numerics::DefaultRandomGenerator::instance();

  // If SIGINT handler hasn't been registered, intHandler will be 0
  // and it won't allow ctrl-C breaking from the loop.      
  const SIGINT_Handler *intHandler = dynamic_cast<const SIGINT_Handler*>
                (SignalHandler::instance()->getHandler(SIGINT));
  try
  {
     // Not resetting verbosity for this since we don't want to affect
     // output in the nested functions below.
     const bool doChatter = (tpout.maxChatter() >= 20);

     while (i<=totalLength)
     {
        if (intHandler && intHandler->interrupted())
        {
           throw ChainInterrupt();
        }
	saveParamValues = fit->variableParameterValues();
        Real saveStat = fit->statistic();
        fit->randomizeForChain();

	// for high chatter tell user values we are trying
	if ( doChatter )
	{
 	   RealArray paramValues(nPars);
	   paramValues = fit->variableParameterValues();
	   tcout << "Trying parameters for chain: ";
	   for (size_t i=0; i<nPars; i++) tcout << paramValues[i] << " ";
	   tcout << std::endl;
	}

        fit->statManager()->performStats();

	if ( doChatter ) 
	{
	   tcout << "Old -> New statistic: " << saveStat
	         << " -> " << fit->statistic()
	         << " (delta="
	         << saveStat-fit->statistic() << ")"
	         << std::endl;
	}

        bool includeNewParams = false;
        if (fit->statistic() <= saveStat)
        {
           includeNewParams = true;
           if (doChatter)
	      tcout << "New parameters were included (statistic was lower)\n"
		    << std::endl;
        }
        else
        {
           float randNumber=0.;
           randGen.getRandom(&randNumber, 1); 
           Real weightedDiff(exp(.5*(saveStat-fit->statistic())/temperature));
           if (randNumber < weightedDiff)
              includeNewParams = true;
	   if( doChatter )
	   {
	     tcout << "Criterion for inclusion in chain: "
		   << randNumber << " < " << weightedDiff << std::endl;

	     if(includeNewParams)
	       tcout << "=> parameters were included (statistic was higher)\n"
		     << std::endl;
	     else
	       tcout << "=> parameters NOT included (statistic was higher)\n"
		     << std::endl;
	   }
        }
        fit->reportChainAcceptance(includeNewParams);

        if (!includeNewParams)
        {
	   fit->setVariableParameterValues(saveParamValues,'v');
           fit->statManager()->totalStatistic(saveStat);
        }
        if (i > m_burnLength || appendLength)
           m_IO->writePoint();
        ++i;
     }
     if (appendLength)
     {
        m_length += appendLength;
        m_IO->doCloseFile();
        m_IO->doOpenForWrite();
        m_IO->adjustLengthInfo();
        if (isTempChange)
        {
           m_tempering.push_back(make_pair(origLength+(size_t)1, temperature));
           m_IO->adjustTemperInfo();
        }
     }
  }
  catch (YellowAlert&)
  {
     // The -1 comes from the fact that i would have been incremented
     // one more time after the last point was written.
     if (appendLength)
     {
        m_length = origLength + i-1;
        m_IO->doCloseFile();
        m_IO->doOpenForWrite();
        if (m_length != origLength && isTempChange)
        {
           m_tempering.push_back(make_pair(origLength+(size_t)1, temperature));
           m_IO->adjustTemperInfo();
        }
        tcerr << "\n*** Warning: Chain append run has been interrupted." << std::endl;
     }
     else
     {
        m_length = (i-1 > m_burnLength) ? i-1-m_burnLength : 0;
        tcerr << "\n*** Warning: Chain run has been interrupted.\n"
             <<  "    The chain will not be loaded." << std::endl;
     }
     m_IO->adjustLengthInfo();     
     cleanupRun(originalParamValues);
     fit->isStillValid(wasFitStillValid);
     // Even if exception is coming from something other than a
     // Ctrl-C break, we'll treat it the same way in this context.
     throw ChainInterrupt();
  }
  cleanupRun(originalParamValues);
  fit->isStillValid(wasFitStillValid);
}

void Chain::runGW (const size_t appendLength)
{
  using namespace std;
  
  // Before reaching here, should have already checked that:
  //   -- m_length and m_walkers are > 0
  //   -- m_length and m_burnLength are divisible by m_walkers
  //   -- m_walkers is even

  // set up iterator on variable parameters in the fit object since we will need
  // these frequently

  const bool wasFitStillValid = fit->isStillValid();
  const map<int,ModParam*>& varPars = fit->variableParameters();
  map<int,ModParam*>::const_iterator itVp = varPars.begin();
  map<int,ModParam*>::const_iterator itVpEnd = varPars.end();

  // At present this does not work with response parameters so check whether there are any.

  while (itVp != itVpEnd)
  {
     if (dynamic_cast<ResponseParam*>(itVp->second))
     {
        string errMsg("Chain runs are not yet implemented for response parameters.");
        throw ChainError(errMsg);
     }
     ++itVp;
  }

  m_paramIDs.clear();
  const size_t nPars = varPars.size();
  if (!nPars)
  {
     throw ChainError("No variable parameters");
  }

  // set up the parameter ID objects and save the current parameter values

  m_paramIDs.resize(nPars);
  RealArray originalParamValues(nPars);
  itVp = varPars.begin();
  for (size_t i=0; i<nPars; ++i, ++itVp)
  {
     const ModParam* param = itVp->second;
     ParamID& parID = m_paramIDs[i];
     parID.modName = param->modelName();
     parID.parName = param->name();
     parID.index = param->index();
     parID.units = param->unit();
     originalParamValues[i] = param->value();
  }

  m_width = nPars + 1;
  const size_t origLength = appendLength ? m_length : 0;

  // To handle the walkers we will need to keep all the current sets of parameters available
  // so set up a vector of RealArrays for this. We will also need to store the statistic values
  // for each walker so also set up a vector for this.

  vector<RealArray> walkerParamValues(m_walkers);
  for (size_t i=0; i<m_walkers; i++) walkerParamValues[i].resize(nPars);
  vector<Real> walkerStatisticValue(m_walkers);

  // Set up the chain files. If appending then check for consistency and load the final parameter
  // values into the walkerParamValues array. If creating new file then do so, create and save
  // the initial walkers 

  size_t istep = 1;
  if (appendLength)
  {
    RealArray startingVals(.0,nPars);
    Real startingStatVal;

    m_IO->doOpenForReadPoints();
    // fill the walker vectors with the last rows of the file
    for (size_t iwalk=0; iwalk<m_walkers; ++iwalk)
    {
       m_IO->doReadPointFromLine(m_length-m_walkers+iwalk, startingVals, startingStatVal);
       for (size_t j=0; j<nPars; j++) walkerParamValues[iwalk][j] = startingVals[j];
       walkerStatisticValue[iwalk] = startingStatVal;
    }
    m_IO->doCloseFile();
        
    // Now open for append.
    m_IO->appendFile(startingVals, startingStatVal);
    if (varPars.size() != startingVals.size()) {
      string errMsg("Pars in ");
      errMsg += m_fileName;
      errMsg += string(" do not match fit variable parameters.");
      throw ChainError(errMsg);
    }
  }
  else
  {
    // Creating new chain file for walkers.
    m_IO->createFile(); 
    m_IO->writeFileInfo(); 

    // Initialize the walkers with the first point and save the values
    // If there is a fit covariance matrix available then use it otherwise
    // generate a ball around the current parameter values. Note that have
    // to reset parameters to saved values for each walker.

    try {
      initializeWalkers(originalParamValues, walkerParamValues, walkerStatisticValue);
      ++istep;
    } catch (YellowAlert&) {
      cleanupRun(originalParamValues);
      fit->isStillValid(wasFitStillValid);
      throw;
    }
  }

  std::vector<Real> saveParamValues(nPars);

  // The number of iterations will be the required length divided by the number
  // of walkers. This ensures that length will work correctly in other chain
  // options. The Handler/xsChain code needs to check that the length (and burn length)
  // are divisible by the number of walkers.

  size_t totalLength = appendLength ? appendLength/m_walkers : (m_length + m_burnLength)/m_walkers;

  // If SIGINT handler hasn't been registered, intHandler will be 0
  // and it won't allow ctrl-C breaking from the loop.      
  const SIGINT_Handler *intHandler = dynamic_cast<const SIGINT_Handler*>
                (SignalHandler::instance()->getHandler(SIGINT));
  ProcessManager procs(new ParallelStats(), "walkers");
  
  try
  {
     // Not resetting verbosity for this since we don't want to affect
     // output in the nested functions below.
     const bool doChatter = (tpout.maxChatter() >= 20);

     // get all the uniform random numbers we need in one go

     RealArray randNumber(totalLength*m_walkers*3);
     Numerics::UniformRand(randNumber);
     size_t irand = 0;

     // Set of walkers will be split in two for parallelization.
     const size_t nParallel = m_walkers/2;
     procs.createProcesses((int)nParallel);
     
     // start the main loop over the chain steps

     while (istep<=totalLength)
     {
        if (intHandler && intHandler->interrupted())
        {
           throw ChainInterrupt();
        }

	for (size_t iset=0; iset<2; iset++) {
	  // loop over the walkers in this set
          vector<Real> Zsaved(nParallel);
          const size_t lowerWalker = iset*nParallel;
          const size_t upperWalker = (iset+1)*nParallel; 
          // These two vectors will be sized to the number
          //   of walker sets which produce 'good' newParamVals.
          vector<TransferStruct> parallelInput;
          vector<size_t> iNewWalk;        
	  for (size_t iwalk=lowerWalker; iwalk<upperWalker; iwalk++) 
          {
	      RealArray& currentParamValues = walkerParamValues[iwalk];

	      // draw a random walker from one of the complementary set

	      size_t jwalk;
	      jwalk = (size_t)(randNumber[irand++]*(m_walkers/2));
	      if ( iset == 0 ) jwalk += m_walkers/2;

	      RealArray& complementaryParamValues = walkerParamValues[jwalk];

	      // Use A as 2.0 as recommended by Goodman & Weare although we may want to 
	      // make this a variable which can be set.

	      Real A(2.0);

	      // draw a random number, Z, from the distribution 1/sqrt(z) for z between 1/A and A.

	      Real Z = (A-1.0)*randNumber[irand++] + 1;
	      Z = Z*Z / A;
              Zsaved[iwalk-lowerWalker] = Z;

	      // set the proposed parameter values for this walker and evaluate the statistic
	      // if any of the new parameter values fall outside their hard limits then
	      // do not update

	      RealArray newParamValues(nPars);
	      newParamValues = (1-Z)*complementaryParamValues + Z*currentParamValues;
	      bool good = fit->goodVariableParameterValues(newParamValues,'v');

	      if ( doChatter ) {
	        tcout << "Test parameters : ";
	        for (size_t i=0; i<nPars; i++) tcout << newParamValues[i] << ", ";
	        tcout << "good = " << good << std::endl;
	      }
              
              if (good)
              {
                 parallelInput.push_back(TransferStruct());
                 TransferStruct& currentStruct = parallelInput.back();
                 currentStruct.dValues.resize(1);
                 vector<double>& valVector = currentStruct.dValues[0];
                 valVector.resize(newParamValues.size());
                 for (size_t iVal=0; iVal<valVector.size(); ++iVal)
                    valVector[iVal] = newParamValues[iVal];
                 iNewWalk.push_back(iwalk);
              }
              
          } // end loop over walkers in this set
            
          ProcessManager::ParallelResults results;
          procs.run(parallelInput, results);  
          ProcessManager::ParallelResults::const_iterator itResults = results.begin();
          size_t iResults=0;
          while (itResults != results.end())
          {
             const size_t iwalk = iNewWalk[iResults];
             const TransferStruct& output = itResults->second;
             if (output.status < 0)
             {
                string errMsg("Error occurred while calculating statistic from random walkers.");
                throw ChainError(errMsg);
             }
             const Real newStatisticValue = output.dValues[0][0];

	     // calculate the acceptance criterion - note the -0.5 factor because the statistic
	     // is -2 log likelihood.
             
             const Real Z = Zsaved[iwalk-lowerWalker];
	     Real& currentStatisticValue = walkerStatisticValue[iwalk];
	     Real lndiff = (nPars-1)*log(Z) - 0.5*(newStatisticValue - currentStatisticValue);

	     // if required update this walker

	     if ( log(randNumber[irand++]) < lndiff ) {
                RealArray& currentParamValues = walkerParamValues[iwalk];
                const TransferStruct& currentStruct = parallelInput[iResults];
                const vector<double>& newValVector = currentStruct.dValues[0];
                for (size_t iVal=0; iVal<newValVector.size(); ++iVal)
                   currentParamValues[iVal] = newValVector[iVal];                               
	        currentStatisticValue = newStatisticValue;
	     } 

             ++iResults;
             ++itResults;
          }  // end results struct loop
           
	}  // end loop over two sets


	// now update the chain file. This could have been done in the previous loop
	// but separated it out for clarity.

        if (istep > (m_burnLength/m_walkers) || appendLength) {
	  for (size_t iwalk=0; iwalk<m_walkers; iwalk++) {
	    m_IO->writePoint(walkerParamValues[iwalk], walkerStatisticValue[iwalk]);
	  }
	}

	// end the main loop over chain steps.

        ++istep;
     }

     procs.killProcesses();
     
     //debug
     //     debugfile.close();

     // if have been appending then reset lengths to match the total chain

     if (appendLength)
     {
        m_length += appendLength;
	m_IO->doCloseFile();
	m_IO->doOpenForWrite();
	m_IO->adjustLengthInfo();
     }
  }
  catch (YellowAlert&)
  {
     // The -1 comes from the fact that istep would have been incremented
     // one more time after the last point was written.
     if (appendLength)
     {
        m_length = origLength + istep-1;
	m_IO->doCloseFile();
	m_IO->doOpenForWrite();
        tcerr << "\n*** Warning: Chain append run has been interrupted." << std::endl;
     }
     else
     {
        m_length = (istep-1 > m_burnLength) ? istep-1-m_burnLength : 0;
        tcerr << "\n*** Warning: Chain run has been interrupted.\n"
             <<  "    The chain will not be loaded." << std::endl;
     }
     m_IO->adjustLengthInfo();
     cleanupRun(originalParamValues);
     fit->isStillValid(wasFitStillValid);
     procs.killProcesses();
     // Even if exception is coming from something other than a
     // Ctrl-C break, we'll treat it the same way in this context.
     throw ChainInterrupt();
  }
  cleanupRun(originalParamValues);
  fit->isStillValid(wasFitStillValid);
}

void Chain::initializeWalkers(const RealArray& originalParamValues,
			      std::vector<RealArray>& walkerParamValues,
			      std::vector<Real>& walkerStatisticValue)
{

  // find out whether we can use the covariance matrix to set up the initial walkers
  bool useCovar = fit->isStillValid();
  if ( useCovar ) {
    try {
      // initialize the parameter randomization
      fit->randomizeForChain(true);
    } catch(...) {
      useCovar = false;
    }
  }

  // if we are not using the covariance matrix then we will need the parameter deltas
  RealArray paramDeltas = fit->variableParameterValues('d');

  // loop round walkers
  for (size_t i=0; i<m_walkers; i++) {

    if ( useCovar ) {

      // for this walker set variable parameters to original values
      fit->setVariableParameterValues(originalParamValues);
      // randomize parameters using the proposal distribution set then store the result
      fit->randomizeForChain();
      walkerParamValues[i] = fit->variableParameterValues();

    } else {

      // we will need a uniform random number for each parameter
      RealArray randNumber(originalParamValues.size());
      Numerics::UniformRand(randNumber);
      walkerParamValues[i] =  originalParamValues + paramDeltas * (randNumber-0.5) * 20.0;
      fit->setVariableParameterValues(walkerParamValues[i]);

    }

    // calculate the fit statistic for these parameters and store
    fit->statManager()->performStats();
    walkerStatisticValue[i] = fit->statistic();
    // if required save the results to the output file
    if (!m_burnLength) m_IO->writePoint();
  }

}

void Chain::cleanupRun (const RealArray& origPars)
{
  m_IO->doCloseFile();
  fit->setVariableParameterValues(origPars,'v');
  fit->statManager()->performStats();
}

void Chain::openForReadPoints () const
{
  m_IO->doOpenForReadPoints();
}

void Chain::closeFile () const
{
  m_IO->doCloseFile();
}

void Chain::readPoint (std::vector<Real>& chainVals) const
{
  m_IO->doReadPoint(chainVals);
}

void Chain::calcStats (size_t iPar, Real& mean, Real& variance, size_t& nRepeats)
{
  // Assumes iPar is already verified to be >= 0.
  if (iPar >= m_width-1)
  {
     std::ostringstream msg;
     msg << "Stored chain width value is not consistent with parameter labels in file "
         << m_fileName;
     throw RedAlert(msg.str());
  }
  RealArray parVals(.0,m_length);
  Real total = .0;
  m_IO->doOpenForReadPoints();
  // Checking for repeats is simple for mh chains but needs to be done for individual walkers 
  // in the case of gw so deal with the two cases separately.
  if ( m_chainType == "MetropolisHastings" ) {
    // This setting of prevPoint is not likely to match initial point.
    std::vector<Real> prevPoint(m_width,-9.876e99);
    nRepeats = 0;
    std::vector<Real> tmp;
    for (size_t i=0; i<m_length; ++i)
    {
      m_IO->doReadPoint(tmp);
      if (checkForRepeat(prevPoint, tmp))
        ++nRepeats;
      prevPoint = tmp;
      parVals[i] = tmp[iPar];
      total += parVals[i];
    }
  } else if ( m_chainType == "GoodmanWeare" ) {
    std::vector<std::vector<Real> > walkerParamValues(m_walkers);
    for (size_t i=0; i<m_walkers; i++) {
      walkerParamValues[i].resize(m_width);
      for (size_t j=0; j<m_width; j++) walkerParamValues[i][j] = -9.876e99;
    }
    nRepeats = 0;
    std::vector<Real> tmp;
    for (size_t i=0; i<m_length/m_walkers; ++i) {
      for (size_t j=0; j<m_walkers; ++j) {
	m_IO->doReadPoint(tmp);
	if (checkForRepeat(walkerParamValues[j], tmp)) ++nRepeats;
	walkerParamValues[j] = tmp;
	parVals[i*m_walkers+j] = tmp[iPar];
	total += parVals[i*m_walkers+j];
      }
    }
  }
  m_IO->doCloseFile();
  mean = total/m_length;
  variance = calcVar(parVals, mean);
  return;
}

void Chain::findBestPoint (size_t& lineNum, RealArray& parVals, Real& statVal)
{
  size_t nPars(m_width-1);
  parVals.resize(nPars);

  m_IO->doOpenForReadPoints();

  std::vector<Real> tmp;
  m_IO->doReadPoint(tmp);
  statVal = tmp[tmp.size()-1];
  for (size_t j=0; j<nPars; j++) parVals[j] = tmp[j];
  lineNum = 1;

  for (size_t i=1; i<m_length; ++i) {
    m_IO->doReadPoint(tmp);
    if ( tmp[tmp.size()-1] < statVal ) {
      statVal = tmp[tmp.size()-1];
      for (size_t j=0; j<nPars; j++) parVals[j] = tmp[j];
      lineNum = i+1;
    }
  }
  m_IO->doCloseFile();
  return;
}

Real Chain::calcVar (RealArray& parVals, Real mean) const
{
  Real variance = .0;
  size_t sz = parVals.size();
  parVals -= mean;
  for (size_t i=0; i<sz; ++i)
  {
     Real parVals_i = parVals[i];
     variance += parVals_i*parVals_i;
  }
  return variance;
}

void Chain::findParsInChain (std::vector<Chain::ParamID>& parIDs, std::vector<size_t>& found) const
{
   // Find location of parIDs in m_paramIDs.  This is an O(N^2)
   // algorithm, but N > 10^2 is currently unimaginable.
   found.clear();
   size_t nPars = m_paramIDs.size();
   for (size_t i=0; i<parIDs.size(); ++i)
   {
      const string& modName = parIDs[i].modName;
      size_t index = parIDs[i].index;
      for (size_t j=0; j<nPars; ++j)
      {
         const ParamID& chainPar = m_paramIDs[j];
         if (chainPar.modName == modName && chainPar.index == index)
         {
            found.push_back(j);
            // Also, fill in the remainder of the calling function's
            // parID struct.  We assume it doesn't have this info
            // till now.
            parIDs[i].units = chainPar.units;
            parIDs[i].parName = chainPar.parName;
            break;
         }
      }
   }
}

std::vector<Chain::ParamID> Chain::findParsInChain (size_t par1, const string& modName1, size_t par2, const string& modName2) const
{
    //get parameter names from header of first chain file

    //pos is not needed in this case, except to compile
    std::vector<size_t> pos;
    std::vector<Chain::ParamID> paramIDs(2);

    Chain::ParamID 
	& xParamID = paramIDs[0], & yParamID = paramIDs[1];

    xParamID.modName = modName1;
    xParamID.index = par1;
    yParamID.modName = modName2;
    yParamID.index = par2;

    findParsInChain(paramIDs, pos);

    return paramIDs;
}

bool Chain::checkForRepeat (const std::vector<Real>& prevPoint, const std::vector<Real>& point)
{
   // ASSUMES both input vectors are same size, no checking.
   bool isRepeat = true;
   const Real EPS = 1.0e-15;
   const size_t nPar = prevPoint.size();
   for (size_t i=0; i<nPar; ++i)
   {
      Real prev = prevPoint[i];
      Real curr = point[i];
      Real diff = std::fabs((curr-prev)/curr);
      if (diff >= EPS)
      {
         isRepeat = false;
         break;
      }
   }
   return isRepeat;
}

int Chain::FWIDTH ()
{
   return ChainIO::FWIDTH();
}

void Chain::readPointFromLine (size_t lineNum, RealArray& parVals) const
{
  m_IO->doReadPointFromLine(lineNum, parVals);
}

int Chain::TEMPERPREC ()
{
   return ChainIO::TEMPERPREC();
}

const string& Chain::format () const
{
   return m_IO->getFormatName();
}

// Additional Declarations
