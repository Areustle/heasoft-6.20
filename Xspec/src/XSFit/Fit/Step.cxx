//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Fit
#include <XSFit/Fit/Fit.h>
// Step
#include <XSFit/Fit/Step.h>

#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUtil/Numerics/ModularCounter.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <XSsymbol.h>
#include <XSstreams.h>  
#include <iomanip>
#include <limits>
#include <sstream>
#include <utility>


// Class Step 

Step::Step (Fit* fit)
  : m_best(false),
    m_bestFit(.0),
    m_fit(fit),
    m_minStatFound(std::numeric_limits<double>::max()),
    m_minStatParams(),
    m_isFirstProc(false),
    m_freeParVals()
{
}


Step::~Step()
{
}

void Step::doGrid()
{
   // this is intended to replace doGrid when complete
   
  using namespace std;
        
  const bool saveFitStatus = m_fit->isStillValid();
  m_minStatFound = numeric_limits<double>::max();
  m_minStatParams.clear();
  
  IntegerArray nSteps;
  SpecContainer::iterator itP    = parameter().begin();
  SpecContainer::iterator itPEnd = parameter().end();
  while (itP != itPEnd )
  {
     ParameterSpec*& spec = *itP; 
     nSteps.push_back((int)spec->parameterValues.size());

     // freeze each parameter by removing from list of fit's variable
     // parameters. They will be added on command exit. 
     m_fit->freezeByIndex(spec->fullFitIndex);
     ModParam* mp = spec->address;
     // Freeze call really only matters for norm param, since that will
     // be accessed even if it's not part of fit->variableParameters map
     // (see StatMethod::renorm function).  
     mp->freeze();
     ++itP;
  } 
  
  // Now that steppars are frozen (and pulled from fit->variableParameters), 
  //  initialize arrays which will hold the values at each grid point for the 
  //  non-steppar parameters.
  m_freeParVals.clear();
  const size_t nGridPts = grid().size();
  std::map<int,ModParam*>::const_iterator itFreeVar = m_fit->variableParameters().begin();       
  std::map<int,ModParam*>::const_iterator itFreeVarEnd = m_fit->variableParameters().end();
  while (itFreeVar != itFreeVarEnd)
  {
     const string modParStr = itFreeVar->second->getParameterLabel();
     m_freeParVals.insert(make_pair(modParStr,RealArray(0.0,nGridPts)));
     ++itFreeVar;
  }
  
  
  bool TITLE=true;
  report(TITLE);
  
  // For now, not going to allow multiple processes at both the 'steppar' and
  //  Jacobian calculation levels.  If user has set 'leven', we'll temporarily
  //  set it to 1 before calling run.
  int savedLevenProcs=-1;
  int nStepProcs = 1;
  map<string,int>::iterator itEnd = ProcessManager::maxProcs().end();
  map<string,int>::iterator itStep = ProcessManager::maxProcs().find("steppar");
  if (itStep != itEnd && itStep->second > 1)
  {
     nStepProcs = itStep->second;
     map<string,int>::iterator itLeven=ProcessManager::maxProcs().find("leven");
     if (itLeven != itEnd)
     {
        savedLevenProcs = itLeven->second;
        itLeven->second = 1;
     }
  }
  IntegerArray elemRanges(nStepProcs);
  divideIntoProcs(elemRanges);
  
  ProcessManager procs(new ParallelGrid(nSteps), "steppar");
  try
  {
     procs.createProcesses(nStepProcs);
  }
  catch(YellowAlert&)
  {
     restoreFit();
     m_fit->isStillValid(saveFitStatus);
     if (savedLevenProcs > 1)
        ProcessManager::maxProcs()["leven"] = savedLevenProcs;
     throw;
  }
    
  vector<TransferStruct> parallelInput;
  for (int i=0; i<nStepProcs; ++i)
  {
     TransferStruct parInfo;
     vector<int> elemRange(2);
     elemRange[0] = (i == 0) ? 0 : elemRanges[i-1];
     elemRange[1] = elemRanges[i];
     parInfo.iValues.push_back(elemRange);
     // Only the first process should output its results in real time.
     // We'll "mark" it by adding a string (its actual content is irrelevant).
     if (i==0)
     {
        parInfo.sValues.push_back("first");
     }   
     parallelInput.push_back(parInfo);
  }
  ProcessManager::ParallelResults results;
  // This will not throw:
  procs.run(parallelInput, results);
  
  if (savedLevenProcs > 1)
     ProcessManager::maxProcs()["leven"] = savedLevenProcs;
  
  size_t iProc=0;
  ProcessManager::ParallelResults::const_iterator itResults = results.begin();
  while (itResults != results.end())
  {
     const TransferStruct& output = itResults->second;
     if (output.status < 0)
     {
        restoreFit();
        m_fit->isStillValid(saveFitStatus);
        procs.killProcesses();
        throw YellowAlert();
     }
     else
     {
        RealArray& completeGrid = grid();
        const std::vector<int>& gridPos = output.iValues[0];
        const std::vector<Real>& vals = output.dValues[0];
        const size_t nVals = vals.size();
        for (size_t iVal=0; iVal<nVals; ++iVal)
        {
           completeGrid[gridPos[iVal]] = vals[iVal];
        }
        itFreeVar = m_fit->variableParameters().begin();
        size_t iOutVec = 1;
        while (itFreeVar != itFreeVarEnd)
        {
           RealArray& parVals = 
                        m_freeParVals[itFreeVar->second->getParameterLabel()];
           const std::vector<Real>& outVals = output.dValues[iOutVec];
           for (size_t iVal=0; iVal<nVals; ++iVal)
           {
              parVals[gridPos[iVal]] = outVals[iVal];
           }
           ++itFreeVar, ++iOutVec;
        }
                
        if (iProc > 0)
        {
           reportProcess(nSteps, elemRanges[iProc-1], vals);
        }
        // If this is single-process, the parent m_minStat members will 
        // already have been filled with the correct values.  If parallel, 
        // must sift through the results from the child processes.
        if (procs.isParallel())
        {
           Real localMin = output.dValues[1+m_freeParVals.size()][0];
           if (localMin < m_minStatFound)
           {
              m_minStatFound = localMin;
              const std::vector<int>& minParFullIdx = output.iValues[1];
              const std::vector<double>& minParVals = output.dValues[2+m_freeParVals.size()];
              for (size_t iPar=0; iPar<minParFullIdx.size(); ++iPar)
                 m_minStatParams[minParFullIdx[iPar]] = minParVals[iPar];
           }
        }
        ++iProc;
     }
     
     ++itResults;
  }
  restoreFit();
  m_fit->isStillValid(saveFitStatus);
  procs.killProcesses();
  
}
// end doGrid


void Step::retrieveParameter (const Fit* fit, const string& paramIDStr, ModParam*& parameter, int& paramIndex, int& fullIndex, string& paramName)
{
  using namespace XSContainer;

  if (paramIDStr.empty())
  {
     throw InvalidParameter("Empty parameter identifier");
  }  

  // Must check for response param.  These will be indicated by "r<par idx>"
  // or "<source idx>:r<par idx>".  Look for the 'r'.
  string::size_type rPos = XSparse::checkForRespPar(paramIDStr);
  const bool isRespPar = (rPos != string::npos);

  const Parameter* par=0; 
  string modelName;
  if (isRespPar)
  {
     int parIdx=0;
     int sourceIdx=0;
     // must remove the 'r'
     string correctedIDStr(paramIDStr);
     correctedIDStr.erase(rPos,1);
     if (XSparse::integerPair(correctedIDStr, sourceIdx, parIdx))
     {
        if (parIdx == -1)
        {
           parIdx = sourceIdx;
           sourceIdx = 1;
        }
        const RespParContainer& respPars = responses->respParContainer();
        if (static_cast<int>(respPars.size()) >= sourceIdx)
           if (parIdx > 0 && static_cast<int>(respPars[sourceIdx-1].size()) >= parIdx)
           {
              par = respPars[sourceIdx-1][parIdx-1];
              // full index is given by respFloor = (2^15 - 1)*2^16 = 2^31 - 2^16
              // + 1-based position in the respPars container
              // (see Fit::initializeFitParameters).
              size_t pos=1;
              for (int i=0; i<(sourceIdx-1); ++i)
                 pos += respPars[i].size();
              pos += parIdx-1;
              fullIndex = (Fit::RESPAR_INDEX() << ModelContainer::SHIFT()) + 
                                static_cast<int>(pos);   
              paramIndex = parIdx;
           }
        if (sourceIdx > 1)
        {
           std::ostringstream oss;
           oss << sourceIdx;
           // "modelName" in this context is simply the source number
           modelName = oss.str();
        }
     } 
  } // end if respPar
  else
  {
     size_t index=0;  
     XSparse::stringIntPair(paramIDStr,modelName,index);
     paramIndex = static_cast<int>(index);
     fullIndex = models->fullIndex(index,modelName);
     par = models->lookupParameter(fullIndex);
  }

  if (par)
  {
     if ( modelName.length())
     {
         paramName = modelName + string(":");       
     }
     paramName += par->name();
     parameter = fit->variableParameters(fullIndex);
     if ( !parameter )
     {
         string msg (" Step: Parameter ");
         msg += paramIDStr;
         msg += " is not variable";
         throw InvalidParameter(msg);                       
     }               
  }
  else
  {
     string msg (" Step: Parameter ");
     msg += paramIDStr;
     msg += " is not defined.";
     msg += "\n   If parameter belongs to a named model, must enter <model name>:<par idx>.";
     msg += "\n   If trying to access a response parameter, use 'r' specifier: [<sourc num>:]r<par idx>.";
     throw InvalidParameter(msg);        
  }

}


void Step::restoreFit ()
{
  SpecContainer& parameters = parameter();     
  SpecContainer::iterator sp = parameters.begin();
  SpecContainer::iterator spEnd = parameters.end();

  // put back parameters into fit's variable parameters set. 
  while ( sp != spEnd )
  {
        ParameterSpec*& p = *sp;
        p->address->thaw();  
        m_fit->variableParameters(p->fullFitIndex,p->address);         
        ++sp;       
  }

  std::map<int,ModParam*>::const_iterator v (m_fit->variableParameters().begin());       
  std::map<int,ModParam*>::const_iterator vEnd (m_fit->variableParameters().end());
  while ( v != vEnd )
  {
          int index ( v->first );
          Real oldVal = m_fit->oldParameterValues(index)->value('v');
          m_fit->variableParameters(index)->setValue(oldVal,'v'); 
          ++v;     
  }       

  // go through again and set the value fields of the parameter specs to the 
  // best fit value.
  sp = parameters.begin();
  for ( ; sp != spEnd; ++sp ) (*sp)->value = (m_fit->variableParameters((*sp)->fullFitIndex)->value('v'));

  m_fit->calculateModel();

  // We want the parameters set back to their original values prior to steppar,
  // so no renormalization.
  Fit::RenormSetting saveRenorm = m_fit->renormType();
  m_fit->renormType(Fit::NONE);
  try
  {
     m_fit->initializeStatistic();
  }
  catch (...)
  {
     m_fit->renormType(saveRenorm);
     throw;
  }
  m_fit->renormType(saveRenorm);
}

void Step::report (bool title) const
{
    using namespace std;
    // preserve current chatter level settings and force output.
    int con (tpout.consoleChatterLevel());
    int log (tpout.logChatterLevel());
    tpout.consoleChatterLevel( tpout.conVerbose() );
    tpout.logChatterLevel( tpout.logVerbose() );

    StatManager* stat = m_fit->statManager();

    const SpecContainer& parameters = getParameter();
    const size_t nPar  (parameters.size());
    if ( title )
    {
       const StatMethod* statMeth = stat->usingSingleStat();
       string fullName = statMeth ? statMeth->fullName() : string("total stat");
       tpout << "\n" << setw(16) << fullName
             << setw(12)   <<   "  Delta   "; 
       for (size_t  j = 0; j < nPar; ++j )
       {
           tpout << setw(14) << parameters[j]->name;       
       }

       tpout << "\n";
       tpout << setw(16) <<    " "
             << setw(12)   << fullName; 
       for (size_t  j = 0; j < nPar; ++j )
       {
           tpout << setw(14) << parameters[j]->address->index();       
       }
       tpout << endl << endl;

    }
    tpout << flush;


    tpout.consoleChatterLevel(con);
    tpout.logChatterLevel(log);
}

void Step::divideIntoProcs(IntegerArray& elemRanges) const
{
   const size_t nProcs = elemRanges.size();
   if (nProcs)
   {
      const SpecContainer& parameters = getParameter();
      const size_t nPars = parameters.size();
      int totalSteps = 1;
      for (size_t iPar=0; iPar<nPars; ++iPar)
      {
         totalSteps *= parameters[iPar]->intervals + 1;
      }
      int arrayStop=0;
      for (size_t iProc=0; iProc<nProcs; ++iProc)
      {
         int nStepsForProc = totalSteps/(int)nProcs;
         if (iProc < (totalSteps % nProcs))
            ++nStepsForProc;
         arrayStop += nStepsForProc;
         elemRanges[iProc] = arrayStop;
      }      
   }
}

void Step::doPoint(const IntegerArray& coordinates)
{
   const size_t nPars = parameter().size();
   for (size_t i=0; i<nPars; ++i)
   {
      ParameterSpec* spec = parameter()[i];
      const Real parVal = spec->parameterValues[coordinates[i]];
      spec->value = parVal;
      ModParam* par = spec->address;
      par->setValue(parVal, 'v');
   }
   if (m_best)
   {
      // the set of indices of variableParameters is a subset of that of
      // oldParameterValues
      std::map<int,ModParam*>::const_iterator itV (m_fit->variableParameters().begin());       
      std::map<int,ModParam*>::const_iterator itVEnd (m_fit->variableParameters().end());
      while (itV != itVEnd )
      {
         int index ( itV->first );
         Real oldVal = m_fit->oldParameterValues(index)->value('v');
         m_fit->variableParameters(index)->setValue(oldVal,'v'); 
         ++itV;     
      }       
   }
   
   m_fit->calculateModel();
   if (m_fit->variableParameters().size())
   {
     // renorm, DOF check, recompute current statistic
     m_fit->initializeStatistic();

     m_fit->perform();     
   }
   else
   {
      m_fit->statManager()->performStats();
   }
   
   if (m_fit->statistic() < m_minStatFound)
   {
      m_minStatFound = m_fit->statistic();
      const SpecContainer& parameters = parameter();
      for (size_t iPar=0; iPar<nPars; ++iPar)
      {
         m_minStatParams[parameters[iPar]->fullFitIndex] = 
                parameters[iPar]->value;
      }
      std::map<int,ModParam*>::const_iterator itVar = m_fit->variableParameters().begin();       
      std::map<int,ModParam*>::const_iterator itEnd = m_fit->variableParameters().end();
      while (itVar != itEnd)
      {
         m_minStatParams[itVar->first] = itVar->second->value('v');
         ++itVar;
      }
      
   }
   
   if (m_isFirstProc)
      reportPoint(coordinates);   
}

void Step::reportPoint(const IntegerArray& coordinates) const
{
   using namespace std;
   
   ios_base::fmtflags saveFmt(tpout.flags());
   size_t savePrecision(tpout.precision());
   tpout.setf(ios_base::showpoint|ios_base::right,ios_base::scientific);
   tpout.precision(5);

   Real stat = m_fit->statistic();
   tpout << right << setw(16) << stat
         << right << setw(12) << stat - m_bestFit;

   for(int i = 0; i < static_cast<int>(coordinates.size()); ++i)
       tpout << right << setw(5) << coordinates[i]
	     << right << setw(12) << getParameter()[i]->parameterValues[coordinates[i]];

   tpout << endl;
   
   tpout.flags(saveFmt);
   tpout.precision(savePrecision);
}

void Step::reportProcess(const IntegerArray& nSteps, int startingElem, const std::vector<Real>& vals) const
{
   using namespace std;
   
   Numerics::ModularCounter stepCounter(nSteps, true);
   stepCounter.reset(startingElem);
   const size_t nVals = vals.size();
   const IntegerArray& coords = stepCounter.counter();
   const size_t nPars = coords.size();
   
   ios_base::fmtflags saveFmt(tpout.flags());
   size_t savePrecision(tpout.precision());
   tpout.setf(ios_base::showpoint|ios_base::right,ios_base::scientific);
   tpout.precision(5);
   for (size_t iVal=0; iVal<nVals; ++iVal)
   {
      tpout << right << setw(16) << vals[iVal]
            << right << setw(12) << vals[iVal] - m_bestFit;
      for (size_t iPar=0; iPar<nPars; ++iPar)
      {
         tpout << right << setw(5) << coords[iPar]
	     << right << setw(12) << getParameter()[iPar]->parameterValues[coords[iPar]];
      }
      tpout << endl;   
      ++stepCounter;
   }

   tpout.flags(saveFmt);
   tpout.precision(savePrecision);
}

Step::ParallelGrid::ParallelGrid(const IntegerArray& nSteps)
  : ParallelFunc(),
    m_nSteps(nSteps)
{
}

void Step::ParallelGrid::execute(const bool isParallel, const TransferStruct& input, TransferStruct& output)
{
   using namespace XSContainer;
   
   int iStart = input.iValues[0][0];
   int iStop = input.iValues[0][1];
   
   if (input.sValues.size())
      fit->stepGrid()->isFirstProc(true);
   else
      fit->stepGrid()->isFirstProc(false);  
   
   const size_t nFreePars = fit->variableParameters().size();
   std::vector<Real> gridVals;
   std::vector<int> gridPos;
   std::vector<std::vector<Real> > freeParVals(nFreePars);
   try
   {
      Numerics::ModularCounter stepCounter(m_nSteps, true);
      stepCounter.reset(iStart);
      for (int iStep=iStart; iStep<iStop; ++iStep)
      {
         fit->stepGrid()->doPoint(stepCounter.counter());
         gridVals.push_back(fit->statistic());
         gridPos.push_back(stepCounter.rasterToStandardPos());
         std::map<int,ModParam*>::const_iterator itVar = fit->variableParameters().begin();       
         std::map<int,ModParam*>::const_iterator itEnd = fit->variableParameters().end();
         size_t iFree=0;
         while (itVar != itEnd)
         {
            freeParVals[iFree].push_back(itVar->second->value());
            ++itVar, ++iFree;
         }
         ++stepCounter;
      }
   }
   catch (...)
   {
      output.status = -1;
      throw;
   }
   
   output.dValues.clear();
   output.iValues.clear();
   output.dValues.push_back(gridVals);
   output.iValues.push_back(gridPos);
   for (size_t iFree=0; iFree<nFreePars; ++iFree)
   {
      output.dValues.push_back(freeParVals[iFree]);
   }
   if (isParallel)
   {
      output.dValues.push_back(std::vector<double>(1, fit->stepGrid()->minStatFound()));
      output.iValues.push_back(std::vector<int>());
      output.dValues.push_back(std::vector<double>());
      std::map<int,Real>::const_iterator itPars = fit->stepGrid()->minStatParams().begin();
      std::map<int,Real>::const_iterator itEnd = fit->stepGrid()->minStatParams().end();
      while (itPars != itEnd)
      {
         output.iValues[1].push_back(itPars->first);
         output.dValues[2+nFreePars].push_back(itPars->second);
         ++itPars;
      }
   }
}

   

