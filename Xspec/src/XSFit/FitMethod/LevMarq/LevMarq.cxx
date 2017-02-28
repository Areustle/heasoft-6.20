//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// LevMarq
#include <XSFit/FitMethod/LevMarq/LevMarq.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSModel/Model/Model.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Parameter/ParamLinkList.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Signals/SignalHandler.h>
#include <XSUtil/Utils/ProcessManager.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <cmath>
#include <sstream>
#include <typeinfo>
#include <algorithm>
#include <iomanip>
#include <set>

void invertSVD(double* matrix, int n, bool qzero, double *wvec, double *vmat);


// Class LevMarq 

LevMarq::LevMarq(const LevMarq &right)
  : FitMethod(right), 
    m_dLambda(right.m_dLambda),
    m_variableParameters(right.m_variableParameters), // non-owning
    m_fitStatistic(right.m_fitStatistic),
    m_alpha(right.m_alpha),
    m_beta(right.m_beta)
{
}

LevMarq::LevMarq (const std::string& initString)
  : FitMethod(initString),
    m_dLambda(0.001),
    m_variableParameters(), // non-owning
    m_fitStatistic(.0),
    m_alpha(),
    m_beta()
{
  firstDerivativeRequired(true);
  secondDerivativeRequired(true);  
}


void LevMarq::doPerform (Fit* fit)
{
  // this is the driver program for levenberg-marquadt. It contains the main
  // iteration loop and mainly takes care of temporarily freezing and pegging 
  // parameters as necessary. it calls curFitXspec to do the calculation of
  // new parameter values in each iteration.


  using namespace std;
  using namespace XSContainer;

  double oldFtstat = fit->statManager()->totalStatistic();
  if (isnan(oldFtstat))
  {
     throw YellowAlert("Unable to fit when starting with fit statistic = NaN\n");
  }

  m_fitStatistic = oldFtstat;

  m_dLambda = 0.001;  

  int errorFlag = 0;
  IntegerArray errorPar;

  if (!fit->errorCalc())
  {
        tcout << xsverbose(15)
           << "Number of trials and critical delta: " << numberOfTrials() 
           << "  " << deltaCrit() << std::endl << xsverbose();
  }

  set<int> tempFrozenParams;
  IntegerArray peggedParams;

  bool writeHeader (true);

  if (!fit->errorCalc())  fit->reportParameters(tcout,writeHeader);

  int trialsRemaining = int(numberOfTrials());
  Real curDel = 2.*deltaCrit();

  // set up iterator for variable parameters

  const map<int,ModParam*>::const_iterator vpBegin = fit->variableParameters().begin();
  map<int,ModParam*>::const_iterator vp      = vpBegin;
  const map<int,ModParam*>::const_iterator vpEnd   = fit->variableParameters().end();

  ModelMapConstIter mi;
  ModelMapConstIter miEnd (models->modelSet().end());


  bool done (false);
  bool wereParamsUnpegged(false);

  // Must NEVER exit this routine without first killing the 
  //  child processes created here.
  ProcessManager& procs = fit->statManager()->jacobianCalc();
  procs.createProcesses(fit->variableParameters().size());

  try
  {
     // If calling code has registered the SIGINT_Handler, this
     // will allow user to break with ctrl-C.  Otherwise, 
     // intHandler = 0 and has no affect.

     const SIGINT_Handler *intHandler = dynamic_cast<const SIGINT_Handler*>
                   (SignalHandler::instance()->getHandler(SIGINT));
     bool ctrl_c_exit = false;
     bool slash_star_exit = false;
     do  {    
        --trialsRemaining;          
        tempFrozenParams.clear();
        m_variableParameters.clear();

	mi = models->modelSet().begin();
	// Find which parameters should be temporarily frozen due to 
	// zero norms.
	while (mi != miEnd)
	{
	  Model* m (mi->second);
	  if ( m->isActive()) 
             m->checkZeroNorms(tempFrozenParams);
	  ++mi;       
	}
        ParamLinkList::Instance()->checkLinksToZeroNorms(tempFrozenParams);
        set<int>::const_iterator itFrozEnd = tempFrozenParams.end();
	if (tempFrozenParams.size())
	{
          set<int>::const_iterator itFroz = tempFrozenParams.begin();
	  tcout << " Due to zero model norms, the following fit parameters are"
		<< " temporarily frozen:";
	  while (itFroz != itFrozEnd) 
          {
             tcout << *itFroz << " ";
             ++itFroz;
          }
	  tcout << std::endl;
	}          


        // create temporary param array consisting of variable parameters
        // that are not temporarily frozen.
        // all parameters found in the array of temporarily  frozen
        // parameters are NOT loaded into the algorithm engine.
        vp =  vpBegin;
        while (vp != vpEnd)
        {
           if ((tempFrozenParams.find(vp->first) == itFrozEnd)
	       && (find(peggedParams.begin(),peggedParams.end(),
				static_cast<int>(vp->first))==peggedParams.end()) )
           {
              m_variableParameters[vp->first] = vp->second;
           }
           ++vp;
        }

        // if there are no free parameters then give up
        if (m_variableParameters.size() == 0)  throw Fit::NotVariable();

	// call the routine which calculates the new parameters for this 
	// iteration including looping over lambda values if necessary

        curFitXspec(errorFlag, errorPar);

        // errorFlag == 0,-1,+1 

        if (errorFlag == 0)
        {
           // if we have not just unpegged parameters check whether any have run
	   // past their limits and peg them.

	   if ( !wereParamsUnpegged ) {
	     std::map<int,ModParam*>::const_iterator iv = m_variableParameters.begin();
	     std::map<int,ModParam*>::const_iterator ivEnd = m_variableParameters.end();
	     while ( iv != ivEnd ) {
	       ModParam* par = iv->second;
	       if (!par->isPeriodic()) {
		 if ( par->value() <= par->value('l') ||
		      par->value() >= par->value('h') ) {
		   tcout << xsverbose(15) << " Fit Parameter: "  << iv->second->getParameterLabel()
			 << " has pegged due to running past its ";
		   if ( par->value() <= par->value('l') ) {
		     tcout << "lower limit.";
		   } else {
		     tcout << "upper limit.";
		   }
		   tcout << std::endl << xsverbose();
		   peggedParams.push_back(iv->first);
		 }
	       }
	       ++iv;       
	     }
	   }

           // report parameter values
           fit->reportParameters(tcout);
           // set current and reset previous value.
           curDel = oldFtstat - m_fitStatistic;
           oldFtstat = m_fitStatistic;
	   Real curBetaNorm = fit->statManager()->betaNorm()/m_beta.size();

           // check for convergence. Use either the convergence criterion on the
	   // difference between present and last statistic values or the convergence
	   // criterion on the norm of the derivative vector. If either convergence
	   // criterion is set to a negative number then do not use.

	   done = ( curDel >= 0 && 
		      ((deltaCrit() > 0.0 && curDel <= deltaCrit()) ||
		      (betaNormCrit() > 0.0 && curBetaNorm <= betaNormCrit())
		    ) );

           // keep parameters that have been pegged
           // in that state until  convergence criterion
           // has been met.
           if (!done ) wereParamsUnpegged = false;

        }
        else if ( errorFlag == -1 )
        {
           // If we're in here the diagonal element corresponding to one of 
           // the parameters in m_variableParameters is invalid.  We
           // CANNOT let this get to the invertCorrelationMatrix call in 
           // its present state.  Therefore make sure at least one more
           // iteration is run with the troublesome parameter pegged, even
           // if there are 0 trials remaining.  We can't let the user end
           // the fit just yet.  (If they ctrl-c at any point though, that's
           // OK since invertCorrelationMatrix won't be called.)
	   for (size_t i=0; i<errorPar.size(); i++) {
	     ModParam* ppar = fit->variableParameters(errorPar[i]);
	     peggedParams.push_back(errorPar[i]);
	     tcout << " Parameter " << ppar->getParameterLabel() 
		   << " is pegged at " << ppar->value()
		   << " due to zero or negative pivot element, likely\n" 
		   << " caused by the fit being insensitive to the parameter."<< std::endl;
	   }
           done = false;
           if (!trialsRemaining)
              trialsRemaining = 1;    
        }
        else if (errorFlag == +1)
        {
           // get out of the loop, convergence failed, unless there
           // are pegged parameters, in which case we unpeg them and 
           // try again 
           done = ( peggedParams.size() == 0);
 	   if ( !done )
           {
              errorFlag=0;
              m_dLambda = .001;
           }
        }

        if ( errorFlag > 0 || ((errorFlag == 0) && done ))
        {
	   size_t numberUnpegged (peggedParams.size());
	   peggedParams.clear();
           if ( numberUnpegged == 0 || !wereParamsUnpegged )
           {
              wereParamsUnpegged = numberUnpegged > 0;
              if ( wereParamsUnpegged )
              {
                 if ( tpout.maxChatter() > 20 )
                 {
                    tcout << " Unpegged  " << numberUnpegged 
                                    << " parameters" << std::endl;  
                 }
                 done = false;
                 m_dLambda = 0.001;     
              }   

           } 
        }


        if ( trialsRemaining == 0 && !done)
        {
            // check for number of iterations exceeded.
            // prompt user for continuation -
            if ( fit->queryMode() == Fit::ON)
            {
                // Treat a "/*" as if it were a ctrl-c break.
                int reply = XSutility::yesToQuestion("Number of trials exceeded: continue fitting? ",
                            1,tcin);
                if (reply == 1)
                   trialsRemaining = int(numberOfTrials());
                else if (reply == -1)
                   slash_star_exit = true;
                else
                   done = true;
            }
            else
            {
                if  ( fit->queryMode() == Fit::YES )
                {
                   trialsRemaining = int(numberOfTrials());
                   done = false;
                }    
                else
                {
                   done = true;
                }   
            }

        }

        tcout << std::flush;
        if (intHandler)
        {
           ctrl_c_exit = static_cast<bool>(intHandler->interrupted());
        }
     

     }   while (!done && !ctrl_c_exit && !slash_star_exit); 
     if (ctrl_c_exit || slash_star_exit)
     {
        throw Fit::FitInterrupt();
     }
  }
  catch (...)
  {
     procs.killProcesses();
     throw;
  }     
  procs.killProcesses();
  
  // If not called from error calculations, compute covariance matrix.
  if (!fit->errorCalc())
  {
     invertCorrelationMatrix();
     reportEigenvectors();
  }
}

LevMarq* LevMarq::clone () const
{
  return new LevMarq(*this);
}

void LevMarq::reportProgress (std::ostream& s, Fit* fit) const
{
  using namespace std;
  // Calling function will deal with restoring default format/precision.
  int logLevel = int(std::log10(m_dLambda) + 0.5);
  s.precision(6);
  s << setw(13) << left << fit->statManager()->totalStatistic() 
    << setw(12) << left << fit->statManager()->betaNorm()/m_beta.size() 
    << right << setw(3) << logLevel;
}

string LevMarq::settingString () const
{
    std::ostringstream out;
    out << numberOfTrials() << ' ' << deltaCrit();
    return out.str();
}

string LevMarq::fullName () const
{
  return string("Levenberg-Marquardt");
}

void LevMarq::curFitXspec (int& errorFlag, IntegerArray& errorPar)
{
   // A version of the Marquardt proceedure for minimization, following the 
   // lead of the Bevington program CURFIT.  The calling sequence,
   // intermediate value storage, and logic has been optimized for 
   // use with XSPEC.

   // This is specifically for the isnan call below.  On Darwin
   // it appears that the <cmath> isnan requires the std qualifier,
   // but it can't have it on Solaris.  This way the compilers can
   // choose if they wish to use std::isnan or isnan.
   using namespace std;

   bool delay = delayedGratification();

   // set up array of current parameter values

   RealArray parVals(0.0,m_variableParameters.size());
   std::map<int,ModParam*>::const_iterator itPar = m_variableParameters.begin();
   std::map<int,ModParam*>::const_iterator itParEnd = m_variableParameters.end();
   size_t i=0;
   while (itPar != itParEnd) {
     parVals[i] = itPar->second->value('a');
     ++itPar, ++i;
   }

   Real oldFitStat = m_fitStatistic;

   //  Save the original parameter values, in case the initial matrix
   //  inversion does not decrease the fit statistic and it must
   //  be redone with an increased value of dlamda.
   const RealArray savedParVals(parVals);

   try
   {
      // calcStatAndDerivatives can throw during model calculations.
      // Table models are the most likely to do this.

      // calculate the derivatives of the quantity T == (Obs-Mod)/sigma
      // (folded through the response) wrt the free parameters then 
      // calculate the alpha matrix and beta vectors.
      calcStatAndDerivatives(parVals, true, errorFlag, errorPar);

      // errorFlag = -1 is the only possible error return from 
      // calcStatAndDerivatives and occurs if a pivot element is zero.
      if (errorFlag == -1) return;

      //  loop round changing the parameters and recalculating the
      //  fit parameter until a better fit is achieved. After each
      //  failure the lambda parameter is multiplied by ten.

      const size_t MAXITER = 100;
      bool isDone = false;
      size_t nIter = 0;
      while (!isDone && nIter < MAXITER)
      {
         ++nIter;
         if (m_dLambda > 1.0e300)
            errorFlag = 1;
         else
         {
	    parVals = savedParVals;
            calcNewPars(parVals);

            // Evaluate the the new model, fold through the response, 
            // and compare with the data.
            calcStatAndDerivatives(parVals, false, errorFlag, errorPar);

            // We deliberately are NOT testing and throwing for NaN fitStat 
            // in calcStatAndDerivatives, due to the fact that it gets called
            // again in the catch block which handles this.  For certain
            // strange errors it could conceivably throw again even after
            // the savedParVals are reinserted, which would trigger a
            // RedAlert.  
            if (isnan(m_fitStatistic))
            {
               string err("A fit statistic = NaN was encountered, possibly from ");
               err += "model calculation error.  Fit unable to continue\n";
               throw YellowAlert(err);      
            }       
         }
         isDone = (m_fitStatistic <= oldFitStat);

	 // If required follow Transtrum's Delayed Gratification scheme and 
	 // multiply by a factor of 2 for uphill steps (while dividing by a 
	 // factor of 10 for downhill steps) otherwise use factor of 10 in both
	 // directions.
	 if ( isDone ) {
	   if (m_dLambda > 1.0e-30) m_dLambda *= 0.1;
	 } else {
	   if ( delay ) {
	     m_dLambda *= 2.0;
	   } else {
	     m_dLambda *= 10.0;
	   }
	 }

      } // end while loop

      // If the number of iterations was exceeded before a better fit was found
      // then return the non-convergence error and reset to input parameter values

      if ( !isDone ) {
	tcout << xsverbose(15) <<" Iteration failed - backing up: error = " 
	      << errorFlag << " delta stat = " << (m_fitStatistic-oldFitStat)
	      << std::endl << xsverbose();
	parVals = savedParVals;
	calcStatAndDerivatives(parVals, false, errorFlag, errorPar);
	errorFlag = 1;
      }
   }
   catch (YellowAlert&)
   {
      // If we're in here, presumably some model calculation went astray
      // in calcStatAndDerivatives.  Reset pars and calculations to the
      // previous attempt.  
      parVals = savedParVals;
      try
      {
         calcStatAndDerivatives(parVals, false, errorFlag, errorPar);
      }
      catch (...)
      {
         // Hard to see how this can happen, but if it throws again 
         // then something is seriously wrong.
         throw RedAlert("Exit from LevMarq::curFitXspec");
      }
      throw;
   }
}

void LevMarq::calcNewPars (RealArray& parVals)
{
   // central method which calculates a new set of parameter values
   // based on the derivative array and matrix stored in m_beta and
   // m_alpha.

   // ASSUMES m_alpha and m_beta are correctly sized and properly
   // filled in, and that parVals is the same size as m_beta. 
   // No checking performed here.
   const size_t nPar = m_beta.size();

   // Normalize by the diagonal elements to improve the numerical 
   // accuracy of the inversion (basically this corrects for the 
   // large differences in magnitude between parameter values).
   RealArray diags(.0, nPar);
   RealArray invArray(.0, nPar*nPar);
   for (size_t i=0; i<nPar; ++i)
      diags[i] = m_alpha[i*nPar+i];
   for (size_t i=0; i<nPar; ++i)
      for (size_t j=0; j<nPar; ++j)
         invArray[i*nPar+j] = m_alpha[i*nPar+j]/sqrt(diags[i]*diags[j]);

   Real da = 1.0 + m_dLambda;
   for (size_t i=0; i<nPar; ++i)
      invArray[i*nPar+i] = da;

   // These 2 arrays are only needed for the invertSVD call.
   // This function won't be using their return values.
   XSutility::auto_array_ptr<double> pWvec(new double[nPar]);
   XSutility::auto_array_ptr<double> pVmat(new double[nPar*nPar]);
   // invert the alpha array. Now uses svd and trapping of any singularity.
   invertSVD(&invArray[0], nPar, true, pWvec.get(), pVmat.get());

   // Calculate the new model parameters.

   RealArray deltaVals(0.0,nPar);
   for (size_t i=0; i<nPar; ++i)
   {
      deltaVals[i] = 0.0;
      for (size_t j=0; j<nPar; ++j) {
         deltaVals[i] += m_beta[j]*invArray[i*nPar+j]/sqrt(diags[i]*diags[j]);
      }
   }

   // Update the parameter values. If the new parameter values lie outside any
   // limits then scale the deltaVals vector so the new values lie within or on
   // the limits

   std::map<int,ModParam*>::const_iterator itPar = m_variableParameters.begin();
   for (size_t i=0; i<nPar; ++i, ++itPar) {
    
     const ModParam* modPar = itPar->second;
     Real absDelt(fabs(deltaVals[i]));

     if ( !modPar->isPeriodic() && absDelt > 0.0 ) {

       Real oldVal = parVals[i];
       Real newVal = parVals[i] + deltaVals[i];
       Real lowVal(modPar->value('l'));
       Real highVal(modPar->value('h'));

       Real frac(1.0);

       // to match old method as closely as possible force frac to be 1/2^n for the
       // smallest value of n such that frac < 1/2^n.

       // temporarily reverted to old version of this test which is not correct
       // when soft and hard limits differ
       //       if ( newVal < lowVal && lowVal == modPar->value('b') ) {
       if ( newVal < lowVal ) {
	 frac = (oldVal-lowVal)/absDelt;
         if (frac != 0.0)
         {
	    int n = (int)(-log(frac)/log(2.0)) + 1;
	    frac = 1.0/pow(2.0,(Real)n);
         }
	 //       } else if ( newVal > highVal && highVal == modPar->value('t') ) {
       } else if ( newVal > highVal ) {
	 frac = (highVal-oldVal)/absDelt;
         if (frac != 0.0)
         {
	    int n = (int)(-log(frac)/log(2.0)) + 1;
	    frac = 1.0/pow(2.0,(Real)n);
         }
       }

       deltaVals[i] *= frac;

     }
   }

   parVals += deltaVals;

}

void LevMarq::invertCorrelationMatrix ()
{
   // Adapted from ivermt.f

   // This calls invertSVD twice.  The first time, the alpha matrix is sent in with its 
   // diagonals UNNORMALIZED. This allows us to pull out the true eigenvalues and
   // vectors.  For the second call, the alpha matrix is normalized the same way 
   // as in calcNewPars. This is to make the matrix inversion more robust when the 
   // original diagonals differ by many orders of magnitude.  

   // set up the correlation matrix from the alpha matrix
   RealArray& eValue = evalue();
   RealArray& eVector = evector();

   const size_t nPar = m_beta.size();
   eValue.resize(nPar);
   eVector.resize(m_alpha.size());
   RealArray dummyMat(m_alpha);

   // The inverse matrix (covariance) is ignored here.
   invertSVD(&dummyMat[0], nPar, false, &eValue[0], &eVector[0]);

   RealArray& covar = covariance();
   covar.resize(m_alpha.size());
   covar = m_alpha;
   RealArray diags(.0, nPar);
   RealArray dummyVal(.0, nPar);
   for (size_t i=0; i<nPar; ++i)
      diags[i] = m_alpha[i*nPar+i];
   for (size_t i=0; i<nPar; ++i)
      for (size_t j=0; j<nPar; ++j)
         covar[i*nPar+j] /= sqrt(diags[i]*diags[j]);

   // Cannot get eignevalues and vectors here.
   invertSVD(&covar[0], nPar, false, &dummyVal[0], &dummyMat[0]);

   // Now recover the true (unnormalized) covariance.
   for (size_t i=0; i<nPar; ++i)
      for (size_t j=0; j<nPar; ++j)
         covar[i*nPar+j] /= sqrt(diags[i]*diags[j]);

   // If a parameter is not also included in the subset m_variableParameters, it will be left
   // with a sigma of -1.  This indicates something went wrong, ie. presumably it was left 
   // pegged or belongs to a group with a zero norm.
   std::map<int,ModParam*>::const_iterator vp   = XSContainer::fit->variableParameters().begin();
   std::map<int,ModParam*>::const_iterator vpEnd   = XSContainer::fit->variableParameters().end();
   while (vp != vpEnd)
   {
      vp->second->setValue(-1.0,'s');
      ++vp;
   }

   std::map<int,ModParam*>::iterator itPar = m_variableParameters.begin();
   std::map<int,ModParam*>::iterator itParEnd = m_variableParameters.end();
   size_t i=0;
   while (itPar != itParEnd)
   {
      Real var = covar[i*nPar+i];
      if (var < 0.0)
      {
         tcout << "***Warning: LevMarq::invertCorrelationMatrix: "
             << "\n            Negative diagonal element for parameter "
             << itPar->second->index() << std::endl;
      }
      else
      {
         itPar->second->setValue(sqrt(var),'s');
      }
      ++i, ++itPar;
   }
}

void LevMarq::calcStatAndDerivatives (const RealArray& parValues, bool doDerivatives, int& errorFlag, IntegerArray& errorPar)
{
   using namespace XSContainer;
   errorFlag = 0;
   errorPar.clear();

   StatManager* stat = fit->statManager();
   // 'z' set adjusted value without setting recompute flags.
   // 'a' set adjusted value and force recomputation.
   char key('z');
   if (!doDerivatives)
      key = 'a';
   const size_t nPar = parValues.size();
   IntegerArray diffParams(nPar);
   size_t i=0;
   std::map<int,ModParam*>::iterator itPar = m_variableParameters.begin();
   std::map<int,ModParam*>::iterator itParEnd = m_variableParameters.end();
   while (itPar != itParEnd)
   {
      itPar->second->setValue(parValues[i],key);
      diffParams[i] = itPar->first;
      ++itPar, ++i;
   }

   // differentiate statistic if required  
   if (doDerivatives)
   {
      stat->differentiateStats(diffParams);

      // Can't assume stat's beta and alpha arrays are of size 
      // nPar and nPar*nPar respectively, since nPar may be less
      // than fit->variableParameters.size() due to pegged pars
      // and/or zero norms.  However CAN assume that the first
      // nPar and nPar*nPar blocks in beta and alpha correspond to
      // the actual varying parameters in parValues array. 
      // (See how these get filled in StatMethod::analyticDifferentiate.) 

      const RealArray& b = stat->beta();
      const RealArray& a = stat->alpha();
      m_beta.resize(nPar);
      m_alpha.resize(nPar*nPar);
      for (size_t i=0; i<nPar; ++i)
      {
         m_beta[i] = b[i];
         size_t ni = nPar*i;
         for (size_t j=i; j<nPar; ++j)
         {
            size_t nj = nPar*j;
            m_alpha[ni+j] = a[ni+j];
            if (i != j)  m_alpha[nj+i] = a[nj+i];
         }
      }

      std::map<int,ModParam*>::const_iterator itPar = m_variableParameters.begin();
      for (size_t i=0; i<nPar; ++i, ++itPar)
      {
         Real diagElem = a[(nPar+1)*i];
         if (diagElem <= 0.0)
         {
            // Negative diagElem can occur when model-dependent syst error is used.
            errorFlag = -1;
            errorPar.push_back(itPar->first);
            tcout << "***Warning: ";
	    if ( diagElem < 0.0 ) {
	      tcout << "Negative";
	    } else {
	      tcout << "Zero";
	    }
	    tcout << " alpha-matrix diagonal element for parameter " 
		  << itPar->second->getParameterLabel() << std::endl;
         }
      }

   }
   stat->performStats();       

   m_fitStatistic = stat->totalStatistic(); 
}

void LevMarq::reportEigenvectors () const
{
   using namespace std;
   const RealArray& eValue = evalue();
   const RealArray& eVector = evector();
   const size_t nPar = eValue.size();
   ios_base::fmtflags saveFormat(tcout.flags());
   const int savePrecision = tcout.precision();

   const size_t linelen(10+min(10*nPar,static_cast<size_t>(80)));
   const string topBar(linelen,'=');
   const string bottomBar(linelen,'-');
   const size_t MAXSHOW = 40;
   const size_t nShowPar = min(nPar, MAXSHOW);

   tcout << topBar << "\n Variances and Principal Axes" << endl;
   tcout << "         ";
   size_t i=0;
   map<int,ModParam*>::const_iterator itPar = m_variableParameters.begin();
   while (i < nShowPar)
   {
      tcout << right << setw(9) 
                << itPar->second->getParameterLabel();
      ++i, ++itPar;
   }
   string ellsp = (nShowPar == MAXSHOW) ? " ... " : "  ";
   tcout << ellsp << endl;

   i=0;
   while (i < nShowPar)
   {
      tcout << ' ';
      tcout << uppercase << scientific << setprecision(4) << left
          << setw(10) << eValue[i] << "| " << fixed;
      for (size_t j=0; j<nShowPar; ++j)
      {
         tcout << setw(7) << right << showpoint 
            << eVector[j*nPar + i] << "  ";               
      } 
      if (nShowPar < nPar)
         tcout << " ... ";
      tcout << endl;
      ++i;
   }

   tcout << bottomBar << endl;

   tcout.flags(saveFormat);
   tcout.precision(savePrecision);
}

// Additional Declarations
