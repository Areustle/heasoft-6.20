//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Minuit
#include <XSFit/FitMethod/Minuit/Minuit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/Fit.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Signals/SignalHandler.h>
#include <XSstreams.h>
#include <typeinfo>
#include <cstring>

#include "Minuit2/FunctionMinimum.h"
#include "Minuit2/MnUserParameters.h"
#include "Minuit2/MnUserParameterState.h"
#include "Minuit2/MnStrategy.h"
#include "Minuit2/MnMigrad.h"
#include "Minuit2/MnMinimize.h"
#include "Minuit2/MnFumiliMinimize.h"
#include "Minuit2/MnSimplex.h"
#include "Minuit2/MnMinos.h"
#include "Minuit2/MnHesse.h"
#include "Minuit2/MnMachinePrecision.h"

#include "MinuitCalcStat.h"


int svdcmp(double* mat, int m, int n, double* wvec, double* vmat, double* rv1);

Minuit::Minuit(const Minuit &right)
  : FitMethod(right)
{
}

Minuit::Minuit (const string& initString)
        : FitMethod(initString)
{
  firstDerivativeRequired(true);
  secondDerivativeRequired(false);
}


Minuit::~Minuit()
{
}


void Minuit::doPerform (Fit* fit)
{
  // Driver program for Minuit.

  using namespace std;
  using namespace XSContainer;

  // For Minuit2:
  using namespace ROOT::Minuit2;
  
  StatManager* stats = fit->statManager();

  if (isnan(stats->totalStatistic())) {
    throw YellowAlert("Unable to fit when starting with fit statistic = NaN\n");
  }

  if ( !fit->errorCalc() ) {
    tcout << xsverbose(15)
	  << "Number of trials = " << numberOfTrials() << std::endl <<xsverbose();
  }

  // load the Minuit parameters. Have to a bit careful with limits here. Since
  // we are working with the adjusted values this means that if the soft limits
  // and hard limits differ then no limits should be passed to minuit. If the
  // soft limits and hard limits are the same then need to pass a limit to minuit.
  // Write a warning if both lower and upper limits are passed and their 
  // difference exceeds 1e6 because this will likely cause numerical issues.

  size_t nPar(fit->variableParameters().size());
  RealArray savedParVals(0.0, nPar);
  MnUserParameters upar;
  std::map<int,ModParam*>::const_iterator vp = fit->variableParameters().begin();
  std::map<int,ModParam*>::const_iterator vpEnd   = fit->variableParameters().end();
  size_t ivar = 0;
  while (vp != vpEnd) {
    ModParam& current = *(vp->second);
    Real val = current.value('a');
    std::stringstream name;
    name << current.name() << ivar+1;
    upar.Add(name.str().c_str(),val,0.1);
    tcout << xsverbose(15)
	  << "Setting starting value for " << name.str() << " to " 
	  << val << std::endl << xsverbose();
    if ( current.value('l') == current.value('b') ) {
      upar.SetLowerLimit(name.str().c_str(), current.value('l'));
      tcout << xsverbose(15)
	    << "Setting lower limit for " << name.str() << " to " 
	    << current.value('l') << std::endl << xsverbose();
    }
    if ( current.value('h') == current.value('t') ) {
      upar.SetUpperLimit(name.str().c_str(), current.value('h'));
      tcout << xsverbose(15)
	    << "Setting upper limit for " << name.str() << " to " 
	    << current.value('h') << std::endl << xsverbose();
    }
    if ( current.value('l') == current.value('b') && 
	 current.value('h') == current.value('t') && 
	 current.value('h') - current.value('l') > 1e6 ) {
      tcout << "WARNING: Setting MINUIT upper and lower limits which differ by "
	    << current.value('h') - current.value('l') << " which may cause "
	    << "numerical issues. Try setting different hard and soft limits."
	    << std::endl;
    }
    savedParVals[ivar] = val;
    ++ivar;
    ++vp;
  } 

  // Set up precision as single precision since some models are

  upar.SetPrecision(1.0e-6);

  // Set up the object which calculates the statistic within Minuit
  MinuitCalcStat theStat;

  // do the minimization

  string method(selectedSubMethod());
  FunctionMinimum min = doMinuitMinimize(fit, theStat, upar, method);

  // first do an actual Hessian calculation to ensure that the matrix of 
  // second derivatives is fully calculated and update the output parameter state

  MnHesse hesse((unsigned int)2);
  hesse(theStat, min);

  // get the output on the new parameter values

  MnUserParameterState output = min.UserState();

  // now load the new parameter information

  vector<double> values(nPar);
  vector<double> errors(nPar);
  vector<double> globalCC(nPar);
  values = output.Params();
  errors = output.Errors();
  MnGlobalCorrelationCoeff glob = output.GlobalCC();
  globalCC = glob.GlobalCC();
  MnUserCovariance MinuitCovariance = output.Covariance();

  vp = fit->variableParameters().begin();
  ivar = 0;
  while (vp != vpEnd) {
   // No check for frozen is performed - all in the variableParameters array
   // should be unfrozen.     
    ModParam& current = *(vp->second);
    current.setValue(values[ivar], 'a');    
    current.setValue(errors[ivar], 's');
    current.setValue(errors[ivar], 'r');
    current.setValue(globalCC[ivar], 'g');
    ++ivar;
    ++vp;
  }
  stats->performStats();

  // if necessary and possible load the eigenvalues and eigenvectors of the 
  // covariance matrix

  if (!fit->errorCalc() && fit->variableParameters().size() == MinuitCovariance.Nrow()) {
    const size_t nPar = fit->variableParameters().size();
    const size_t nPar2 = nPar*nPar;
    XSutility::auto_array_ptr<double> pCovar(new double[nPar2]);
    double* covar = pCovar.get();
    RealArray& coVariance = covariance();
    coVariance.resize(nPar2);
    for (size_t i=0; i<nPar; ++i) {
      for (size_t j=0; j<nPar; ++j) {
	covar[j*nPar + i] = MinuitCovariance((unsigned int)i,(unsigned int)j);
	coVariance[i*nPar + j] = covar[j*nPar + i];
      }
    }
    XSutility::auto_array_ptr<double> pVmat(new double[nPar2]);
    XSutility::auto_array_ptr<double> pRv1(new double[nPar]);
    XSutility::auto_array_ptr<double> pEvalue(new double[nPar]);
    double* evalue = pEvalue.get();
    double* vmat = pVmat.get();
    double* rv1 = pRv1.get();
    // Note: this call will change covar.
    svdcmp(covar, nPar, nPar, evalue, vmat, rv1);
    fit->fitMethod()->setEvalue(RealArray(evalue, nPar));
    RealArray& eVector = evector();
    eVector.resize(nPar2);
    for (size_t i=0; i<nPar2; ++i) eVector[i] = vmat[i];
    reportEigenvectors(fit);
  }

}

ROOT::Minuit2::FunctionMinimum Minuit::doMinuitMinimize(Fit* fit, MinuitCalcStat& theStat, ROOT::Minuit2::MnUserParameters& upar, const string& method)
{

  using namespace ROOT::Minuit2;

  // If calling code has registered the SIGINT_Handler, this
  // will allow user to break with ctrl-C.  Otherwise, 
  // intHandler = 0 and has no affect.

  const SIGINT_Handler *intHandler = dynamic_cast<const SIGINT_Handler*>
    (SignalHandler::instance()->getHandler(SIGINT));


  // On each iteration run 10*(number of variable parameters) function evals
  // The Minuit internal convergence criterion is EDM < 0.001 * precisionFactor.

  unsigned int fcnevals = 10 * upar.VariableParameters();
  double precisionFactor = 100.0;

  size_t trialsRemaining = numberOfTrials();
  double saveStat(0.0);

  // Different minuit options

  if ( method == "migrad" ) {

    MnMigrad migrad(theStat, upar, 2);

    FunctionMinimum min = migrad(fcnevals, precisionFactor);
    if ( !fit->errorCalc() ) reportProgress(fit, min, false);

    // main iteration loop for migrad

    while ( !isFitDone(fit, trialsRemaining, saveStat, min.Fval()) && min.HasReachedCallLimit() ) {

      // need to reset the precision since this is not being propagated
      // correctly at the moment
      migrad.SetPrecision(min.Seed().Precision().Eps());
      min = migrad(fcnevals, precisionFactor);

      if ( !fit->errorCalc() ) reportProgress(fit, min, false);

      // check for interrupts

      tcout << std::flush;
      if ( intHandler && intHandler->interrupted() ) throw Fit::FitInterrupt();

    }

    return min;

    //  } else if ( method == "fumili") {
    //
    //    MnFumiliMinimize fumili(theStat, upar);
    //    m_min = fumili();

  } else if ( method == "simplex") {

    MnSimplex simplex(theStat, upar, 2);
    precisionFactor = .01;
    simplex.SetPrecision(1e-8);
    FunctionMinimum min = simplex(fcnevals, precisionFactor);
    if ( !fit->errorCalc() ) reportProgress(fit, min, false);

    // main iteration loop for simplex

    while ( !isFitDone(fit, trialsRemaining, saveStat, min.Fval()) && (min.HasReachedCallLimit() || min.IsAboveMaxEdm()) ) {

      // need to reset the precision since this is not being propagated
      // correctly at the moment
      simplex.SetPrecision(min.Seed().Precision().Eps());
      min = simplex(fcnevals, precisionFactor);

      if ( !fit->errorCalc() ) reportProgress(fit, min, false);

      // check for interrupts

      tcout << std::flush;
      if ( intHandler && intHandler->interrupted() ) throw Fit::FitInterrupt();

    }

    return min;

  } else {

    string msg = "Unrecognized method: " + method;
    throw YellowAlert(msg);

  }

}

Minuit* Minuit::clone () const
{

  return new Minuit(*this);
}

bool Minuit::getErrors (Fit* fit, const IntegerArray& paramNums)
{
  // For Minuit2:
  using namespace ROOT::Minuit2;

  // use MINOS to find errors for requested delta statistic
  // first need to get the FunctionMinimum object. also save
  // the current parameter values so we can restore them at
  // the end

  MinuitCalcStat theStat;

  size_t nPar(fit->variableParameters().size());
  RealArray savedParVals(0.0, nPar);
  MnUserParameters upar;
  size_t ivar = 0;
  std::map<int,ModParam*>::const_iterator vp = fit->variableParameters().begin();
  std::map<int,ModParam*>::const_iterator vpEnd   = fit->variableParameters().end();
  while (vp != vpEnd) {
    ModParam& current = *(vp->second);
    Real val = current.value('a');
    std::stringstream name;
    name << current.name() << ivar+1;
    upar.Add(name.str().c_str(),val,0.1);
    if ( current.value('l') == current.value('b') ) {
      upar.SetLowerLimit(name.str().c_str(), current.value('l'));
    }
    if ( current.value('h') == current.value('t') ) {
      upar.SetUpperLimit(name.str().c_str(), current.value('h'));
    }
    savedParVals[ivar] = val;
    ivar++;
    ++vp;
  } 

  // Set up precision as single precision since some models are
  // and do an initial fit to set up the min object required by minos.

  upar.SetPrecision(1.0e-6);

  string method(selectedSubMethod());
  FunctionMinimum min = doMinuitMinimize(fit, theStat, upar, method);
  MnHesse hesse((unsigned int)2);
  hesse(theStat, min);

  // if fit has failed then cannot run Minos
  if ( !min.IsValid() ) {
    tcout << "Set-up minimization failed so Minos cannot be run." << std::endl;
    return false;
  }

  // Use high value of strategy (ie careful at cost of more evaluations)

  MnMinos minos(theStat, min, (unsigned int)2);

  theStat.setErrorDef(Fit::deltaStat());

  for (size_t i=0; i<paramNums.size(); i++) {
    // No checking performed here to see if parameter with key =
    // paramNums[i] exists in map.  This should already have been
    // accomplished by the checking performed in the Fit::getErrors
    // wrapper.
    std::map<int,ModParam*>::const_iterator vp 
     		= fit->variableParameters().find(paramNums[i]);
    ModParam& current = *(vp->second);
    std::map<int,ModParam*>::difference_type dist=0;
#ifndef STD_COUNT_DEFECT
    dist = distance(fit->variableParameters().begin(),vp);
#else
    distance(fit->variableParameters().begin(),vp, dist);
#endif     
    size_t ivar = static_cast<size_t>(dist);
    std::pair<double,double> error;
    error = minos(ivar);
    Real val = min.UserState().Value(ivar);
    current.setValue(val+error.second, 'p');
    current.setValue(val+error.first, 'm');
    current.setValue(val,'a');
    reportMinos(current);
  }

  // restore state

  ivar=0;
  vp = fit->variableParameters().begin();
  while (vp != vpEnd) {
    vp->second->setValue(savedParVals[ivar],'a');
    ivar++;
    ++vp;
  }
  fit->statManager()->performStats();


  return true;
}

string Minuit::settingString () const
{
  return string();
}

string Minuit::fullName () const
{
  return string("Minuit ") + selectedSubMethod();
}

void Minuit::reportMinos (const ModParam& param) const
{
  using namespace std;

  ios_base::fmtflags saveFlags(tcout.flags());
  tcout.precision(6);
  tcout << setw(6) << param.index() << setw(13) << param.value('m') 
        << setw(13) << param.value('p') << "   (" << param.value('m') -
        param.value('v') << "," << param.value('p') - param.value('v')
        << ")" << endl;
  tcout.flags(saveFlags);
}

// this option is no longer valid and does not do anything

void Minuit::improve (Fit* fit)
{
  string msg = "The improve command is no longer supported by Minuit.\n";
  throw YellowAlert(msg);
}

void Minuit::reportProgress(Fit* fit, ROOT::Minuit2::FunctionMinimum& min, const bool& first)
{
  using namespace std;

  unsigned int nPar = min.UserParameters().VariableParameters();

  // if first time round then write the header

  if ( first ) {
    tcout << setw(42) << "Parameters" << endl;
    tcout << setw(13) << left << "Statistic" << setw(12) << left << "EDM";
    for (unsigned int i=0; i<nPar; i++) {
      tcout << setw(14) << right << min.UserParameters().Parameter(i).GetName();
    }
    tcout << endl;
  }

  tcout << setw(13) << left << min.Fval() << setw(12) << left
	<< min.Edm() << right << showpoint;

  std::map<int,ModParam*>::const_iterator vp = fit->variableParameters().begin();
  std::map<int,ModParam*>::const_iterator vpEnd   = fit->variableParameters().end();
  while (vp != vpEnd) {
    ModParam& current = *(vp->second);
    tcout << setw(14) << current.value('v');
    vp++;
  }
  tcout << endl;

}

void Minuit::reportEigenvectors (Fit* fit) const
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
   map<int,ModParam*>::const_iterator itPar = fit->variableParameters().begin();
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

bool Minuit::isFitDone(Fit* fit, size_t& trialsRemaining, double& saveStat, double currentStat) 
{

  bool done(false);

  // decrement the number of trials remaining

  trialsRemaining--;

  // this is a trap for something going wrong and minuit not thinking it has
  // converged. it avoids a potential infinite loop if the user has querying off.
  if ( fabs(saveStat-currentStat) < 1.0e-6 ) done = true;
  saveStat = currentStat;

  // if not then check whether we have run enough trials and if so ask the 
  // user whether they want to continue

  if ( trialsRemaining == 0 && !done ) {
    if ( fit->queryMode() == Fit::ON ) {
      int reply = XSutility::yesToQuestion("Number of trials exceeded: continue fitting ?", 1, tcin);
      if ( reply == 1 )
	trialsRemaining = numberOfTrials();
      else if ( reply == -1 )
	throw Fit::FitInterrupt();
      else
	done = true;
    } else {
      if ( fit->queryMode() == Fit::YES ) {
	trialsRemaining = numberOfTrials();
	done = false;
      } else {
	done = true;
      }
    }
  }

  return done;

}

unsigned int Minuit::numberFunctionEvals()
{
  return m_FunctionEvals;
}

void Minuit::initializeNumberFunctionEvals()
{
  m_FunctionEvals = 0;
}

void Minuit::incrementNumberFunctionEvals(const unsigned int& number)
{
  m_FunctionEvals += number;
}



// Additional Declarations
