// Methods for Runs statistic class

// Fit
#include <XSFit/Fit/Fit.h>
// Background
#include <XSModel/Data/BackCorr/Background.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// Weight
#include <XSModel/GlobalContainer/Weight.h>
// Model
#include <XSModel/Model/Model.h>
// Runs
#include <XSFit/StatMethod/Runs/Runs.h>

#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatManager.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/DataSetTypes.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Numerics/Gamma.h>
#include <XSUtil/Error/Error.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <XSsymbol.h>
#include <iostream>
#include <iomanip>
#include <typeinfo>
#include <set>
#include <numeric>
#include <sstream>
#include <cmath>
using namespace XSContainer;


// Class Runs 

Runs::Runs()
  : StatMethod("runs"), 
    m_difference(),
    m_fullName("Runs"),
    m_scriptName("Runs")
{
   isDerivAnalytic(true);
}

void Runs::doPerform (Fit* fit)
{

  // alright, now we perform the sum of the statistic over the models.

  doReset(fit);

  // calculate the data - model

  ModelMapConstIter ml (models->modelSet().begin());      
  ModelMapConstIter  mlEnd (models->modelSet().end());      
  while ( ml != mlEnd )
  {
     // D - F for each model. In the case where there are multiple
     // sources difference will be iteratively defined.
     Model* m (ml->second);
     if (m->isActive())
     {
        m->difference(m_difference);                  
     }
     ++ml;
  }

  // Now calculate runs statistic
  int runs(0);
  int numberPos(0);
  int numberNeg(0);
  bool positiveRun(false);
  bool first(true);
  
  tpout << xsverbose(35);
  if ( tpout.maxChatter() >= 35 ) {
    tcout << "Runs::doPerform output" <<std::endl;
    tcout << " spec# channel diff npos nneg nruns" <<std::endl; 
  }

  std::vector<const SpectralData*>::const_iterator itSpec =
                spectraForStat().begin();
  std::vector<const SpectralData*>::const_iterator itSpecEnd =
                spectraForStat().end();
  while (itSpec != itSpecEnd)
  {
     const size_t index = (*itSpec)->spectrumNumber();
     const RealArray& diff = m_difference[index];
     for (size_t i=0; i<diff.size(); i++) {

       if ( diff[i] > 0.0 ) {
	 numberPos++;
       } else {
	 numberNeg++;
       }

       if ( first ) {
	 runs++;
	 positiveRun = false;
	 if ( diff[i] > 0.0 ) positiveRun = true;
	 first = false;
       }

       if ( positiveRun && diff[i] <= 0.0 ) {
	 runs++;
	 positiveRun = false;
       }
       if ( !positiveRun && diff[i] > 0.0 ) {
	 runs++;
	 positiveRun = true;
       }

       if ( tpout.maxChatter() >= 35 ) {
	 tcout << index << " " << i << " " << diff[i] << " " << numberPos << " "
	       << numberNeg << " " << runs << std::endl;
       }

     }
     ++itSpec;       
  } 
  tpout << xsverbose();
  // output section, to be encapsulated.

  // calculate the Runs statistic

  Real numberTot = numberPos + numberNeg;
  Real multPosNeg = numberPos * numberNeg;
  Real expectedRuns = 2.0*multPosNeg/(numberTot) + 1.0;
  Real varianceRuns = 2.0*multPosNeg*(2.0*multPosNeg-numberPos-numberNeg)/numberTot/numberTot/(numberTot-1.0);

  Real RunsStat = (runs - expectedRuns)/sqrt(varianceRuns);

  statistic(RunsStat);
}

bool Runs::checkWeight (const XSContainer::Weight* weightMethod) const
{
  // Runs is compatible with all of the current Weight methods.
  return true;
}

void Runs::doInitialize (Fit* fit)
{

    // Important: initialize has two definitions, one where allocations 
    // take place (called for each fit) and one per iteration where
    // arrays for variance, difference are reset to their values for
    // a new iteration.
    // SO NEED to ADD new function reset() to do the 

    // deal with the common operations: initialize arrays needed for holding
    // derivatives (if required by fit's FitMethod).

    StatMethod::doInitialize(fit);   

    // now, initialize arrays that are specific to Runs.
    // each is an array of arrays of size # of noticed channels,
    // one per spectrum loaded. 

    ArrayContainer::const_iterator pw (protoWorkspace().begin());
    ArrayContainer::const_iterator pwEnd  (protoWorkspace().end());

    while ( pw != pwEnd )
    {
          m_difference.insert(ArrayContainer::value_type(pw->first,pw->second));
          ++pw;
    }

    doReset(fit);
}

void Runs::doDeallocate ()
{
  StatMethod::doDeallocate();      

  m_difference.clear();
}

void Runs::doReset (Fit* fit)
{

  // StatMethod::doReset(fit);        

  // reset quantities specific to Runs object for new iteration.

  std::vector<const SpectralData*>::const_iterator itSpec =
                spectraForStat().begin();
  std::vector<const SpectralData*>::const_iterator itSpecEnd =
                spectraForStat().end();
  while (itSpec != itSpecEnd)
  {
        const SpectralData* spectrum = *itSpec;
        const size_t index = spectrum->spectrumNumber();
        const std::valarray<size_t>& IN = spectrum->indirectNotice();
        // Subtle note: Because we are retrieving a subset from
        // a non-const valarray, if we don't do the static cast
        // it will remain as type indirect_array<Real>.  For some
        // reason on Solaris, this then causes a memory leak when 
        // assigned to m_difference[index].  The cast seems to get
        // rid of this.  
        m_difference[index] = RealArray(spectrum->spectrum()[IN]);
        if (spectrum->background())
        {
           m_difference[index] -= 
	     spectrum->background()->spectrum()[IN];
        }
        if (spectrum->correction() && spectrum->correctionScale() != 0) 
        {
	  m_difference[index] -=  spectrum->correctionScale()
	    *spectrum->correction()->spectrum()[IN];         
        }     
        ++itSpec;
  }

}

void Runs::doReport () const
{
  using namespace std;
  using namespace XSContainer;
  if (!getSpectraForStat().empty())
  {
     ios_base::fmtflags save(tcout.flags());
     streamsize savePrec(tcout.precision());
     const size_t bins = nBinsUsed();
     Real truncatedStat = StatMethod::setOutputFormat(statistic());
     tcout << "Runs statistic = " << setw(14) << truncatedStat << " using " << bins << " PHA bins." << endl;

     //     // Print reduced chi-sq and null hypothesis probability only
     //     // if chi-squared is the only test stat in use.
     //     const StatMethod* singleStat = fit->statManager()->usingSingleTestStat();
     //     if (singleStat == this)
     //     {
     //        std::pair<int,size_t> dofVals = fit->statManager()->totalDegreesOfFreedom();
     //        Numerics::GammaQ GQ;
     //        Real nullHyp = GQ( dofVals.first/2. , statistic()/2.);
     //        if (nullHyp == -99)
     //        {
     //           // Something went wrong in GammaQ.
     //           tcout << " Cannot calculate null hypothesis probability."<<endl;
     //        }
     //        else
     //        {
     //           truncatedStat = StatMethod::setOutputFormat(statistic(), dofVals.first);
     //           tcout << " Reduced chi-squared = " << setw(14) << truncatedStat << " for " 
     //                 << setw(6) << dofVals.first << " degrees of freedom " << setprecision(6)
     //                 << "\n Null hypothesis probability = " << setw(14) << scientific << nullHyp << endl;  
     //        }
     //     }
     tcout.precision(savePrec);
     tcout.flags(save);

  }
}

// plotChi is included because it is required by StatMethod but
// is not used because Runs is a test statistic not a fit statistic

Real Runs::plotChi (Real obs, Real mod, Real error, Real areaTime, const Real& NODATA) const
{
  return NODATA;       
}

// plotDeltaChi is included because it is required by StatMethod but
// is not used because Runs is a test statistic not a fit statistic

Real Runs::plotDeltaChi (Real obs, Real mod, Real error, Real areaTime, const Real& NODATA) const
{
  return NODATA;
}

ArrayContainer Runs::getDifferences (const Fit* fit) const
{
  // seems a waste perhaps to return just a copy of previously calculated array,
  // but not all statistics will have calculated this.
  // the object of providing this virtual function is to support
  // peak residual calculation - may be there are other uses also.
  return m_difference;
}

// sumSecondDerivs is included because it is required by StatMethod but
// is not used because Runs is a test statistic not a fit statistic

Real Runs::sumSecondDerivs (const std::map<size_t,ArrayContainer>& dFolded, int pi, int pj)
{
  return 0.0;
}

// sumDerivs is included because it is required by StatMethod but
// is not used because Runs is a test statistic not a fit statistic

Real Runs::sumDerivs (const ArrayContainer& dFolded, int parameterIndex) const
{
    return 0.0;
}

// adjustForFrozen is included because it is required by StatMethod but
// is not used because Runs is a test statistic not a fit statistic

std::pair<Real,Real> Runs::adjustForFrozen (const SpectralData* spec, const std::vector<Model*>& models, const std::map<Model*,ArrayContainer >& savedValsForFrozen)
{
   std::pair<Real,Real> fSums(0.,0.);
   return fSums;
}

// Additional Declarations
