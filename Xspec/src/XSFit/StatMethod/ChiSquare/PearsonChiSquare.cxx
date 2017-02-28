//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

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
// PearsonChiSquare
#include <XSFit/StatMethod/ChiSquare/PearsonChiSquare.h>

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


// Class PearsonChiSquare 

PearsonChiSquare::PearsonChiSquare()
  : StatMethod("pchi"), 
    m_specsWithZeroVarChans(),
    m_difference(),
    m_variance(),
    m_fullName("Pearson Chi-Squared"),
    m_scriptName("\\gx\\u2\\d\\dPearson\\u")
{
   isDerivAnalytic(true);
}

void PearsonChiSquare::doPerform (Fit* fit)
{

  // alright, now we perform the sum of the statistic over the models.
  Real chiSquare(0.);

  doReset(fit);

  m_specsWithZeroVarChans.clear();

  ModelMapConstIter ml (models->modelSet().begin());      
  ModelMapConstIter  mlEnd (models->modelSet().end());      
  while ( ml != mlEnd )
  {
     // D - F for each model. In the case where there are multiple
     // sources difference will be iteratively defined. The variance
     // is simply the folded model in this case.
     Model* m (ml->second);
     if (m->isActive())
     {
        m->difference(m_difference);

	// code for the folded model pulled out of Model::difference but without the size
	// checking since that will have been done by the m->difference call immediately above.

	ArrayContainer::iterator itData = m_variance.begin();
	ArrayContainer::iterator itDataEnd = m_variance.end();
	ArrayContainer::const_iterator itFluxEnd = m->modelFlux().end();
	while (itData != itDataEnd)
	{
	  const size_t specNum = itData->first;
	  RealArray& var = itData->second;
	  ArrayContainer::const_iterator itFlux = m->modelFlux().find(specNum);
	  if (itFlux != itFluxEnd)
	  {
	    const SpectralData* obs = m->spectrum(specNum);
	    const std::valarray<size_t>& IN = obs->indirectNotice();
	    var = m->foldedModel(specNum)[IN];

	    // if there are model values which are zero set them to a minimum of 1 count
	    // then save that information
	    if (applyMinVariance(obs, var)) m_specsWithZeroVarChans.push_back(specNum);
	  }
	  ++itData;
	}
     }
     ++ml;
  }

  // Now calculate chi-square    
  tpout << xsverbose(35);
  std::vector<const SpectralData*>::const_iterator itSpec =
                spectraForStat().begin();
  std::vector<const SpectralData*>::const_iterator itSpecEnd =
                spectraForStat().end();
  while (itSpec != itSpecEnd)
  {
     const size_t index = (*itSpec)->spectrumNumber();
     const RealArray& diff = m_difference[index];
     RealArray& var  = m_variance[index];
     RealArray quot  (diff*diff/var);

     // Pearson chi-square requires that we are operating in counts so need to 
     // multiply by area and exposure time.

     Real exposure = (*itSpec)->exposureTime();
     const RealArray& area = (*itSpec)->areaScale();
     for (size_t i=0; i<quot.size(); i++) quot[i] *= exposure*area[i];

     if ( tpout.maxChatter() >= 35 )
     {
        tcout << "PearsonChiSquare::doPerform output" <<std::endl;
        tcout << " i data  diff  var \n" ;
        size_t N (diff.size());
	std::vector<Real> PS(N);
	std::partial_sum(&quot[0],&quot[0]+N,PS.begin());

        for (size_t i = 0; i < N; ++i)
        {
           tcout << i << "  " << (*itSpec)->spectrum()[i] 
                 << " " << diff[i]
                 << " " << var[i] << " "  << quot[i]
                 << " " << PS[i]  << '\n';
        }
        tcout << std::endl;

     }
     chiSquare += quot.sum();             
     ++itSpec;       
  } 
  tpout << xsverbose();
  // output section, to be encapsulated.

  statistic(chiSquare);
}

bool PearsonChiSquare::checkWeight (const XSContainer::Weight* weightMethod) const
{
  // PearsonChiSquare is compatible with all of the current Weight methods.
  return true;
}

void PearsonChiSquare::doInitialize (Fit* fit)
{

    // Important: initialize has two definitions, one where allocations 
    // take place (called for each fit) and one per iteration where
    // arrays for variance, difference are reset to their values for
    // a new iteration.
    // SO NEED to ADD new function reset() to do the 

    // deal with the common operations: initialize arrays needed for holding
    // derivatives (if required by fit's FitMethod).

    StatMethod::doInitialize(fit);   

    // now, initialize arrays that are specific to PearsonChiSquare.
    // each is an array of arrays of size # of noticed channels,
    // one per spectrum loaded. 

    // for every weighting technique except model weighting, the variance
    // is constant (independent of the model) apart from a possible systematic
    // term we can add later. If the weighting is Model, then the data variance
    // is given by the sum of the model weight and the background variance. 
    // so here we need to store the background variance in the variance array.
    // If model systematic errors or model errors are called for these will be
    // added during iteration by the Weight::dataVariance() function.
    ArrayContainer::const_iterator pw (protoWorkspace().begin());
    ArrayContainer::const_iterator pwEnd  (protoWorkspace().end());




    while ( pw != pwEnd )
    {
          m_variance.insert(ArrayContainer::value_type(pw->first,pw->second));
          m_difference.insert(ArrayContainer::value_type(pw->first,pw->second));
          ++pw;
    }

    doReset(fit);
}

void PearsonChiSquare::doDeallocate ()
{
  StatMethod::doDeallocate();      

  m_variance.clear();
  m_difference.clear();
}

void PearsonChiSquare::doReset (Fit* fit)
{

  // StatMethod::doReset(fit);        

  // reset quantities specific to PearsonChiSquare object for new iteration.

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
	m_variance[index] = RealArray(.0, IN.size());
        if (spectrum->background())
        {
           m_difference[index] -= 
                           spectrum->background()->spectrum()[IN];
           if (spectrum->background()->variance().size() > 0)
           {
                   m_variance[index] += 
                           spectrum->background()->variance()[IN];
           }

        }
        if (spectrum->correction() && spectrum->correctionScale() != 0) 
        {
                m_difference[index] -=  spectrum->correctionScale()
                        *spectrum->correction()->spectrum()[IN];         
        }     
        ++itSpec;
  }

}

void PearsonChiSquare::doReport () const
{
  using namespace std;
  using namespace XSContainer;
  if (!getSpectraForStat().empty())
  {
     ios_base::fmtflags save(tcout.flags());
     streamsize savePrec(tcout.precision());
     const size_t bins = nBinsUsed();
     Real truncatedStat = StatMethod::setOutputFormat(statistic());
     tcout << "Pearson Chi-Squared = " << setw(14) << truncatedStat << " using " << bins << " PHA bins." << endl;

     // Print reduced chi-sq and null hypothesis probability only
     // if chi-squared is the only test stat in use.
     const StatMethod* singleStat = fit->statManager()->usingSingleTestStat();
     if (singleStat == this)
     {
        std::pair<int,size_t> dofVals = fit->statManager()->totalDegreesOfFreedom();
        Numerics::GammaQ GQ;
        Real nullHyp = GQ( dofVals.first/2. , statistic()/2.);
        if (nullHyp == -99)
        {
           // Something went wrong in GammaQ.
           tcout << " Cannot calculate null hypothesis probability."<<endl;
        }
        else
        {
           truncatedStat = StatMethod::setOutputFormat(statistic(), dofVals.first);
           tcout << " Reduced chi-squared = " << setw(14) << truncatedStat << " for " 
                 << setw(6) << dofVals.first << " degrees of freedom " << setprecision(6)
                 << "\n Null hypothesis probability = " << setw(14) << scientific << nullHyp << endl;  
        }
     }
     tcout.precision(savePrec);
     tcout.flags(save);
     if (m_specsWithZeroVarChans.size())
     {
        tcout <<"\n***Warning: Pearson Chi-square may not be valid due to bins with zero model value"
              <<"\n            in spectrum number(s): ";
        for (size_t i=0; i<m_specsWithZeroVarChans.size(); ++i)
           tcout << m_specsWithZeroVarChans[i] <<" ";
        tcout << "\n" << std::endl;
     }
  }
}

// plotChi is included because it is required by StatMethod but
// is not used because PearsonChiSquare is a test statistic not a fit statistic

Real PearsonChiSquare::plotChi (Real obs, Real mod, Real error, Real areaTime, const Real& NODATA) const
{
  return NODATA;       
}

// plotDeltaChi is included because it is required by StatMethod but
// is not used because PearsonChiSquare is a test statistic not a fit statistic

Real PearsonChiSquare::plotDeltaChi (Real obs, Real mod, Real error, Real areaTime, const Real& NODATA) const
{
  return NODATA;
}

ArrayContainer PearsonChiSquare::getDifferences (const Fit* fit) const
{
  // seems a waste perhaps to return just a copy of previously calculated array,
  // but not all statistics will have calculated this.
  // the object of providing this virtual function is to support
  // peak residual calculation - may be there are other uses also.
  return m_difference;
}

// sumSecondDerivs is included because it is required by StatMethod but
// is not used because PearsonChiSquare is a test statistic not a fit statistic

Real PearsonChiSquare::sumSecondDerivs (const std::map<size_t,ArrayContainer>& dFolded, int pi, int pj)
{
  return 0.0;
}

// sumDerivs is included because it is required by StatMethod but
// is not used because PearsonChiSquare is a test statistic not a fit statistic

Real PearsonChiSquare::sumDerivs (const ArrayContainer& dFolded, int parameterIndex) const
{
  return 0.0;
}

// adjustForFrozen is included because it is required by StatMethod but
// is not used because PearsonChiSquare is a test statistic not a fit statistic

std::pair<Real,Real> PearsonChiSquare::adjustForFrozen (const SpectralData* spec, const std::vector<Model*>& models, const std::map<Model*,ArrayContainer >& savedValsForFrozen)
{
   std::pair<Real,Real> fSums(0.,0.);
   return fSums;
}

bool PearsonChiSquare::applyMinVariance (const SpectralData* sd, RealArray& variance)
{
     bool isRequired = false;
     const std::valarray<size_t>& IN = sd->indirectNotice();
     RealArray areaTime = sd->exposureTime()*sd->areaScale()[IN];
     const size_t sz = variance.size();
     RealArray backAreaTime(-1.0, areaTime.size());
     RealArray bscaleRatio(1.0, areaTime.size());
     if (sd->background())
     {
        const SpectralData* bckSpec = sd->background()->data();
        backAreaTime = bckSpec->exposureTime()*bckSpec->areaScale()[IN];
        bscaleRatio = sd->backgroundScale()[IN]/bckSpec->backgroundScale()[IN];
     }
     for (size_t i=0; i<sz; ++i)
     {
        if (variance[i] == 0.0)
        {
           variance[i] = PearsonChiSquare::calcMinVariance(areaTime[i], backAreaTime[i],
                                bscaleRatio[i]);
           isRequired = true;
        }
     }
     return isRequired;
}

Real PearsonChiSquare::calcMinVariance (Real areaTime, Real backAreaTime, Real bscaleRatio)
{
   Real minVar = .0;
   // Use negative backAreaTime as a flag indicating no background file.
   if (backAreaTime < .0)
   {
      minVar = 1.0/(areaTime*areaTime);
   }
   else
   {
      Real factor = 1.0/(areaTime*areaTime);
      Real backFactor = (bscaleRatio*bscaleRatio)/(backAreaTime*backAreaTime);
      minVar = std::min(factor, backFactor);
   }
   return minVar;
}

// Additional Declarations
