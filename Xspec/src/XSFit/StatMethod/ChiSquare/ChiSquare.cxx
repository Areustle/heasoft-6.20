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
// ChiSquare
#include <XSFit/StatMethod/ChiSquare/ChiSquare.h>

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


// Class ChiSquare 

ChiSquare::ChiSquare()
  : StatMethod("chi"), 
    m_specsWithZeroVarChans(),
    m_difference(),
    m_variance(),
    m_fullName("Chi-Squared"),
    m_scriptName("\\gx\\u2\\d"),
    m_dSigma()
{
   isDerivAnalytic(true);
}

void ChiSquare::doPerform (Fit* fit)
{

  // alright, now we perform the sum of the statistic over the models.
  Real chiSquare(0.);

  doReset(fit);

  // Check and fix zero-variance bins PRIOR to adding any model
  // systematic variance. But if "model" weighting is in use,
  // we can only check AFTER weighting is applied.
  m_specsWithZeroVarChans.clear();
  std::vector<const SpectralData*>::const_iterator itSpec =
                spectraForStat().begin();
  std::vector<const SpectralData*>::const_iterator itSpecEnd =
                spectraForStat().end();
  if (DataUtility::statWeight().name() != string("model"))
  {
     while (itSpec != itSpecEnd)
     {
        const size_t specNum = (*itSpec)->spectrumNumber();
        RealArray& var  = m_variance[specNum];
        if (applyMinVariance(*itSpec, var))
        {
           m_specsWithZeroVarChans.push_back(specNum);
        }
        ++itSpec;
     }
  }

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
        DataUtility::statWeight().dataVariance(*m,m_variance);
     }
     ++ml;
  }

  // Now do check for case of model weighting.
  if (DataUtility::statWeight().name() == string("model"))
  {
     itSpec = spectraForStat().begin();
     while (itSpec != itSpecEnd)
     {
        const size_t specNum = (*itSpec)->spectrumNumber();
        const RealArray& var = m_variance[specNum];
        const size_t varSz = var.size();
        for (size_t i=0; i<varSz; ++i)
        {
           if (var[i] == 0.0)
           {
              std::ostringstream oss;
              oss << "Cannot calculate chi-square using model weighting on spectrum "<< specNum 
                   <<"\nModel weighting causes it to contain 1 or more channels with zero variance.\n\n";
              statistic(-1.0);
              throw YellowAlert(oss.str());
           } 
        }
        ++itSpec;
     }
  }    

  // Now calculate chi-square    
  tpout << xsverbose(35);
  itSpec = spectraForStat().begin();        
  while (itSpec != itSpecEnd)
  {
     const size_t index = (*itSpec)->spectrumNumber();
     const RealArray& diff = m_difference[index];
     RealArray& var  = m_variance[index];
     RealArray quot  (diff*diff/var);
     if ( tpout.maxChatter() >= 35 )
     {
        tcout << "ChiSquare::doPerform output" <<std::endl;
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

bool ChiSquare::checkWeight (const XSContainer::Weight* weightMethod) const
{
  // ChiSquare is compatible with all of the current Weight methods.
  return true;
}

void ChiSquare::doInitialize (Fit* fit)
{

    // Important: initialize has two definitions, one where allocations 
    // take place (called for each fit) and one per iteration where
    // arrays for variance, difference are reset to their values for
    // a new iteration.
    // SO NEED to ADD new function reset() to do the 

    // deal with the common operations: initialize arrays needed for holding
    // derivatives (if required by fit's FitMethod).

    StatMethod::doInitialize(fit);   

    // now, initialize arrays that are specific to ChiSquare.
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

void ChiSquare::doDeallocate ()
{
  StatMethod::doDeallocate();      

  m_variance.clear();
  m_difference.clear();
  m_dSigma.clear();
}

void ChiSquare::doReset (Fit* fit)
{

  // StatMethod::doReset(fit);        

  // reset quantities specific to ChiSquare object for new iteration.

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
        if (DataUtility::statWeight().name() == string("model"))
           m_variance[index] = RealArray(.0, IN.size());
        else
           m_variance[index] = RealArray(spectrum->variance()[IN]);
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

  ArrayContainer::iterator dsig = m_dSigma.begin();
  ArrayContainer::iterator dsigEnd = m_dSigma.end();
  while (dsig != dsigEnd)
  {
     dsig->second = 0.;
     ++dsig;
  }      
}

void ChiSquare::doReport () const
{
  using namespace std;
  using namespace XSContainer;
  if (!getSpectraForStat().empty())
  {
     ios_base::fmtflags save(tcout.flags());
     streamsize savePrec(tcout.precision());
     const size_t bins = nBinsUsed();
     Real truncatedStat = StatMethod::setOutputFormat(statistic());
     tcout << "Chi-Squared = " << setw(14) << truncatedStat << " using " << bins << " PHA bins." << endl;

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
        tcout <<"\n***Warning: Chi-square may not be valid due to bins with zero variance"
              <<"\n            in spectrum number(s): ";
        for (size_t i=0; i<m_specsWithZeroVarChans.size(); ++i)
           tcout << m_specsWithZeroVarChans[i] <<" ";
        tcout << "\n" << std::endl;
     }
  }
}

Real ChiSquare::plotChi (Real obs, Real mod, Real error, Real areaTime, const Real& NODATA) const
{
  Real point (obs - mod > 0 ? (obs - mod)*(obs - mod) : -(obs - mod)*(obs - mod) );
  Real msyst (models->modelSystematicError());
  Real var   (msyst > 0 ? error*error + msyst*msyst*mod*mod : error*error);
  point /= var;

  return point;       
}

Real ChiSquare::plotDeltaChi (Real obs, Real mod, Real error, Real areaTime, const Real& NODATA) const
{
  Real point ( obs - mod );
  Real msyst (models->modelSystematicError());
  Real var ( msyst > 0 ?  sqrt(error*error + msyst*msyst*mod*mod) : error ); 
  point /= var;
  return point;
}

ArrayContainer ChiSquare::getDifferences (const Fit* fit) const
{
  // seems a waste perhaps to return just a copy of previously calculated array,
  // but not all statistics will have calculated this.
  // the object of providing this virtual function is to support
  // peak residual calculation - may be there are other uses also.
  return m_difference;
}

Real ChiSquare::sumSecondDerivs (const std::map<size_t,ArrayContainer>& dFolded, int pi, int pj)
{

  // If pi or pj is a response parameter, its corresponding
  // dFolded ArrayContainer will have just one spectrum.  Otherwise all 
  // dFolded ArrayContainers should have entries for all fit spectra.
  // Only the subset of spectra that are using chisq should be accessed in here.

  Real alpha_ij=.0;
  ArrayContainer::const_iterator itVar = m_variance.begin();
  ArrayContainer::const_iterator itVarEnd = m_variance.end();
  ArrayContainer::const_iterator sigmaEnd = m_dSigma.end();
  // Calculate (2/var)*(dF/dpi)(dF/dpj) summed over fit spectra using chisq.
  const ArrayContainer& piBins = dFolded.find(pi)->second;
  const ArrayContainer& pjBins = dFolded.find(pj)->second;
  ArrayContainer::const_iterator piEnd = piBins.end();
  ArrayContainer::const_iterator pjEnd = pjBins.end();
  while (itVar != itVarEnd)
  {
     const size_t specNum = itVar->first;
     ArrayContainer::const_iterator piSpec = piBins.find(specNum);
     ArrayContainer::const_iterator pjSpec = pjBins.find(specNum);
     if (piSpec != piEnd && pjSpec != pjEnd)
     {
        RealArray secondDeriv(piSpec->second);
        secondDeriv *= pjSpec->second;
        secondDeriv /= itVar->second;
        ArrayContainer::const_iterator sigmaSpec = m_dSigma.find(specNum);
        if (sigmaSpec != sigmaEnd)
        {
           RealArray sigmaCorrection;
           calcD2Sigma(specNum, sigmaCorrection);
           secondDeriv *= sigmaCorrection;
        } 
        alpha_ij += secondDeriv.sum();
     }
     ++itVar;
  }
  alpha_ij *= 2.0;
  return alpha_ij;
}

Real ChiSquare::sumDerivs (const ArrayContainer& dFolded, int parameterIndex) const
{
    // The m_difference array will contain the quantity
    // d = (D-F), the m_variance array will contain the variance var, 
    // corrected for any model systematic terms. Calculation of m_difference
    // is done in Model::difference(ArrayContainer&) which is invoked by
    // ChiSquare::doPerform

    // The derivative of X^2 wrt each parameter j for each folded model F is then 
    // -2(D-F)(1/var_i +  (D-F)/(sigma_i)^3 [dsigma_i/dF])dF_i/dpj
    // the second term is the 'weight derivative' and is provided by the
    // Weight::varianceDeriv function for each array in the model.

    // dF/dp is independent of StatMethod, and so it is calculated one
    // time in StatManager and passed to StatMethods as an argument.

    // dsigma/dF is also independent of StatMethod, but since Cstat/Lstat
    // do not need the 'weight derivative' at all (due to dStat/dsigma = 0),
    // it is calculated and stored in local getWeightDerivs function.

    // If multiple StatMethods are in use, the spectra contained in
    // m_difference and m_variance will be just a subset of those contained
    // in dFolded (and dsigma/dF if it's used), UNLESS this is a response
    // parameter in which case dFolded contains just one spectrum.

    ArrayContainer::const_iterator itDmEnd = dFolded.end();

    Real dSdpj (0.);
    ArrayContainer::const_iterator itDiff = m_difference.begin();
    ArrayContainer::const_iterator itDiffEnd = m_difference.end();
    tpout << xsverbose(33);         
    while (itDiff != itDiffEnd)
    {
	const size_t iSpec = itDiff->first;
        // Remember, this could be a response par -- only 1
        // spec in dFolded.
        ArrayContainer::const_iterator itDm = dFolded.find(iSpec);
        if (itDm != itDmEnd)
        {
	   RealArray dSdpA (itDiff->second);        
           const RealArray& dm_iSpec = itDm->second;
           dSdpA *= dm_iSpec;
	   dSdpA /= m_variance.find(iSpec)->second;
           ArrayContainer::const_iterator itSig = m_dSigma.find(iSpec);
	   if ( itSig != m_dSigma.end() )
	   {
               const RealArray& dSig_i = itSig->second;
	       dSdpA *= (1. + itDiff->second/m_variance.find(iSpec)->second
		         *dSig_i);
	   }
	   dSdpj += -2.0*dSdpA.sum();
           if ( tpout.maxChatter() >= 33 )
	   {
	       size_t N = itDiff->second.size();
	       std::vector<Real> PS(N);
	       std::partial_sum(&dSdpA[0],&dSdpA[0]+N,PS.begin());
	       tcout << " Spectrum " << iSpec << " Parameter# " << parameterIndex  
                               << " D:  " << dSdpj << '\n';
	       for (size_t j = 0; j < N; ++ j)
	       {
		   tcout << j << " " << std::setw(12) << itDiff->second[j]  << " "
		         << std::setw(12) << m_variance.find(iSpec)->second[j] << " "
		         << std::setw(12) << dm_iSpec[j]  << " "
		         << std::setw(12) << dSdpA[j] << " "
		         << std::setw(12) << PS[j]  << '\n'; 
	       }
	       tcout << " d " << iSpec << "  " << dSdpj << '\n';  
	   }
        }          
	++itDiff;
    } // end spectra loop.
    tpout << xsverbose();

    return dSdpj;
}

void ChiSquare::calcD2Sigma (size_t specNum, RealArray& correction) const
{
  // If variance has a model dependency, calculate contribution
  // to second derivatives.  This assumes dSigma has already
  // been determined to have array corresponding to specNum.
  // Let g = ((D-F)*sigma*d(sigma)/dF)/sigma^2,  then correction
  // = (1 + 4g + 3g^2).
    RealArray g(m_dSigma.find(specNum)->second);
    g *= m_difference.find(specNum)->second;
    g /= m_variance.find(specNum)->second;
    correction.resize(g.size());
    correction = g;
    correction *= g;
    correction *= 3.0;
    correction += 4.0*g;
    correction += 1.0;
}

void ChiSquare::getWeightDerivs ()
{

    // Weight's varianceDeriv function will resize m_dSigma to nSpec
    // arrays where nSpec is ALL fit spectra, not just those using
    // ChiSquare.  Remember this is for calculating dsigma/dF, which is 
    // independent of the StatMethods in use.  It actually fills the 
    // m_dSigma arrays with the quantity SIGMA*dsigma/dF, which makes
    // the calculations a little easier here and in sumDerivs.

    ModelMapConstIter mm (models->modelSet().begin());
    ModelMapConstIter mmEnd (models->modelSet().end());
    while ( mm != mmEnd )
    {
        Model& m (*mm->second);
        if ( m.isActive()) 
           DataUtility::statWeight().varianceDeriv(m,m_dSigma);
	++mm;       
    }
}

std::pair<Real,Real> ChiSquare::adjustForFrozen (const SpectralData* spec, const std::vector<Model*>& models, const std::map<Model*,ArrayContainer >& savedValsForFrozen)
{
    // NOTE:  This ASSUMES m_difference and m_variance have been reset to their
    // initial values in doReset, prior to their usage here.  It is perhaps less
    // dangerous to simply call doReset from here, but that would result in many
    // redundant calls (see how adjustForFrozen is called from StatMethod::renormalize).
   const std::valarray<size_t>& IN = spec->indirectNotice();
   const size_t N = IN.size(); 
   size_t specNum = spec->spectrumNumber();
   std::pair<Real,Real> fSums(0.,0.);
   const RealArray& pha = m_difference[specNum];
   RealArray totalModel(0.,N);
   RealArray totModFroz(.0,N);
   RealArray var(m_variance[specNum]);
   applyMinVariance(spec, var);
   for (size_t i=0; i<models.size(); ++i)
   {
      Model* mod = models[i];
      std::map<Model*,ArrayContainer >::const_iterator itSaved =
                savedValsForFrozen.find(mod);
      if (itSaved != savedValsForFrozen.end())
      {
         totalModel += itSaved->second.find(specNum)->second[IN];
         totModFroz += mod->foldedModel(specNum)[IN];
      }
      else
      {
         totalModel += mod->foldedModel(specNum)[IN];
      }
   }
   RealArray totModNonFroz(totalModel - totModFroz);
   RealArray obsNonFroz((pha - totModFroz)*totModNonFroz);
   fSums.first = (obsNonFroz/var).sum();
   fSums.second = (totModNonFroz*totModNonFroz/var).sum();

   //print frozen and total arrays to log file for debugging purposes.
   tpout << xsverbose(33);         
   if ( tpout.maxChatter() >= 33 )
   {
      using std::setw;
      tcout.precision(6);
      for ( size_t j = 0; j < N ; ++j)
      {
         tcout   <<  " i " << setw(5) << j 
                 << " obs " << setw(12) << pha[j]
                 << " mod " << setw(12) << totalModel[j] ;
            tcout << " fro " << setw(12) << totModFroz[j];
         tcout << '\n';
      }
      tcout << " ofSum " << fSums.first << " mfSum " << fSums.second 
                << std::endl;
    }        
   tpout << xsverbose();

   return fSums;
}

bool ChiSquare::applyMinVariance (const SpectralData* sd, RealArray& variance)
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
           variance[i] = ChiSquare::calcMinVariance(areaTime[i], backAreaTime[i],
                                bscaleRatio[i]);
           isRequired = true;
        }
     }
     return isRequired;
}

Real ChiSquare::calcMinVariance (Real areaTime, Real backAreaTime, Real bscaleRatio)
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
