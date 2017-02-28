//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSFit/MCMC/Chain.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Numerics/ModularCounter.h>
#include <XSstreams.h>
#include <sstream>

// ChainManager
#include <XSFit/MCMC/ChainManager.h>
// MarginGrid
#include <XSFit/MCMC/MarginGrid.h>
#include <XSFit/MCMC/IntegProbGrid.h>



// Class MarginGrid::MarginInfo 

// Class MarginGrid 

MarginGrid::MarginGrid (ChainManager* chainManager)
  : Grid(),
    m_isSimple(false),
    m_marginInfo(),
    m_chainManager(chainManager),
    m_paramIDs(),
    m_fraction(),
    m_integProbGrid(0)
    
{
}


MarginGrid::~MarginGrid()
{
   delete m_integProbGrid;
}


void MarginGrid::doGrid ()
{
   setMarginInfo();
   ProbabilityHist histogram(m_marginInfo.minRanges, m_marginInfo.maxRanges,
                 m_marginInfo.nSteps, m_marginInfo.isLog);
   const ChainManager::ChainContainer& chains = m_chainManager->chains();
   if (!chains.size())
   {
      throw Chain::ChainError("Cannot bin probabilities, no chains currently loaded.");
   }
   const SpecContainer& paramSpecs = getParameter();
   const size_t nPars = paramSpecs.size();
   if (nPars != histogram.nDim())
   {
      throw RedAlert("nParams and Histogram dimensions mismatch.");
   }
   // Bundle user-entered parameter specs into form that
   // Chain can deal with:
   m_paramIDs.resize(nPars);
   for (size_t i=0; i<nPars; ++i)
   {
      string modName = paramSpecs[i]->name;
      if (modName.empty())  modName = Model::DEFAULT();
      m_paramIDs[i].modName = modName;
      m_paramIDs[i].index = paramSpecs[i]->parIndex;
      // ParamID.parName and units are not needed for lookup
      // in Chain.  If Chain finds par based on modName and index,
      // it will fill in the rest of m_paramIDs[i] info.
   }

   ChainManager::ChainContainer::const_iterator itChain = chains.begin();
   ChainManager::ChainContainer::const_iterator itEnd = chains.end();
   while (itChain != itEnd)
   {
      std::vector<size_t> found;
      const Chain* chain = itChain->second;
      // this function fills in rest of paramID struct for
      // each requested parameter it finds in Chain.
      chain->findParsInChain(m_paramIDs, found);
      if (found.size() != m_paramIDs.size())
      {
         string msg("Unable to locate all requested parameters in chain file ");
         msg += chain->getFileName();
         throw Chain::ChainError(msg);
      }
      // Check for simple case of requested params
      // matching up 1 to 1 and in order with those 
      // stored in chain file, and treat this as special case
      // which won't need time-consuming makePoint procedure.
      m_isSimple = true;
      if (found.size() == chain->paramIDs().size())
      {
         for (size_t j=0; j<found.size(); ++j)
         {
            if (found[j] != j)
            {
               m_isSimple = false;
               break;
            }
         }
      }
      else
      {
         m_isSimple = false;
      } 
      try
      { 
         chain->openForReadPoints();
         size_t nPoints = chain->length();
         if (m_isSimple)
         {
            std::vector<Real> point;
            for (size_t i=0; i<nPoints; ++i)
            {
               chain->readPoint(point);
               // remove ChiSquare value from point array
               point.pop_back();
               histogram.addToHist(point);
            }
         }
         else
         {
            std::vector<Real> point;
            for (size_t i=0; i<nPoints; ++i)
            {
               chain->readPoint(point);
               makePoint(found, point);
               histogram.addToHist(point);
            }
         }
      }
      catch (...)
      {
         chain->closeFile();
         throw;
      }
      chain->closeFile();
      ++itChain;
   } // end chain loop
   // Convert histogram counts to probabilities.
   long allPoints = histogram.totalAttempts();
   if (!allPoints)
   {
      throw Chain::ChainError("Cannot calculate probabilities, no points found in chains.");
   }
   // NOTE: Up to this point, both Grid::m_grid and ParameterSpec::
   //  parameterValues have been sized by the number of BOUNDARY
   // values.  Since this is a histogram though, we ultimately are
   // concerned with the CENTERS of the bins.  The following function
   // call therefore resizes these arrays.   
   calcBinCenters();
   RealArray& probabilityDensities = grid();
   RealArray& fractions = fraction();
   const std::valarray<long>& counts = histogram.counts();
   size_t gridSz = probabilityDensities.size();
   if (gridSz != counts.size())
   {
      throw RedAlert("Margin grid - histogram size mismatch");
   }

   for (size_t i=0; i<gridSz; ++i) probabilityDensities[i] = static_cast<Real>(counts[i]);
   probabilityDensities /= allPoints;
   fractions.resize(probabilityDensities.size());
   fractions = probabilityDensities;

   // Divide probabilityDensities by the bin sizes for all parameters in order
   // to turn them from fractions into PDFs.

   Numerics::ModularCounter idxCounter(m_marginInfo.nSteps);
   const IntegerArray& currCounts = idxCounter.counter();

   RealArray delta(nPars);
   for (size_t i=0; i<nPars; i++) {
     if ( m_marginInfo.isLog[i] ) {
       delta[i] = (log(m_marginInfo.maxRanges[i])-log(m_marginInfo.minRanges[i]))/static_cast<Real>(m_marginInfo.nSteps[i]);
     } else {
       delta[i] = (m_marginInfo.maxRanges[i]-m_marginInfo.minRanges[i])/static_cast<Real>(m_marginInfo.nSteps[i]);
     }
   }

   Real pdfMax = 0.0;
   for (size_t i=0; i<gridSz; ++i)
   {
      for (size_t j=0; j<nPars; j++) {
	size_t ip = currCounts[j];
	Real paramStepSize;
	if ( m_marginInfo.isLog[j] ) {
	  paramStepSize = m_marginInfo.minRanges[j]*(exp((ip+1)*delta[j])-exp(ip*delta[j]));
	} else {
	  paramStepSize = delta[j];
	}

	probabilityDensities[i] /= paramStepSize;

      }
      // save the position of the maximum in the grid so we can plot it using
      // the cross-hair
      if ( probabilityDensities[i] > pdfMax ) {
	pdfMax = probabilityDensities[i];
	for (size_t j=0; j<nPars; j++) {
	  size_t ip = currCounts[j];
	  paramSpecs[j]->value = paramSpecs[j]->parameterValues[ip];
	}
      }

      idxCounter++;
   }

}

void MarginGrid::report (bool title) const
{
   using namespace std;
   const RealArray& probdens = getGridValues();
   const RealArray& fractions = getFractionValues();
   const size_t probSz = probdens.size();
   if (probSz)
   {
      ios_base::fmtflags saveFlags(tcout.flags());
      streamsize savePrecision = tcout.precision();
      size_t nPars = m_marginInfo.minRanges.size();
      const int precision = 6;
      const int fwidth = precision + 8;
      tcout.precision(precision);  
      tcout << showpoint; 
      tcout << right << setw(fwidth) << "Prob. density" 
	    << setw(fwidth) << "Fraction" << right;
      for (size_t i=0; i<nPars; ++i)
      {
         tcout << setw(fwidth) << "Mod param";
      }
      tcout << endl << setw(fwidth) << " " << setw(fwidth) << " ";
      const SpecContainer& paramSpecs = getParameter();
      for (size_t i=0; i<nPars; ++i)
      {
         ostringstream tmpSS;
         if (paramSpecs[i]->name.length())
         {
            tmpSS << paramSpecs[i]->name << ":";
         }
         tmpSS << paramSpecs[i]->parIndex;
         tcout << setw(fwidth) << tmpSS.str();
      }
      tcout << endl;
      Numerics::ModularCounter idxCounter(m_marginInfo.nSteps);
      const IntegerArray& currCounts = idxCounter.counter();
      ios_base& (*formatType) (ios_base& ) = fixed;
      for (size_t i=0; i<probSz; ++i)
      {
         formatType = (probdens[i]>.0 && abs(log10(probdens[i])) >= 2.0)
                 ? scientific : fixed; 
         tcout << setw(fwidth) << formatType << probdens[i];
         formatType = (fractions[i]>.0 && abs(log10(fractions[i])) >= 2.0)
                 ? scientific : fixed; 
         tcout << setw(fwidth) << formatType << fractions[i];
         for (size_t j=0; j<nPars; ++j)
         {
            ParameterSpec* par = paramSpecs[j];
            Real val = par->parameterValues[currCounts[j]];
            formatType = (val>.0 && abs(log10(val)) >= 2.0)
                 ? scientific : fixed; 
            tcout << setw(fwidth) << formatType << val;
         }
         ++idxCounter;
         tcout << endl;
      }
      tcout.precision(savePrecision);
      tcout.flags(saveFlags);
   }
}

void MarginGrid::setMarginInfo ()
{
   const SpecContainer& info = getParameter();
   size_t sz = info.size();
   m_marginInfo.minRanges.resize(sz);
   m_marginInfo.maxRanges.resize(sz);
   m_marginInfo.nSteps.resize(sz);
   m_marginInfo.isLog.resize(sz);
   for (size_t i=0; i<sz; ++i)
   {
      m_marginInfo.minRanges[i] = info[i]->lowRange;
      m_marginInfo.maxRanges[i] = info[i]->highRange;
      m_marginInfo.nSteps[i] = info[i]->intervals;
      m_marginInfo.isLog[i] = info[i]->log;
   }
}

void MarginGrid::makePoint (const std::vector<size_t>& indices, std::vector<Real>& point)
{
    size_t sz = indices.size();
   std::vector<Real> tmp;
   // CAUTION!: This function ASSUMES all indices have been verified
   // to fall within size of point vector.  In effort to speed 
   // things up a bit, it will not check for them here.
   for (size_t i=0; i<sz; ++i)
   {
      tmp.push_back(point[indices[i]]);
   }
   std::swap(point, tmp);
}

void MarginGrid::calcBinCenters ()
{
   // Replace bin boundary values in ParameterSpec::parameterValue
   // arrays with their center values, and resize arrays.
   SpecContainer& parameters = parameter();
   size_t totSize = 1;
   for (size_t i=0; i<parameters.size(); ++i)
   {
      ParameterSpec* par = parameters[i];
      std::vector<Real>& parVals = par->parameterValues;
      if (par->log)
      {
         size_t nBins = parVals.size()-1;
         for (size_t j=0; j<nBins; ++j)
         {
            parVals[j] = sqrt(parVals[j]*parVals[j+1]);
         }
      }
      else
      {
         size_t nBins = parVals.size()-1;
         for (size_t j=0; j<nBins; ++j)
         {
            parVals[j] = (parVals[j] + parVals[j+1])/2.0;
         }
      }
      parVals.pop_back();
      totSize *= parVals.size();      
   }
   grid().resize(totSize,.0);
}

// Additional Declarations
