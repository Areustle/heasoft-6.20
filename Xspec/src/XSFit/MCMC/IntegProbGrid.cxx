//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSFit/MCMC/Chain.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Numerics/ModularCounter.h>
#include <XSstreams.h>
#include <sstream>

// IntegProbGrid
#include <XSFit/MCMC/IntegProbGrid.h>



// Class IntegProbGrid::IntegProbInfo 

// Class IntegProbGrid 

IntegProbGrid::IntegProbGrid (MarginGrid* marginGrid)
  : Grid(),
    m_marginGrid(marginGrid)
{
}


IntegProbGrid::~IntegProbGrid()
{
}


void IntegProbGrid::doGrid ()
{
  // reset the bin ranges

  calcBinCenters();

  // Load the probability densities from the margin grid

  size_t gridSz = m_marginGrid->getGridSize();
  RealArray& pdf = grid();
  pdf.resize(gridSz);
  pdf = m_marginGrid->getGridValues();

  // Now need to convert probability densities into cumulative probabilities. 
  // The algorithm is to accumulate grid elements in order of increasing PDF.
  // set up a vector of pairs whose first value is the pdf and second the index into the
  // grid

  std::vector<std::pair<Real,size_t> > pdfpairs;
  for (size_t i=0; i<gridSz; i++) {
    std::pair<Real,size_t> currPDF = std::make_pair(pdf[i],i);
    pdfpairs.push_back(currPDF);
  }

  // the sort command sorts into increasing order based on the first element of the pair.
  // if there are multiple first elements with the same value then it sorts based on the
  // second element of the pair

  std::sort(pdfpairs.begin(),pdfpairs.end());

  // now construct the cumulative probabilities

  Real currSum(0.0);
  for (size_t i=0; i<gridSz; i++) {
    size_t icurr = pdfpairs[gridSz-i-1].second;
    currSum += pdfpairs[gridSz-i-1].first;
    pdf[icurr] = currSum;
  }

  // normalize to 1.

  pdf /= currSum;

}

void IntegProbGrid::report (bool title) const
{
}

void IntegProbGrid::calcBinCenters ()
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
