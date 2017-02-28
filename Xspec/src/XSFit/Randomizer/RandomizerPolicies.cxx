#include <XSFit/Randomizer/RandomizerPolicies.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Numerics/RandomGenerator.h>
#include <XSUtil/Parse/XSparse.h>
#include <fstream>
#include <sstream>
#include <cmath>
#include <algorithm>

int svdcmp(double* mat, int m, int n, double* wvec, double* vmat, double* rv1);

const string RandomizerSourceInfo<Fit>::name = string("fit");
const string RandomizerSourceInfo<ChainManager>::name = string("chain");
const string RandomizerSourceInfo<RandomizerCovarFile>::name = string("<filename>");
const string RandomizerSourceInfo<RandomizerCmdLine>::name = string("<cmdline>");

void RandomizerSourceInfo<Fit>::fillVarInfo(const Fit* fit)
{
   // Even if fit is valid, it's still possible for the number of pars
   // in the covar matrix to differ from the current number of free
   // parameters.  This can happen if a parameter was pegged during the
   // fit.  
   if (fit->getSVDevalue().size() != fit->variableParameters().size())
   {
      string errMsg("Unable to use fit covariance matrix for randomization:\n");
      errMsg += "     Number of values in matrix does not match current number of variable parameters.\n";
      throw YellowAlert(errMsg);
   }
   covariance(fit);
}

const RealArray& RandomizerSourceInfo<Fit>::covariance(const Fit* fit)
{
   m_eigenValues.resize(fit->getSVDevalue().size());
   m_eigenValues = fit->getSVDevalue();
   m_eigenVectors.resize(fit->getSVDevector().size());
   m_eigenVectors = fit->getSVDevector();
   m_covariance.resize(fit->getSVDcovariance().size());
   m_covariance = fit->getSVDcovariance();
   return m_covariance;  
}

void RandomizerSourceInfo<ChainManager>::fillVarInfo(const Fit* fit)
{
   if (!fit->chainManager()->checkCovarForSynch())
   {
      string errMsg("The parameters for the stored chain covariance do not match");
      errMsg += "\nthe current variable parameters.  Either run chain recalc or try";
      errMsg += "\nsetting a different chain proposal.\n";
      throw YellowAlert(errMsg);
   }
   covariance(fit);
}

const RealArray& RandomizerSourceInfo<ChainManager>::covariance(const Fit* fit)
{
   m_eigenValues.resize(fit->chainManager()->chainSVDevalue().size());
   m_eigenValues = fit->chainManager()->chainSVDevalue();
   m_eigenVectors.resize(fit->chainManager()->chainSVDevector().size());
   m_eigenVectors = fit->chainManager()->chainSVDevector();
   m_covariance.resize(fit->chainManager()->chainCovariance().size());
   m_covariance = fit->chainManager()->chainCovariance();
   return m_covariance;   
}

void RandomizerSourceInfo<RandomizerCovarFile>::fillVarInfo(const Fit* fit)
{
   size_t nPar = m_eigenValues.size();
   if (nPar != fit->variableParameters().size())
   {
      std::ostringstream oss;
      oss << "Number of pars in input covariance matrix (" << nPar
         << ")\ndoes not match number of variable fit pars (" 
         << fit->variableParameters().size() << ")\n";
      throw YellowAlert(oss.str());      
   }
}

void RandomizerSourceInfo<RandomizerCovarFile>::validate(const string& initString)
{
   const string& fileName = initString;
   if (!fileName.length())
   {
      throw YellowAlert("Cannot load covariance, no file has been specified.\n");
   }
   readFromFile(fileName);
   // Assume readFromFile has made sure m_covariance is a square,
   // no need to check for that characteristic here.
   const int nPar = static_cast<int>(sqrt(static_cast<Real>(m_covariance.size())));
   const int nPar2 = nPar*nPar;
   m_eigenValues.resize(nPar,.0);
   RealArray eVectsF(.0,nPar2);
   RealArray workspace(.0,nPar);
   // This function modifies the matrix input, so send it only a 
   // copy of covariance.
   RealArray tmpCovariance(m_covariance);
   svdcmp(&tmpCovariance[0],nPar,nPar,&m_eigenValues[0],&eVectsF[0],&workspace[0]);
   m_eigenVectors.resize(nPar2,.0);
   for (int i=0; i<nPar2; ++i)
   {
      m_eigenVectors[i] = eVectsF[i];
   }   
}

void RandomizerSourceInfo<RandomizerCovarFile>::readFromFile(const string& fileName)
{
   using namespace std;
   ifstream covarFile(fileName.c_str());
   if (!covarFile)
   {
      string errMsg("Cannot open covariance file named: ");
      errMsg += fileName + "\n";
      throw YellowAlert(errMsg);
   }
   size_t lineCount = 0;
   vector<Real> fileVals;
   do
   {
      const string WS(" \t\n");
      string lineString;
      getline(covarFile, lineString);
      // Skip any blank lines.
      if (lineString.find_first_not_of(WS) != string::npos)
      {
         ++lineCount;
         istringstream issLine(lineString.c_str());
         for (size_t i=1; i<=lineCount; ++i)
         {
            Real covarVal = 0.0;
            if (!(issLine >> covarVal))
            {
               string errMsg("Improper matrix format in covariance file: ");
               errMsg += fileName + "\n";
               throw YellowAlert(errMsg);
            }
            fileVals.push_back(covarVal);
         }
      }      
   }
   while (!covarFile.eof());

   if (!lineCount)
   {
      string errMsg("Covariance file is empty: ");
      errMsg += fileName + "\n";
      throw YellowAlert(errMsg);      
   }

   // Only at this point do we know the actual size of the matrix.
   m_covariance.resize(lineCount*lineCount);
   size_t linearIndex = 0;
   // Fill in m_covariance using C-style [i][j] = row,col notation.
   for (size_t i=0; i<lineCount; ++i)
   {
      for (size_t j=0; j<=i; ++j)
      {
         if (j==i)
         {
            // diagonal element
            m_covariance[i*(lineCount + 1)] = fileVals[linearIndex];
         }
         else
         {
            m_covariance[i*lineCount+j] = fileVals[linearIndex];
            m_covariance[j*lineCount+i] = fileVals[linearIndex];
         }
         ++linearIndex;
      }
   }
}

void RandomizerSourceInfo<RandomizerCmdLine>::fillVarInfo(const Fit* fit)
{
   size_t nPar = m_eigenValues.size();
   if (nPar != fit->variableParameters().size())
   {
      std::ostringstream oss;
      oss << "Number of pars in input covariance matrix (" << nPar
         << ")\ndoes not match number of variable fit pars (" 
         << fit->variableParameters().size() << ")\n";
      throw YellowAlert(oss.str());      
   }
}

void RandomizerSourceInfo<RandomizerCmdLine>::validate(const string& initString)
{
   const int nPar = parseCmdLine(initString);
   const int nPar2 = nPar*nPar;
   m_eigenValues.resize(nPar,.0);
   m_eigenVectors.resize(nPar2,.0);
   if (m_onlyDiag)
   {
      for (int i=0; i<nPar; ++i)
      {
         m_eigenValues[i] = m_covariance[i*(nPar+1)];
         m_eigenVectors[i*(nPar+1)] = 1.0;
      }
   }
   else
   {
      RealArray eVectsF(.0,nPar2);
      RealArray workspace(.0,nPar);
      // This function modifies the matrix input, so send it only a 
      // copy of covariance.
      RealArray tmpCovariance(m_covariance);
      svdcmp(&tmpCovariance[0],nPar,nPar,&m_eigenValues[0],&eVectsF[0],&workspace[0]);
      for (int i=0; i<nPar2; ++i)
      {
         m_eigenVectors[i] = eVectsF[i];
      }
   }
}

int RandomizerSourceInfo<RandomizerCmdLine>::parseCmdLine(const string& lineArgs)
{
   // Assume if it got here lineArgs has 1 arg at the very least, 
   // for "matrix" or "diagonal" (these will be full names by
   // this point).
   const string MATRIX("matrix");
   const string DIAGONAL("diagonal");  
   string nonConstLineArgs(lineArgs);
   if (nonConstLineArgs.find(DIAGONAL) == 0)
   {
      nonConstLineArgs = nonConstLineArgs.substr(DIAGONAL.size());
      m_onlyDiag = true;
   }
   else
   {
      nonConstLineArgs = nonConstLineArgs.substr(MATRIX.size());
      m_onlyDiag = false;
   }
   // In this context commas can be treated exactly as spaces.
   const string delim(" ,\t");
   std::vector<Real> cmdLineVals;
   string::size_type startPos = nonConstLineArgs.find_first_not_of(delim);
   while (startPos != string::npos)
   {      
      string::size_type endPos = nonConstLineArgs.find_first_of(delim,startPos);
      endPos = std::min(nonConstLineArgs.length(), endPos);
      string arg(nonConstLineArgs.substr(startPos,endPos-startPos));
      std::istringstream issArg(arg);
      Real testFloat=.0;
      if (!(issArg >> testFloat) || !issArg.eof())
      {
         string errMsg("Invalid matrix value argument: ");
         errMsg += arg + "\n";
         throw YellowAlert(errMsg);
      }
      cmdLineVals.push_back(testFloat); 
      startPos = nonConstLineArgs.find_first_not_of(delim,endPos);
   }
   if (!cmdLineVals.size())
   {
      throw YellowAlert("No matrix values entered.\n");
   }

   int nPar = 0;
   if (m_onlyDiag)
   {
      nPar = (int)cmdLineVals.size();
      m_covariance.resize(nPar*nPar,.0);
      for (int i=0; i<nPar; ++i)
      {
         m_covariance[i*(nPar+1)] = cmdLineVals[i];
      }
   }
   else
   {
      // nCmdLineVals should correspond to lower half of matrix and the
      // diagonal.  So nCmdLineVals = (1/2)*nPar*(nPar+1), or
      // nPar = (-1 + sqrt(1 + 8*nCmdLineVals))/2.
      Real testNPar = (-1.0+sqrt(1.0+8.0*cmdLineVals.size()))/2.0;
      if (testNPar != floor(testNPar))
      {
         string errMsg("Must enter number of values corresponding to\n");
         errMsg += "   lower half (with diagonal) of a square matrix.\n";
         throw YellowAlert(errMsg); 
      }
      nPar = static_cast<int>(testNPar);

      m_covariance.resize(nPar*nPar,.0);
      size_t linearIndex = 0;
      for (size_t i=0; i<(size_t)nPar; ++i)
      {
         for (size_t j=0; j<=i; ++j)
         {
            if (i==j)
            {
               m_covariance[i*(nPar+1)] = cmdLineVals[linearIndex];
            }
            else
            {
               m_covariance[i*nPar+j] = cmdLineVals[linearIndex];
               m_covariance[j*nPar+i] = cmdLineVals[linearIndex];
            }
            ++linearIndex;
         }
      }
   } 
   return nPar;
}

const string RandomizerGaussDist::name = string("gaussian");
const string RandomizerCauchyDist::name = string("cauchy");

void RandomizerGaussDist::getValuesFromDistribution(const RealArray& scaling, RealArray& randVals)
{
   Numerics::GaussRand(randVals);
   randVals *= sqrt(scaling);
}

void RandomizerCauchyDist::getValuesFromDistribution(const RealArray& scaling, RealArray& randVals)
{
   Numerics::CauchyRand(randVals);
   randVals *= sqrt(scaling);
}
