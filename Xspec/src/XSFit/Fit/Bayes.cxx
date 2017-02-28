//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Fit
#include <XSFit/Fit/Fit.h>
// Bayes
#include <XSFit/Fit/Bayes.h>

#include <XSModel/Parameter/ModParam.h>
#include <XSUtil/Error/Error.h>
#include <sstream>

// Class Bayes 

Bayes::Bayes()
  : m_isOn(false)
{
}


Bayes::~Bayes()
{
}


Real Bayes::lPrior (Fit* fit) const
{

   // Routine to return the sum of the  log of the priors for all 
   // the fit parameters.
   Real result = .0;
   // If not in Bayesian inference mode, return 0.
   if (m_isOn)
   {
      std::map<int,ModParam*>::const_iterator it = 
                fit->variableParameters().begin();
      std::map<int,ModParam*>::const_iterator itEnd = 
                fit->variableParameters().end();
      while (it != itEnd)
      {

         const ModParam* modPar = it->second;
         if (modPar->priorType() == ModParam::CONS)
         {
            // do nothing?
         }
         else if (modPar->priorType() == ModParam::EXP)
         {
	   Real hyper = modPar->hyperParam(0);
	   result -= modPar->value('v')/hyper + std::log(hyper);
         }
         else if (modPar->priorType() == ModParam::JEFFREYS)
         {
	   if ( modPar->value('v') != 0.0 ) {
	     result -= std::log(modPar->value('v'));
	   } else {
	     result -= -1.0e10;
	   }
         }
         else if (modPar->priorType() == ModParam::GAUSS)
         {

	   Real hyperMean = modPar->hyperParam(0);
	   Real hyperSigma = modPar->hyperParam(1);
	   if ( hyperSigma <= 0.0 ) hyperSigma = 1.e-10;
	   result -= 0.5*std::log(2.0*M_PI*hyperSigma*hyperSigma) + 
	     0.5*pow((hyperMean-modPar->value('v'))/hyperSigma, 2);
         }
         ++it;
      }
   }
   return result;
}

Real Bayes::dlPrior (Fit* fit, const int iPar) const
{
   // Routine to return the derivative wrt iPar of the log  
   // of the prior probability for that parameter.
   Real result = .0;
   // If not in Bayesian inference mode, return 0.
   if (m_isOn)
   {
      const ModParam* modPar = fit->variableParameters(iPar);
      if (!modPar)
      {
         std::ostringstream msg;
         msg <<"Bayes::dlPrior: Unable to locate param idx " << iPar 
                << " among Fit params.";
         throw YellowAlert(msg.str());
      }

      if (modPar->priorType() == ModParam::EXP)
      {
         // Of course hyperParam had better not be 0 by this point.
	Real hyper = modPar->hyperParam(0);
	if ( hyper != 0.0 ) {
	  result = -1.0/hyper;
	} else {
	  result = -1.0e10;
	}
      }
      else if (modPar->priorType() == ModParam::JEFFREYS)
      {
	if ( modPar->value('v') != 0.0 ) {
	  result = -1.0/(modPar->value('v'));
	} else {
	  result = -1.0e10;
	}
      }
      else if (modPar->priorType() == ModParam::GAUSS)
      {
	Real hyperMean = modPar->hyperParam(0);
	Real hyperSigma = modPar->hyperParam(1);
	result = (hyperMean-modPar->value('v'))/hyperSigma/hyperSigma;
      }
      // else do nothing
   }
   return result;
}

Real Bayes::d2lPrior (Fit* fit, const int iPar, const int jPar) const
{
   // Routine to return the second derivative wrt iPar and 
   // jPar of the log of the prior probability for that 
   // parameter. This will be zero unless iPar=jPar or the
   // prior probability of the parameters is linked.
   Real result = .0;
   if (m_isOn && iPar == jPar)
   {
      const ModParam* modPar = fit->variableParameters(iPar);
      if (!modPar)
      {
         std::ostringstream msg;
         msg <<"Bayes::d2lPrior: Unable to locate param idx " << iPar 
                << " among Fit params.";
         throw YellowAlert(msg.str());
      }

      if (modPar->priorType() == ModParam::JEFFREYS)
      {
	if ( modPar->value('v') != 0.0 ) {
	  result = 1.0/(modPar->value('v')*modPar->value('v'));
	} else {
	  result = 1.0e10;
	}
      }
      else if (modPar->priorType() == ModParam::GAUSS)
      {
	Real hyperSigma = modPar->hyperParam(1);
	result = -1.0/hyperSigma/hyperSigma;
      }
      // else do nothing

   }
   return result;
}

// Additional Declarations
