//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <algorithm>
#include <sstream>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSUtil/Numerics/RandomGenerator.h>

// Fit
#include <XSFit/Fit/Fit.h>
// FitErrorCalc
#include <XSFit/Fit/FitErrorCalc.h>
#include <XSFit/Fit/FitErrorOutput.h>



// Class FitErrorCalc::ParamCalcError 

FitErrorCalc::ParamCalcError::ParamCalcError (const string& msg)
  : YellowAlertNS(string("***Warning: " + msg))
{
}


// Class FitErrorCalc::NewMinFound 

FitErrorCalc::NewMinFound::NewMinFound (const string& msg)
  : YellowAlertNS(msg)
{
}


// Class FitErrorCalc 

// Deliberately give this a higher priority than output of fit warning messages,
//  which enables the user to filter out the latter.
const int FitErrorCalc::s_outVerbose = 5;

FitErrorCalc::FitErrorCalc (Fit* fit, const int iParam, bool isParallel)
  : m_fit(fit),
    m_fullIndex(iParam),
    m_direction(-1),
    m_savedChatter(0),
    m_bestStat(.0),
    m_deltaStat(.0),
    m_currentDeltaStat(.0),
    m_hardLimit(.0),
    m_xTrial(.0),
    m_limits(),
    m_coeffs(3,.0),
    m_interpPoints(3),
    m_param(fit->variableParameters(iParam)),
    m_bestParValue(.0),
    m_status(OK),
    m_isParallel(isParallel),
    m_msgWriter(0),
    m_msgQueue()
{
   if (!m_param)
   {
      throw RedAlert("Missing fit parameter in FitErrorCalc constructor.");
   }
   m_param->lastErrorStatus(errCodeToString(OK));
   if (m_isParallel)
      m_msgWriter = new ParallelErrorOutput();
   else
      m_msgWriter = new SingleErrorOutput();
}


FitErrorCalc::~FitErrorCalc()
{
   delete m_msgWriter;
}


void FitErrorCalc::interpolate ()
{
  static Real oldDelta = -1.0e20;
  Real& xmin = m_limits.first.first;
  Real& ymin = m_limits.first.second;
  Real& xmax = m_limits.second.first;
  Real& ymax = m_limits.second.second;

  // If there is no change between current fit statistic and last,
  // then parameter is insensitive to change of this magnitude.
  // Choose a random value between xmin,xmax and try again.
  if (fabs(oldDelta-m_currentDeltaStat) <= SMALL)
  {
     RealArray random(0., 1);
     Numerics::UniformRand(random);
     m_xTrial = xmin + (xmax - xmin)*random[0];
     return;
  } 

  if (m_currentDeltaStat > ymin && m_currentDeltaStat < m_deltaStat)
  {
     ymin = m_currentDeltaStat;
     xmin = m_xTrial;
     if (fabs(xmin-m_hardLimit) <= SMALL)
     {
        std::ostringstream os;
        ErrorCalcCodes errFlag = OK;
        Real pegVal = .0;
        if (m_direction > 0)
        {
           pegVal = m_bestParValue + m_hardLimit;
           errFlag = UPLIM; 
        }
        else
        {
           pegVal = m_bestParValue - m_hardLimit;
           errFlag = LOWLIM; 
        }
        os << "Parameter pegged at hard limit " << pegVal 
           << "\nwith delta fit statistic = " << m_currentDeltaStat;
        m_status = ErrorCalcCodes(m_status | errFlag);
        throw ParamCalcError(os.str());           
     }
  }

  if (m_currentDeltaStat < ymax && m_currentDeltaStat > m_deltaStat)
  {
     ymax = m_currentDeltaStat;
     xmax = m_xTrial;
  }

  // Replace in the 3pt array the point farthest away with the
  // current point.
  if (m_interpPoints[1].second >= .0)
  {
     Real dif = .0;
     int idif = 0;
     for (int jdif=0; jdif<=2; ++jdif)
     {
        Real cdif = fabs(m_interpPoints[jdif].second - m_deltaStat);
        if (cdif > dif)
        {
           idif = jdif;
           dif = cdif;
        }
     }
     m_interpPoints[idif].first = m_xTrial;
     m_interpPoints[idif].second = m_currentDeltaStat;
     int status = fit3Points(m_interpPoints, m_coeffs);
     if (status)
     {
        string msg(" *** 3 point fit failed during interpolation *** ");
        string dirMsg = (m_direction == 1) ? string("\nUpper") : string("\nLower");
        msg += dirMsg; 
        msg += " bound error calculation is invalid.";
        m_status = ErrorCalcCodes(m_status | GENPROB);
        throw ParamCalcError(msg);
     }
  }
  else
  {
     // This is the first trial, so explicitly fit to a simple
     // quadratic.
     m_interpPoints[1].first = m_xTrial;
     m_interpPoints[1].second = m_currentDeltaStat;
     m_coeffs[2] = m_currentDeltaStat/(m_xTrial*m_xTrial);
  }

  oldDelta = m_currentDeltaStat;

  // If a quadratic fit is to be used as the estimator then solve. Use positive
  // root unless it lies outside the valid range in which case try negative root.
  // If this is outside range too then punt and go for a linear estimator.
  bool useLinear = true;
  if (fabs(m_coeffs[2]) > SMALL)
  {
     useLinear = false;
     Real coef0 = m_coeffs[0];
     Real coef1 = m_coeffs[1];
     Real coef2 = m_coeffs[2];
     Real radix = coef1*coef1 - 4.*coef2*(coef0 - m_deltaStat);

     if (radix >= .0)
     {
        std::ostringstream os1, os2;
        radix = sqrt(radix);
        // Calculate the positive root
        m_xTrial = (radix - coef1)/(2.*coef2);
        os1 << "3 pt interpolation guess: " << m_xTrial;
        m_msgWriter->writeMsg(this, os1.str(), 15);

        // If positive root isn't in the range then try the negative.
        if (m_xTrial < xmin || m_xTrial > xmax)
        {
           m_xTrial = (-radix - coef1)/(2.*coef2);
           os2 << "Revised 3 pt guess: " << m_xTrial;
           m_msgWriter->writeMsg(this, os2.str(), 15);
           if (m_xTrial < xmin || m_xTrial > xmax)
           {
              useLinear = true;
           }
        }
     }
  }

  if (useLinear)
  {
    // if a quadratic estimator was not requested or could not be found then
    // try linear interpolation. if the linear interpolation guess is less
    // than 10% of the current interval then there is probably something going
    // wrong so go for the conservative solution and use the midpoint
     std::ostringstream os;
     m_xTrial = xmin + (m_deltaStat - ymin)*(xmax - xmin)/(ymax - ymin);
     if (fabs(m_xTrial - xmin) < 10.*fabs(xmax - xmin))
     {
        m_xTrial = (xmax + xmin)/2.0;
        os << "Mid-point trial: " << m_xTrial;
        m_msgWriter->writeMsg(this, os.str(), 15);
     }
     else
     {
        os << "Linear interpolation trial: " << m_xTrial;
        m_msgWriter->writeMsg(this, os.str(), 15);
     }
  }
}

void FitErrorCalc::perform ()
{
  try
  {
     m_savedChatter = tpout.consoleChatterLevel();
     m_fit->errorCalc(true);
     m_bestStat = m_fit->statistic();
     m_status = OK;
     m_direction = -1;
     calcUncertainty();
     resetFit();
     m_direction = 1;
     calcUncertainty();
     resetFit();
     // Nothing should throw from FitErrorCalc after reportParameter is
     //  called.  This ensures that parent process can ASSUME that if
     //  status flag is 0, the last string in the parallel process
     //  message queue will be the results and not some diagnostic message.
     m_msgWriter->reportParameter(this, s_outVerbose);
     m_param->lastErrorStatus(errCodeToString(m_status));
     commonRestore();
  }
  catch (NewMinFound& nmerr)
  {
     if (!m_isParallel)
     {
        tcerr << nmerr.message() <<std::endl;
     }
     // do not restore fit
     throw;
  }
  catch (...)
  {
     commonRestore();
     // A ctrl-C exit from fit will be caught here first.
     string dirMsg;
     ErrorCalcCodes errFlag;
     if (m_direction == 1)
     {
        dirMsg = "upper";
        errFlag = POSFAIL;
     }
     else
     {
        dirMsg = "lower";
        errFlag = NEGFAIL;
     }
     m_status = ErrorCalcCodes(m_status | errFlag | GENPROB);
     string fullMsg("Error occurred during " + dirMsg + " bound error calculation.");
     m_msgWriter->writeMsg(this, fullMsg, 10);
     if (m_param)
     {
        m_param->thaw();
        m_fit->variableParameters(m_fullIndex, m_param);
        m_param->lastErrorStatus(errCodeToString(m_status));
     }
     m_fit->resetParameters();
     m_fit->Update();
     throw;
  }
}

void FitErrorCalc::calcUncertainty ()
{
  bool isBracket = false;
  try
  {
     // m_param acts as a hook to hold the specified parameter,
     // which is removed from Fit's m_variableParameters map, and
     // put back at the end of this function.
     m_param->freeze();
     m_fit->freezeByIndex(m_fullIndex);

     m_deltaStat = m_fit->deltaStat();
     m_bestParValue = m_param->value('a');
     Real sigma = m_param->value('s');
     Real delta = m_param->value('d');

     if (fabs(sigma) < SMALL) sigma = delta;

     bool isHardEqualSoft = 
       (m_direction > 0 && fabs(m_param->value('h')-m_param->value('t')) < SMALL) ||
       (m_direction < 0 && fabs(m_param->value('l')-m_param->value('b')) < SMALL);
     if ( isHardEqualSoft && !m_param->isPeriodic())
     {
        m_hardLimit = (m_direction > 0) ? m_param->value('h') - m_bestParValue
                           : m_bestParValue - m_param->value('l');
        if (m_hardLimit < .0)
        {
           string msg;
           if (m_direction > 0)
           {
              msg += "Upper ";
              m_status = ErrorCalcCodes(m_status | UPLIM);
           }
           else
           {
              msg += "Lower ";
              m_status = ErrorCalcCodes(m_status | LOWLIM);
           }
           msg += "range pegged at hard limit";
           throw ParamCalcError(msg);
        }
     } else {
        m_hardLimit = LARGE;
     }

     bool isFirst = true;
     bool isDone = false;
     int iTrials = 0;
     m_coeffs[2] = 1.0/(sigma*sigma);
     m_interpPoints[0] = std::make_pair(.0,.0);
     m_interpPoints[1] = std::make_pair(.0,-1.0e20);
     m_interpPoints[2] = std::make_pair(.0,-1.0e20);
     // prev variables are for fail-safe escape tests
     Real prevFitStat = -9999.99;
     Real prevYmin = -9999.99;
     Real prevYmax = -9999.99;
     while (!isDone)
     {
        ++iTrials;
        ErrorCalcCodes errFlag = OK;
        if (isFirst)
        {
	   m_xTrial = sigma * sqrt(m_deltaStat);
           if ( m_xTrial > m_hardLimit) m_xTrial = std::min(delta, m_hardLimit/2.);
           isFirst = false;
        }
        else if (!isBracket)
        {
           m_xTrial = std::min(2.0*m_xTrial, m_hardLimit);
        }
        else
        {
           interpolate();
        }

        // This will reset all variable params, but NOT m_param,
        // since it was removed by freezeByIndex call.
        m_fit->resetParameters();
	Real newParVal = m_bestParValue + m_direction*m_xTrial;
	if (isHardEqualSoft && !m_param->isPeriodic()) 
        {
 	   newParVal = std::min(newParVal,m_param->value('h'));
 	   newParVal = std::max(newParVal,m_param->value('l'));
        }
        m_param->setValue(newParVal, 'a');
        // Note that for periodic parameters, after the above call 
        // newParVal is not necessarily the same as m_param->value().
        // The latter is always shifted into the first period region.

        resetFit();
        Real fitStat = m_fit->statistic();     
        if ((m_bestStat - fitStat) > m_fit->fitMethod()->deltaCrit())
        {
           // Set current par error string.
           m_status = ErrorCalcCodes(m_status | NEWMIN);
           m_param->lastErrorStatus(errCodeToString(m_status));
           string errMsg;
           if (!m_isParallel)
           {
              // Do NOT reset fit parameters to their old values,
              // however redo this same fit now with all N pars
              // unfrozen.  This should be the true new best fit.
              m_param->thaw();
              m_fit->variableParameters(m_fullIndex, m_param);
              commonRestore();
              // NOTE: resetFit may throw on its own, for example
              // if user hits Ctrl-C during this phase.  In that
              // case exception will be caught and handled at the
              // end of perform the same way all non-NewMinFound
              // exceptions are.
              resetFit();
              m_fit->report();
              // Exit perform function and query for start new
              // FitErrorCalc.
              errMsg = "***Warning: New best fit found, fit parameters will be set to new values.";
           }
           else
           {
              m_param->thaw();
              m_fit->variableParameters(m_fullIndex, m_param);
           
              // For parallel case, just send back the current par num (roundabout
              // via the Exception's msg string.
              std::ostringstream oss;
              oss << m_param->index();
              errMsg = oss.str();
           }
           throw NewMinFound(errMsg);
        }

        m_currentDeltaStat = fitStat - m_bestStat;
        // Could still have -deltaCrit < currentDeltaStat < .0, so:
        if (m_currentDeltaStat < .0)  m_currentDeltaStat = .0;

        // if we have not yet bracketed the target then see whether this 
        // attempt managed it. If it did then set the brackets and go 
        // onto the step of converging on the solution. Otherwise test 
        // whether we are at a hard limit in which case give up.
        Real& xmin = m_limits.first.first;
        Real& xmax = m_limits.second.first;
        Real& ymin = m_limits.first.second;
        Real& ymax = m_limits.second.second;
        if (!isBracket)
        {
           if (fitStat > (m_bestStat + m_deltaStat))
           {
              isBracket = true;
              xmin = .0;
              xmax = m_xTrial;
              ymin = .0;
              ymax = m_currentDeltaStat;
           }
           else if (m_xTrial >= m_hardLimit)
           {
              std::ostringstream os;
              Real pegVal = .0;
              if (m_direction > 0)
              {
                 pegVal = m_bestParValue + m_hardLimit;
                 errFlag = UPLIM; 
              }
              else
              {
                 pegVal = m_bestParValue - m_hardLimit;
                 errFlag = LOWLIM; 
              }
              os << "Parameter pegged at hard limit: " << pegVal;
              m_status = ErrorCalcCodes(m_status | errFlag);
              throw ParamCalcError(os.str());
           }
           else if (iTrials >= Fit::errorTry())
           {
              std::ostringstream os;
              os << "\n***Warning: Number of trials exceeded before bracketing of delta fit-stat."
                 <<"\nLast attempt: " << newParVal << ", with delta statistic: " << m_currentDeltaStat
                 << std::endl;
              m_msgWriter->writeMsg(this, os.str(), s_outVerbose);

              // This code was added as a fail-safe.  Even if user has
              // set automatic reply to 'YES', we want to exit if 
              // search isn't going anywhere.
	      Real fractChange = (prevFitStat == .0) ? std::fabs(fitStat) :
	           std::fabs((fitStat-prevFitStat)/prevFitStat);
              if (fractChange <1e-25)
              {
                 string boundDir = (m_direction > 0) ? "upper " : "lower "; 
                 std::ostringstream errMsg;
                 errMsg << "Error search cannot find bracket around delta fit-stat,\n"
                        << "   will automatically exit from trials loop.\n"
                        << "   parameter " << boundDir << " bound is INVALID.\n";

                 throw ParamCalcError(errMsg.str());
              }
              prevFitStat = fitStat;
           }
        }
        else
        {
           // if the target has already been bracketed then we are in 
           // the convergence phase so check for convergence.
           if (fabs(m_currentDeltaStat - m_deltaStat) <= m_fit->tolerance())
           {
              isDone = true;
           }
           else
           {
              std::ostringstream os;
              os << iTrials << "  " << m_direction << " "
                    << m_xTrial << "  " << m_currentDeltaStat << '\n'
                    << xmin <<"  "<< ymin <<"  "<< xmax <<"  "<< ymax;
              m_msgWriter->writeMsg(this, os.str(), 25);

            // If the calculated statistic for the trial value exceeds 
            // the upper limit, ymax, then there is probably something 
            // wrong and the user should use steppar to investigate the 
            // situation. Return a value midway betweenthe bracketing trials.
              if (m_currentDeltaStat > ymax || m_currentDeltaStat < ymin)
              {
                 std::ostringstream os2;
                 os2 << "Apparent non-monotonicity in statistic space detected."
                 << "\nCurrent bracket values " << m_direction*xmin+m_bestParValue
                 <<", "<< m_direction*xmax+m_bestParValue << "\nand delta stat "
                 << ymin <<", "<< ymax <<"\nbut latest trial " 
                 << m_direction*m_xTrial+m_bestParValue << " gives " << m_currentDeltaStat
                 << "\nSuggest that you check this result using the steppar command.";
                 m_msgWriter->writeMsg(this, os2.str(), s_outVerbose);
                 m_xTrial = (xmin + xmax)/2.0;
                 m_currentDeltaStat = (ymin + ymax)/2.0;
                 m_status = ErrorCalcCodes(m_status | NONMON);
                 isDone = true;
              }

             // If the tolerance has not been achieved but the top and 
             // bottom of the xlimit range are equal then we cannot calculate 
             // the delta to the required tolerance so have to give up.
              if (fabs(xmin-xmax)/xmax < 1.0e-5)
              {
                 string msg("Identical values of the parameter give different values of the statistic.");
                 msg += "\nPlease check your result ";
                 if (m_direction < .0)
                 {
                    msg += "for the low end of the confidence range.";
                    m_status = ErrorCalcCodes(m_status | NEGFAIL);
                 }
                 else
                 {
                    msg += "for the high end of the confidence range.";
                    m_status = ErrorCalcCodes(m_status | POSFAIL);
                 }
                 throw ParamCalcError(msg);
              }

              if (!isDone && iTrials >= Fit::errorTry())
              {
                 std::ostringstream os2;
                 os2 << "\n***Warning: Number of trials exceeded before convergence."
                    <<"\nCurrent trial values " << m_direction*xmin+m_bestParValue <<", "
                    << m_direction*xmax+m_bestParValue <<"\nand delta statistic "
                    << ymin <<", " << ymax << std::endl;
                 m_msgWriter->writeMsg(this, os2.str(), s_outVerbose);

                 // This code was added as a fail-safe.  Even if user has
                 // set automatic reply to 'YES', we want to exit if 
                 // search isn't going anywhere.
	         Real fractChange1 = (prevYmin == .0) ? std::fabs(ymin) :
	              std::fabs((ymin-prevYmin)/prevYmin);
	         Real fractChange2 = (prevYmax == .0) ? std::fabs(ymax) :
	              std::fabs((ymax-prevYmax)/prevYmax);
                 if (fractChange1 <1e-25 && fractChange2 <1e-25)
                 {
                    string errMsg("Error search cannot converge any further,\n");
                    errMsg += "              will automatically exit from trials loop.\n";
                    throw ParamCalcError(errMsg);
                 }
                 prevYmin = ymin;
                 prevYmax = ymax;
              }
           } // end if not converged
        } // end if bracketted
        // if too many times round the loop give the user a chance to get out
        if (!isDone && iTrials >= Fit::errorTry())
        {
           isDone = true;
           errFlag = (m_direction > 0) ?  POSFAIL : NEGFAIL;
           string boundDir = (m_direction > 0) ? "upper " : "lower "; 
           if (m_fit->queryMode() == Fit::ON)
           {
              int reply = 
                 XSutility::yesToQuestion("Continue error search in this direction? ",0,tcin);
              if (reply == 1)
              {
                 iTrials = 0;
                 isDone = false;
              }
              else if (reply == -1)
              {
                 // User entered "/*", exit error command entirely.
                 throw YellowAlert();
              }
              else 
              {
                 m_status = ErrorCalcCodes(m_status | errFlag);
                 if (!isBracket)
                 {
                    std::ostringstream os;
                    os << "\n*** Parameter " << boundDir 
                       << "bound is INVALID.\n";
                    m_msgWriter->writeMsg(this, os.str(), s_outVerbose);
                 }
              }                    
           }
           else if (m_fit->queryMode() == Fit::YES)
           {
              iTrials = 0;
              isDone = false;
           }
           else  // query = no
           {
              m_status = ErrorCalcCodes(m_status | errFlag);
              if (!isBracket)
              {
                 std::ostringstream os;
                 os << "\n*** Parameter " << boundDir << "bound is INVALID.\n";
                 m_msgWriter->writeMsg(this, os.str(), s_outVerbose);
              }
           }
        }
     } // end iterations loop

     // the answer has converged so save the value and reset the values 
     // of the fit parameters to the values at invocation     
  }
  catch (ParamCalcError& excpt)
  {
     ErrorCalcCodes errFlag = (m_direction > 0) ?
            POSFAIL : NEGFAIL;
     m_status = ErrorCalcCodes(m_status | errFlag);
     m_msgWriter->reportException(this, excpt.message()); 
  }
  catch (Fit::NotVariable& )
  {
     ErrorCalcCodes errFlag;
     string errMsg;
     if (m_direction == 1)
     {
        errFlag = POSFAIL;
        errMsg = string("upper");
     }
     else
     {
        errFlag = NEGFAIL;
        errMsg = string("lower");
     }
     
     string fullMsg("\n Fit error occurred during "+errMsg+" bound error calculation.");
     m_msgWriter->writeMsg(this, fullMsg, 10);
     m_status = ErrorCalcCodes(m_status | errFlag);
  }

  if (isBracket)
  {
     char pm = (m_direction == 1) ? 'p' : 'm';
     Real newParVal = m_direction*m_xTrial+m_bestParValue;
     if (!m_param->isPeriodic())
     {
        if ( fabs(m_param->value('h')-m_param->value('t')) < SMALL)
          newParVal = std::min(newParVal,m_param->value('h'));
        if ( fabs(m_param->value('l')-m_param->value('b')) < SMALL)
          newParVal = std::max(newParVal,m_param->value('l'));
        m_param->setValue(newParVal, 'a');
        m_param->setValue(m_param->value('v'), pm);
     }
     else
     {
        m_param->setValue(newParVal,'v');
        m_param->setValue(newParVal, pm);
     }
  }
  m_param->thaw();
  m_fit->variableParameters(m_fullIndex, m_param);
  m_fit->resetParameters();
}

int FitErrorCalc::fit3Points (const std::vector<Coord2D >& fitPoints, std::vector<Real>& coeffs)
{
  if (fitPoints.size() < 3 || coeffs.size() < 3)  return -1;
  if (fitPoints[2].first == fitPoints[1].first)  return 1;

  Real delta = 1./(fitPoints[2].first - fitPoints[1].first);
  std::vector<Coord2D > tmpPoints(2);
  std::vector<Real> coef01(2);
  std::vector<Real> coef02(2);
  tmpPoints[0] = fitPoints[0];
  tmpPoints[1] = fitPoints[1];
  int ierr = fit2Points(tmpPoints, coef01);
  if (ierr)  return ierr;

  tmpPoints[1] = fitPoints[2];
  ierr = fit2Points(tmpPoints, coef02);
  if (ierr)  return ierr;

  Real x1 = fitPoints[1].first;
  Real x2 = fitPoints[2].first;
  coeffs[0] = delta*(coef01[0]*x2 - coef02[0]*x1);
  coeffs[1] = delta*(coef01[1]*x2 - coef02[1]*x1 - coef01[0] + coef02[0]);
  coeffs[2] = delta*(coef02[1] - coef01[1]);

  return 0;
}

int FitErrorCalc::fit2Points (const std::vector<Coord2D >& fitPoints, std::vector<Real>& coeffs)
{
  if (fitPoints.size() < 2 || coeffs.size() < 2)  return -1;

  Real x1 = fitPoints[0].first;
  Real x2 = fitPoints[1].first;
  Real y1 = fitPoints[0].second;
  Real y2 = fitPoints[1].second;
  if (x1 == x2)  return 1;

  Real delta = 1./(x2-x1);
  coeffs[0] = delta*(y1*x2 - y2*x1);
  coeffs[1] = delta*(y2 - y1);
  return 0;
}

void FitErrorCalc::commonRestore ()
{
  m_fit->errorCalc(false);
  tpout.consoleChatterLevel(m_savedChatter);
}

void FitErrorCalc::resetFit ()
{
   m_fit->calculateModel();
   m_fit->initializeStatistic();
   if (m_fit->variableParameters().size())
      m_fit->perform();
}

string FitErrorCalc::errCodeToString (FitErrorCalc::ErrorCalcCodes errCode)
{
   string errString("FFFFFFFFF");
   int testBitVal = 1;
   size_t i=0;
   while (i < errString.size())
   {
      if (ErrorCalcCodes(testBitVal) & errCode)
      {
         errString[i] = 'T';
      }
      testBitVal <<= 1;
      ++i;
   }
   return errString;
}

FitErrorCalc::ErrorCalcCodes FitErrorCalc::stringToErrCode (const string& errString)
{
   int errCode = 0;
   int testVal = 1;
   string::size_type i=0;
   while (testVal <= TOOLARGE && i < errString.length())
   {
      if (errString[i] == 'T')
         errCode |= testVal;
      testVal *= 2;
      ++i;
   }
   return ErrorCalcCodes(errCode);
}


// Additional Declarations
