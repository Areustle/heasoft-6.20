// Class which Minuit uses to evaluate the statistic

#include <xsTypes.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSUtil/Error/Error.h>
#include <XSContainer.h>
#include <XSstream.h>
#include <XSstreams.h>

#include "MinuitCalcStat.h"

MinuitCalcStat::MinuitCalcStat()
{
  theErrorDef = 1.0;
}

MinuitCalcStat::~MinuitCalcStat()
{
}

// method to calculate the fit statistic

double MinuitCalcStat::operator()(const std::vector<double>& par) const 
{
  using namespace XSContainer;

  std::map<int,ModParam*>::const_iterator vp = fit->variableParameters().begin();
  std::map<int,ModParam*>::const_iterator vpEnd   = fit->variableParameters().end();

  StatManager* stats = fit->statManager();

  if ( par.size() != fit->variableParameters().size() ) {
    throw YellowAlert("Wrong number of variable parameters in MinuitCalcStat::operator()\n");
  }

  size_t ivar = 0;   
  while (vp != vpEnd) {
    ModParam& current = *(vp->second);
    current.setValue(par[ivar], 'a');
    ivar++;
    ++vp;
  }

  stats->performStats();
  Real statistic = stats->totalStatistic();

  tcout << xsverbose(20) << "MinuitCalcStat: ";
  vp = fit->variableParameters().begin();
  while (vp != vpEnd) {
    ModParam& current = *(vp->second);
    tcout << current.value('v') << " ";
    vp++;
  }
  tcout << statistic << std::endl << xsverbose();

  return (double) statistic;
}

std::vector<double> MinuitCalcStat::Gradient(const std::vector<double>& par) const
{
  using namespace XSContainer;

  if ( par.size() != fit->variableParameters().size() ) {
    throw YellowAlert("Wrong number of variable parameters in MinuitCalcStat::Gradient\n");
  }

  std::vector<double> grad(par.size());

  StatManager* stats = fit->statManager();

  std::map<int,ModParam*>::const_iterator vp = fit->variableParameters().begin();
  std::map<int,ModParam*>::const_iterator vpEnd   = fit->variableParameters().end();

  // set all parameter values

  size_t ivar = 0;   
  while (vp != vpEnd) {
    ModParam& current = *(vp->second);
    current.setValue(par[ivar], 'a');
    ivar++;
    ++vp;
  }

  // now loop round parameters calculating derivatives

  ivar = 0;
  vp = fit->variableParameters().begin();
  while (vp != vpEnd) {
    ModParam& current = *(vp->second);
    Real origVal = current.value('a');
    Real pardel = current.value('d');
    Real delta = 0.0;
    if ( !(origVal == current.value('h') && 
	   current.value('t') == current.value('h')) ) {
      current.setValue(origVal+pardel, 'a');
      delta += pardel;
    }
    stats->performStats();
    Real plusDeltaStat = stats->totalStatistic();
    if ( !(origVal == current.value('l') && 
	   current.value('b') == current.value('l')) ) {
      current.setValue(origVal-pardel, 'a');
      delta += pardel;
    }
    stats->performStats();
    Real minusDeltaStat = stats->totalStatistic();
    grad[ivar] = (plusDeltaStat-minusDeltaStat)/delta;
    current.setValue(origVal, 'a');
    ivar++;
    ++vp;
  }

  stats->performStats();

  tcout << xsverbose(20) << "MinuitCalcStat grad: ";
  for (size_t i=0; i<grad.size(); i++) tcout << grad[i] << " ";
  tcout << std::endl << xsverbose();

  return grad;

}
