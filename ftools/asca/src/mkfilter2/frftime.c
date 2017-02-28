#include <stdio.h>
#include "atFunctions.h"
#include "ascatime.h" 
typedef int FRFTIME;

double attime2asca();

frftime2attime(FRFTIME frftime[], AtTime *attime)
{
  attime->yr = frftime[0];
  attime->mo = frftime[1];
  attime->dy = frftime[2];
  attime->hr = frftime[3];
  attime->mn = frftime[4];
  attime->sc = frftime[5];
  attime->ms = (float)frftime[6]/10.0;
}

attime2frftime(AtTime attime, FRFTIME frftime[])
{
  double round();

  frftime[0] = attime.yr;
  frftime[1] = attime.mo;
  frftime[2] = attime.dy;
  frftime[3] = attime.hr;
  frftime[4] = attime.mn;
  frftime[5] = attime.sc;
  frftime[6] = (int)(round(attime.ms*10.0));
  if (frftime[0] > 1900)
    frftime[0] -= 1900;
}

double frftime2mjd(FRFTIME frftime[])
{
  double mjd;
  AtTime attime;

  frftime2attime(frftime, &attime);
  atMJulian(&attime, &mjd);

  return mjd;
}

mjd2frftime(double mjd, FRFTIME frftime[])
{
  AtTime attime;

  atMJDate(mjd, &attime);
  attime2frftime(attime, frftime);
  if (frftime[0] > 1900)
    frftime[0] -= 1900;
}

frftime2keyword(FRFTIME sftime[], char dates[], char times[])
{
  sprintf(dates, "%02d/%02d/%02d", sftime[2], sftime[1], sftime[0]);
  sprintf(times, "%02d:%02d:%02d", sftime[3], sftime[4], sftime[5]);
}

double frftime2asca(FRFTIME sftime[7])/* sftime[6]¤Ï0.1msÃ±°Ì */
{
  double mjd, mjd0;
  double ascatime;
  AtTime attime;

  frftime2attime(sftime, &attime);
  ascatime = attime2asca(attime);

  return (ascatime);
}

asca2frftime(double ascatime, FRFTIME sftime[7])
{
  double mjd, mjd0;
  AtTime attime;

  asca2attime(ascatime, &attime);
  attime2frftime(attime, sftime);
  if (sftime[0] > 1900)
    sftime[0] -= 1900;
}
