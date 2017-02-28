#include <fitsio.h>
#include <math.h>
#include <string.h>
#include "pil.h"
#include "headas.h"
#include "headas_gti.h"
#include "battblocks.h"

/* 
 * battblocks - task for computing various time blocks from event or
 * light curve data
 *
 * Routines to compute burst duration
 *
 * C. Markwardt
 *
 * $Id: burstdur.c,v 1.18 2010/06/11 18:58:35 craigm Exp $
 *
 */

/* =========================================================== */
/* Estimate duration of the burst crudely, by using the time interval
 * between the outermost two Bayesian blocks.
 * 
 *  double *t - center times of each bin;
 *  double *dt - bin widths;
 *  int nrows - number of bins;
 *  struct gti_struct *bbgti - array of Bayesian block time intervals;
 *  long int *istart0 - upon return, the start of the burst;
 *  long int *istop0 - upon return, the stop of the burst;
 *
 * RETURNS: number of selected bins
 *
 * NOTE: *istart0 points to the *last* background bin before the 
 *       burst and *istop0 points to the *first* background bin after
 *       the burst.
 */
int burstspan(double *t, double *dt, int nrows,
	      double tstart, double tstop,
	      long int *istart0, long int *istop0)
{
  int istart, istop;
  int i;

  /* Find start and stop times:
     1. start time is end of first BB, which is assumed to be the
     pre-burst background.
     2. stop is the start of the last BB, which is assumed to be the
     post-burst background.
  */
  /* 
  tstart = bbgti->stop[0];
  tstop  = bbgti->start[bbgti->ngti-1];
  */
  
  for (i=0; i<nrows; i++) {
    if (t[i] >= tstart) break;
  }
  istart = i-1;
  if (i == 0) istart = 0;
  for (i=istart+1; i<nrows; i++) {
    if (t[i] >= tstop) break;
  }
  istop = i;
  if (i == nrows) istop = nrows-1;

  headas_chat(5, "  ...time range start=%f(%d) stop=%f(%d)...\n",
	      t[istart], istart, t[istop], istop);

  *istart0 = istart;
  *istop0 = istop;
  
  return (istop-istart);
}

int icrosstime(double *cumsum, double cummin, double maxcts, 
	       double thresh,
	       int istart, int istop, int durerrmeth, double rms,
	       int *ifirst0, int *ilast0)
{
  int outside, idelt = 1;
  double deffsign, deffsign0 = 1.0;
  int ifirst, ilast, nstep;
  int i, j;

  if (istop > istart) {
    /* From low -> high */
    idelt = 1;
    deffsign0 = +1.0;
    nstep = istop - istart + 1;
  } else {
    /* From high -> low */
    idelt = -1;
    deffsign0 = -1.0;
    nstep = istart - istop + 1;
  }
  
  deffsign = deffsign0;

  ifirst = -1; ilast = -1; 
  outside = 1;
  deffsign = deffsign0;
  for (i=istart, j=0;   j<nstep;   j++, i += idelt) {
    double eff = (cumsum[i]-cummin)/(maxcts-cummin);
    double deff;
    int pass;

    if (eff < 0) eff = 0;
    if (eff > 1) eff = 1;
    if (durerrmeth == 0) {
      deff = 0;
    } else if (durerrmeth == 1) {
      /* Total variance */
      deff = deffsign*rms;
    } else {
      /* Fractional variance */
      deff = deffsign*rms*sqrt(eff*(1.0-eff));
    }

    /* Threshold passing criteria */
    if (idelt == 1) {
      pass = (eff + deff) >= thresh;  /* Passing low -> high */
    } else {
      pass = (eff + deff) <= thresh;  /* Passing high -> low */
    }

    if (outside && pass) {
      outside = 0;
      ilast = i;
      if (ifirst == -1) ifirst = i;
      deffsign = -deffsign0;
    } else if (!outside && !pass) {
      outside = 1;
    }
  }

  *ifirst0 = ifirst; *ilast0 = ilast;
  return 0;

}

/* =========================================================== */
/* 
 * Compute burst duration measures - the method is to scan
 *    the "cumulative" counts array, starting from the left and
 *    right hand side until X% of the counts have been scanned
 *    over.  The algorithm accounts for the possibility that
 *    the threshold may be crossed several times, and uses an
 *    estimate of the light curve uncertainties to establish
 *    a threshold "band" rather than a hard threshold number.
 *
 * double *t - array of times
 * double *dt - array of time bin sizes
 * double *cumsum - array of cumulative counts
 * int nrows - number of time/counts samples
 * double *txxlist - list of TXX% percentage points
 *                   i.e. {0.9, 0.50}
 * int ntxx - number of txxlist values
 * double tpeak - number of seconds over which to measure the peak
 *                count rate
 * struct gti_struct *txxgti - array of GTI structures which will
 *        contain TXX% time intervals upon output
 * struct gti_struct *peak1sgti - upon output, the Tpeak time interval
 * double *txx_uncertainty - points to an array of size ntxx; upon
 *        output, contains the estimated TXX% uncertainties
 * double rms - upon input, the estimated rms variation of the light curve
 *
 * RETURNS: 0 upon success; otherwise a non-zero error code
 */
int burstdur(double *t, double *dt, double *cumsum, int nrows,
	     double *txxlist, int ntxx, double tpeak,
	     struct gti_struct *txxgti, 
	     struct gti_struct *peak1sgti,
	     double *txx_uncertainty, double rms, int durerrmeth)
	     
{
  double maxcts, cummin;
  int istart, istop, imax;
  /* No confidence bands */
  int ilow0first = 0, ilow0last = 0;  /* First and last crossings low TXX */
  int ihigh0first = 0, ihigh0last = 0;/* First and last crossings high TXX */
  /* With confidence bands */
  int ilowfirst = 0, ilowlast = 0;    /* First and last crossings low TXX */
  int ihighfirst = 0, ihighlast = 0;  /* First and last crossings high TXX */
  int i, j;
  int status = 0;
  int warned = 0;
  double dt_avg = 0;

  int j1, j2;
  double cts;
  
  /* These are vestigial */
  istart = 0;
  istop = nrows-1;
  /* Average time separation */
  dt_avg = (t[istop] - t[istart])/nrows;
  
  /* Normalize the sum to the total number of counts.
     NOTE: it is assumed that the first bin is a background bin,
     and the last bin is a background bin. */
  maxcts = cumsum[istop];
  cummin = cumsum[istart];

  headas_chat(5,"  ...maxcts=%f...\n", maxcts);
  /*  for (i=0; i<nrows; i++) cumsum[i] = (cumsum[i]-cummin) / maxcts; */
  
#if 0
  fprintf(stderr, "ERROR: debugging code has not been removed!!\n");
  { 
    FILE *outfile = fopen("/tmp/test.dat", "w");
    for (i=0; i<nrows; i++) 
      fprintf(outfile, "%d %f %f\n", i, t[i], cumsum[i]);
    fclose(outfile);
  }
#endif
  
  /* Search for these percentage points */
  for (j=0; j<ntxx; j++) {
    double txx = txxlist[j];            /* Percent of flux to search for */
    double txxstart = (100.0-txx)/2;    /*   Start percentage point */
    double txxstop  = txx + txxstart;   /*   Stop  percentage point */
    double vstart, vstop;
    double tlowavg = 0, thighavg = 0;
    double lowdiff, highdiff;
    
    headas_chat(5,"  ...txxstart=%f txxstop=%f...\n", txxstart, txxstop);
    /* Look for 5% crossing point; record the first and last crossings */

    txxstart /= 100;
    txxstop  /= 100;

    /* Converting percentages to counts ... */
    vstart = txxstart*maxcts + cummin; /* ... start counts ... */
    vstop  = txxstop *maxcts + cummin; /* ... stop  counts ... */

    /* Here "first" means first when approaching from the outside, so
       "last" is the innermost crossing. */

    /* Without any error band, TXX alone */
    icrosstime(cumsum, cummin, maxcts, txxstart, istart, istop,
	       0 /* = durerrmeth */ , rms, &ilow0first, &ilow0last);

    /* With error band to determine confidence limits */
    icrosstime(cumsum, cummin, maxcts, txxstart, istart, istop,
	       durerrmeth, rms, &ilowfirst, &ilowlast);

    /* Same for the 95% crossing points, but this time we approach
       from the outside on the right.  "Last" is still the innermost
       crossing. */
    icrosstime(cumsum, cummin, maxcts, txxstop, istop, 0,
	       0 /* = durerrmeth */, rms, &ihigh0first, &ihigh0last);
    icrosstime(cumsum, cummin, maxcts, txxstop, istop, 0,
	       durerrmeth, rms, &ihighfirst, &ihighlast);

    headas_chat(5, "     inner T%d = %f(%d)   T%d = %f(%d)\n",
		(int)rint(txxstart*100), t[ilowlast], ilowlast,
		(int)rint(txxstop*100),  t[ihighlast], ihighlast);
    headas_chat(5, "     outer T%d = %f(%d)   T%d = %f(%d)\n",
		(int)rint(txxstart*100), t[ilowfirst], ilowfirst,
		(int)rint(txxstop*100),  t[ihighfirst], ihighfirst);


    HDgti_grow(&txxgti[j], 1, &status);
    if (status) {
      fprintf(stderr, "ERROR: gti grow %d %d\n", j, status);
      return status;
    }


    txxgti[j].ngti = 1;
    /* TXX start/stop times, based on the runs with no confidence bands */
    tlowavg = 0.5*(t[ilow0last]+t[ilow0first]);
    thighavg = 0.5*(t[ihigh0last]+t[ihigh0first]);

    /* If things are really screwed up, then high will be less than
       low.  Bad. This bit of code will rever to the "outer" bins
       only, which should be robustly different. */
    if (tlowavg >= thighavg && ! warned) {
      warned = 1;
      fprintf(stderr, "WARNING: the light curve was not fully resolved.\n"
	              "         Txx determinations may be unreliable.\n"
	              "         You might consider supplying a light curve with\n"
	              "         finer time bins.\n");

      tlowavg = t[ilow0first];
      thighavg = t[ihigh0first];
    }
    txxgti[j].start[0] = tlowavg;
    txxgti[j].stop[0]  = thighavg;

    /* Compute the uncertainty based on the times only, assuming no
       bin-width information */
    lowdiff = tlowavg-t[ilowlast];
    if (fabs(tlowavg-t[ilowfirst]) > fabs(lowdiff)) {
      lowdiff = tlowavg-t[ilowfirst];
    }
    highdiff = thighavg-t[ihighlast];
    if (fabs(thighavg-t[ihighfirst]) > fabs(highdiff)) {
      highdiff = thighavg-t[ihighfirst];
    }
      
    txx_uncertainty[j] = sqrt(lowdiff*lowdiff + highdiff*highdiff);

    /* If bin width information is known, scan around the T5/T95 times
       to find the corresponding bin.  (We took the average above, so
       tlowavg and thighavg no longer correspond to a specific
       bin.) */
    if (dt) {
      double dtr = 0;
      for (i=ilowfirst; i<=ilowlast; i++) /* Outward in */
	if (tlowavg >= (t[i]-dt[i]/2) && tlowavg < (t[i]+dt[i]/2)) break;
      /* Never the first or last bin, which are background */
      if (i == istart) i++; if (i == istop) i--;
      txxgti[j].start[0] = t[i] - dt[i]/2;
      dtr += (dt[i]*dt[i])/4;

      for (i=ihighfirst; i>=ihighlast; i--) /* Outward in */
	if (thighavg >= (t[i]-dt[i]/2) && thighavg < (t[i]+dt[i]/2)) break;
      if (i == istart) i++; if (i == istop) i--;
      /* Never the first or last bin, which are background */
      txxgti[j].stop[0]  = t[i] + dt[i]/2;
      dtr += (dt[i]*dt[i])/4;

      dtr = sqrt(2*dtr);
      if (txx_uncertainty[j] < dtr) txx_uncertainty[j] = dtr;
    }

    /* Fall-back uncertainty */
    if (txx_uncertainty[j] == 0) {
      double dtr = 0;
      dtr += (t[ilowlast]-t[ilowlast-1])*(t[ilowlast]-t[ilowlast-1])/4;
      dtr += (t[ihighlast]-t[ihighlast-1])*(t[ihighlast]-t[ihighlast-1])/4;

      txx_uncertainty[j] = sqrt(2*dtr);
    }
    

    headas_chat(5, "     final T%d = %f   T%d = %f\n",
		(int)rint(txxstart*100), txxgti[j].start[0],
		(int)rint(txxstop*100),  txxgti[j].stop[0]);

  }

  /* ---------------------------------------------- */
  /* Find the peak tpeak seconds of the burst */
  /* The idea is we walk "i" through the light curve one bin at a
     time, and move j1 to the left by tpeak/2, and move j2 to the
     right by tpeak/2, in order to bracket i.  We keep track of the
     counts in each j1..j2 interval, and the maximum.

     The extra shennanigans here are to save some execution time,
     which may be significant if the light curve is really finely
     sampled.  j1 and j2 are preserved from one bin to the next.
  */
  maxcts = -1e307;
  imax = istart;
  headas_chat(5,"  ...searching for peak %f seconds...\n",tpeak);
  for (i=istart; i<=istop; i++) {
    for (j1=i-1; (j1 >= istart) && ((t[i]-t[j1]) < tpeak/2); j1--);
    if (j1 < istart) j1 = istart;

    /* Scan j1 until it is 1/2 tpeak away from the center (high side) */
    for (j2=i+1; (j2 <= istop) && ((t[j2]-t[i]) < tpeak/2); j2++);
    if ((t[j2]-t[i]) > tpeak/2) j2--;
    if (j2 > istop) j2 = istop;

    cts = cumsum[j2]-cumsum[j1];
    if (cts > maxcts) {
      maxcts = cts;
      imax = i;
      ilowfirst = j1;
      ihighfirst = j2;
    }

    /*
    headas_chat(5, "    t[%d] = %f (j1,j2)=(%d,%d) cts=%f maxcts=%f imax=%d (low,high)=(%d,%d)\n",
		i,t[i],j1,j2,cts,maxcts,imax,ilowfirst,ihighfirst);
    */
  }

  HDgti_grow(peak1sgti, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: peak 1s gti grow %d %d\n", j, status);
    return status;
  }

  peak1sgti->ngti = 1;
  peak1sgti->start[0] = t[ilowfirst];
  peak1sgti->stop[0]  = t[ihighfirst];
  
  if (dt) {
    peak1sgti->start[0] = t[ilowfirst+1]-dt[ilowfirst+1]/2;
    peak1sgti->stop[0]  = t[ihighfirst] +dt[ihighfirst]/2;
  }

  headas_chat(5, "  ...peak %f seconds t = [%f,%f] = [%d,%d]  cts=%f dur=%f...\n", 
	      tpeak, peak1sgti->start[0], peak1sgti->stop[0],
	      ilowfirst, ihighfirst, maxcts,
	      peak1sgti->stop[0] - peak1sgti->start[0]);

  /* Make sure that the 1-second interval is actually 1 second! */
  if (fabs(peak1sgti->stop[0] - peak1sgti->start[0] - tpeak) > dt_avg*0.001) {
    double tavg; 

    headas_chat(5,"  ...NOTE: peak interval is not exactly %f sec long...\n",
		tpeak);
    tavg = (peak1sgti->stop[0] + peak1sgti->start[0])/2.0;
    peak1sgti->start[0] = tavg - tpeak/2.0;
    peak1sgti->stop[0]  = tavg + tpeak/2.0;

    headas_chat(5, "     (resetting to %f)\n", tpeak);
    headas_chat(5, "  ...peak %f seconds t = [%f,%f] = [%d,%d]  cts=%f dur=%f...\n", 
	      tpeak, peak1sgti->start[0], peak1sgti->stop[0],
	      ilowfirst, ihighfirst, maxcts,
	      peak1sgti->stop[0] - peak1sgti->start[0]);
  }

  return 0;
}


