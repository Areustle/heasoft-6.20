#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "bat_gswdev.h"

/*
 * Swift BAT Energy conversion routines
 *
 */

/*
 * bat_pha_to_energy - conversion from BAT pulse height to energy
 *
 * float pha  - input pulse height [ADU]
 * int method - method for computation
 *   LIN_METH - equivalent to flight linear conversion
 *   QUAD_METH   - quadratic residuals
 *   CUBIC_METH  - cubic residuals
 *   FIXEDDAC_METH - cubic residuals corrected to DAC voltage
 * float ftotgain, ftotoffset - flight-derived "gain" and "offset"
 *   values, scaled to units of [keV/ADU] and [ADU] respectively.
 * float fpulseTokeV, fpulse0keV - flight pulser-to-energy conversion
 *   factors in units of [keV/DACU] and [DACU] respectively
 *     IMPORTANT: fpulseTokeV is 1/GAIN_pulseflt from calibration file
 * float gpulseTokeV, gpulse0keV - ground-derived pulser-to-energy
 *   conversion factors in units of [keV/DACU] and [DACU] respectively
 *     IMPORTANT: gpulseTokeV is 1/GAIN_pulsecal from calibration file
 * float DAClow - pulser DAC setting for "offset" pulses [DACU]
 * float gpulresid0,1,2,3 - cubic residual function values from
 *   quadres calibration file.
 * float gpul_nom_offset, gpul_nom_gain - from quadres calibration file,
 *   contains "nominal" pulser-to-PHA gain and offset based on ground
 *   calculation.
 */
int bat_pha_to_energy(float *pha, float *energy, int n,    
		      int method,
		      float ftotgain,    float ftotoffset, 
		      float fpulseTokeV, float fpulse0keV,
		      float gpulseTokeV, float gpulse0keV,
		      float DAClow,
		      float gpulresid0, float gpulresid1,
		      float gpulresid2, float gpulresid3,
		      float gpul_nom_offset, float gpul_nom_gain, 
		      int which_scale, 
		      int *status)
{
  float fcalcgain, poff;
  float gtotgain, gtotoffset;
  int i;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if (pha == 0 || energy == 0) {
    return (*status = NULL_INPUT_PTR);
  }

  /* ============== */
  /* FIRST, attempt the methods which compute energy directly, in this
     case the linear transformation which needs only limited data.
     This is the basic linear conversion, using the flight derived
     total gain and offset. We do the linear conversion here because
     many of the variables used below will be filled with garbage data.  */
  if (method == LIN_METH) {
    for (i=0; i<n; i++) {
      energy[i] = ftotgain*(ftotoffset - pha[i]);  /* [keV] */
    }
    return 0;
  }


  /* In FIXEDDAC method and DIRECTCUBIC method, the full coefficients
     are stored in the "residuals" file.  For FIXEDDAC, we are
     treating the residuals as residuals, so need to subtract off the
     large nominal levels.
  */
  if (method == FIXEDDAC_METH && gpulresid0 && gpulresid1) {
    gpulresid0 = gpulresid0 - gpul_nom_offset;  /* [DACU] */
    gpulresid1 = gpulresid1 - gpul_nom_gain;    /* [DACU/ADU] */
  }  
  
  /* fcalcgain is the flight estimate of the pulse to PHA conversion
     scale factor.  It's buried within the telemetered "gain" value,
     which has an additional conversion to energy, so we divide that
     out to get the pulser-to-PHA only conversion. */
  fcalcgain = ftotgain/fpulseTokeV;   /* [DACU/ADU] */
  
  /* poff is the pulse height value at the DAClow pulser setting.
     Here again we start with the flight "offset" value, and then back
     out the portion of that quantity which is due to the "pulser
     offset". */
  poff      = ftotoffset - (DAClow - fpulse0keV)/fcalcgain; /* [ADU] */
  
  /* gtotgain is the total linear gain, this time using the improved
     ground-derived pulser to keV conversion.  gtotoffset is the total
     linear offset, using the improved ground-derived pulser to keV
     offset. */
  gtotgain   = fcalcgain * gpulseTokeV;  /* [keV/ADU] */
  gtotoffset = poff + (DAClow - gpulse0keV)/fcalcgain;  /* [ADU] */

  for (i=0; i<n; i++) {
    float pha1, dac1, energy1;
    float resid;

    /* Pulse height from input array */
    pha1 = pha[i]; /* [ADU] */

    /* ============== */
    /* Now, attempt the methods which compute energy directly,
       without resorting to the formulations with cubic residuals.
       Each of these calculations will 'continue' at the end since
       they don't need the rest of the complicated calculations. */
    if (method == DIRECT_METH) {
      /* Direct cubic calculation of DAC level without using any
	 special residuals.  In this case the 'residuals' coefficients
	 are actually the full cubic coefficients which convert ADU to
	 DAC.
      */
      dac1 = (gpulresid0 +         /* [DACU] */
	      pha1*(gpulresid1 +   /* [ADU   * DACU/ADU] */
	      pha1*(gpulresid2 +   /* [ADU^2 * DACU/ADU^2] */
	      pha1*gpulresid3)));  /* [ADU^3 * DACU/ADU^3] */
      /* Now convert to energy using the linear pulser to keV conversion */
      energy[i] = gpulseTokeV*(dac1 - gpulse0keV);
      continue;
    }

    /* ============== */
    /* OTHERWISE, continue calculating the cubic/quadratic residuals. */

    /* XXX Range checking here... */
    
    /* Basic linear conversion, using the ground derived total gain
       and offset. */
    energy1 = gtotgain*(gtotoffset - pha1);  /* [keV] */
    
    /* For FIXEDDAC method, we compute an adjusted pulse height that
       corresponds to the pulse height it *would* have had on the
       pre-launch scale.  The pre-launch gain scale is given by nom_gain
       and nom_offset. */
    if (method == FIXEDDAC_METH) {

      /* dac1 is the flight-derived pulser DAC voltage corresponding to
	 the input pulse height, simply using linear coefficients. */
      dac1 = DAClow - fcalcgain*(pha1-poff);    /* [DACU] */

      pha1 = (dac1 - gpul_nom_offset) / gpul_nom_gain;  /* [ADU] */
    }
    
    /* resid is a cubic polynomial, computing the pulser DAC residuals
       based on the (adjusted) pulse height).
    */
    resid = (gpulresid0 +          /* [DACU] */
	     pha1*(gpulresid1 +    /* [ADU   * DACU/ADU  ] */
	     pha1*(gpulresid2 +    /* [ADU^2 * DACU/ADU^2] */
	     pha1* gpulresid3)));  /* [ADU^3 * DACU/ADU^3] */

    /* Residual is in pulser DAC units, so scale this by the ground
       pulser to keV scale factor. */
    if (which_scale == 0) {
      /* Scale used by bateconvert */
      energy1 += (resid * gpulseTokeV);   /* [DACU * keV/DACU] */
    } else {
      /* XXX Scale used by baterebin */
      energy1 += (resid * fpulseTokeV);   /* [DACU * keV/DACU] */
    }

    /* Save result! */
    energy[i] = energy1;
  }

  return 0;
}

int bat_flight_energy_to_pha(float *flight_energy, float *pha,
			     int n,
			     float ftotgain, float ftotoffset,
			     int quantize)
{
  float pha1;
  int i;

  for (i=0; i<n; i++) {
    pha1 = ftotoffset - flight_energy[i]/ftotgain;  /* [ADU] */

    if (quantize) {
      /* Quantize to nearest half-bin */
      pha1 = ((int) (pha1 + 1)) - 0.5;       /* [ADU] */
    }
    
    pha[i] = pha1;
  }

  return 0;
}
