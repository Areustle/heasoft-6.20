/* Multi-MuTau model response, called by calc_response() */

#define EXP_REF 31.0
#define EXP_CUTOFF 0.01

#define min(x,y) ((x<y)?(x):(y))
#define max(x,y) ((x>y)?(x):(y))

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_utils.h"
#include "bat_gswdev.h"
#include "batdrmgen.h"


float hecht (float lambda_e, float lambda_h, float depth)

/* function: hecht
 *
 * Inputs:
 *   lambda_e: mean distance electrons travel in the detector (cm)
 *   lambda_h: mean distance holes travel in the detector (cm)
 *   depth: distance below the top surface of the detector (cm)
 *
 * Output:
 *   charge induction efficiency at that depth (dimensionless)
 *

 */

{
  return (lambda_e*(1-exp(-(DET_THICKNESS-depth)/lambda_e)) +
      lambda_h*(1-exp(-depth/lambda_h)))/DET_THICKNESS;
}  


int exp_tail (
    struct resp_struct *resp, struct multi_mt_struct *cal_parms,
    float photon_energy, float dist_eff_area, double *result)

/* function: exp_tail
 *
 * Removes some of the counts from the mutau model and places
 * them in the exponential tail
 *
 * Inputs:
 *
 *   resp:           structure with response parameters
 *   cal_parms:      structure with calibration parameters
 *   photon_energy:  energy of incident photons (keV)
 *   dist_eff_area:  effective area (cm^2) according to the energy
 *                     deposition distributions
 *                     (this is NOT the total effective area;
 *                     the total effective area is dist_eff_area*norm)
 *
 *   result:         counts spectrum in units of effective area (cm^2)
 *                     (modified to include the exponential tail)
 *
 * The exponential tail, integrated from -inf to max_e, is 1
 *
 * This function reduces the overall effective area because it removes an 
 * amount for the tail but then only replaces those the fraction of those
 * that are in the energy range being used
 *
 * The calling function is responsible to make sure that result
 * has enough memory allocated
 */

{
  int i, peak_channel;
  float norm_etail, etail, exp_ratio, temp_sum;
  /* double sum; */

  headas_chat(4,"...Inside exp_tail...\n");

  /* calculate exp_ratio */

  exp_ratio = cal_parms->exp_coeff*
    pow(max(photon_energy,EXP_REF)/EXP_REF,-cal_parms->exp_index);
  headas_chat(5,"exp_ratio: %f\n",exp_ratio);

  /* Check to see if exp_ratio is greater than EXP_CUTOFF
   * If it is, add the exponential tail */

  if (exp_ratio>EXP_CUTOFF) {

    /* peak_channel:
     *   the channel corresponding to the 
     *   incident photon energy) */

    peak_channel=0;
    while (resp->emax_pre[peak_channel] < photon_energy) peak_channel++;
    headas_chat(5,"peak channel: %d\n",peak_channel);

    /* a portion of the normalization "norm" corresponds to the main peak
     * a portion of the normalization "norm" corresponds to the exponential
     * tail
     *
     * the "norm" parameter is dimensionless, while the normalization for 
     * the exponential tail is in units of cm^2,
     * so it must be multiplied by dist_eff_area
     *
     * note: for now, if photon_energy is less than EXP_REF, exp_ratio
     * is calculated for photon_energy=EXP_REF.  This prevents exp_ratio
     * from getting unreasonably large.
     */

    norm_etail = (cal_parms->norm)*exp_ratio*dist_eff_area;
    headas_chat(5,"fraction staying in peak: %f\n",(1.0-exp_ratio));
    headas_chat(5,"area removed for tail (cm2): %f\n",norm_etail);

    /* scale result down by a factor of (1.0-exp_ratio) */

    for (i=0;i<resp->nphabins_pre;i++) result[i]*=(1.0-exp_ratio);

    /* for every channel below peak_channel, 
     * find a value for the exponential tail */

    temp_sum=0;
    for (i=0;i<=peak_channel;i++) {
      etail=
        (exp((min(resp->emax_pre[i],photon_energy)-photon_energy)/
	     (photon_energy*cal_parms->exp_lambda))-
         exp((resp->emin_pre[i]-photon_energy)/
	     (photon_energy*cal_parms->exp_lambda)));
      temp_sum+=norm_etail*etail;
      result[i]+=norm_etail*etail;
    }
    headas_chat(5,"actual area put into tail: %f\n",temp_sum);

  }
  headas_chat(4,"...Leaving exp_tail...\n");
  return 0;
}


float trans (
    struct multi_mt_struct *p, int flight_data, float energy, 
    double *srcpos)

/* function: trans
 *
 * Inputs:
 *   p:            structure containing parameters used to find transmission
 *   flight_data:  1 if the data is flight data, 0 if it is ground data
 *                    if the data is ground data, transmission due to
 *                    the source packaging and air is included
 *   energy:       energy of incident photons (keV)
 *   srcpos:       a 3-element array:
 *      srcpos[0]: bat x position of source (cm)
 *      srcpos[1]: bat y position of source (cm)
 *      srcpos[2]: bat z position of source (cm)
 *
 * Output:
 *   fraction of photons that are transmitted through passive materials,
 *   lead tile edges, and (if flight_data = 0) air
 */

{
  double src_ind, air_ind, passive_ind, pb_mu;
  double t_src, t_air, t_passive, t_pb_near, t_pb_far, t_pb, t_pb_overall, t;
  double r, cos_theta, tan_theta, tan_theta_x, tan_theta_y;
  double nonedge_fraction, edge_fraction;

  /* theta is always between 0 and something less than pi/2 for any 
   * source in the field of view, so tan(theta) and cos(theta) are 
   * always positive */

  headas_chat(4,"...Inside trans...\n");

  r=sqrt(srcpos[0]*srcpos[0]+srcpos[1]*srcpos[1]+srcpos[2]*srcpos[2]);
  tan_theta=fabs(sqrt(srcpos[0]*srcpos[0]+srcpos[1]*srcpos[1])/srcpos[2]);
  tan_theta_x=fabs(srcpos[0]/srcpos[2]);
  tan_theta_y=fabs(srcpos[1]/srcpos[2]);
  cos_theta=fabs(srcpos[2]/r);
  headas_chat(5,"r: %f\n",r);
  headas_chat(5,"tan(theta): %f\n",tan_theta);
  headas_chat(5,"tan(theta_x): %f\n",tan_theta_x);
  headas_chat(5,"tan(theta_y): %f\n",tan_theta_y);
  headas_chat(5,"cos(theta): %f\n",cos_theta);

  /* passive_ind: the model depends on the bat file version (batfilev) */

  switch (p->batfilev) {
    case 1:

      /* a smoothly broken power law
       * 
       * [(psv[0]*energy^psv[1])^(1/psv[4])+
       *  (psv[2]*energy^psv[3])^(1/psv[4])]^psv[4]
       */

       passive_ind=pow(pow(p->psv[0]*pow(energy,p->psv[1]),
	     1/p->psv[4])+pow(p->psv[2]*pow(energy,
	         p->psv[3]),1/p->psv[4]),p->psv[4]);

       headas_chat(5,"using case 1: \n");

       break;

    case 2:

       if (energy<p->psv[2]) {

         /* a power law
          * 
          * psv[0]*energy^psv[1]
          */

	 passive_ind=p->psv[0]*pow(energy,p->psv[1]);

         headas_chat(5,"using case 2 (low energies): \n");

       } else if ((energy>=p->psv[2])&&(energy<p->psv[8])) {

         /* a smoothly broken power law
          * 
          * [(psv[3]*energy^psv[4])^(1/psv[7])+
          *  (psv[5]*energy^psv[6])^(1/psv[7])]^psv[7]
          */

         passive_ind=pow(pow(p->psv[3]*pow(energy,p->psv[4]),
	       1/p->psv[7])+pow(p->psv[5]*pow(energy,
	           p->psv[6]),1/p->psv[7]),p->psv[7]);

         headas_chat(5,"using case 2 (mid energies): \n");

       } else {

         /* a smoothly broken power law
          * 
          * [(psv[9]*energy^psv[10])^(1/psv[13])+
          *  (psv[11]*energy^psv[12])^(1/psv[13])]^psv[13]
          */

         passive_ind=pow(pow(p->psv[9]*pow(energy,p->psv[10]),
	       1/p->psv[13])+pow(p->psv[11]*pow(energy,
	           p->psv[12]),1/p->psv[13]),p->psv[13]);

         headas_chat(5,"using case 2 (high energies): \n");

       }
       break;

    case 3:

       if (energy<p->psv[2]) {

         /* a power law
          * 
          * psv[0]*energy^psv[1]
          */

	 passive_ind=p->psv[0]*pow(energy,p->psv[1]);

         headas_chat(5,"using case 2 (low energies): \n");

       } else if ((energy>=p->psv[2])&&(energy<p->psv[8])) {

         /* a smoothly broken power law
          * 
          * [(psv[3]*energy^psv[4])^(1/psv[7])+
          *  (psv[5]*energy^psv[6])^(1/psv[7])]^psv[7]
          */

         passive_ind=pow(pow(p->psv[3]*pow(energy,p->psv[4]),
	       1/p->psv[7])+pow(p->psv[5]*pow(energy,
	           p->psv[6]),1/p->psv[7]),p->psv[7]);

         headas_chat(5,"using case 2 (mid energies): \n");

       } else {

         /* a smoothly broken power law
          * 
          * [(psv[9]*energy^psv[10])^(1/psv[13])+
          *  (psv[11]*energy^psv[12])^(1/psv[13])]^psv[13]
          */

         passive_ind=pow(pow(p->psv[9]*pow(energy,p->psv[10]),
	       1/p->psv[13])+pow(p->psv[11]*pow(energy,
	           p->psv[12]),1/p->psv[13]),p->psv[13]);

         headas_chat(5,"using case 2 (high energies): \n");

       }
       break;

    case 4:

       if (energy<p->psv[2]) {

         /* a power law
          * 
          * psv[0]*energy^psv[1]
          */

	 passive_ind=p->psv[0]*pow(energy,p->psv[1]);

         headas_chat(5,"using case 2 (low energies): \n");

       } else if ((energy>=p->psv[2])&&(energy<p->psv[8])) {

         /* a smoothly broken power law
          * 
          * [(psv[3]*energy^psv[4])^(1/psv[7])+
          *  (psv[5]*energy^psv[6])^(1/psv[7])]^psv[7]
          */

         passive_ind=pow(pow(p->psv[3]*pow(energy,p->psv[4]),
	       1/p->psv[7])+pow(p->psv[5]*pow(energy,
	           p->psv[6]),1/p->psv[7]),p->psv[7]);

         headas_chat(5,"using case 2 (mid energies): \n");

       } else {

         /* a smoothly broken power law
          * 
          * [(psv[9]*energy^psv[10])^(1/psv[13])+
          *  (psv[11]*energy^psv[12])^(1/psv[13])]^psv[13]
          */

         passive_ind=pow(pow(p->psv[9]*pow(energy,p->psv[10]),
	       1/p->psv[13])+pow(p->psv[11]*pow(energy,
	           p->psv[12]),1/p->psv[13]),p->psv[13]);

         headas_chat(5,"using case 2 (high energies): \n");
       }
       break;

    default:
      fprintf(stderr,
          "ERROR: batdrmgen does not recognize format version %d\n",
	  p->batfilev);
      return 1;
      break;
  }

  headas_chat(5,"passive materials effective mu*t: %f\n",passive_ind);

  /* t_passive: transmission coefficient through passive materials */

  t_passive=exp(-1*passive_ind/cos_theta);
  headas_chat(4,"transmission thru passive materials: %f\n",t_passive);

  /* pb_mu: cross section of lead (a power law) */

  if (energy < 88.0)
    pb_mu=p->pb_low_coeff*pow(energy,p->pb_low_index);
  else
    pb_mu=pow(
      pow(p->pb_mid_coeff*pow(energy,p->pb_mid_index),p->pb_smooth)+
      pow(p->pb_high_coeff*pow(energy,p->pb_high_index),p->pb_smooth),
      1.0/p->pb_smooth);
  headas_chat(5,"lead cross section: %f\n",pb_mu);

  /* t_pb: transmission coefficient through centers of lead tiles */

  t_pb = exp(-0.1*pb_mu*p->pb_density/cos_theta);
  headas_chat(5,"transmission through centers of lead tiles: %f\n",t_pb);

  /* t_pb_near: transmission coefficient near edges of lead tiles */

  t_pb_near=cos_theta/(0.05*pb_mu*p->pb_density)*
    (1.0-exp(-0.05*pb_mu*p->pb_density/cos_theta));
  headas_chat(5,"transmission through near edges of lead tiles: %f\n",
      t_pb_near);

  /* t_pb_far: transmission coefficient far edges of lead tiles */

  t_pb_far=cos_theta/(0.05*pb_mu*p->pb_density)*
    (exp(-0.05*pb_mu*p->pb_density/cos_theta)-
     exp(-0.1*pb_mu*p->pb_density/cos_theta));
  headas_chat(5,"transmission through far edges of lead tiles: %f\n",
      t_pb_far);

  /* edge_fraction: fraction of a lead tile that is made up of an edge */

  nonedge_fraction=(1.0-0.2*tan_theta_x)*(1.0-0.2*tan_theta_y);
  headas_chat(5,"fraction of lead tile that is not an edge: %f\n",
	      nonedge_fraction);

  edge_fraction = 1.0 - nonedge_fraction;
  edge_fraction = p->pb_edge_norm * edge_fraction;
  headas_chat(5,"fraction of lead tile that is an edge: %f (renormalized by PB_EDGEN)\n",
	      edge_fraction);

  /* t_pb_overall: overall absorption by lead */

  t_pb_overall=(1.0 - edge_fraction)*(1.0-t_pb)+
    (edge_fraction)*(t_pb_near-t_pb_far);
  headas_chat(4,"overall absorption by lead tiles: %f\n",t_pb_overall);

  if (!flight_data) {

    /* src ind (a smoothly broken power law)
     * 
     * [(src_low_coeff*energy^src_low_index)^(1/src_smooth)+
     * (src_high_coeff*energy^src_high_index)^(1/src_smooth)]^
     * src_smooth
     *
     */

    src_ind=pow(pow(p->src_low_coeff*pow(energy,p->src_low_index),
	  1/p->src_smooth)+pow(p->src_low_coeff*pow(energy,
	      p->src_low_index),1/p->src_smooth),p->src_smooth);
    headas_chat(5,"power law index for source packaging: %f\n", src_ind);

    /* t_src: transmission coefficient through the source packaging */

    t_src=exp(-1*p->src_density*src_ind*p->src_thickness/cos_theta);
    headas_chat(4,"transmission through source packaging: %f\n", t_src);

    /* air ind (a smoothly broken power law)
     * 
     * [(air_low_coeff*energy^air_low_index)^(1/air_smooth)+
     * (air_high_coeff*energy^air_high_index)^(1/air_smooth)]^
     * air_smooth
     *
     */

    air_ind=pow(pow(p->air_low_coeff*pow(energy,p->air_low_index),
	  1/p->air_smooth)+pow(p->air_low_coeff*pow(energy,
	      p->air_low_index),1/p->air_smooth),p->air_smooth);
    headas_chat(5,"power law index for air: %f\n", air_ind);

    /* t_air: transmission coefficient through a column of air */

    t_air=exp(-1*p->air_density*air_ind*r);
    headas_chat(4,"transmission through air: %f\n", t_air);

    /* all transmission coefficients combined 
     * (including source packaging and air) */

    t=t_src*t_air*t_passive*t_pb_overall;
  }
  else {

    /* all transmission coefficients combined 
     * (excluding source packaging and air) */

    t=t_passive*t_pb_overall;
  
  }
  headas_chat(5,"...Leaving trans...\n");
  return t;
}
  

int mutau_model (
    float mutaue, float mutauh, float voltage, float gain_adjust, int n_depths,
    int n_bins, float energy, float norm, double *emax, float *dist, 
    double *result)

/* function: mutau_model
 *
 * Adds mutau model for this distribution to "result"
 *
 * Inputs:
 *
 *   mutaue:       mu-tau product for electrons (cm^2/V/s)
 *   mutauh:       mu-tau product for holes (cm^2/V/s)
 *   voltage:      bias voltage on detectors (V)
 *   gain_adjust:  gain adjustment factor (dimensionless)
 *   n_depths:     number of values in the "dist" array
 *   n_bins:       number of values in the "emax" and "result" arrays
 *   energy:       energy deposited in the detector (keV)
 *   norm:         normalization factor (dimensionless)
 *   emax:         an array containing the upper edges of the pha bins
 *   dist:         an array of size n_depths containing the depth 
 *                 distribution values for this particular deposited energy
 *
 *   result:   counts spectrum in units of effective area (cm^2)
 *             (modified to include mutau model for this distribution)
 */

{

  float lambda_e = mutaue*voltage/DET_THICKNESS;
  float lambda_h = mutauh*voltage/DET_THICKNESS;
  float dx = DET_THICKNESS/(float)n_depths;
  float max_hecht_depth = lambda_h/(lambda_e+lambda_h)*DET_THICKNESS;

  float depth;
  float slice_eff_area;
  float eff_energy;
  int i, j;

  /* headas_chat(5,"...Inside mutau_model...\n"); */

  /* loop through each element in the distribution "dist"
   * (each detector "slice") */

  for (i=0;i<n_depths;i++) {

    depth = (i+0.5)*dx;
    slice_eff_area = dist[i]*dx;

    /* find the effective energy at that depth */

    eff_energy = energy * hecht(lambda_e,lambda_h,depth) /
      hecht(lambda_e,lambda_h,max_hecht_depth) * gain_adjust;

    /* make sure eff_energy is below the highest bin edge
     * (if not, don't include counts associated with it) */

    if (eff_energy <= emax[n_bins-1]) {

      /* find the bin (j) that corresponds to eff_energy */

      j=0;
      while (emax[j]<eff_energy) j++;	 

      /* add norm*slice_eff_area to the contents of that ph bin */
    
      result[j]+=norm*slice_eff_area;

    }
  }
  /* headas_chat(5,"...Leaving mutau_model...\n"); */
  return 0;
}


double erf_fast(float x)

/* function: erf_fast
 * 
 * computes the error function erf(x) using an approximation that is
 * computationally efficient and remarkably precise
 *
 * Based upon "erfcc" from
 *      "Numerical Recipes in C++: The Art of Scientific
 *      Computing", Second Edition, by
 *      W. Press, S. Teukolsky, W. Vetterling, and B. Flannery
 *      Cambridge University Press, 2002, p. 226
 */

{
  float t,z;
  double ans;

  z=fabs(x);
  t=1.0/(1.0+0.5*z);
  ans=t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+
    t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
    t*(-0.82215223+t*0.17087277)))))))));
  return x >= 0.0 ? 1.0-ans : ans-1.0;
}


int gauss_convolve (
    struct resp_struct *resp, float sigma,
    double *input_spec, double *output_spec
    )

/* function: gauss_convolve
 *
 * Convolves a spectrum with a gaussian to reproduce effect of
 * imperfect energy resolution
 *
 * Inputs:
 *   resp:         structure with response parameters
 *   sigma:        standard deviation for gaussian
 *   input_spec:   spectrum in effective area units (cm^2)
 *                 (before convolving with gaussian)
 *
 * Output:
 *   output_spec:  spectrum in effective area units (cm^2)
 *                 (after convolving with gaussian)
 *
 * The calling function is responsible to make sure that output_spec
 * has enough memory allocated
 */

{
  float sig_limit = 4.0;
  float sqrt_2 = 1.41421;     /* sqrt(2) */

  int input_bin;
  double input_bin_e;

  int output_bin;
  int min_output_bin=0, max_output_bin=0;
  double min_output_bin_e, max_output_bin_e;

  double input_bin_contents;

  headas_chat(5,"...Inside gauss_convolve...\n");

  /* initialize output_spec */

  for (output_bin=0;output_bin<resp->nphabins;output_bin++)
    output_spec[output_bin]=0;

  /* loop through all of the bins */

  for (input_bin=0;input_bin<resp->nphabins_pre;input_bin++) {

    /* the contents of the current bin */

    input_bin_contents=input_spec[input_bin];

    if (sigma == 0) {
      output_bin = input_bin;
      output_spec[output_bin] = input_bin_contents;
      continue;
    }

    /* the energy corresponding to the center of the current bin */

    input_bin_e=(resp->emin_pre[input_bin]+resp->emax_pre[input_bin])/2.0;

    /* find the energies corresponding to sig_limit*sigma above and
     * below current energy
     * 
     * these are the energies between which the convolution will be
     * done (outside this range of energies, the convolution is
     * assumed to be neglibibly small) */

    min_output_bin_e=input_bin_e-sigma*sig_limit;
    if (min_output_bin_e < resp->emin[0])
      min_output_bin_e = resp->emin[0];
    max_output_bin_e=input_bin_e+sigma*sig_limit;
    if (max_output_bin_e > resp->emax[resp->nphabins-1]) 
      max_output_bin_e = resp->emax[resp->nphabins-1];

    /* find the bin numbers corresponding to these two energies */

    while (resp->emax[min_output_bin]<min_output_bin_e) min_output_bin++;
    max_output_bin=min_output_bin;
    while (resp->emax[max_output_bin]<max_output_bin_e) max_output_bin++;

    /* loop through these bins */

    for (output_bin=min_output_bin;output_bin<=max_output_bin;output_bin++) {

      /* the probability that a count at the center of 
       * the input bin (energy = e) will be redistributed
       * to a bin with edges between emin and emax is given by:
       *
       *    0.5*{erf[(emax+e)/(sqrt(2)*sigma)]-
       *         erf[(emin+e)/(sqrt(2)*sigma)]}
       */

      output_spec[output_bin]+=input_bin_contents*0.5*
	(erf_fast((resp->emax[output_bin]-input_bin_e)/(sqrt_2*sigma))-
	 erf_fast((resp->emin[output_bin]-input_bin_e)/(sqrt_2*sigma)));

    }
  }
  headas_chat(5,"...Leaving gauss_convolve...\n");
  return 0;
}


int multi_mutau_func (
    struct resp_struct *resp, struct multi_mt_struct *cal_parms,
    int n_distvectors, int n_depths, float *dist, float energy,
    int flight_data, double *result)
{

/* function: multi_mutau_func
 *
 * generates a column in the response matrix, corresponding to a particular
 * incident photon energy
 *
 * Inputs:
 *
 *   resp:          structure containing response parameters
 *   cal_parms:     structure containing calibration parameters
 *   n_distvectors: number of different peaks for each incident photon energy
 *                    (each having its own distribution)
 *                    (for example, 1 main peak + 2 escape peaks = 3)
 *   n_depths:      number of values in each distribution
 *   dist:          an (n_distvectors)x(n_depths) array containing the depth 
 *                    distribution values for this particular incident photon 
 *                    energy and incident angle
 *   energy:         incident photon energy (keV)
 *
 *   result:   counts spectrum in units of effective area (cm^2)
 *             for this particular incident photon energy and incident angle)
 */

  float dist_eff_area, dx;
  float transmission; /*, x, y, z, x0, y0, r, rsq, cos_theta, cos_theta_ff */
  float norm_this_mt;
  int i, status, mt_index, n_pre_gauss;
  float *dist_ptr;
  double *result_pre_gauss;
  float energy_deposit[3]; 
  float gain_adjust;
  float sigma;
  double temp_sum;

  headas_chat(5,"...Inside multi_mutau_func...\n");

  n_pre_gauss=resp->nphabins*SUB_BINS;

  energy_deposit[0] = energy;
  energy_deposit[1] = energy-EK1_CD;
  energy_deposit[2] = energy-EK1_TE;

  headas_chat(4,"energy_deposit[0]: %f\n",energy_deposit[0]);
  headas_chat(4,"energy_deposit[1]: %f\n",energy_deposit[1]);
  headas_chat(4,"energy_deposit[2]: %f\n",energy_deposit[2]);
  if (energy_deposit[2]<0)
    headas_chat(4,"  (those < 0 are ignored)\n");

  /* dx: slice thickness */

  dx = DET_THICKNESS/(float)n_depths;
  headas_chat(4,"dx: %f\n",dx);

  /* gain_adjust: gain correction factor */

  if (cal_parms->use_gain_adj) {
    if (energy<cal_parms->gain_adj[0]) 
      gain_adjust=cal_parms->gain_adj[1]*energy*energy+
	cal_parms->gain_adj[2]*energy+cal_parms->gain_adj[3];
    else gain_adjust=cal_parms->gain_adj[4]*energy+cal_parms->gain_adj[5];
  } else {
    gain_adjust=cal_parms->gain_coeff*pow(energy,cal_parms->gain_index); 
  }

  headas_chat(4,"gain_adjust: %f\n",gain_adjust);

  /* sigma: energy resolution standard deviation */

  if (cal_parms->use_sigma) {
    if (energy<cal_parms->sigma_coeffs[0]) 
      sigma=cal_parms->sigma_coeffs[1]*energy*energy+
	cal_parms->sigma_coeffs[2]*energy+cal_parms->sigma_coeffs[3];
    else 
      sigma=cal_parms->sigma_coeffs[4]*energy*energy+
	cal_parms->sigma_coeffs[5]*energy+cal_parms->sigma_coeffs[6];
  } else {
    sigma=cal_parms->sigma;
  }
  
  headas_chat(4,"sigma: %f\n",sigma);

  /* allocate memory for and initialize result_pre_gauss */

  result_pre_gauss = (double *) malloc (resp->nphabins_pre*sizeof(double));
  for (i=0;i<resp->nphabins_pre;i++) result_pre_gauss[i]=0;

  /* loop over all pairs of mu-tau values */

  for (mt_index=0;mt_index<N_MT;mt_index++) {

    /* calculate normalization factor of the model with this
     * pair of mutau values 
     * (scaled down by fraction[mt_index]) */

    norm_this_mt=(cal_parms->norm)*(cal_parms->fraction[mt_index]);

    /* do the main peak */

    status=mutau_model(cal_parms->mutau_e[mt_index],
	cal_parms->mutau_h[mt_index],cal_parms->voltage,gain_adjust,n_depths,
	resp->nphabins_pre,energy_deposit[0],norm_this_mt,resp->emax_pre,
	dist,result_pre_gauss);
	
    
    /* do the Cd escape peak */

    if (energy_deposit[0]>CD_EDGE) {
        dist_ptr=dist+n_depths;
        status=mutau_model(cal_parms->mutau_e[mt_index],
	    cal_parms->mutau_h[mt_index],cal_parms->voltage,gain_adjust,
	    n_depths,resp->nphabins_pre,energy_deposit[1],norm_this_mt,
	    resp->emax_pre,dist_ptr,result_pre_gauss);
    }

    /* do the Te escape peak */

    if (energy_deposit[0]>TE_EDGE) {
        dist_ptr=dist+2*n_depths;
        status=mutau_model(cal_parms->mutau_e[mt_index],
	    cal_parms->mutau_h[mt_index],cal_parms->voltage,gain_adjust,
	    n_depths,resp->nphabins_pre,energy_deposit[2],norm_this_mt,
	    resp->emax_pre,dist_ptr,result_pre_gauss);
    }
  }

  /* report total of result_pre_gauss
   *
   * Note: this value is equal to
   *   dist_eff_area * cal_parms->norm * sum(cal_parms->fraction[mt_index]);
   *   (not just dist_eff_area)
   */

  temp_sum=0;
  for (i=0;i<resp->nphabins_pre;i++) temp_sum+=result_pre_gauss[i];
  headas_chat(4,"total of result_pre_gauss (before exp tail): %f\n",temp_sum);

  /* calculate the exponential tail */

    /* dist_eff_area: 
     *   total "model" effective area from the distribution */

    dist_eff_area = 0;
    for (i=0;i<n_distvectors*n_depths;i++) 
      dist_eff_area += dist[i]*dx;
    headas_chat(4,"dist_eff_area: %f\n",dist_eff_area);

    /* calculate tail and add it to result_pre_gauss */

    status=exp_tail(resp,cal_parms,energy,dist_eff_area,result_pre_gauss);

    temp_sum=0;
    for (i=0;i<resp->nphabins_pre;i++) temp_sum+=result_pre_gauss[i];
    headas_chat(4,"total of result_pre_gauss (after exp tail): %f\n",temp_sum);

  /* calculate transmission through passive materials */

  transmission=trans(cal_parms,flight_data,energy,resp->srcpos);
  headas_chat(4,"transmission: %f\n",transmission);

  /* multiply result_pre_gauss by:
   *   - transmission through passive materials
   *   - mskwtsqf
   *
   * Note:
   *    mskwtsqf includes only enabled, coded detectors
   */

  for (i=0;i<resp->nphabins_pre;i++) 
    result_pre_gauss[i]*=transmission*resp->mskwtsqf;
  headas_chat(4,"multiply by transmission (%f) & mskwtsqf (%e)\n",
      transmission,resp->mskwtsqf);

  temp_sum=0;
  for (i=0;i<resp->nphabins_pre;i++) temp_sum+=result_pre_gauss[i];
  headas_chat(4,"total of result_pre_gauss (after trans & mskwtsqf): %e\n",
      temp_sum);

  /* optional corrections:
   *   - multiply by flatfielding term (if ffapp equal 1)
   *   - multiply by ngoodpix (if ngpixapp equals 1)
   *   - multiply by pcodefr (if pcodeapp equals 1)
   *
   *   THESE CORRECTIONS ARE NOW APPLIED USING THE MSKWTSQF KEYWORD
   *   SO THIS CODE IS NO LONGER NEEDED
   */

  /* ======================= COMMENTED OUT ============================ 
     (these corrections are applied in batmaskwt** now )
  x=resp->srcpos[0];
  y=resp->srcpos[1];
  z=resp->srcpos[2];
  rsq=(x*x)+(y*y)+(z*z);
  r=sqrt(rsq);
  cos_theta=fabs(z/r);

  if (resp->ffapp) {
    x0 = x;  if (x0 == 0) x0 = 1e-20;  * Guard against divide-by-zero *
    y0 = y;  if (y0 == 0) y0 = 1e-20;
    cos_theta_ff=cos_theta+
      min(0.15,0.05*fabs(z/x0))*cos(atan(sqrt(y*y+z*z)/x0))+
      min(0.15,0.05*fabs(z/y0))*cos(atan(sqrt(x*x+z*z)/y0));
    headas_chat(5,"multiply by flatfielding factor (%f)\n",cos_theta_ff);
    for (i=0;i<resp->nphabins;i++) result_pre_gauss[i]*=cos_theta_ff;
  }

  if (resp->ngpixapp) {
    for (i=0;i<resp->nphabins;i++) result_pre_gauss[i]*=resp->ngoodpix;
    headas_chat(5,"multiply by ngoodpix (%f)\n",resp->ngoodpix);
  }  

  if (resp->pcodeapp) {
    for (i=0;i<resp->nphabins;i++) result_pre_gauss[i]*=resp->pcodefr;
    headas_chat(5,"multiply by pcodefr (%f)\n",resp->pcodefr);
  } 

  temp_sum=0;
  for (i=0;i<resp->nphabins;i++) temp_sum+=result_pre_gauss[i];
  headas_chat(4,"total of result_pre_gauss (after optional corrections): %e\n",
      temp_sum);
  ======================= END COMMENTED SECTION ===================== 
  */

  /* convolve result_pre_gauss with a gaussian */

  status=gauss_convolve(resp,sigma,result_pre_gauss,result);

  temp_sum=0;
  for (i=0;i<resp->nphabins;i++) temp_sum+=result[i];
  headas_chat(4,"total of result (after gauss_convolve): %e\n",
      temp_sum);

  /* free memory used by result_pre_gauss */

  free (result_pre_gauss);

  headas_chat(4,"...Leaving multi_mutau_func...\n");
  return 0;

}
