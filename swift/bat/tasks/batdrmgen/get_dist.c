/* ----------- get_dist.c - depth distribution generator------------ */
/* ----------------------------------------------------------------- */

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

void get_interp_vectors(long n_phi, long n_theta,
  long phi_ind, long theta_ind, long energy_ind,
  int n_depths, fitsfile *dist_fits,
  double lp_lt_le[], double hp_lt_le[], double lp_ht_le[],
  double lp_lt_he[], double hp_ht_le[], double hp_lt_he[],
  double lp_ht_he[], double hp_ht_he[])  {

long lp_lt_le_ind;
long hp_lt_le_ind;
long lp_ht_le_ind;
long lp_lt_he_ind;
long hp_ht_le_ind;
long hp_lt_he_ind;
long lp_ht_he_ind;
long hp_ht_he_ind;
long n_depthsl;
int status=0;



/* get_interp_vectors reads in (from a fits file) the 8 distribution 
 *  vectors that correspond to:                                     
 *  the phi values indexed by phi_ind and phi_ind+1,            
 *  the theta values indexed by theta_ind and theta_ind+1, and  
 *  the energy values indexed by energ_ind and energy_ind+1    
 */

  headas_chat(5," Using get_interp_vectors to get the theta, phi\n");
  headas_chat(5," and energy depth dist vectors for interpolaton\n");

/*  calculate the row numbers of the vectors to extract           */

lp_lt_le_ind=1+phi_ind+n_phi*(theta_ind+n_theta*energy_ind);
hp_lt_le_ind=1+phi_ind+1+n_phi*(theta_ind+n_theta*energy_ind);
lp_ht_le_ind=1+phi_ind+n_phi*(theta_ind+1+n_theta*energy_ind);
lp_lt_he_ind=1+phi_ind+n_phi*(theta_ind+n_theta*(energy_ind+1));
hp_ht_le_ind=1+phi_ind+1+n_phi*(theta_ind+1+n_theta*energy_ind);
hp_lt_he_ind=1+phi_ind+1+n_phi*(theta_ind+n_theta*(energy_ind+1));
lp_ht_he_ind=1+phi_ind+n_phi*(theta_ind+1+n_theta*(energy_ind+1));
hp_ht_he_ind=1+phi_ind+1+n_phi*(theta_ind+1+n_theta*(energy_ind+1));
n_depthsl = n_depths;

/*  read the 8 distribution vectors  */

fits_movnam_hdu(dist_fits,BINARY_TBL,"PROBABILITY",0,&status);
fits_read_col(dist_fits,TDOUBLE,1,lp_lt_le_ind,1,n_depthsl,0,
    lp_lt_le,0,&status);			 
fits_read_col(dist_fits,TDOUBLE,1,hp_lt_le_ind,1,n_depthsl,0,
    hp_lt_le,0,&status);			 
fits_read_col(dist_fits,TDOUBLE,1,lp_ht_le_ind,1,n_depthsl,0,
    lp_ht_le,0,&status);			 
fits_read_col(dist_fits,TDOUBLE,1,lp_lt_he_ind,1,n_depthsl,0,
    lp_lt_he,0,&status);			 
fits_read_col(dist_fits,TDOUBLE,1,hp_ht_le_ind,1,n_depthsl,0,
    hp_ht_le,0,&status);			 
fits_read_col(dist_fits,TDOUBLE,1,hp_lt_he_ind,1,n_depthsl,0,
    hp_lt_he,0,&status);			 
fits_read_col(dist_fits,TDOUBLE,1,lp_ht_he_ind,1,n_depthsl,0,
    lp_ht_he,0,&status);			 
fits_read_col(dist_fits,TDOUBLE,1,hp_ht_he_ind,1,n_depthsl,0,
    hp_ht_he,0,&status);

}


long search_vector (float vector[], long size, float target_value) 
{


/*  searches a vector, the elements of which increase in value    */
/*  as the index increases, and finds the index for which         */
/*      vector[index] <= target_value < vector[index+1]           */
/*                                                                */
/*      exception: if target_value == the last element in the     */
/*          vector, then index = sizeof(vector)-2                 */
/*                                                                */
/*  inputs:                                                       */
/*      vector[]:                                                 */
/*         a 1-dimensional vector to search                       */
/*         (the elements of vector must increase in value with    */
/*         increasing index)                                      */
/*      size:                                                     */   
/*         the number of elements in the vector to search         */
/*      target_value:                                             */
/*         the value to search for in the array                   */
/*                                                                */
/*  output:                                                       */
/*     the index for which                                        */
/*         vector[index] <= target_value < vector[index+1]        */
/*                                                                */
/*	   if target_value==vector[size-1], size-2 is returned    */
/*                                                                */
/*     if target_value is outside the range of the vector         */
/*         -1 is returned                                         */

    double middle_value_lower, middle_value_upper;
    long middle_position;
    long low=0, high=size-1;


    headas_chat(5,"Using search_vector to get the indeces where the\n");
    headas_chat(5,"target value is bounded by the vector values\n");

    while (low < high) {

        middle_position=(low+high)/2;
        middle_value_lower=vector[middle_position];
        middle_value_upper=vector[middle_position+1];
        if ((middle_value_lower <= target_value)
            && (target_value < middle_value_upper))
            return middle_position;
        else if (middle_value_lower > target_value)
                high = middle_position;
        else low = middle_position+1;
    }

    if (target_value==vector[size-1]) return size-2;

    return -1;
}


int dist_interp (double lp_lt_le[], double hp_lt_le[], 
    double lp_ht_le[], double lp_lt_he[], double hp_ht_le[], 
    double hp_lt_he[], double lp_ht_he[], double hp_ht_he[], 
    double dist[], int n_depths,
    float lp, float hp, float lt, float ht, float le,
    float he, float p, float e, float t)
{


/*  this function uses bilinear interpolation to find the       */
/*  distribution vector at the given phi (p), theta (t),        */
/*  and energy (e)                                              */
/*                                                              */
/*  bilinear interpolation algorithm is taken from              */
/*      "Numerical Recipes in C++: The Art of Scientific        */
/*      Computing", Second Edition, by                          */
/*      W. Press, S. Teukolsky, W. Vetterling, and B. Flannery  */
/*      Cambridge University Press, 2002, p. 126                */
/*                                                              */
/*  inputs:                                                     */
/*	lp_lt_le:                                               */
/*	    distribution for low phi, low theta, low energy     */
/*	hp_lt_le:                                               */
/*	    distribution for high phi, low theta, low energy    */
/*	lp_ht_le:                                               */
/*	    distribution for low phi, high theta, low energy    */
/*	lp_lt_he:                                               */
/*	    distribution for low phi, low theta, high energy    */
/*	hp_ht_le:                                               */
/*	    distribution for high phi, high theta, low energy   */
/*	hp_lt_he:                                               */
/*	    distribution for high phi, low theta, high energy   */
/*	lp_ht_he:                                               */
/*	    distribution for low phi, high theta, high energy   */
/*	hp_ht_he:                                               */
/*	    distribution for high phi, high theta, high energy  */
/*      dist (output):                                          */
/*	    distribution vector at the specified phi,           */
/*	    theta, and energy                                   */
/*                                                              */
/*	n_depths:                                               */
/*	    the number of elements in the distributions         */
/*                                                              */
/*      lp:                                                     */
/*          lower phi between which to interpolate              */
/*      hp:                                                     */
/*          higher phi between which to interpolate             */
/*      lt:                                                     */
/*          lower theta between which to interpolate            */
/*      ht:                                                     */
/*          higher theta between which to interpolate           */
/*      le:                                                     */
/*          lower energy between which to interpolate           */
/*      he:                                                     */
/*          higher energy between which to interpolate          */
/*                                                              */
/*	p:  phi of the distribution that is sought              */
/*	t:  theta of the distribution that is sought            */
/*	e:  energy of the distribution that is sought           */
/*                                                              */

    int i;
    float e_param, t_param, p_param;

    headas_chat(5,"Using dist_interp to get the interpolated\n");
    headas_chat(5,"depth distribution vector\n");

/*  calculate p_param, t_param, and e_param                     */
/*  (for use in bilinear interpolation)                         */

    p_param = (p - lp) / (hp-lp);
    t_param = (t - lt) / (ht-lt);
    e_param = (e - le) / (he-le);

/*  use bilinear interpolation to calculate dist                 */

    for (i=0;i<n_depths;i++) dist[i]=
	    (1-p_param)*(1-t_param)*(1-e_param)*
		lp_lt_le[i]+
            (p_param)*(1-t_param)*(1-e_param)*
		hp_lt_le[i]+
            (1-p_param)*(t_param)*(1-e_param)*
		lp_ht_le[i]+
	    (1-p_param)*(1-t_param)*(e_param)*
		lp_lt_he[i]+
	    (p_param)*(t_param)*(1-e_param)*
		hp_ht_le[i]+
	    (p_param)*(1-t_param)*(e_param)*
		hp_lt_he[i]+
	    (1-p_param)*(t_param)*(e_param)*
		lp_ht_he[i]+
	    (p_param)*(t_param)*(e_param)*
		hp_ht_he[i];

return 0;
}


int get_dist(float phi, float theta, float energy, 
             char *dist_name, int *n_depths,
             float **depths,double **dist) {

  

/*  returns the energy deposition distribution for a given         */
/*  phi, theta, and energy                                         */
/*                                                                 */
/*  if the phi, theta, or energy is out of the range of those      */
/*  included in the dist fits file, dist is returned filled        */
/*  with 0's                                                       */
/*                                                                 */
/*  inputs:                                                        */
/*      phi: the angle phi (in degrees)                            */
/*      theta: the angle theta (in degrees)                        */
/*      energy: the energy (in keV)                                */
/*      dist_name: the name of the fits file that holds the        */
/*          distributions between which to interpolate             */
/*                                                                 */
/*  outputs:                                                       */
/*      n_depths: the size (number of depths) in the distribution  */
/*      depths: an array giving the depths at which the            */
/*          distribution is calculated                             */
/*      dist: an array giving the probability of energy            */
/*          deposition per cm per flux at each of the depths       */
/*          for the phi, theta, and energy provided                */
/*                                                                 */
/*      depths and dist are dynamically allocated within this      */
/*          function, so they must be freed outside the function   */
/*          when no longer needed                                  */

fitsfile *dist_fits;

int status=0;
int i;

int n_depths_int;
long n_depths_long;
long n_phi;
long n_theta;
long n_energy;

float *phi_vector;
float *theta_vector;
float *energy_vector;
float *depths_temp;
double *dist_temp;

long phi_ind;
long theta_ind;
long energy_ind;

int return_val=0;

float lp,hp,lt,ht,le,he;

double *lp_lt_le;
double *hp_lt_le;
double *lp_ht_le;
double *lp_lt_he;
double *hp_ht_le;
double *hp_lt_he;
double *lp_ht_he;
double *hp_ht_he;

headas_chat(5,"Using get_dist to construct the appropriate\n");
headas_chat(5,"depth dist. vector given phi, theta & energy \n");
headas_chat(5,"Open the fits file for reading\n");

if (fits_open_file(&dist_fits, dist_name, READONLY, &status)) {
    fits_report_error(stderr,status);
    exit(status);
}

headas_chat(5,"Read in the number of depths and the depths vector\n");

fits_movnam_hdu(dist_fits,BINARY_TBL,"DEPTHS",0,&status);
fits_get_num_rows(dist_fits,&n_depths_long,&status);
*n_depths=n_depths_long;
n_depths_int=*n_depths;
depths_temp = (float *) malloc (n_depths_int*sizeof(float));
fits_read_col(dist_fits,TFLOAT,1,1,1,n_depths_int,0,depths_temp,
    0,&status);
*depths=&depths_temp[0];

headas_chat(5,"Read in phi_vector, theta_vector, and energy_vector\n");

fits_movnam_hdu(dist_fits,BINARY_TBL,"PHI",0,&status);
fits_get_num_rows(dist_fits,&n_phi,&status);
phi_vector = (float *) malloc (n_phi*sizeof(float));
fits_read_col(dist_fits,TFLOAT,1,1,1,n_phi,0,phi_vector,0,&status);

fits_movnam_hdu(dist_fits,BINARY_TBL,"THETA",0,&status);
fits_get_num_rows(dist_fits,&n_theta,&status);
theta_vector = (float *) malloc (n_theta*sizeof(float));
fits_read_col(dist_fits,TFLOAT,1,1,1,n_theta,0,theta_vector,
    0,&status);

fits_movnam_hdu(dist_fits,BINARY_TBL,"ENERGY",0,&status);
fits_get_num_rows(dist_fits,&n_energy,&status);
energy_vector = (float *) malloc (n_energy*sizeof(float));
fits_read_col(dist_fits,TFLOAT,1,1,1,n_energy,0,energy_vector,
    0,&status);

headas_chat(5,"Find phi_ind, theta_ind, and energy_ind\n");

phi_ind=search_vector(phi_vector,4,phi);
theta_ind=search_vector(theta_vector,11,theta);
energy_ind=search_vector(energy_vector,30,energy);
    
headas_chat(5,"energy = %f\n",energy);


dist_temp = (double *) malloc (sizeof(double)*n_depths_int);

if (((phi_ind!=-1)&&(theta_ind!=-1))&&(energy_ind!=-1)) {

    headas_chat(5,"Find the lower and upper phi, theta, and\n"); 
    headas_chat(5,"energy between which to interpolate\n"); 

    lp = phi_vector[phi_ind];
    hp = phi_vector[phi_ind+1];
    lt = theta_vector[theta_ind];
    ht = theta_vector[theta_ind+1];
    le = energy_vector[energy_ind];
    he = energy_vector[energy_ind+1];

    headas_chat(5,"Allocate memory for the 8 vectors between\n"); 
    headas_chat(5,"which to interpolate\n");

    lp_lt_le = (double *) malloc (n_depths_long*sizeof(double));
    hp_lt_le = (double *) malloc (n_depths_long*sizeof(double));
    lp_ht_le = (double *) malloc (n_depths_long*sizeof(double));
    lp_lt_he = (double *) malloc (n_depths_long*sizeof(double));
    hp_ht_le = (double *) malloc (n_depths_long*sizeof(double));
    hp_lt_he = (double *) malloc (n_depths_long*sizeof(double));
    lp_ht_he = (double *) malloc (n_depths_long*sizeof(double));
    hp_ht_he = (double *) malloc (n_depths_long*sizeof(double));
    
    headas_chat(5,"Read the 8 vectors between which to interpolate\n");
    
    get_interp_vectors(n_phi, n_theta, phi_ind,theta_ind,energy_ind,
	n_depths_int,dist_fits,
	lp_lt_le,hp_lt_le,lp_ht_le,lp_lt_he,
	hp_ht_le,hp_lt_he,lp_ht_he,hp_ht_he);

    
    headas_chat(5,"Interpolate between those 8 vectors to get the dist\n");

    dist_interp(lp_lt_le,hp_lt_le,lp_ht_le,lp_lt_he,
	hp_ht_le,hp_lt_he,lp_ht_he,hp_ht_he,dist_temp,
	n_depths_int,lp,hp,lt,ht,le,he,
	phi,energy,theta);

    /*  free memory  */

    free (lp_lt_le);
    free (hp_lt_le);
    free (lp_ht_le);
    free (lp_lt_he);
    free (hp_ht_le);
    free (hp_lt_he);
    free (lp_ht_he);
    free (hp_ht_he);

}

else {
    headas_chat(5,"**one of the indeces was -1 so jumped to else\n");
    for (i=0;i<n_depths_int;i++) dist_temp[i] = 0;
    return_val=1;
}

/*  free memory  */

free (phi_vector);
free (theta_vector);
free (energy_vector);

headas_chat(5,"close the dist fits file\n");

fits_close_file(dist_fits,&status);

*dist=&dist_temp[0];

return return_val;

}
