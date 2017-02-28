/**
 * \file utils.c
 * \brief  Holds all low-level math and manipulation utilities.
 * \author David Riethmiller
 * \date $Date $
 */

#include "skyback.h"
#include <stdlib.h>
#include <stdio.h>
#include <funcWrappers.h>  /* Xspec defs, funcWrappers.cxx */
#include <math.h>
#include <ctype.h>
//#include <xlocale.h>
#include <sys/time.h>
#include <string.h>


/* FUNCTION NAME: MTseed                                      */
/*                                                            */
/* PURPOSE:                                                   */
/* Return an unsigned long int seed for use in HDmtInit(seed) */
/*                                                            */
/* CALLING SEQUENCE:  MTseed();                               */
/*                                                            */
/* INPUTS: none                                               */
/*                                                            */
/* OUTPUTS: returns unsigned long int from system time        */
/*                                                            */
/* CALLED BY: doWork(), timeset(), C_ReturnChannel(),         */
/*          imagedis(), sourcedis(), apply_vignette(),        */
/*          apply_psf(), gasdev(), poidev()                   */
/*                                                            */

unsigned long int MTseed(void){
    struct timeval tt;      /* time structure */
    unsigned int time_s=0;    /* time in seconds */
    unsigned int time_us=0;   /* time in micro seconds */
    unsigned long int seed=0; /* the seed to return */

    gettimeofday(&tt, NULL);                     /* get the system time */
    time_s = tt.tv_sec;             /* time in seconds */
    time_us = tt.tv_usec;           /* time in micro seconds */
    seed = time_s + time_us;   /* combine into powerfully random seed */

    return seed;
}



/* FUNCTION NAME: intmax                                      */
/*                                                            */
/* PURPOSE:                                                   */
/*  Return the greater of two ints.                           */
/*                                                            */
/* CALLING SEQUENCE:  intmax(a,b);                            */
/*                                                            */
/* INPUTS: a, b                                               */
/*                                                            */
/* OUTPUTS: returns the greater of a and b.                   */
/*                                                            */

int intmax(int a, int b){
    if (a >= b){ return a; } else { return b; }
}


/* FUNCTION NAME: intmin                                      */
/*                                                            */
/* PURPOSE:                                                   */
/*  Return the lesser of two ints.                            */
/*                                                            */
/* CALLING SEQUENCE:  intmin(a,b);                            */
/*                                                            */
/* INPUTS: a, b                                               */
/*                                                            */
/* OUTPUTS: returns the lesser of a and b.                    */
/*                                                            */

int intmin(int a, int b){
    if (a <= b){ return a; } else { return b; }
}


/* FUNCTION NAME: dblmax                                      */
/*                                                            */
/* PURPOSE:                                                   */
/*  Return the greater of two doubles.                           */
/*                                                            */
/* CALLING SEQUENCE:  dblmax(a,b);                            */
/*                                                            */
/* INPUTS: a, b                                               */
/*                                                            */
/* OUTPUTS: returns the greater of a and b.                   */
/*                                                            */

double dblmax(double a, double b){
    if (a >= b){ return a; } else { return b; }
}


/* FUNCTION NAME: dblmin                                      */
/*                                                            */
/* PURPOSE:                                                   */
/*  Return the lesser of two doubles.                            */
/*                                                            */
/* CALLING SEQUENCE:  dblmin(a,b);                            */
/*                                                            */
/* INPUTS: a, b                                               */
/*                                                            */
/* OUTPUTS: returns the lesser of a and b.                    */
/*                                                            */

double dblmin(double a, double b){
    if (a <= b){ return a; } else { return b; }
}



/* FUNCTION NAME: poidev                                                                           */
/*                                                                                                 */
/* CALLING SEQUENCE:                                                                               */
/*   result = poidev(mean, MTstate)                                                                */
/*                                                                                                 */
/* PURPOSE:                                                                                        */
/*   For a spectral channel with low counts, recompute the number of counts in the channel         */
/*   assuming Poisson statistics.  Taken directly from Numerical Recipies, but modified            */
/*   to use the Mersenne Twister algorithm as a random number generator, and a double version      */
/*   of PI.                                                                                        */
/*                                                                                                 */
/* INPUTS:                                                                                         */
/*   xxm,                                                                                          */
/*                                                                                                 */
/* OUTPUTS:                                                                                        */
/*   Returns the new value of number of counts in the channel.                                     */
/*                                                                                                 */
/* SUBROUTINES: MTseed()                                                                           */
/*                                                                                                 */


double poidev(double xm,
              HDmt_state *MTstate){      /* "save" state of Mersenne Twister RNG */

    static double sq, alxm, g, oldm=(-1.0);
    static double d_PI = 3.141592654e0;
    double em, t,y;

    if (xm < 12.0e0) {
        if (xm != oldm) {
            oldm = xm;
            g = exp(-xm);
        }
        em = -1.0e0;
        t = 1.0e0;
        do {
            em += 1.0;
            t *= HDmt_drand(MTstate);
        } while (t > g);
    } else {
        if (xm != oldm) {
            oldm = xm;
            sq = sqrt(2.0e0*xm);
            alxm = log(xm);
            g = xm*alxm - gammln(xm+1.0e0);
        }

        do {
            do{
                y = tan(d_PI * HDmt_drand(MTstate));
                em = sq*y + xm;
            } while (em < 0.0);
            em = floor(em);
            t = 0.9 * (1.+y*y) * exp(em * alxm - gammln(em+1.0) - g);
        } while (HDmt_drand(MTstate) > t);
    }


    return em;

} /* end function poidev  */




/* FUNCTION NAME: gammln                                             */
/*                                                                   */
/* CALLING SEQUENCE:                                                 */
/*   result = gammln(double xx);                                     */
/*                                                                   */
/* PURPOSE: Return natural log of gamma function                     */
/*                                                                   */
/* INPUT: xx  - input value                                          */
/*                                                                   */
/* OUTPUT: natural log of a gamma function for xx is returned        */
/*                                                                   */
/* CALLED BY:                                                        */
/*   poidev()                                                        */
/*                                                                   */
/* REVISION HISTORY                                                  */
/*   Taken directly from Numerical Recipes, modified to use double   */
/*   precision.                                                      */
/*                                                                   */

double gammln(double xx) {
    double ser, tmp, x;

    static double cof[6] = {76.18009172947146e0, -86.50532032941677e0, 24.01409824083091e0,
                            -1.231739572450155e0, 0.1208650973866179e-2,-0.5395239384953e-5};
    static double stp = 2.5066282746310005e0;
    int j=0;

    x = xx - 1.0;
    tmp = x + 5.5e0;
    tmp -= (x + 0.5e0) * log(tmp);
    ser = 1.0e0;
    for (j = 0; j <= 5; j++) {
        x += 1.0;
        ser += cof[j]/x;
    }
    return -tmp + log(stp * ser);
}




/* FUNCTION NAME: eta                                                */
/*                                                                   */
/* CALLING SEQUENCE:                                                 */
/*   eta(omal1f, omal1b, ymax, ymin);                                */
/*                                                                   */
/* PURPOSE: dimensionless N(>S) used to derive point source flux     */
/*                                                                   */
/* INPUT: sl1, sl2, xmax, x                                          */
/*                                                                   */
/* OUTPUT: eta                                                       */
/*                                                                   */
/* CALLED BY:                                                        */
/*   doWork()                                                        */
/*                                                                   */

double eta(double sl1, double sl2, double xmax, double x){

    double eta = 0.0;

    if (x > 1){ 
	if (sl2 != 0.0) {
	    eta = ( pow(xmax,sl2) - pow(x,sl2) ) / sl2;
	} else {
	    eta = log(xmax) - log(x);
	}
    } else {
	if (sl2 != 0.0) {
	    eta = ( pow(xmax,sl2) - 1.0) / sl2;
	} else {
	    eta = log(xmax);
	}
	
	if (sl1 != 0.0) {
	    eta += (1.0 - pow(x,sl1) ) / sl1;
	} else {
	    eta -= log(x);
	}
    }

    return eta;
}


/* FUNCTION NAME: skyrand                                                                  */
/*                                                                                         */
/* CALLING SEQUENCE:                                                                       */
/*         errstatus =  skyrand(ns_1, &s_obs, sincenterdec, coscenterdec, &ra_cat_1,       */ 
/*                              &dec_cat_1, MTstate);                                      */
/*                                                                                         */
/* PURPOSE: randomly populate the circular region defined by the radius and central        */
/*          coordinates ra and dec with ns positions, returned in the ns-sized     */
/*          vectors ra_cat and dec_cat.                                                    */
/*                                                                                         */
/* INPUT: ns, s_obs, sincenerdec, coscenterdec, MTstate                                    */
/*                                                                                         */
/* OUTPUT: ra_cat, dec_cat                                                                 */
/*                                                                                         */
/* CALLED BY:                                                                              */
/*   psources()                                                                            */
/*                                                                                         */

int skyrand(int ns,                     /* number of sources */
	    Obs_params_struct * s_obs,  /* structure containing observation parameters */
	    double sincenterdec,        /* sin of central dec */
	    double coscenterdec,        /* cos of central dec */
	    double ** ra_cat,           /* output catalog of RAs */
	    double ** dec_cat,          /* output catalog of DECs */
	    HDmt_state *MTstate){       /* state of random number generator */

    int ii = 0;
    double rphi = 0.0;
    double dphi = 0.0;
    double rtheta = 0.0;
    double dtheta = 0.0;
    double xrad = 0.0;
    double yrad = 0.0;
    double rrad = 0.0;
    double sinr = 0.0;
    double cosr = 0.0;
    double ra = 0.0;
    double dec = 0.0;
    double fract = 0.0;
    double sind = 0.0;
    double cosd = 0.0;
    double sina = 0.0;
    double cosa = 0.0;

    (*ra_cat) = calloc(ns, sizeof(double));
    (*dec_cat) = calloc(ns, sizeof(double));

    for (ii=0; ii<ns; ii++){
	rphi = HDmt_drand(MTstate);
	dphi = TWOPI * rphi;
	rtheta = HDmt_drand(MTstate);
	dtheta = sqrt(s_obs->radius * s_obs->radius * rtheta);
	
	xrad = D2R *(dtheta / 60.0) * cos(dphi);
	yrad = D2R *(dtheta / 60.0) * sin(dphi);

	rrad =  sqrt(xrad*xrad + yrad*yrad);
	sinr = sin(rrad);

	if (rrad == 0) {
	    ra = s_obs->ra;
	    dec = s_obs->dec;
	} else  {
	    fract = sinr / rrad;
	    cosr = cos(rrad);
	    sind = cosr*sincenterdec + fract*coscenterdec*yrad;
	    cosd = sqrt(1.0 - sind*sind);
	    if (cosd == 0) {
		dec = 90.0;
		if (sind < 0) dec = -dec;
		ra = s_obs->ra;
	    } else {
		dec = atan2(sind,cosd)/D2R;
		sina = fract * xrad / cosd;
		cosa = (cosr *coscenterdec - fract*sincenterdec*yrad) / cosd;
		ra = s_obs->ra + atan2(sina,cosa)/D2R;
	    }          
	}
	(*ra_cat)[ii] = ra;
	(*dec_cat)[ii] = dec;
    }


    return 0;

}

/* FUNCTION NAME: spec_string_to_type                                                      */
/*                                                                                         */
/* CALLING SEQUENCE:                                                                       */
/*       spectype = spec_string_to_type(s_logNS->specmod2);                               */
/*                                                                                         */
/* PURPOSE: convert string description of model type to int                                */
/*                                                                                         */
/* INPUT: spec_model                                                                       */
/*                                                                                         */
/* OUTPUT: spec_type                                                                       */
/*                                                                                         */
/* CALLED BY:                                                                              */
/*   doWork()                                                                              */
/*                                                                                         */

int spec_string_to_type(char * spec_model){
    int spectype = 0;

    /* match the string spec_model to its corresponding spectype int */

    if ( (0 == strcasecmp(spec_model,"pl")) ||
	 (0 == strcasecmp(spec_model,"plaw")) ||
	 (0 == strcasecmp(spec_model,"pow")) ||
	 (0 == strcasecmp(spec_model,"power")) ){
	spectype = 1;

    } else if ( (0 == strcasecmp(spec_model,"raymond-smith")) ||
		(0 == strcasecmp(spec_model,"raymond")) ||
		(0 == strcasecmp(spec_model,"ray")) ||
		(0 == strcasecmp(spec_model,"rs")) ){
	spectype = 2;

    } else if ( (0 == strcasecmp(spec_model,"blackbody")) ||
                (0 == strcasecmp(spec_model,"black")) ||
                (0 == strcasecmp(spec_model,"blac")) ||
                (0 == strcasecmp(spec_model,"bl")) ||
                (0 == strcasecmp(spec_model,"bbody")) ||
                (0 == strcasecmp(spec_model,"bb")) ){
        spectype = 3;

    } else if ( (0 == strcasecmp(spec_model,"bremsstrahlung")) ||
                (0 == strcasecmp(spec_model,"brems")) ||
                (0 == strcasecmp(spec_model,"brem")) ||
                (0 == strcasecmp(spec_model,"br")) ){
        spectype = 4;

    } else if ( (0 == strcasecmp(spec_model,"mono-energetic")) ||
                (0 == strcasecmp(spec_model,"mono")) ){
        spectype = 5;
    
    } else {
	printf("ERROR: unrecognized spec_model string: %s\n",spec_model);
	spectype = -1;
    }

    printf("   Matching spec_model %s to spectype %d.\n",spec_model,spectype);

    return spectype;
}


/* FUNCTION NAME: remove_substring                                   */
/*                                                                   */
/* CALLING SEQUENCE:                                                 */
/*    remove_substring(base,".fits");                                */
/*                                                                   */
/* PURPOSE: remove a substring from a string                         */
/*                                                                   */
/* INPUT: a string s, and a substring toremove                       */
/*                                                                   */
/* OUTPUT: the same string, without the substring                    */
/*                                                                   */
/* CALLED BY:                                                        */
/*    write_table_model()                                            */

void remove_substring(char *s, const char *toremove){
    while ( ( s=strstr(s,toremove) ) )
        memmove(s,s+strlen(toremove),1+strlen(s+strlen(toremove)));
}


/* FUNCTION NAME: find_index                                                                       */
/*                                                                                                 */
/* CALLING SEQUENCE:                                                                               */
/*   result = find_index(pos, nvec, vec);                                                          */
/*                                                                                                 */
/* PURPOSE:                                                                                        */
/*   Given a monotonically-increasing, ordered vector vec of size nvec, find the smallest          */
/*   index such that vec[index] >= pos; i.e. vec[index-1] < pos <= vec[index].  The previous       */
/*   version is replaced with one based on the bisection method in Numerical Recipes               */
/*   (subroutine "locate").                                                                        */
/*                                                                                                 */
/* INPUTS:                                                                                         */
/*   pos, nvec, vec                                                                                */
/*                                                                                                 */
/* OUTPUTS:                                                                                        */
/*   returns the smallest index such that vec[index] >= pos                                        */
/*                                                                                                 */
/* CALLED BY:                                                                                      */
/*   spec_scale()                                                                                  */
/*                                                                                                 */
/*                                                                                                 */

int find_index(double pos,       /* value for which vec is to be searched */
               int nvec,         /* size of 1D array vec */
               double * vec){    /* 1D array to be searched */

    int jl=0;  /* lower index */
    int ju=0;  /* upper index */
    int jm=0;  /* middle index */

    jl=0;
    if (pos <= vec[0])
        return jl;

    ju=nvec;
    if (pos > vec[nvec-1])
        return ju;

    while (ju-jl > 1){
        jm = (ju+jl)/2;
        if (pos <= vec[jm]){
            ju=jm;
        }else
            jl=jm;
    }
    return ju;

}




/* FUNCTION NAME: interpol_hunt                                                                    */
/*                                                                                                 */
/* CALLING SEQUENCE:                                                                               */
/*   result = interpol_hunt(n, energy_in, int_spec, binlo_out);                                    */
/*                                                                                                 */
/* PURPOSE:                                                                                        */
/*   Given y(x) at N points, interpol_hunt two-point interpolates to estimate y(z) at the          */
/*   single point x=z.                                                                             */
/*                                                                                                 */
/* INPUTS:                                                                                         */
/*   n, x, y, z                                                                                    */
/*                                                                                                 */
/* OUTPUTS:                                                                                        */
/*   returns y(z) from the linear interpolation on y(x)                                            */
/*                                                                                                 */
/* CALLED BY:                                                                                      */
/*   spec_interp                                                                                   */
/*                                                                                                 */
/*                                                                                                 */

double interpol_hunt(int n,       /* number of points where y is evaluated */
                     double *x,   /* points where y is evaluated */
                     double *y,   /* function evaluated at x */
                     double z){   /* single point where y(z) is to be estimated */

    double grad=0,d1=0,df=0,f=0;
    int inc=0, jl=0, jm=0, ju=0;

    if (z <= x[0]){  /* if z is off the low end of the grid, return the lowest point */
        f = y[0];

    } else if (z >= x[n-1]){ /* if z is off the high end of the grid, return the highest point */
        f = y[n-1];

    } else {  /* otherwise, perform the interpolation */

        inc = (x[n-1] > x[0]) ? TRUE : FALSE;
        jl = 0;
        ju = n;
        while (ju - jl > 1) {
            jm = (ju + jl) / 2;
            if ((z > x[jm]) == inc) {
                jl = jm;
            } else {
                ju = jm;
            }
        }
        /* ------     Z is sandwiched between JL and JU ------ */
        if ((x[jl] > 0. && x[ju] > 0.) && (y[jl] > 0. && y[ju] > 0.)) {
            grad = (log10(y[ju]) - log10(y[jl])) /
                (log10(x[ju]) - log10(x[jl]));
            df = grad * (log10(z) - log10(x[jl]));
            d1 = log10(y[jl]) + df;
            f = pow(10., d1);
        } else {
            f = y[jl]+(y[ju]-y[jl])*((z-x[jl])/(x[ju]-x[jl]));
        }

    }
    return f;
}




/* FUNCTION NAME: count_ave                                                                        */
/*                                                                                                 */
/* CALLING SEQUENCE:                                                                               */
/*   cr = count_avg(&c_arr, 360, 180, ira-1, idec-1, 1);                                           */
/*                                                                                                 */
/* PURPOSE:                                                                                        */
/*   Average count over larger map areas of local coverage gap                                     */
/*                                                                                                 */
/* INPUTS:                                                                                         */
/*   array, nx, ny, jx, jy, jdel                                                                   */
/*                                                                                                 */
/* OUTPUTS:                                                                                        */
/*   returns count_avg                                                                             */
/*                                                                                                 */
/* CALLED BY:                                                                                      */
/*   galactic()                                                                                    */
/*                                                                                                 */

double count_avg(double ** array,  /* array of count map */
		 int nx,           /* x map dimension */
		 int ny,           /* y map dimension */
		 int jx,        /* x counts */
		 int jy,        /* y counts */
		 int jdel){     /* delta counts */
    
    /* function that goes +/- idel on either size for a 2D array and averages */
    double ixmin=0.0, ixmax=0.0, iymin=0.0, iymax=0.0;
    int npts=0;
    double total = 0.0;
    double count_avg = 0.0;

    ixmin=intmax(0,jx-jdel);
    ixmax=intmin(nx-1,jx+jdel);
    iymin=intmax(0,jy-jdel);
    iymax=intmin(ny-1,jy+jdel);
    npts=(1+ixmax-ixmin)*(1+iymax-iymin);
    
    total=0.0;
    for (int ix = ixmin; ix < (ixmax+1); ix++) {
	for (int iy = iymin; iy < (iymax+1); iy++) {
	    total+=array[ix][iy];    
	}
    }
    count_avg = total / (double)npts;
    
    return count_avg;
}


/* FUNCTION NAME: get_version                          */
/*                                                     */
/* CALLING SEQUENCE: get_version(version);             */
/*                                                     */
/* PURPOSE: Query all of the .c files built in heasim  */
/*          and return the most recent CVS date.       */
/*                                                     */
/* INPUTS/OUTPUTS: version                             */
/*                                                     */
/* CALLED BY: main()                                   */
/*                                                     */

void get_version(char * version){
    FILE *fp = 0;                          /* pointer to ascii file to be opened and read */
    char relpath[STR_MAX_LEN];             /* relative path name of heasim directory */
    char * abspath = 0;                    /* absolute path name of heasim directory */
    char source_file[STR_MAX_LEN];         /* name of .c file to be opened and read */
    char line[STR_MAX_LEN];                /* line string in source file to be read */
    char * junk = 0;                       /* dummy string */
    char * temp = 0;                       /* temporary string holder */
    char *revword=0, *vv=0, *date=0;       /* components of CVS date entry */
    int ii=0;                              /* loop index */
    int year=0, month=0, day=0;            /* year, month, and day read from CVS date */
    int hi_year=0, hi_month=0, hi_day=0;   /* most recent found year, month, and day from CVS date */
    char ** file_list=0;                     /* list of .c files to be queried */
    int numfiles = 0;                      /* number of .c files to be queried */

    /* Resolve the $HEADAS path variable into an absolute path */
    strcpy(relpath,"$HEADAS/../heasim/skyback");
    abspath = resolve_pathname(relpath);

    /* Open $HEADAS/../heasim/heasimc/Makefile */
    sprintf(source_file,"%s/Makefile",abspath);
    fp = fopen(source_file,"r");
    if (fp == NULL)
        printf("Failed to open file %s.\n",source_file);

    /* Search the make file for the list of .c files that are built */
    while(fgets(line,sizeof(line),fp) != NULL){
        if (strncasecmp("HD_CTASK_SRC_c",line,14) == 0){
            junk = strtok(line,"=");
            temp = strtok(NULL,"=");
            trim_string(temp);

            /* Count the instances of "." */
            for (ii=0, numfiles=0; temp[ii]; ii++)  numfiles +=(temp[ii] == '.');

            file_list = calloc(numfiles,sizeof(char*));

            for (ii=0; ii<numfiles; ii++){
                file_list[ii] = calloc(STR_MAX_LEN,sizeof(char));
                if (ii == 0){
                    strcpy(file_list[ii],strtok(temp," "));
                } else {
                    strcpy(file_list[ii],strtok(NULL," "));
                }
            }
        }
    }
    fclose(fp);

    /* Search each .c file found above for the most recent CVS date entered. */
    for (ii=0; ii<numfiles; ii++){
        sprintf(source_file,"%s/%s",abspath,file_list[ii]);

        /*printf("Checking dates in %s...\n",source_file);*/

        fp = fopen(source_file,"r");
        if (fp == NULL)
            printf("Failed to open %s.\n",source_file);

        while(fgets(line,sizeof(line),fp) != NULL){
            trim_string(line);
            if (strncasecmp("! Revision",line,10) == 0){
                junk = strtok(line," ");
                revword = strtok(NULL," ");
                vv = strtok(NULL," ");
                date = strtok(NULL,"  ");
                year = atoi(strtok(date,"/"));
                month = atoi(strtok(NULL,"/"));
                day = atoi(strtok(NULL,"/"));

                if (year > hi_year){
                    hi_year = year;
                    hi_month = month;
                    hi_day = day;
                } else if (year == hi_year){
                    if (month > hi_month){
                        hi_month = month;
                        hi_day = day;
                    } else if (month == hi_month){
                        if (day > hi_day)
                            hi_day = day;
                    }
                }
            }

        }
        fclose(fp);
    }

    /* Report the most recent date as the version. */
    sprintf(version,"%d-%d-%d",hi_year,hi_month,hi_day);

    /* Clean up */
    free(abspath);
    for (ii=0; ii<numfiles; ii++)
        free(file_list[ii]);
    free(file_list);

    return;
}


/* FUNCTION NAME: resolve_pathname                                          */
/*                                                                          */
/* PURPOSE:                                                                 */

/*    Resolve environment variables and relative path names in a string     */
/*    input into the full absolute path name.                               */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*    char * abspath = resolve_pathname(char * relpath);                    */
/*                                                                          */
/* INPUTS: intput_string                                                    */
/*                                                                          */
/* OUTPUTS: output_string                                                   */
/*                                                                          */
/* CALLED BY: get_version()                                                 */
/*                                                                          */


char * resolve_pathname(char * input_string){

    int ii=0;
    int num_sections = 0;
    char tempstring[300] = "/";
    char * resolved_var_string=0;
    char * string_section=0;
    char * output_string=0;
    int topdir = 0;

    if (0 == strcasecmp(input_string,"none")){
        return "none";

    } else {

        /* allocate here - remember to free! */
        output_string = calloc(STR_MAX_LEN, sizeof(char) );

        if ( (strstr(input_string,"$") == NULL) ||
             (strstr(input_string,"..") == NULL) ){

            strcpy(output_string,input_string);

        } else {

            /* if first character is "/", then remove and shift */
            if (input_string[0] == '/'){
                memmove(&input_string[0], &input_string[0 + 1], strlen(input_string) - 0);
                topdir=1;
            }

            /* count number of "/" characters in string */
            for (ii=0, num_sections=0; input_string[ii]; ii++)
                num_sections += (input_string[ii] == '/');

            for (ii=0; ii<=num_sections; ii++){

                /* break input string into string sections, delimited by "/" */
                if (ii == 0){
                    string_section = strtok(input_string,"/");
                } else
                    string_section = strtok(NULL,"/");

                /* replace the directory divider */
                strcat(tempstring,"/");

                if (strchr(string_section,'$')){
                    /* if the string section begins with "$", resolve the envirnoment variable */
                    /* and append to tempstring */
                    memmove(&string_section[0], &string_section[0 + 1], strlen(string_section) - 0);
                    resolved_var_string = getenv(string_section);
                    strcat(tempstring,resolved_var_string);
                } else {
                    /* otherwise append string_section to tempstring */
                    strcat(tempstring,string_section);
                }
            }

	    strcpy(output_string,tempstring);
        }

        return output_string;
    }
}



/* FUNCTION NAME: trim_string                                   */
/*                                                              */
/* PURPOSE: Remove white space at beginning and end of string   */
/*                                                              */
/* INPUTS/OUTPUTS: str                                          */
/*                                                              */
/* CALLED BY: get_version                                       */
/*                                                              */

char *trim_string(char *str){
    size_t len = 0;
    char *frontp = str - 1;
    char *endp = NULL;

    if( str == NULL )
        return NULL;

    if( str[0] == '\0' )
        return str;

    len = strlen(str);
    endp = str + len;

    /* Move the front and back pointers to address the first non-whitespace characters from each end.  */
    while( isspace(*(++frontp)) );
    while( isspace(*(--endp)) && endp != frontp );

    if( str + len - 1 != endp )
        *(endp + 1) = '\0';
    else if( frontp != str &&  endp == frontp )
        *str = '\0';

    /* Shift the string so that it starts at str so that if it's dynamically allocated, we can
     * still free it on the returned pointer.  Note the reuse of endp to mean the front of the
     * string buffer now. */
    endp = str;
    if( frontp != str )
        {
            while( *frontp ) *endp++ = *frontp++;
            *endp = '\0';
        }


    return str;
}



/* FUNCTION NAME: image_to_array                                       */
/*                                                                     */
/* PURPOSE: read FITS image, store in array                            */
/*                                                                     */
/* CALLING SEQUENCE:                                                   */
/*   errstatus = image_to_array("ranh.fits", &nx_nh, &ny_nh, &nh_arr); */
/*                                                                     */
/* INPUTS:  ifilename, nx, ny                                          */
/*                                                                     */
/* OUTPUTS: errstatus, array                                           */
/*                                                                     */
/* CALLED BY: doWork(), galactic()                                     */
/*                                                                     */


int image_to_array(char * ifilename, long * nx, long * ny, double *** array, int debug){

    fitsfile * iunit = NULL;
    int fstat=0, anyf=0, extend=0;
    char * com = NULL;
    long naxes[10] = {0};
    int simple=0, bitpix=0, naxis=0;
    long pcount=0, gcount=0;
    double * list=0;
    int ii=0;
    int imx=0, imy=0;
    long index=0, num_AXIS=0;

    int HDU_num=1;
    int HDU_TYPE=-1;
    char * HDU=0;
    char tempstring[STR_MAX_LEN];
    char * extnum=0;
    char * xten=0;

    if (debug == 1) printf("\n   Coping image to array...\n");

    /* parse the filename, see if HDU extension is given, otherwise = 1 */
    HDU_num = 1;
    strcpy(tempstring,ifilename);
    ifilename = strtok(tempstring,"[");
    extnum = strtok(NULL,"]");
    if (extnum != NULL)
        HDU_num = atoi(extnum);
    if (debug == 1) printf("     Image file: %s  Expect image in HDU header %d\n",ifilename,HDU_num);


    /* Open fits image */
    fits_open_file(&iunit, ifilename, READONLY, &fstat);
    if (fstat > 0) {
        printf("ERROR: Failed to open image file %s.\n",ifilename);
        return fstat;
    }

    /* move to HDU_num */
    fits_movabs_hdu(iunit, HDU_num, &HDU_TYPE, &fstat);
    if (HDU_TYPE == IMAGE_HDU){ HDU="IMAGE_HDU";}
    else if (HDU_TYPE == ASCII_TBL) { HDU="ASCII_TBL"; }
    else if (HDU_TYPE == BINARY_TBL) {HDU="BINARY_TBL"; }
    else { HDU="unknown"; }
    if (debug == 1) printf("     Moving to HDU %d, type %s.\n",HDU_num,HDU);
    if (fstat > 0) {
        printf("  ERROR: Failed to locate IMAGE HDU in image file %s.\n",ifilename);
        printf("       Are you certain this is a FITS image file?\n");
        return(fstat);
    }

    /* If not in the primary HDU, check that XTENSION keyword is "IMAGE" */
    if (HDU_num > 1){
        fits_read_key_longstr(iunit, "XTENSION", &xten, com, &fstat);
        if (debug == 1) printf("     Checking XTENSION keyword: %s\n",xten);
        if (0 != strcasecmp(xten,"IMAGE")){
            printf("  ERROR! Image XTENSION is not IMAGE, cannot read!\n");
            fstat = -1;
            free(xten);  /* fits_read_key_longstr allocates this, need to free. */
            return(fstat);
        } else {
            free(xten);
        }
    } else {
        if (debug == 1) printf("     Image should be found in primary HDU.\n");
    }


    /* Check that NAXIS keyword is 2.  Otherwise it's impossible that this is an image. */
    fits_read_key_lng(iunit, "NAXIS", &num_AXIS, com, &fstat);
    if (fstat != 0)
        printf("Problem reading NAXIS from header!\n");
    if (num_AXIS != 2){
        printf("  ERROR! NAXIS keyword indicates dimensions other than 2!  Not an image!\n");
        fstat = -1;
        return fstat;
    } else {
        if (debug == 1) printf("     Image NAXIS keyword confirmed at 2 dimensions.\n");
    }

    /* Read NAXIS1 and NAXIS2 from image header */
    fits_read_imghdr(iunit, 10, &simple, &bitpix, &naxis, naxes, &pcount, &gcount, &extend, &fstat);
    *nx = naxes[0];
    *ny = naxes[1];

    /* Allocate arrays */
    list = calloc( (*nx) * (*ny), sizeof(double));
    *array = (double **)calloc(*nx+1, sizeof(double *));
    for (ii=0; ii<*nx+1; ii++)
        (*array)[ii] = (double *)calloc(*ny+1, sizeof(double));

    /* Read the image into 1D array list */
    fits_read_2d_dbl(iunit, 0, 0, naxes[0], naxes[0], naxes[1], list, &anyf, &fstat);

    /* Put image values into 2D array */
    for (imx=1; imx<(*nx)+1; imx++){    /* loop over nh image columns */
        for (imy=1; imy<(*ny)+1; imy++){     /* loop over nh image rows */
            index = (imy-1) * (*nx) + imx-1;
            (*array)[imx][imy] = list[index];
        } /* end loop over rows */
    } /* end loop over columns */

    free(list);
    /* REMEMBER TO FREE ARRAY WHEN FINISHED! */

    fits_close_file(iunit,&fstat);

    if (debug == 1) printf("   ...done.\n");

    return fstat;

}


double fz(double z0, double z){

    /* function needed for redshift (cumulative) probabilities, assuming redshifts distributed as
       ~ze-z/z0 - can go in utils.c
    */
    double x = z/z0;
    double fz = (1.0+x) * exp(-x);

    return fz;
}



/*
 ! $Log: utils.c,v $
 ! Revision 1.16  2015/05/19 17:13:48  driethmi
 ! Directory path for get_version() was set to retrieve heasim files; reset
 ! to retrieve skyback files.
 !
 ! Revision 1.15  2015/05/19 17:12:16  driethmi
 ! Added CVS macro to print file revision history at end of file.
 !
*/
