/**
 * \file utils.c
 * \brief  Holds all low-level math and manipulation utilities.
 * \author David Riethmiller
 * \date $Date $
*/

#include "heasim.h"
#include <stdlib.h>
#include <stdio.h>
#include <funcWrappers.h>  /* Xspec defs, funcWrappers.cxx */
#include <math.h>
#include <ctype.h>
//#include <xlocale.h>       
#include <sys/time.h>
#include <limits.h>


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

    /* The lower-most index is zero */
    jl=0;

    /* If position is off the lower bound, then return the lower bound */
    if (pos <= vec[0])
        return 0;

    /* Set the upper index to the size of the array */
    ju=nvec;

    /* If position is off the upper bound, then return the upper bound */
    if (pos >= vec[nvec-1])
        return nvec-1;

    /* Otherwise, search through the array until we find the smallest index such that vec[index] >= pos */
    while (ju-jl > 1){
        jm = (ju+jl)/2;
        if (pos <= vec[jm]){
            ju=jm;
        }else
            jl=jm;
    }
    return ju;

}




/* FUNCTION NAME: get_nlines                                                                       */
/*                                                                                                 */
/* CALLING SEQUENCE:                                                                               */
/*   result = get_nlines(filename)                                                                 */
/*                                                                                                 */
/* PURPOSE:                                                                                        */
/*   Find the number of populated numeric lines in a file, i.e. that do not start with #, !, @,    */
/*   a letter of the alphabet, or string length of zero.                                           */ 
/*                                                                                                 */
/* INPUTS:                                                                                         */
/*   filename                                                                                      */
/*                                                                                                 */
/* OUTPUTS:                                                                                        */
/*   returns the number of valid lines                                                             */
/*                                                                                                 */
/* CALLED BY:                                                                                      */
/*   spectra()                                                                                     */
/*                                                                                                 */
/*                                                                                                 */


int get_nlines(char * filename, int debug){  /* source input filename */

    FILE *fp;                     /* The file to be opened*/
    char line[STR_MAX_LEN];       /* string line in file */
    int n_commentlines=0;         /* number of comment lines or spaces in file */
    int numlines=0;               /* number of valid source lines in file */
    int len=0;                      /* string length */
    int j=0, k=0;                 /* loop index */
    char temp_line[STR_MAX_LEN];  /* temporary string buffer */

    fp = fopen(filename,"r");
    while(fgets(line,sizeof(line),fp) != NULL){

        /* strip trailing '\n' if it exists */
        len = strlen(line)-1;  if(line[len] == '\n') line[len] = 0;

        /* remove spaces in line */
        k=0;
        for (j=0; j<=len; j++)  if (line[j] != ' ')  temp_line[k++] = line[j];
        temp_line[k] = 0;
        strcpy(line,temp_line);

        if ( (line[0] == '#') ||
             (line[0] == '!') ||
             (line[0] == '@') ||
             (isalpha(line[0])) ||
             (strlen(line) == 0) ||
             (line[0] == ' ') ){/* ignore commented lines denoted by # */
            n_commentlines++;
            /* printf("Char line: %s\n",line); */
        }else
            numlines++;
    }

    if (debug) printf("Found %d valid lines in file %s\n",numlines,filename);
    fclose(fp);
    return numlines;
}



/* FUNCTION NAME: reverse                                                                          */
/*                                                                                                 */
/* CALLING SEQUENCE:                                                                               */
/*   reverse(array, array_size);                                                                   */
/*                                                                                                 */
/* PURPOSE:                                                                                        */
/*   Reverse the order of an array of doubles.                                                     */
/*                                                                                                 */
/* INPUTS:                                                                                         */
/*   arr, num                                                                                      */
/*                                                                                                 */
/* OUTPUTS:                                                                                        */
/*   returns the reordered array                                                                   */
/*                                                                                                 */
/* CALLED BY:                                                                                      */
/*   spectra()                                                                                     */
/*                                                                                                 */
/*                                                                                                 */

void reverse(double * arr,  /* The array to be reversed */
	     int num){      /* Number of elements in the array */

    int i=0;              /* loop index */
    double * tmp_arr=0;   /* temporary array */

    /* allocate the temporary array to correct size */
    tmp_arr = calloc(num, sizeof(double));

    /* copy the primary array to the temporary array */
    for (i=0; i<num; i++)
        tmp_arr[i] = arr[i];

    /* copy the temporary array in reverse order to the primary array */
    for (i=0; i<num; i++)
        arr[i] = tmp_arr[num-1-i];

    /* clean up */
    free(tmp_arr);
    return;
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
/* CALLED BY:                                                                                      */
/*   specmod()                                                                                     */
/*                                                                                                 */


double poidev(double xm, 
	      HDmt_state *MTstate){      /* "save" state of Mersenne Twister RNG */

    static double sq=0, alxm=0, g=0, oldm=(-1.0);
    static double d_PI = 3.141592654e0;
    double em=0, t=0,y=0;

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
    double ser=0, tmp=0, x=0;

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



/* FUNCTION NAME: string2upper                                       */
/*                                                                   */
/* CALLING SEQUENCE:                                                 */
/*   string2upper_status = string2upper(<string>);                   */
/*                                                                   */
/* PURPOSE: convert a string to upper-case                           */
/*                                                                   */
/* INPUT: a string to be converted                                   */
/*                                                                   */
/* OUTPUT: the same string, in upper case                            */
/*                                                                   */
/* CALLED BY:                                                        */
/*   setup_output()                                                  */
/*                                                                   */
/* REVISION HISTORY                                                  */
/*   Written by Nicholas Collins (ncollins), Wyle ST&E, 20121120     */
/*                                                                   */

int string2upper(char * input_string  /* a string input */) {
    char * letter = 0;  /* each letter within the input string */

    for (letter = input_string; *letter != '\0'; ++letter) {
        *letter = toupper(*letter);
    }

    /* make sure input_string is not a pointer to NULL before returning */
    if (NULL != input_string) {
        return(0);
    } else {
        fprintf(stderr,"\nstring2upper: Problem, pointer to char input_string is NULL\n");
        return -1;
    }
    return(0);

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
/*   setup_output()                                                  */
/*                                                                   */

void remove_substring(char *s, const char *toremove){
    while ( ( s=strstr(s,toremove) ) )
	memmove(s,s+strlen(toremove),1+strlen(s+strlen(toremove)));
}




/* FUNCTION NAME: rotpol                                             */
/*                                                                   */
/* CALLING SEQUENCE:                                                 */
/*                                                                   */
/*                                                                   */
/* PURPOSE:                                                          */
/*   ROTPOL returns coordinates of the direction given transformed   */
/*   to a polar system where up is at the old CENLON,CENLAT.         */   
/*                                                                   */
/* INPUT: CENLON, CENLAT, MODE, INLO, INLA, OUTLO, OUTLA             */
/*                                                                   */
/* OUTPUT: INLO, INLA, OUTLO, OUTLA                                  */
/*                                                                   */
/* CALLED BY: imagedis()                                             */
/*                                                                   */
/* REVISION HISTORY:                                                 */
/*   Written by Steve Snowden, James Peachey, Randall Smith          */
/*   Converted from FORTRAN to C by Nicholas Collins                 */
/*                                                                   */

void rotpol(double CENLON,  /* longitude in degrees for new north pole */
	    double CENLAT,  /* latitude in degrees for new north pole */
            double INLO,    /* longitude of position to be rotated */
	    double INLA,    /* latitude of position to be rotated */
            double *OUTLO,  /* longitude of position in rotated coords */
	    double *OUTLA,  /* latitude of position in rotated coords */
	    int MODE) {     /*  = 1 rotate to prime (new) system, = -1 rotate from prime (new) system */


    double DINLA=0, DINLO=0, DOUTLA=0, DOUTLO=0, E1=0, E2=0, E3=0;
    double V[3], V1[3];

    double DEGRAD=0.01745329252;

    E1 = (90. + CENLON);
    if (E1 > 360.) E1 = E1 - 360.;
    E1 = E1*DEGRAD;
    E2 = (90. - CENLAT)*DEGRAD;
    E3 = 270.*DEGRAD;
    DINLA = INLA;
    DINLO = INLO;

    if (MODE != 1) {
	/* vect8(DINLA, DINLO, V); */
	V[0] = cos(DINLA*DEGRAD)*cos(DINLO*DEGRAD);
	V[1] = cos(DINLA*DEGRAD)*sin(DINLO*DEGRAD);
	V[2] = sin(DINLA*DEGRAD);
	eulrot8(V1,V,E1,E2,E3,MODE);
	elaz8(V1,&DOUTLA,&DOUTLO);
    } else {
	/* vect8(DINLA, DINLO, V1); */
	V1[0] = cos(DINLA*DEGRAD)*cos(DINLO*DEGRAD);
	V1[1] = cos(DINLA*DEGRAD)*sin(DINLO*DEGRAD);
	V1[2] = sin(DINLA*DEGRAD);
	eulrot8(V1,V,E1,E2,E3,MODE);
	elaz8(V,&DOUTLA,&DOUTLO);
    }

    *OUTLA = DOUTLA;
    *OUTLO = DOUTLO;
}



/* FUNCTION NAME: eulrot8                                           */
/*                                                                  */
/* CALLING SEQUENCE:                                                */
/*   eulrot8(V1,V,E1,E2,E3,MODE);                                   */
/*                                                                  */
/* PURPOSE:                                                         */
/*   Use the euler angles E1, E2, and E3 to rotate the vector R     */
/*   into the vector R1 or vice versa, depending on the value of    */
/*   MTEST.                                                         */
/*                                                                  */
/* INPUT: E1, E2, E3, MTEST, R1, R                                  */
/*                                                                  */
/* OUTPUT: R1, R                                                    */
/*                                                                  */
/* CALLED BY: rotpol()                                              */
/*                                                                  */
/* REVISION HISTORY:                                                */
/*   Copied from SimX 2.0.0                                         */
/*                                                                  */

void eulrot8 (double R[], double R1[],
              double E1, double E2, double E3, int MTEST) {
    
    int i=0, j=0;
    /* Fix: Check this !!! */
    double A[9], A1[9];
    
    A[0*3+0] = cos(E3)*cos(E1) - cos(E2)*sin(E1)*sin(E3);
    A[0*3+1] = cos(E3)*sin(E1) + cos(E2)*cos(E1)*sin(E3);
    A[0*3+2] = sin(E3)*sin(E2);
    
    A[1*3+0] = -sin(E3)*cos(E1) - cos(E2)*sin(E1)*cos(E3);
    A[1*3+1] = -sin(E3)*sin(E1) + cos(E2)*cos(E1)*cos(E3);
    A[1*3+2] = cos(E3)*sin(E2);
    
    A[2*3+0] = sin(E2)*sin(E1);
    A[2*3+1] = -sin(E2)*cos(E1);
    A[2*3+2] = cos(E2);
    
    if (MTEST > 0) {
	
	/* Rotate into the primed system */
	
	for (i=0;i<3;i++) {
	    R1[i] = 0.0;
	    for (j=0;j<3;j++) R1[i] += A[i*3+j]*R[j];
	}
    } else {
	
	/*  Rotate into the unprimed system */
	
	for (i=0;i<3;i++) {
	    for (j=0;j<3;j++) A1[i*3+j] = A[j*3+i];
	}
	
	for (i=0;i<3;i++) {
	    R[i] = 0.0;
	    for (j=0;j<3;j++) R[i] += A1[i*3+j]*R1[j];
	}
    }
}



/* FUNCTION NAME: elaz8                                             */
/*                                                                  */
/* CALLING SEQUENCE:                                                */
/*   elaz8(V,&DOUTLA,&DOUTLO);                                      */
/*                                                                  */
/* PURPOSE:                                                         */
/*   Returns the elevation and azimuth of a given vector            */
/*                                                                  */
/* INPUT: V                                                         */
/*                                                                  */
/* OUTPUT: EL, AZ                                                   */
/*                                                                  */
/* CALLED BY: rotpol()                                              */
/*                                                                  */
/* REVISION HISTORY:                                                */
/*   Copied from SimX 2.0.0                                         */
/*                                                                  */


void elaz8(double V[], double *EL, double *AZ) {
    double CCC=0, RD=57.29577951;
    double tmp=0;
    int divisor=0;

    *EL = RD*asin(V[2]/sqrt(V[0]*V[0] + V[1]*V[1] + V[2]*V[2]));
    CCC = V[0]*V[0] + V[1]*V[1];
    if (CCC == 0.) {
	*AZ = 0.;
    } else {
	tmp = fabs(acos(V[0]/sqrt(V[0]*V[0] + V[1]*V[1]))*RD);
	if (V[1] < 0) tmp = -tmp;
	divisor = (int) (tmp + 720.)/360.0;
	*AZ = (tmp+720.0) - divisor*360.;
    }
}





/* FUNCTION NAME: rd2xy                                                     */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*                                                                          */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Convert (ra,dec) to any pixel coords, e.g. simulator coords (x,y).     */
/*                                                                          */
/*                                                                          */
/* INPUT: ra, dec, x_ref, y_ref, ra_ref, dec_ref, roll, xpixsize, ypixsize  */
/*                                                                          */
/* OUTPUT: x, y                                                             */
/*                                                                          */
/* CALLED BY:                                                               */
/*                                                                          */

void rd2xy(double ra,        /* input RA */
	   double dec,       /* input DEC */
	   double x_ref,     /* reference x-value */
	   double y_ref,     /* reference y-value */
	   double ra_ref,    /* RA at (x_ref,y_ref) */
	   double dec_ref,   /* DEC at (x_ref, y_ref) */
	   double roll,      /* "roll" angle of y-axis from north, postive clockwise for N to x */
	   double xpixsize,  /* degrees per pixel, x-direction, pos/neg if look up/down */
	   double ypixsize,  /* degrees per pixel, y-direction */
	   double *x,        /* output x */
	   double *y){       /* output y */

    double gamma1=0;
    double singam=0, cosgam=0;
    double cosdelra=0,sindelra=0,sindec=0,cosdec=0,sindecref=0,cosdecref=0;
    double a1=0, a2=0;
    double ralde=0,ralder=0;
    double aixnor=0,aiynor=0,aix=0,aiy=0;

    gamma1 = -D2R * roll; 

    singam = sin(gamma1);
    cosgam = cos(gamma1);

    cosdelra = cos(D2R*(ra_ref-ra));
    sindelra = sin(D2R*(ra_ref-ra));
    sindec = sin(D2R*dec);
    cosdec = cos(D2R*dec);
    sindecref = sin(D2R*dec_ref);
    cosdecref = cos(D2R*dec_ref);

    a1 = sindec * sindecref;
    a2 = cosdelra *cosdec * cosdecref;

    /* corner case for roll=0 requires special treatment */
    if ( fabs(a1+a2-1.0) <= 1.0e-15)
	ralde=0.;
    else
	ralde=acos(a1 + a2);
    
    if (ralde < 1.0e-8) {
        ralder=1.0;
    }  else {
        ralder=ralde/sin(ralde);
    }

    aixnor = (ralder/D2R) * cosdec * sindelra;
    aiynor = (ralder/D2R) *(sindec * cosdecref - cosdec *sindecref * cosdelra);
    aix = cosgam * aixnor - singam * aiynor;
    aiy = cosgam * aiynor  + singam * aixnor;
    *x = x_ref - aix/xpixsize;
    *y = y_ref + aiy/ypixsize;

    

}



/* FUNCTION NAME: xy2rd                                                     */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*                                                                          */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Convert pixel coords x,y to ra,dec.  Opposite of rd2xy.                */
/*                                                                          */
/*                                                                          */
/* INPUT: x, y, x_ref, y_ref, ra_ref, dec_ref, theta, xpixsize, ypixsize    */
/*                                                                          */
/* OUTPUT: ra, dec                                                          */
/*                                                                          */
/* CALLED BY:                                                               */
/*                                                                          */

void xy2rd(double x,         /* input x */
	   double y,         /* input y */
	   double x_ref,     /* reference x-value */
	   double y_ref,     /* reference y-value */
	   double ra_ref,    /* ra at (x_ref,y_ref) */
	   double dec_ref,   /* dec at (x_ref,y_ref) */
	   double theta,     /* opposite of "roll" angle of y-axis from north */
	   double xpixsize,  /* degrees per pixel, x-direction, pos/neg if look up/down */
	   double ypixsize,  /* degrees per pixel, y-direction */
	   double *ra,        /* output RA */
	   double *dec){      /* output Dec */

    double cosdecref=0;
    double sindecref=0;
    double costheta=0;
    double sintheta=0;
    double dxin=0, dyin=0;
    double xx=0, yy=0, rr=0;
    double sinr=0, cosr=0;
    double sind=0, cosd=0;
    double sina=0, cosa=0;
    double fract=0;

    /* the variable D2R (degrees to radian) is defined in heasim.h */
    
    cosdecref = cos(D2R * dec_ref);
    sindecref = sin(D2R * dec_ref);
    costheta = cos(D2R * theta);
    sintheta = sin(D2R *theta);

    dxin = x - x_ref;
    dyin = y - y_ref;
    xx = D2R * xpixsize * (dxin*costheta + dyin*sintheta);
    yy = D2R * ypixsize * (dyin*costheta - dxin*sintheta);

    rr = sqrt(xx*xx + yy*yy);
    sinr = sin(rr);

    if (rr == 0){
	*ra = ra_ref;
	*dec = dec_ref;
    } else {
	fract = sinr/rr;
	cosr = cos(rr);
	sind = cosr*sindecref + fract*cosdecref*yy;
	cosd = sqrt(1.0 - sind*sind);
	if (cosd == 0){
	    *dec = 90.0;
	    if (sind < 0) *dec *= -1;
	    *ra += ra_ref;
	} else {
	    *dec = atan(sind/cosd) / D2R;
	    sina = fract*xx/cosd;
	    cosa = (cosr*cosdecref-fract*sindecref*yy) / cosd;
	    *ra = ra_ref + atan(sina/cosa) / D2R;
	}
    }
    return;
}


/* FUNCTION NAME: compare_double                          */
/*                                                        */
/* PURPOSE: compare two values and report >=<             */
/*                                                        */
/* CALLING SEQUENCE:                                      */
/*   compare_double(a,b);                                 */
/*                                                        */
/* INPUT: a, b                                            */
/*                                                        */
/* OUTPUT: 1 if a > b                                     */
/*        -1 if a < b                                     */
/*         0 if a = b                                     */
/*                                                        */


int compare_double(const void *a, const void *b) {

    double x = *(const double *)a;
    double y = *(const double *)b;

    if (x > y){
        return 1;
    } else {
        if (x < y){
            return -1;
        } else {
            return 0;
        }
    }
}



/* FUNCTION NAME: compare_triple                          */
/*                                                        */
/* PURPOSE: compare two values and report >=<             */
/*                                                        */
/* CALLING SEQUENCE:                                      */
/*   compare_triple(a,b);                                 */
/*                                                        */
/* INPUT: a, b                                            */
/*                                                        */
/* OUTPUT: 1 if a > b                                     */
/*        -1 if a < b                                     */
/*         0 if a = b                                     */
/*                                                        */
/* CALLED BY: sort_arrays_DDD(), sort_arrays_DII()        */
/*                                                        */

int compare_triple(const void *a, const void *b) {
    triple *A = (triple*)a;
    triple *B = (triple*)b;

    double x = A->t1;
    double y = B->t1;

    if (x > y){
        return 1;
    } else {
        if (x < y){
            return -1;
        } else {
            return 0;
        }
    }
}



/* FUNCTION NAME: compare_quad                          */
/*                                                        */
/* PURPOSE: compare two values and report >=<             */
/*                                                        */
/* CALLING SEQUENCE:                                      */
/*   compare_quad(a,b);                                 */
/*                                                        */
/* INPUT: a, b                                            */
/*                                                        */
/* OUTPUT: 1 if a > b                                     */
/*        -1 if a < b                                     */
/*         0 if a = b                                     */
/*                                                        */


int compare_quad(const void *a, const void *b) {
    quad *A = (quad*)a;
    quad *B = (quad*)b;

    double x = A->t1;
    double y = B->t1;

    if (x > y){
        return 1;
    } else {
        if (x < y){
            return -1;
        } else {
            return 0;
        }
    }
}



/* FUNCTION NAME: compare_two_level                       */
/*                                                        */
/* PURPOSE: compare two triples by two sort conditions    */
/*                                                        */
/* CALLING SEQUENCE:                                      */
/*   compare_two_levels(a,b);                             */
/*                                                        */
/* INPUT: a, b                                            */
/*                                                        */
/* OUTPUT: 1 if a > b                                     */
/*        -1 if a < b                                     */
/*         0 if a = b                                     */
/*                                                        */
/* CALLED BY: sort_two_conditions_IDI()                   */
/*                                                        */

int compare_two_level(const void *a, const void *b){
    triple *A = (triple*)a;
    triple *B = (triple*)b;

    double x1 = A->t1;
    double y1 = B->t1;

    double x2 = A->t2;
    double y2 = B->t2;

    if (x1 > y1){
	return 1;
    } else if (x1 < y1) {
	return -1;
    }

    /* Otherwise x1 = y1, move on to x2 <=> y2 comparison */
 
    if (x2 > y2){
        return 1;
    } else if (x2 < y2){
	return -1;
    } else {
	return 0;
    }
}


/* FUNCTION NAME: sort_two_conditions_IDI                 */
/*                                                        */
/* PURPOSE: sort three arrays to on two conditions        */
/*                                                        */
/* CALLING SEQUENCE:                                      */
/*   sort_sort_two_conditions_IDI(arr1, arr2, arr3, num); */
/*                                                        */
/* INPUT/OUTPUT: arr1, arr2, arr3                         */
/*                                                        */
/* INPUT: num                                             */
/*                                                        */
/* SUBROUTINES: compare_two_level()                       */
/*                                                        */
/* CALLED BY: flag_pileup()                               */
/*                                                        */

void sort_two_conditions_IDI(int *arr1, double *arr2, int *arr3, int num){
    /* Sort first by arr1, then by arr2. */

    triple * trip_arr;
    int i=0;

    trip_arr = calloc(num,  sizeof(*trip_arr));
    for (i=0; i<num; i++){
        trip_arr[i].t1 = arr1[i];
        trip_arr[i].t2 = arr2[i];
        trip_arr[i].t3 = arr3[i];
    }

    qsort(trip_arr, num, sizeof(triple), compare_two_level);

    for (i=0; i<num; i++){
        arr1[i] = (int)trip_arr[i].t1;
        arr2[i] = (double)trip_arr[i].t2;
        arr3[i] = (int)trip_arr[i].t3;
    }

    free(trip_arr);    
}



/* FUNCTION NAME: sort_arrays_DDD                         */
/*                                                        */
/* PURPOSE: sort three double arrays to match the first   */
/*                                                        */
/* CALLING SEQUENCE:                                      */
/*   sort_arrays_DDD(arr1, arr2, arr3, num);              */
/*                                                        */
/* INPUT/OUTPUT: arr1, arr2, arr3                         */
/*                                                        */
/* INPUT: num                                             */
/*                                                        */
/* SUBROUTINES: compare_triple()                          */
/*                                                        */
/* CALLED BY: spectra()                                   */
/*                                                        */

void sort_arrays_DDD(double *arr1, double *arr2, double *arr3, int num){

    triple * trip_arr;
    int i=0;

    trip_arr = calloc(num,  sizeof(*trip_arr));
    for (i=0; i<num; i++){
        trip_arr[i].t1 = arr1[i];
        trip_arr[i].t2 = arr2[i];
        trip_arr[i].t3 = arr3[i];
    }

    qsort(trip_arr, num, sizeof(triple), compare_triple);

    for (i=0; i<num; i++){
        arr1[i] = trip_arr[i].t1;
        arr2[i] = trip_arr[i].t2;
        arr3[i] = trip_arr[i].t3;
    }

    free(trip_arr);
}



/* FUNCTION NAME: sort_arrays_DII                      */
/*                                                     */
/* PURPOSE: sort a double array and two ints to match  */
/*                                                     */
/* CALLING SEQUENCE:                                   */
/*   sort_arrays_DII(arr1, arr2, arr3, num);           */
/*                                                     */
/* INPUT/OUTPUT: arr1, arr2, arr3                      */
/*                                                     */
/* INPUT: num                                          */
/*                                                     */
/* SUBROUTINES: compare_triple()                       */
/*                                                     */
/* CALLED BY: process_image() - currently disabled     */
/*                                                     */

void sort_arrays_DII(double *arr1, int *arr2, int *arr3, int num){

    triple * trip_arr;
    int i=0;

    trip_arr = calloc(num,  sizeof(*trip_arr));
    for (i=0; i<num; i++){
        trip_arr[i].t1 = arr1[i];
        trip_arr[i].t2 = arr2[i];
        trip_arr[i].t3 = arr3[i];
    }

    qsort(trip_arr, num, sizeof(triple), compare_triple);

    for (i=0; i<num; i++){
        arr1[i] = (double)trip_arr[i].t1;
        arr2[i] = (int)trip_arr[i].t2;
        arr3[i] = (int)trip_arr[i].t3;
    }

    free(trip_arr);
}



/* FUNCTION NAME: sort_arrays_DIIL                        */
/*                                                        */
/* PURPOSE: sort a double array, two ints and a long to match   */
/*                                                        */
/* CALLING SEQUENCE:                                      */
/*   sort_arrays_DLLL(arr1, arr2, arr3, arr4, num);       */
/*                                                        */
/* INPUT/OUTPUT: arr1, arr2, arr3, arr4                   */
/*                                                        */
/* INPUT: num                                             */
/*                                                        */
/* SUBROUTINES: compare_quad()                            */
/*                                                        */


void sort_arrays_DIIL(double *arr1, int *arr2, int *arr3, long *arr4, int num){

    quad * quad_arr;
    int i=0;

    quad_arr = calloc(num,  sizeof(*quad_arr));
    for (i=0; i<num; i++){
        quad_arr[i].t1 = arr1[i];
        quad_arr[i].t2 = arr2[i];
        quad_arr[i].t3 = arr3[i];
	quad_arr[i].t4 = arr4[i];
    }

    qsort(quad_arr, num, sizeof(quad), compare_quad);

    for (i=0; i<num; i++){
        arr1[i] = (double)quad_arr[i].t1;
        arr2[i] = (int)quad_arr[i].t2;
        arr3[i] = (int)quad_arr[i].t3;
	arr4[i] = (long)quad_arr[i].t4;
    }

    free(quad_arr);
}


/* FUNCTION NAME: sort_arrays_DIIIL                        */
/*                                                        */
/* PURPOSE: sort a double array, three ints and a long to match   */
/*                                                        */
/* CALLING SEQUENCE:                                      */
/*   sort_arrays_DLLL(arr1, arr2, arr3, arr4, arr5, num);       */
/*                                                        */
/* INPUT/OUTPUT: arr1, arr2, arr3, arr4, arr5                   */
/*                                                        */
/* INPUT: num                                             */
/*                                                        */
/* SUBROUTINES: compare_quad()                            */
/*                                                        */


void sort_arrays_DIIIL(double *arr1, int *arr2, int *arr3, int *arr4, long *arr5, int num){

    quad * quad_arr;
    int i=0;

    quad_arr = calloc(num,  sizeof(*quad_arr));
    for (i=0; i<num; i++){
        quad_arr[i].t1 = arr1[i];
        quad_arr[i].t2 = arr2[i];
        quad_arr[i].t3 = arr3[i];
        quad_arr[i].t4 = arr4[i];
	quad_arr[i].t5 = arr5[i];
    }

    qsort(quad_arr, num, sizeof(quad), compare_quad);

    for (i=0; i<num; i++){
        arr1[i] = (double)quad_arr[i].t1;
        arr2[i] = (int)quad_arr[i].t2;
        arr3[i] = (int)quad_arr[i].t3;
	arr4[i] = (int)quad_arr[i].t4;
        arr5[i] = (long)quad_arr[i].t5;
    }

    free(quad_arr);
}




/* FUNCTION NAME: gasdev                                   */
/*                                                         */
/* PURPOSE:                                                */
/*   From Num Recipes - return the Gaussian deviation for  */
/*   unit variance.  Modified to use Mersenne Twister.     */
/*                                                         */
/* CALLING SEQUENCE:  gasdev();                            */
/*                                                         */
/* INPUTS: none                                            */
/*                                                         */
/* OUTPUTS: returns gaussian random deviate                */
/*                                                         */
/* SUBROUTINES: MTseed()                                   */
/*                                                         */
/* CALLED BY: apply_psf(), sourcedis()                     */
/*                                                         */

double gasdev(HDmt_state *MTstate){

    double fac=0,rsq=0,v1=0,v2=0;

    do {
	v1=2.0*HDmt_drand(MTstate)-1.0;
	v2=2.0*HDmt_drand(MTstate)-1.0;
	rsq=v1*v1+v2*v2;
    } while (rsq >= 1.0 || rsq == 0.0);
    fac=sqrt(-2.0*log(rsq)/rsq);

    return v2*fac;
}



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



/* FUNCTION NAME: RMFsensitivity                                */
/*                                                              */
/* PURPOSE: Report the energy range for which the RMF           */
/*             sensitivity is nonzero.                          */
/*                                                              */
/* INPUTS: rmffile                                             */
/*                                                              */
/* OUTPUTS: sens_lo, sens_hi                                    */
/*                                                              */
/* CALLED BY: read_RMF                                          */
/*                                                              */

void RMFsensitivity(char * rmffile,   /* RMF fits filename */
		    double *sens_lo,   /* min sensitivity */
		    double *sens_hi){  /* max sensitivity */

    fitsfile * ounit = NULL;  /* fits file unit */
    double *energ_lo=0;         /* low bin energy array */
    double *energ_hi=0;         /* high bin energy array */
    double *energ_ave=0;        /* average bin energy array */
    double *temparr=0;          /* temporary double array */
    double *sensitivity=0;      /* energy sensitivity */
    double sum=0;               /* sum of sensitivity matrix */
    char * comment=NULL;      /* keyword comment */
    char en_unit[20];         /* energy units */
    char en_keyword[8];       /* name of energy keyword */
    long repeat=0;              /* number of elements in each matrix row */
    long width=0;               /* memory size if each matrix row */
    long nrow=0, ncol=0;      /* dimensions of fits table */
    int fstat=0;              /* status flag for fits operations */
    int colnum=0;               /* column number */
    int anynull=0;            /* a null value */
    int ii=0, jj=0;               /* loop index */
    int typecode=0;             /* matrix data type */


    /* open the fits file and get dimension keywords */
    fits_open_file(&ounit,rmffile,READONLY,&fstat);
    fits_movnam_hdu(ounit, BINARY_TBL, "MATRIX", 0, &fstat);
    fits_read_key_lng(ounit, "TFIELDS", &ncol, comment, &fstat);
    fits_read_key_lng(ounit, "NAXIS2", &nrow, comment, &fstat);

    /* allocated necessary arrays */
    energ_lo = calloc(nrow, sizeof(double));
    energ_hi = calloc(nrow, sizeof(double));
    energ_ave = calloc(nrow, sizeof(double));
    sensitivity = calloc(nrow, sizeof(double));

    /* read the energ_lo column */
    fits_get_colnum(ounit, CASEINSEN, "ENERG_LO", &colnum, &fstat);
    fits_read_col_dbl(ounit, colnum, 1, 1, nrow, anynull, energ_lo, NULL, &fstat);

    /* get the energy unit */
    sprintf(en_keyword,"TUNIT%d",colnum);
    if (fits_read_key_str(ounit, en_keyword, en_unit, comment, &fstat)){
	sprintf(en_unit," ");
	fstat=0;
    }

    /* read the energ_hi column */
    fits_get_colnum(ounit, CASEINSEN, "ENERG_HI", &colnum, &fstat);
    fits_read_col_dbl(ounit, colnum, 1, 1, nrow, anynull, energ_hi, NULL, &fstat);

    /* take the average energy from each bin */
    for (ii=0; ii<nrow; ii++)
        energ_ave[ii] = 0.5*(energ_hi[ii]+energ_lo[ii]);

    /* get the dimensions of the sensitivity matrix column */
    fits_get_colnum(ounit, CASEINSEN, "MATRIX", &colnum, &fstat);
    if (fits_get_eqcoltype(ounit, colnum, &typecode, &repeat, &width, &fstat)){
	printf("Could not get RMF Matrix datatype, skipping [non-essential] RMF Energy Sensitivity check.\n\n");
	printf("Fits error status = %d\n",fstat);
	return;
    }

    /* the sum of each matrix populates the sensitivity array */
    for (ii=0; ii<nrow; ii++){
        sum = 0;
        temparr = calloc(repeat, sizeof(double));
        fits_read_col_dbl(ounit, colnum, ii+1, 1, repeat, anynull, temparr, NULL, &fstat);
        for (jj=0; jj<repeat; jj++)
            sum += temparr[jj];
        free(temparr);
        sensitivity[ii] = sum;
    }

    /* get the minimum nonzero sensitivity */
    *sens_lo = energ_ave[0];
    ii=0;
    while(sensitivity[ii] == 0)
        *sens_lo = energ_ave[ii++];

    /* get the maximum nonzero sensitivity */
    *sens_hi = energ_ave[nrow-1];
    ii=nrow-1;
    while(sensitivity[ii]==0)
        *sens_hi = energ_ave[ii--];

    /* report results */
    printf("Nonzero RMF Energy Sensitivity: %f - %f %s.\n\n",*sens_lo, *sens_hi, en_unit);

    /* clean up */
    fits_close_file(ounit,&fstat);
    free(energ_lo);
    free(energ_hi);
    free(energ_ave);
    free(sensitivity);

}



/* FUNCTION NAME: trim_string                                   */
/*                                                              */
/* PURPOSE: Remove white space at beginning and end of string   */
/*                                                              */
/* INPUTS/OUTPUTS: str                                          */
/*                                                              */
/* CALLED BY: spectra                                           */
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

    /* Move the front and back pointers to address
     * the first non-whitespace characters from
     * each end.
     */
    while( isspace(*(++frontp)) );
    while( isspace(*(--endp)) && endp != frontp );

    if( str + len - 1 != endp )
	*(endp + 1) = '\0';
    else if( frontp != str &&  endp == frontp )
	*str = '\0';

    /* Shift the string so that it starts at str so
     * that if it's dynamically allocated, we can
     * still free it on the returned pointer.  Note
     * the reuse of endp to mean the front of the
     * string buffer now.
     */
    endp = str;
    if( frontp != str )
	{
            while( *frontp ) *endp++ = *frontp++;
            *endp = '\0';
	}


    return str;
}


/* FUNCTION NAME: warn_if_detector_gap                                   */
/*                                                                       */
/* PURPOSE:                                                              */
/*    Notify the user if a source has been specified with RA,Dec         */
/*    that coincides with known gaps or bad pixels in the CCD detector.  */
/*                                                                       */
/* CALLING SEQUENCE:                                                     */
/*    warn_if_detector_gap(&s_obs, &s_mdb, nsource, ras, decs);          */
/*                                                                       */
/* INPUTS: s_obs, s_mdb, nsource, ras, decs                              */
/*                                                                       */
/* OUTPUTS: none - just message printed to screen                        */
/*                                                                       */
/* CALLED BY:  main()                                                    */
/*                                                                       */

void warn_if_detector_gap(ObsParams *s_obs,  /* structure containing observatory params */
			  MDB_params *s_mdb, /* structure containing MDB params */
			  int nsource,       /* number of sources */
			  double *ras,        /* RA array for sources */
			  double *decs,      /* Dec array for sources */
			  int debug){

    int isource = 0;   /* loop index over sources */
    char *check = NULL;       /* string indicating warning status */

    double xs_roll=0.0, ys_roll=0.0;  /* rolled xy pos of source */
    double xfoc=0.0, yfoc=0.0;        /* xy in foc coords */
    double xref=0.0, yref=0.0;        /* reference xy location */

    int pixel_status = 1;

    printf("\nChecking if sources fall within %s %s detector (set debug=yes for more detail)...\n",s_obs->mission,s_obs->instrume);
    printf("Pointing:  RA = %f   Dec = %f\n",s_obs->rapoint, s_obs->decpoint);

    for (isource=0; isource<nsource; isource++){
	pixel_status = 1; //initialize as OK
	check="OK!";

	rd2xy(ras[isource],decs[isource],xref,yref,s_obs->rapoint, s_obs->decpoint, s_obs->roll, 
	      s_mdb->cdltx, s_mdb->cdlty, &xs_roll, &ys_roll);
	xfoc = s_mdb->AIM_FOCX + xs_roll * (s_mdb->cdltx / s_mdb->cdltx_foc);
	yfoc = s_mdb->AIM_FOCY + ys_roll * (s_mdb->cdlty / s_mdb->cdlty_foc);
                                  
	pixel_status = isrealpixel(s_mdb, xfoc, yfoc);

	if (pixel_status!=1){
	    check="WARNING - SOURCE MAY BE OFF DETECTOR!";

	} else if (s_mdb->instmap_flag > 0 && pixel_status==1) {
	    pixel_status = apply_instmap(s_mdb, xfoc, yfoc);
	    if (pixel_status!=1) check="WARNING - SOURCE MAY BE IN DETECTOR GAP!";
	}

	if ( (0 != strcasecmp(check,"OK!")) || (debug == 1) )
	    printf("Source %d:  RA = %f   Dec = %f   %s\n",isource+1, ras[isource], decs[isource],check);
    }    
}



/* FUNCTION NAME: rollxy                                                    */
/*                                                                          */
/* PURPOSE:                                                                 */
/*    Roll (xin,yin) to (xout,yout) by angle roll.                          */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*    rollxy(xin, yin, x_ref, y_ref, roll, xpixsize, ypixsize, xout, yout); */
/*                                                                          */
/* INPUTS: xin,yin,x_ref,y_ref,xpixsize,ypixsize                            */
/*                                                                          */
/* OUTPUTS: xout,yout                                                       */
/*                                                                          */
/* CALLED BY: doWork                                                        */
/*                                                                          */

void rollxy(double xin,       /* input x */
	    double yin,       /* input y */
	    double x_ref,     /* reference x value */
	    double y_ref,     /* reference y value */
	    double roll,      /* roll */
	    double xpixsize,  /* degrees per pixel, x-direction, positive/negative if look-down/up */
	    double ypixsize,  /* degrees per pixel, y-direction */
	    double *xout,      /* output x */
	    double *yout){     /* output y */

    double gamma1=0;
    double singam=0, cosgam=0, dsingam=0;

    gamma1 = -D2R * roll; 
    
    singam = sin(gamma1);
    cosgam = cos(gamma1);
    
    dsingam = (ypixsize/xpixsize) * singam;
    
    *xout = x_ref - cosgam * (x_ref - xin) + dsingam * (yin - y_ref);
    *yout = y_ref + cosgam * (yin - y_ref) + dsingam * (x_ref - xin);
	
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
/* CALLED BY: getPars(), read_instmap()                                     */
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

	if ( strstr(input_string,"$") == NULL ) {
	    
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
    strcpy(relpath,"$HEADAS/../heasim/heasimc");
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


/* FUNCTION NAME: array_concat_dbl                                      */
/*                                                                      */
/* CALLING SEQUENCE:                                                    */
/*    array = array_concat_dbl(A, nA, B, nB, C, nC);                    */
/*                                                                      */
/* PURPOSE: concatenate three double arrays together                    */
/*                                                                      */
/* INPUTS: A, nA, B, nB, C, nC                                          */
/*                                                                      */
/* OUTPUTS: returns concatenated double array of size nA+nB+nC          */
/*                                                                      */
/* CALLED BY: main()                                                    */
/*                                                                      */
double * array_concat_dbl(double * A, int nA,
                          double * B, int nB,
                          double * C, int nC){

    int Ntot = nA + nB + nC;
    double * D = calloc(Ntot, sizeof(double));
    int n_counter = 0;

    for (int ii=0; ii<nA; ii++){
        D[n_counter] = A[ii];
        n_counter++;
    }

    for (int ii=0; ii<nB; ii++){
        D[n_counter] = B[ii];
        n_counter++;
    }

    for (int ii=0; ii<nC; ii++){
        D[n_counter] = C[ii];
        n_counter++;
    }

    if (n_counter != Ntot)
        printf("ERROR: double array concatenation mismatched!\n");

    return D;
}


/* FUNCTION NAME: array_concat_int                                      */
/*                                                                      */
/* CALLING SEQUENCE:                                                    */
/*    array = array_concat_int(A, nA, B, nB, C, nC);                    */
/*                                                                      */
/* PURPOSE: concatenate three int arrays together                       */
/*                                                                      */
/* INPUTS: A, nA, B, nB, C, nC                                          */
/*                                                                      */
/* OUTPUTS: returns concatenated int array of size nA+nB+nC             */
/*                                                                      */
/* CALLED BY: main()                                                    */
/*                                                                      */
int * array_concat_int(int * A, int nA,
		       int * B, int nB,
		       int * C, int nC){
    
    int Ntot = nA + nB + nC;
    int * D = calloc(Ntot, sizeof(int));
    int n_counter = 0;

    for (int ii=0; ii<nA; ii++){
        D[n_counter] = A[ii];
        n_counter++;
    }

    for (int ii=0; ii<nB; ii++){
        D[n_counter] = B[ii];
        n_counter++;
    }

    for (int ii=0; ii<nC; ii++){
        D[n_counter] = C[ii];
        n_counter++;
    }

    if (n_counter != Ntot)
        printf("ERROR: int array concatenation mismatched!\n");

    return D;
}

/* FUNCTION NAME: array_concat_str                                      */
/*                                                                      */
/* CALLING SEQUENCE:                                                    */
/*    array = array_concat_str(A, nA, B, nB, C, nC);                    */
/*                                                                      */
/* PURPOSE: concatenate three string arrays together                    */
/*                                                                      */
/* INPUTS: A, nA, B, nB, C, nC                                          */
/*                                                                      */
/* OUTPUTS: returns concatenated string array of size nA+nB+nC          */
/*                                                                      */
/* CALLED BY: main()                                                    */
/*                                                                      */
char ** array_concat_str(char ** A, int nA,
                         char ** B, int nB,
                         char ** C, int nC){

    int Ntot = nA + nB + nC;
    int n_counter = 0;
    char ** D = calloc(Ntot, sizeof(char *));
    for (int ii=0; ii<Ntot; ii++)
        D[ii] = calloc(STR_MAX_LEN,sizeof(char));

    for (int ii=0; ii<nA; ii++){
        strcpy(D[n_counter],A[ii]);
        n_counter++;
    }

    for (int ii=0; ii<nB; ii++){
        strcpy(D[n_counter],B[ii]);
        n_counter++;
    }

    for (int ii=0; ii<nC; ii++){
        strcpy(D[n_counter],C[ii]);
        n_counter++;
    }

    if (n_counter != Ntot)
        printf("ERROR: string array concatenation mismatched!\n");

    return D;
}
    


/* FUNCTION NAME: matrix_concat_dbl                                                */
/*                                                                                 */
/* CALLING SEQUENCE:                                                               */
/*    mat = matrix_concat_dbl(A, nrowsA, B, nrowsB, C, nrowsC, ncols);             */
/*                                                                                 */
/* PURPOSE: concatenate three double matrix together                               */
/*                                                                                 */
/* INPUTS: A, nrowsA, B, nrowsB, C, nrowsC, ncols                                  */
/*                                                                                 */
/* OUTPUTS: returns concatenated double matrix of size nrowsA+nrowsB+nrowsC rows   */
/*          by ncols                                                               */
/*                                                                                 */
/* CALLED BY: main()                                                               */
/*                                                                                 */
double ** matrix_concat_dbl(double ** A, int nrowsA,
			    double ** B, int nrowsB,
			    double ** C, int nrowsC,
			    int ncols){
    int row_counter = 0;
    int nrows = nrowsA + nrowsB + nrowsC;

    double ** D = calloc(nrows, sizeof(double *));
    for (int ii=0; ii<nrows; ii++)
	D[ii] = calloc(ncols, sizeof(double));

    /* Append the original source data to sd_matrix */
    for (int isrc=0; isrc<nrowsA; isrc++){
        for (int imat = 0; imat<ncols; imat++){
            D[row_counter][imat] = A[isrc][imat];
        }
        row_counter++;
    }

    /* Append point source background to sd_matrix */
    for (int isrc=0; isrc<nrowsB; isrc++){
        for (int imat = 0; imat<ncols; imat++){
            D[row_counter][imat] = B[isrc][imat];
        }
        row_counter++;
    }

    /* Append diffuse background to sd_matrix */
    for (int isrc=0; isrc<nrowsC; isrc++){
        for (int imat = 0; imat<ncols; imat++){
            D[row_counter][imat] = C[isrc][imat];
        }
        row_counter++;
    }

    /* Does row_counter now equal the total number of sources? */
    if (row_counter != nrows)
        printf("ERROR: problem concatenating sd_matrix!\n");

    return D;
}



/* FUNCTION NAME: copy_file                                                        */
/*                                                                                 */
/* CALLING SEQUENCE:                                                               */
/*    errstsatus = copy_file(source,target,debug)                                  */
/*                                                                                 */
/* PURPOSE: copy a file without issuing system command "cp"                        */
/*                                                                                 */
/* INPUTS: source, target, debug                                                   */
/*                                                                                 */
/* OUTPUTS: copies the file and returns integer error status                       */
/*                                                                                 */

int copy_file(char * source, char * target, int debug){

    FILE *fp1=0, *fp2=0;
    char buffer[1000000];
    size_t nn = 0;
    
    fp1 = fopen(source,"r");
    fp2 = fopen(target,"w");
    
    if (debug == 1) printf("   Copying %s to %s.\n",source,target);

    while ((nn = fread(buffer, sizeof(char), sizeof(buffer), fp1)) > 0){
	if (fwrite(buffer, sizeof(char), nn, fp2) != nn){
	    printf("ERROR: Write failed.\n");
	    fclose(fp1);
	    fclose(fp2);
	    return -1;
	}
    }

    fclose(fp1);
    fclose(fp2);
    return 0;
}

/* FUNCTION NAME: clean_pi_chan                                                    */
/*                                                                                 */
/* CALLING SEQUENCE:                                                               */
/*    result = (&xe, &ye, &ide, &pi_chans, nkeep);                                 */
/*                                                                                 */
/* PURPOSE: if C_ReturnChannel() has returned a -1, meaning that the event has     */
/*          fallen off the end of the channel array, then remove the event and     */
/*          its x, y, pixel id, and pi_chan values, and resize the arrays to       */
/*          exclude them.                                                          */
/*                                                                                 */
/* INPUTS: xe, ye, ide, pi_chans, nkeep                                            */
/*                                                                                 */
/* OUTPUTS: xe, ye, ide, pi_chans, returns updated nkeep                           */
/*                                                                                 */

long clean_pi_chan(int ** xe, int ** ye, int ** ide, long ** pi_chans, int nkeep){
    long kk = 0;
    long counter = 0;
    int * temp_xe = 0;
    int * temp_ye = 0;
    long * temp_pi = 0;
    int * temp_ide = 0;
    long new_nkeep = 0;

    for (kk=0; kk<nkeep; kk++){
	if ( (*pi_chans)[kk] != -1){
	    counter++;
	}
    }

    temp_xe = calloc(counter, sizeof(int));
    temp_ye = calloc(counter, sizeof(int));
    temp_pi = calloc(counter, sizeof(long));
    temp_ide = calloc(counter, sizeof(int));
    counter = 0;

    for (kk=0; kk<nkeep; kk++){
	if ( (*pi_chans)[kk] != -1){
	    temp_xe[counter] = (*xe)[kk];
	    temp_ye[counter] = (*ye)[kk];
	    temp_pi[counter] = (*pi_chans)[kk];
	    temp_ide[counter] = (*ide)[kk];
	    counter++;
	}
    }

    new_nkeep = counter;

    free(*xe);
    free(*ye);
    free(*pi_chans);
    free(*ide);
    (*xe) = calloc(new_nkeep, sizeof(int));
    (*ye) = calloc(new_nkeep, sizeof(int));
    (*pi_chans) = calloc(new_nkeep, sizeof(long));
    (*ide) = calloc(new_nkeep, sizeof(int));
    
    for (kk=0; kk<new_nkeep; kk++){
	(*xe)[kk] = temp_xe[kk];
	(*ye)[kk] = temp_ye[kk];
	(*pi_chans)[kk] = temp_pi[kk];
	(*ide)[kk] = temp_ide[kk];
    }

    free(temp_xe);
    free(temp_ye);
    free(temp_pi);
    free(temp_ide);

    return new_nkeep;
}

/*
 ! $Log: utils.c,v $
 ! Revision 1.59  2016/09/01 16:40:19  asargent
 ! Fixed bug in resolve_filename when searching for file with relative path.
 !
 ! Revision 1.58  2016/03/30 15:33:22  driethmi
 ! Corrected bug in find_index(), where it could return an index beyond
 ! the bounds of the array.
 !
 ! Revision 1.57  2016/03/23 20:19:15  driethmi
 ! Updated warn_if_detector_gap() routine.  (Actually, its name is misleading,
 ! we warn if the point source or centroid of extended source falls in
 ! detector gaps or outside the field of view.)
 !
 ! Revision 1.56  2015/10/16 15:40:43  driethmi
 ! Changes to make sure pixid column is being sorted along with x, y, and pi
 ! according to time.  Problem didn't show up until pileup was engaged.
 !
 ! Revision 1.55  2015/09/30 15:44:35  driethmi
 ! Modified RMFsensitivity() not to choke if the unit entry on column ENERG_HI or
 ! ENERG_LO is not found.
 !
 ! Revision 1.54  2015/09/28 19:56:12  driethmi
 ! Added block to exit RMFsensitivity() in the event that the MATRIX data type
 ! is not recognized.  This is not an essential function, and serves only to
 ! report diagnostic information to the user.
 !
 ! Revision 1.53  2015/07/13 20:27:23  driethmi
 ! Cleaned up definition of roll.
 !
 ! Revision 1.52  2015/07/01 15:00:33  driethmi
 ! When we do time sorting, we need to re-order X, Y, and PI to match.  Several
 ! mods to accommodate this - new structure "quad" that holds four doubles,
 ! some new sorting routines.
 !
 ! Revision 1.51  2015/06/25 18:15:42  driethmi
 ! Put more print statements under "debug" mode only.  Also changed order of
 ! "final event count" statements at the end of doWork().  Report that we've removed
 ! bad PI events before we report final event count; no change in functionaly,
 ! but let's not confuse the user.
 !
 ! Revision 1.50  2015/06/11 19:54:12  driethmi
 ! Commenting and minor cosmetic changes.
 !
 ! Revision 1.49  2015/06/11 18:35:17  driethmi
 ! Reworked flag_pileup() routine to accommodate new time-block layout.
 !
 ! Revision 1.48  2015/06/05 20:10:42  driethmi
 ! Added routine to compare doubles.
 !
 ! Revision 1.47  2015/06/02 20:32:04  driethmi
 ! Modified clean_pi_chans() routine so that nkeep does not need to be
 ! overwritten.
 !
 ! Revision 1.46  2015/05/29 17:50:48  driethmi
 ! Added function clean_ip_chans(), which removes events for which pi_chans has
 ! a value of -1, indicating that this event fell of the end of the channel array.
 ! It then resizes the xe, ye, ide, and pi_chans arrays to the correct size.  We now
 ! do this inline with the code, instead of at the end of heasim.
 !
 ! Revision 1.45  2015/05/22 17:24:36  driethmi
 ! Updated parameter names throughout code, changed header keywords/comments
 ! to match standard values.  Changed input user seed so that seed=0 triggers
 ! seeding from the system time.
 !
 ! Revision 1.44  2015/05/20 20:12:20  driethmi
 ! Modified mdb parameter names, and instances of these names in the code.  Also
 ! have updated values for mdb parameters.
 !
 ! Revision 1.43  2015/05/07 18:23:29  driethmi
 ! In resolve_pathname(), change critical OR to AND.
 !
 ! Revision 1.42  2015/05/04 17:47:09  driethmi
 ! Removed realpath() calls - discovered realpath behavior may differ between
 ! platforms.
 !
 ! Revision 1.41  2015/05/04 16:16:33  driethmi
 ! Implemented better variable initializations.
 !
 ! Revision 1.40  2015/05/04 13:52:12  driethmi
 ! Added more robust check in resolve_pathname if tempstring is NULL.
 !
 ! Revision 1.39  2015/04/08 15:37:45  driethmi
 ! Replaced system call to copy files (during re-sorting at the end of doWork)
 ! with compiled function that achieves same result.  Other minor cosmetic
 ! changes.
 !
 ! Revision 1.38  2015/04/07 15:47:49  driethmi
 ! Corrected non-critical compilations warnings; unused variables, etc.
 !
 ! Revision 1.37  2015/03/25 20:58:10  driethmi
 ! Improved resolve_pathname() to return the relative path with ENV variables
 ! replaced, in the case where the full resolved pathname may not exist.
 !
 ! Revision 1.36  2015/03/19 20:35:30  driethmi
 ! Added burst capability, and updated function comment blocks.
 !
 ! Revision 1.35  2015/02/18 21:34:07  driethmi
 ! Corrected bug in process_image, needed to use fits_read_key_dbl instead of
 ! fits_read_key_flt - lack of precision was causing zeroes to be read.  Also
 ! improved verbosity of debug statements.
 !
 ! Revision 1.34  2015/02/18 15:53:41  driethmi
 ! Preliminary redshift changes, and cleaned up output chatter so debug = 1
 ! is more useful.
 !
 ! Revision 1.33  2015/01/21 17:25:03  driethmi
 ! Modified heasim to accept data files from sky background tool.
 !
 ! Revision 1.32  2014/12/02 19:57:40  driethmi
 ! Changed floats to doubles for consistency, except where float is required.
 !
 ! Revision 1.31  2014/10/29 18:16:24  driethmi
 ! Removed unnecessary print statement in resolve_pathname()
 !
 ! Revision 1.30  2014/10/29 18:09:52  driethmi
 ! Fixed bug in resolve_pathname(), so that filenames that do not include "$"
 ! or ".." in the string are passed back untouched.
 !
 ! Revision 1.29  2014/10/02 18:07:31  driethmi
 ! Added routine to query most recent CVS commit date in all .c files that
 ! are built with simulator, and return the date string as the version number.
 !
 ! Revision 1.28  2014/07/31 13:43:50  driethmi
 ! SXI aimpoint is no longer at center.  Disabled section of "warn_if_detector_gap"
 ! that checks for this.  As is now, we only check to ensure sources fall
 ! within detector area.
 !
 ! Revision 1.27  2014/07/09 20:43:37  driethmi
 ! Corrected previous change - still need roll2 parameter.
 !
 ! Revision 1.26  2014/07/07 19:29:47  driethmi
 ! Commented out xfoc, yfoc, xs_roll, ys_roll in warn_if_detector_gap.
 !
 ! Revision 1.25  2014/07/07 19:28:18  driethmi
 ! Previous commit did not solve unused variable warnings completely.
 !
 ! Revision 1.24  2014/07/07 19:27:04  driethmi
 ! Eliminated "roll2" as an "unused variable" compiler warning.
 !
 ! Revision 1.23  2014/07/07 15:36:16  driethmi
 ! Fixed typos in rollxy.
 !
 ! Revision 1.22  2014/07/01 13:47:16  driethmi
 ! Current state - added roll angle and image HDU capability.  Still some
 ! issues to work out with response file and background read.  Also, imagedis
 ! seems to have execution bottleneck.
 !
 ! Revision 1.21  2014/06/12 20:40:40  driethmi
 ! Previous commit did not completely fix ra and dec types.
 !
 ! Revision 1.20  2014/06/12 20:37:20  driethmi
 ! In xy2rd routine, need ra and dec input to be *ra and *dec, so that
 ! we can pass them back.
 !
 ! Revision 1.19  2014/06/12 18:53:22  driethmi
 ! Missed a change in the comments, to go with xy2rd.  "roll" is now "theta".
 !
 ! Revision 1.18  2014/06/12 18:31:30  driethmi
 ! Rewrote xy2rd routine, to be inverse of rd2xy, in utils.c.
 !
 ! Revision 1.17  2014/05/28 23:02:21  driethmi
 ! Added catch in interpol_hunt that returns the low or high endpoint if
 ! the attempted interpolation point falls off the low or high end,
 ! respectively.
 !
 ! Revision 1.16  2014/05/23 22:51:24  driethmi
 ! Added routine that warns the user if sources are specified with RA,DEC
 ! that lie within the CCD detector gaps of the instrument being used.
 !
 ! Revision 1.15  2014/05/23 21:51:38  driethmi
 ! Changed random number generator such that MTstate is passed down through
 ! each function using it.  Now, user must specify seed.  If seed > 0, it is
 ! used to seed the RNG.  If seed <=0, we seed using the system time.
 !
 ! Revision 1.14  2014/05/08 20:00:34  driethmi
 ! Routine that reads in user spectral file was overly stringent about the
 ! formatting of white space.  Made this routine more robust by removing
 ! white space before and after the line, and then parsing.
 !
 ! Revision 1.13  2014/04/29 21:10:52  driethmi
 ! rd2xy rewrite needs special consideration for corner-rotation cases, i.e.
 ! when the source RA and Dec is directly overtop the pointing RA and Dec.
 !
 ! Revision 1.12  2014/04/28 20:02:30  driethmi
 ! Updated algorithm for rd2xy - now returns coordinates in simulator xy
 ! rather than sky xy.
 !
 ! Revision 1.11  2014/04/09 14:44:05  driethmi
 ! Added routine to report actual nonzero energy sensitivity of RMF.  If,
 ! for example, spectral lines are given outside of this range, we can expect
 ! a null simulation result.
 !
 ! Revision 1.10  2014/04/03 20:41:06  driethmi
 ! Updated commenting and minor changes to attempt to comply more with coding
 ! standards.  Added doxygen tags.
 !
*/
