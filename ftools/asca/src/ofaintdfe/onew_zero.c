/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/onew_zero.c,v 3.9 2000/09/14 14:34:56 peachey Exp $   */
/*                   */
/*
   new_zero5.41.c
                originally coded by Ezuka
      95/8/10   modified         by C.Otani
*/

#define  DELTA    	1.0E-05	/* iteration limit                   */
#define	 DARK_LV_UP    40.0	/* Discriminate level                */
                                /*      for dark level upper set     */
#define	 DARK_LV_LO   -40.0	/*	for dark level lower set     */

double 	new_zero();
double  g_func();
double  q_g_func();
double  qromb();

/********************/
/*  function        */
/********************/
double g_func( sensor,  ccdmode,  q)
int sensor,  ccdmode;
double q;
{
  double G, P;

  G = 1.0/sqrt(2.0*PI)/sig[sensor][ccdmode] 
    * exp(-pow((q-q0[sensor][ccdmode])/sig[sensor][ccdmode],2.0)/2.0);
  P = 1.0/(2.0*Q[sensor][ccdmode])
    * exp(pow(sig[sensor][ccdmode]/Q[sensor][ccdmode],2.0)/2.0
    -(q-q0[sensor][ccdmode])/Q[sensor][ccdmode])
    *( 2.0 - erfcc_fdfe(1.0/sqrt(2.0)*((q-q0[sensor][ccdmode])/sig[sensor][ccdmode]
    -sig[sensor][ccdmode]/Q[sensor][ccdmode])) );
  
  return ( ( 1.0 - ffff[sensor][ccdmode] ) * G + ffff[sensor][ccdmode] * P );
}


double q_g_func( sensor, ccdmode,  q) 
int sensor,  ccdmode;
double q; 
{
  return ( q * g_func(sensor,ccdmode,q) );
}


/********************/
/*  calculate zero  */
/********************/
double new_zero(  sensor,  ccdmode )
int sensor,  ccdmode ;
{
  double   lower, upper;
  double   zero, newzero;

  newzero = q0[sensor][ccdmode];

  /*  Iteration  */
  do {
    zero  = newzero;
    lower = zero + DARK_LV_LO;
    upper = zero + DARK_LV_UP;
    newzero = qromb ( (double (*) () )q_g_func,sensor,ccdmode,lower,upper)   / qromb ( (double (*) () )g_func,  sensor,ccdmode,lower,upper);
  } while (fabs(newzero - zero) > DELTA);

  return newzero;

}

