/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/ogetTemplate.c,v 3.9 2000/09/14 14:34:56 peachey Exp $   */
/*                   */
/*
   getTemplate.c for Ver.5.41
      95.12.20   Bug fixed for "-l" option
*/

#define PI   3.1415926535897932385
#define NORM 1.0

int makeTemplate(t, sensor, ccdmode, dst_0)
		 double t;
		 int sensor;
		 int ccdmode;
		 double dst_0[3][2*MAX_A+1];
		 
{
  double tt;
  double q;
  double G, P;
  int    i, j;
  AVE_SIG b;

  tt = t - ASCA_t0;
  ffff[sensor][ccdmode] = 1.0 - exp(-pow((tt-tf0[sensor][ccdmode])/Tf[sensor][ccdmode],4.0));
  sig[sensor][ccdmode]  = ss1[sensor][ccdmode]*tt + ss0[sensor][ccdmode];
  Q[sensor][ccdmode]    = QQ1[sensor][ccdmode]*tt + QQ0[sensor][ccdmode];
  q0[sensor][ccdmode]   = qq1[sensor][ccdmode]*tt + qq0[sensor][ccdmode];


/*   set dst_a[ccdmode][]   */
  for (i=(-MAX_A);i<=MAX_A;i++){
    q = (double)i;
    G = 1.0/sqrt(2.0*PI)/sig[sensor][ccdmode] 
      * exp(-pow((q-q0[sensor][ccdmode])/sig[sensor][ccdmode],2.0)/2.0);
    P = 1.0/(2.0*Q[sensor][ccdmode])
      * exp(pow(sig[sensor][ccdmode]/Q[sensor][ccdmode],2.0)/2.0
      -(q-q0[sensor][ccdmode])/Q[sensor][ccdmode])
      *( 2.0 - erfcc_fdfe(1.0/sqrt(2.0)*((q-q0[sensor][ccdmode])/sig[sensor][ccdmode]
      -sig[sensor][ccdmode]/Q[sensor][ccdmode])) );
    dst_0[ccdmode][i+MAX_A] = NORM*
      ((1.0-ffff[sensor][ccdmode])*G+ffff[sensor][ccdmode]*P);
  }


/*   Calculate Averages and Errors   */
  b.ave = b.sig = b.n = 0.0;
  for (j= -MAX_A;j<=MAX_A;j++){
    b.ave += dst_0[ccdmode][j+MAX_A];
    b.sig += ( dst_0[ccdmode][j+MAX_A]*dst_0[ccdmode][j+MAX_A] );
    b.n   += 1.0;
  }
  b.ave /= b.n;
  b.sig /= b.n;
  b.sig = sqrt((b.sig-b.ave*b.ave)*(b.n/(b.n-1.0)));


/*   Normalized by b.sig   */
  for (j= -MAX_A;j<=MAX_A;j++){
    dst_0[ccdmode][j+MAX_A] = ( dst_0[ccdmode][j+MAX_A]-b.ave )/b.sig;
/*debug
    fprintf(stderr,"%d\t%lf\n",j,dst_0[ccdmode][j+MAX_A]);
*/
  }

  return 0;
}

int getTemplate5_41(t, sensor, dst_0)
		   double t;
		   int sensor;
		   double dst_0[3][2*MAX_A+1];
		   
{
  int ccdmode;
  int j;

/*   select CCD mode   */
  if ( LaunchFlag != 0 ){
    fprintf(stderr,"animaldfe5.41: RDD is set to the value at the Launch\n",LaunchFlag);
  }
  if ( LaunchFlag == 0 ){
    ccdmode = 0;    /*   1-CCD mode   */
    makeTemplate(t,sensor,ccdmode,dst_0);
    ccdmode = 1;    /*   2-CCD mode   */
    makeTemplate(t,sensor,ccdmode,dst_0);
    ccdmode = 2;    /*   4-CCD mode   */
    makeTemplate(t,sensor,ccdmode,dst_0);
  }
  else {    /*   1-CCD at Launch: Templates are the same   */
    ccdmode = 0;    /*   1-CCD mode   */
    makeTemplate(ASCA_t0,sensor,ccdmode,dst_0);
    ccdmode = 1;    /*   2-CCD mode   */
    for (j= -MAX_A;j<=MAX_A;j++){
      dst_0[ccdmode][j+MAX_A] = dst_0[0][j+MAX_A];
    }
    ccdmode = 2;    /*   4-CCD mode   */
    for (j= -MAX_A;j<=MAX_A;j++){
      dst_0[ccdmode][j+MAX_A] = dst_0[0][j+MAX_A];
    }
    fprintf(stderr,"animaldfe5.41: 1-CCD Template at Launch is used.\n");
  }
  return 0;
}

