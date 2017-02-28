/*
 * gisres.c
 *
 *	GIS response function		Y.Fukazawa, Y.Ishisaki, Y.Ueda
 *								fukazawa@miranda.phys.s.u-tokyo.ac.jp
 *								ishisaki@miranda.phys.s.u-tokyo.ac.jp
 *								ueda@astro.isas.ac.jp
 *
 *
 * Version 4.0					95/02/03
 *					O-tail parameter etc tuned (see comments below by Y.Ueda)
 *					BeSupport_mesh_trans 0.87 -> 0.84
 *
 * Version 3.3					94/12/9
 *					changeable Xe thickness (Xe_thick)
 *
 * Version 3.2					94/5/15
 *					position check in check_if_new_energy()
 *					gis_psf_sigma() for SimASCA
 *                  PH-E relation little correction arount 6-8keV
 *
 * Version 3.1					94/2/22
 *                  remove absorption filter of Be 1.3um 
 *
 * Version 3.0					94/2/15
 *					gaussian filter E.W. 12eV, sigma=0.2keV at 1.9keV
 *					absorption filter of Be 1.3um 
 *					Xe-L edge energy jump 50,10,10eV -> 65,7,3eV
 *					try 6 value for O-tail kappa
 *                  E-cut-off will be changed to 1-0.5*exp()
 *					Xe L edge energy in efficiency table fix 
 *
 *  Version 2.6					94/1/19
 *					O-tail parameters are returned to exp. value
 *					a little change of value is performed
 *                  E-cut-off will be changed to 1-0.8*exp()
 *
 *  Version 2.5					93/12/16
 *					Static definition of local functions
 *					Ueda supply Be transmission function
 *
 *  Version 2.4					93/12/01
 *					Experimental escape ratio for XeLb[1-3] and XeLc
 *
 *  Version 2.3					93/10/31
 *					O-tail E-cut-off calculation bug fix
 *
 *  Version 2.2					93/09/05
 *					O-tail parameter was corrected to 6kV value
 *					Moreover it was corrected so that Crab was well fitted
 *
 *  Version 2.1					93/08/29
 *					XRT team supply PET transmission
 *					XRT parallax
 *
 *  Version 2.0					93/08/23
 *					new low energy experimental values
 *					Xe M-edge absorption jump
 *					artificial EDGE in 1.14keV
 *					O-tail calculation till 0.3keV
 *
 *  Version 1.5					93/05/13
 *					correct Xe thickness
 *					new Myler thickness by Tawara
 *					new functions	gis_total_spectrum()
 *									gis_point_spread_function()
 *									gis_efficiency()
 *
 *  Version 1.0					93/03/13
 *					use experimental values
 *
 *  functions 
 *
 *  void set_cal_data(double Ex) : set calibration data at Ex(energy) 
 *                                     (This should be called first)
 *  double gis_eff(double energy) : return gis_efficiency with Be,Plasma_shield
 *                                                           Thermal_Shiled,Xe
 *                                     (This should be called second)
 *  double get_ph(void) : get ph channel
 *  double get_Eres(void) : get energy resolution
 *  double get_phkappa(void) : get O-bunpu parameter
 *  double get_escape(double rate[]) : get escape ratio
 *  double get_posres(void) : get position resolution
 *
 */

/*
  gisres version 3.9g Y.Ueda 1995.01.09
  tau = 0.7 -> 0.0
  kappa -> kappa*1.15 

  gisres version 3.9h Y.Ueda 1995.01.09
  ph = ph*1.005
  
  gisres version 3.9h2 Y.Ueda 1995.01.10
  Xe cross section data at 1 keV is skipped
  at 0.85 keV data added(x 1.00), at 0.9 keV x1.12, at 0.9705 keV x1.17 
  
  gisres version 3.9h3 Y.Ueda 1995.01.10
  changed Energy resolution below 0.97 keV -> edge jump is considered

  gisres version 3.9h4 Y.Ueda 1995.01.13
  Xe cross section data at 1.1575keV -> x1.05 

  gisres version 3.9h4-2 Y.Ueda 1995.02.02
  PHLD 20 -> 5

  gisres version 3.9l-6 Y.Ueda 1995.02.03
  gradient 1/2 up to 1/2 peak only for > 2.5keV

*/ 
  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cfortran.h"

char gisres_version[] = "gisres version 4.0";

#define GISINFO	void

#define gNE		1024	/* number of PH channels */
#define gNX		256		/* number of XP channels */
#define gNY		256		/* number of YP channels */
#define Nesc	5		/* number of escape lines */
#define gMX		120		/* XP center channel */
#define gMY		120		/* YP center channel */
#define gNE2	2048	/* number of PH channels */
#define MnKa	5.8942	/* Energy of Mn K-alpha */

double	PH_Mn = 500.0;
#define PH_Ld		5.0

#ifndef M_PI
#define M_PI            3.14159265358979323846
#endif

/* for HVH = 6kV */
#define Mn_Eres0	7.7		/* PH resolution [%] at 5.9keV for HV 8kV */
#define Mn_Eres		8.0		/* PH resolution [%] at 5.9keV for HV 6kV */
#define Mn_posres0	0.50	/* position [mm] resulution at 5.9keV for HV 8kV*/
#define Mn_posres	0.60	/* position [mm] resulution at 5.9keV for HV 6kV*/
/*#define Al_kappa0	0.050	/* O-bunpu kappa at 1.49keV for HV 8kV */
/*#define Al_kappa0	0.045	/* O'-bunpu kappa at 1.49keV for HV 8kV */
/*#define Al_kappa	(Al_kappa0*1.1547)  /* O'-bunpu kappa at 1.49keV for 6kV */
/*#define Al_tau		0.70	/* O'-bunpu tau at 1.49keV for 6kV */

double Be_thick = (105*10e-6);		/* (cm) Be 10.5 um */
#define PET_thick	(100*540e-9)	/* (cm) PET(Mylar) C10H8O4 */
#define Al_thick	(100*37e-9)		/* (cm) Al of alminized PET */
#define ThermalShield_mesh_trans	0.942
#define PlasmaShield_mesh_trans		0.90
#define BeSupport_mesh_trans		0.84	/* 0.87 -> 0.84 @ ver 4.0 */
/*#define Correction_of_trans		0.88	/* 7% XRT, 5% unknown @ ver 4.0 */
#define Xe_pressure	1.152	/* (atm) at 273.15 K */
#define He_pressure	0.048	/* (atm) at 273.15 K */
double Xe_thick	= 1.0;	/* cm */
#define Drift_Region	Xe_thick
#define Scinti_Region	1.5	/* cm */

#define unless(a)	if(!(a))
#define until(a)	while(!(a))
#define squ(x)	((x)*(x))

/* complementary error function approximation */

double
erfcc(x)
double x;
{
        double e, z, t;
        z = abs(x);
        t = 1.0/(1.0+z/2.0);
        e = t * exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+
            t*(0.09678418+t*(-0.18628806+t*(0.27886807+t*(-1.13520398+
            t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))));
        if ( x < 0.0 ) {
           e *= -1.0;
           e += 2.0;
	}
        return e;
}

static struct absorpm1 {
	double lmd_min, lmd_max;	/* 有効範囲 in A*/
	double n[10];				/* 巾 abs = Ciλ**ni */
} absorpm1 = {
	0.17837, 10.0,
	{ 2.83,2.6628,2.6865,2.5825,2.5065,2.328,2.1785,2.11,2.261,2.2199 }
};

static struct absorpm2 {
	char element[10];
	int Z;			/* 原子番号 */
	double density;		/* 密度 (g/cm^3, if gas  at 1.0atm) */
	double dcde[19];	/* C0, エッジの波長 in A, C1, ..., C10 */
} absorpm2[] = {   /* from the encyclopedia of X-rays and Gamma-rays */
	{ "He",2,0.000179,
	  {0.195,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
	{ "Be",4,1.84,
	  {0.475,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
	{ "Al",13,2.702,
	  {14.30,7.951,1.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
	{ "Cu",29,8.940,
	  {129.20,1.3804,17.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
	{ "Mo",42,10.2,
	  {338.07,0.61977,50.48,4.32066,33.06,4.71330,27.69,4.90930,10.75,0,0,0,0,0,0,0,0,0,0}},
	{ "Xe",54,0.005896,
	  {651.81,0.35849,104.72,2.27449,72.66,2.43058,57.06,2.59330,21.2,0,0,0,0,0,0,0,0,0,0}},
	{ "PET-C",6,1.41*0.62502,
	  {1.55,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
	{ "PET-H",1,1.41*0.04196,
	  {0.0025,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
	{ "PET-O",8,1.41*0.33018,
	  {3.52,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
};

struct absorption_data {
	char	*element;
	double	c[10],edge[9],den,thick;
	int		windo;
};

struct Cross_Section {
	double ee,cs;   /* energy cross_section (cm^2/g) */
};
/* 0.3-1.5  from graph: 1.5-20.0 from table : ref Lester Library (Contact T.Ohashi) */

/* Y.Ueda data at 1 keV is skipped */
/* at 0.85 keV data added(x 1.00), at 0.9 keV x1.12, at 0.9705 keV x1.17 */
/* at 1.1575keV x1.05 */
static struct cs_data {
	char mat[10];
	double atom_weight;
	struct Cross_Section cross_section[35];
} cs_data[] = {
	{ "Xe", 131.3, {
		{  0.30, 8252.8 },
		{  0.35, 7335.9 },
		{  0.40, 6418.9 },
		{  0.45, 5593.6 },
		{  0.50, 4584.9 },
		{  0.60, 3667.9 },
		{  0.6965, 2292.5 },
		{  0.6975, 26592.5 },
		{  0.70, 25675.5 },
		{  0.75, 16047.2 },
		{  0.80, 11462.3 },
		{  0.849367, 9421.6 },
		{  0.90, 8729.7 },
		{  0.9705, 8046.6 },
		{  0.9715, 9169.8 },
		{  1.1565, 6877.4 },
	   	{  1.1575, 7702.7 },
		{  1.5, 4376.0 },
		{  2.0, 2166.0  },
		{  3.0, 805.6  },
		{  4.0, 400.5  },
		{  4.780, 260.3  },
		{  4.790, 739.6 },
		{  5.0, 659.1 },
		{  5.100, 626.6 },
		{  5.110, 881.0 },
		{  5.450, 741.5 },
		{  5.456, 859.0 },
		{  6.0, 669.5 },
		{  8.0, 314.0 },
		{  10.0, 173.2 },
		{  15.0, 57.9 },
		{  20.0, 26.4 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 } }
	},
	{ "O", 16.0, {
		{ 0.3, 5643.8 },
		{ 0.4, 2633.8 },
		{ 0.5, 1316.9 },
		{ 0.5315, 1128.8 },
		{ 0.5325, 20693.8 },
		{ 0.6, 15050.0 },
		{ 0.7, 11287.5 },
		{ 0.8, 7525.0 },
		{ 0.9, 5643.7 },
		{ 1.0, 4515.0 },
		{ 1.5, 1570.0 },
		{ 2.0, 706.2 },
		{ 3.0, 219.2 },
		{ 4.0, 93.1 },
		{ 5.0, 47.4 },
		{ 6.0, 27.1 },
		{ 8.0, 11.2 },
		{ 10.0, 5.67 },
		{ 15.0, 1.72 },
		{ 20.0, 0.814 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 },
		{ 0.0, 0.0 } }
	}
};

static void
set_matter(matter, name, windo, thick)
struct absorption_data *matter;
char *name;
int windo;
double thick;
{
	int i, j, m;
	matter->element = name;
	matter->windo = windo;		/* windo 0:gas 1:Solid */
	matter->thick = thick;		/* thick は、cm であたえよ */
	for (i = 0; i < sizeof(absorpm2)/sizeof(absorpm2[0]); i++) {
		if ( 0 == strcmp(name, absorpm2[i].element) ) break;
	}
	if ( i == sizeof(absorpm2)/sizeof(absorpm2[0]) ) {
		fprintf(stderr, "set_matter: bad element\n");
		exit(1);
	}
	matter->den = absorpm2[i].density;
	m = 0;
	for (j = 0; j < 9; j++) {
		double de = absorpm2[i].dcde[2*(9-j)-1];
		if ( 0.0 < de ) {
			matter->edge[j] = 12.396/de;
			m = 1;
		} else if ( 0 == m ) {
			matter->edge[j] = 0.0;
		} else {
			matter->edge[j] = 1.0e32;
		}
	}
	for (j = 0; j < 10; j++) {
		matter->c[j] = absorpm2[i].dcde[2*(9-j)];
	}
}

static double
calc_eff(nn, matter, o, n, energy)
int nn;
struct absorption_data matter[/*nn*/];
int o[/*nn*/];
double n[10], energy;
{
	int	i;
	double	sigma, gas, wind;
	gas = wind = 0.0;
	for (i = 0; i < nn; i++ ) {
		sigma = matter[i].thick * matter[i].den * matter[i].c[o[i]]
		      * pow(12.396/energy, n[o[i]]);
		if ( matter[i].windo ) {
			wind += sigma;
		} else {
			gas  += sigma;
		}
	}
	return exp(-wind) * (1-exp(-gas));
}

static double
interp_log(x, x0, y0, x1, y1)
double x, x0, y0, x1, y1;
{
	double ts, s, t;
	ts = log(x1/x0);
	t = log(x/x0) / ts;
	s = log(x1/x) / ts;
	return exp(s*log(y0) + t*log(y1));
}

static double
calc_matter_tau(element, energy, thick /* (g/cm^2) */ )
char *element;
double energy, thick;
{
	int	i;
	double sigma, ts, s_E, t_E;
	
	for (i = 0; i < sizeof(cs_data) / sizeof(cs_data[0]); i++) {
		struct cs_data *p = &cs_data[i];
		if ( strstr(p->mat, element) ) {
			struct Cross_Section *cs = p->cross_section;
			if ( energy < cs[0].ee ) return 0.0;
			for (i = 0; 0.0 < cs[i+1].ee; i++) {
				if ( energy < cs[i+1].ee ) {
					sigma = interp_log(energy,
									   cs[i].ee, cs[i].cs,
									   cs[i+1].ee, cs[i+1].cs);
					return sigma * thick;
				}
			}
			return 0.0;
		}
	}
	return 0.0;
}

static double
calc_trans(nn, matter, energy)
int nn;
struct absorption_data matter[];
double energy;
{
	static double n[10];
	static int first_time = 1;
	int i, j;
	double sigma, gas, wind, trans;;
	if ( first_time ) {
		first_time = 0;
		for (i = 0; i < 10; i++) {
			n[i] = absorpm1.n[9-i];
		}
	}
	if ( energy <= 0 ) return 0;
	gas = wind = 0;
	for (i = 0; i < nn; i++) {
		struct absorption_data *m = &matter[i];
		for (j = 0; j < 9; j++) {
			if ( energy < m->edge[j] ) break;
		}
		sigma = m->thick * m->den * m->c[j] * pow(12.396/energy, n[j]);
		if ( m->windo ) {
			wind += sigma;
		} else {
			gas += sigma;
		}
	}
	trans = exp(-wind);
	unless ( 0.0 == gas ) {
		trans *= 1 - exp(-gas);
	}
	return trans;
}

static double
sigmabe(energy)
double energy;
{
	static double conv = 14.96;
	static double a[4] = {
		9.04511,
		-2.83487,
		-0.210021,
		0.0229526
	};
    static double b[4] = {
		2.0086,
		-0.0461920,
		-0.337018,
		0.0186939
	};
	static double c[4] = {
		-0.690079,
		0.946448,
		-0.171142,
		0.00651413
	};
    double loe, loee, loeee, sig;
	if ( energy <= 0 ) return 0.0;
	loe = log(energy);
	loee = loe * loe;
	loeee = loee * loe;
	sig  = exp(a[0]) * pow(energy,a[1]) * exp(a[2]*loee) * exp(a[3]*loeee);
    sig += exp(b[0]) * pow(energy,b[1]) * exp(b[2]*loee) * exp(b[3]*loeee);
    sig += exp(c[0]) * pow(energy,c[1]) * exp(c[2]*loee) * exp(c[3]*loeee);
    return sig / conv;
}

static double
calc_Be_trans(energy, tBe)
double energy, tBe;
{
#if 0
	static first_time = 1;
	static struct absorption_data matter[1];
	if ( first_time ) {
		first_time = 0;
		set_matter(&matter[0], "Be", 1, tBe);
	}
	matter[0].thick = tBe;
	return calc_trans(1, matter, energy);
#else
	return exp( - 1.848 * tBe * sigmabe(energy) );
#endif
}

PROTOCCALLSFFUN1(DOUBLE,MYLER,myler,DOUBLE)
#define MYLER(A) CCALLSFFUN1(MYLER,myler,DOUBLE,A)
PROTOCCALLSFFUN1(DOUBLE,ALMNM,almnm,DOUBLE)
#define ALMNM(A) CCALLSFFUN1(ALMNM,almnm,DOUBLE,A)

static double
calc_PET_trans(energy, tPET, tAl)
double energy, tPET, tAl;
{
#if 0
	static first_time = 1;
	static struct absorption_data matter[3];
	double trans;
	if ( first_time ) {
		first_time = 0;
		set_matter(&matter[0], "PET-C", 1, tPET);
		set_matter(&matter[1], "PET-H", 1, tPET);
		set_matter(&matter[2], "Al", 1, tAl);
	}
	matter[0].thick = matter[1].thick = tPET;
	matter[2].thick = tAl;
	trans = calc_trans(3, matter, energy);
	trans *= exp( -calc_matter_tau("O", energy, tPET*1.41*0.33018) );
	return trans;
#else
	double eV;
	if ( energy <= 0.0 ) return 0;
	eV = energy * 1000;
	return exp(-tPET*1.41*MYLER(eV) - tAl*2.69*ALMNM(eV));
#endif
}

static double
calc_Gas_trans(energy, tGas, pHe, pXe)
double energy, tGas, pHe, pXe;
{
	static first_time = 1;
#if 0
	static struct absorption_data matter[2];
	double trans;
	if ( first_time ) {
		first_time = 0;
		set_matter(&matter[0], "He", 1, pHe*tGas);
		set_matter(&matter[1], "Xe", 1, pXe*tGas);
	}
	matter[0].thick = pHe*tGas;
	matter[1].thick = pXe*tGas;
	trans = calc_trans(2, matter, energy);
#else
	static struct absorption_data matter[1];
	double trans;
	if ( first_time ) {
		first_time = 0;
		set_matter(&matter[0], "He", 1, pHe*tGas);
	}
	matter[0].thick = pHe*tGas;
	trans = calc_trans(1, matter, energy);
	trans *= exp( - calc_matter_tau("Xe", energy, pXe*tGas*131.3/22.4e3) );
#endif
	return trans;
}

static double
calc_Gas_mean_free_path(energy, pHe, pXe)
double energy, pHe, pXe;
{
	double trans;
	trans = calc_Gas_trans(energy, 1.0, pHe, pXe);
	return -1/log(trans);
}

double
gis_eff_correction(energy)
double energy;
{
	/*static double PowerCorrect = -0.02;*/
	static double CrabIndex = -2.09;
	/*static double additional_Be_thick = 1.3 * 1.0e-4;		/* um -> cm */
	static struct {
		double E0, Sigma, EW, norm;
	} Gauss = {
		1.90, 0.20, 0.012, 0
	};
	double cor, x;
	if ( 0.0 == Gauss.norm ) {
		Gauss.norm = pow(Gauss.E0, CrabIndex) / sqrt(2*M_PI) / Gauss.Sigma;
		Gauss.norm *= Gauss.EW;
	}
	x = ( energy - Gauss.E0 ) / Gauss.Sigma;
	cor = 1 + Gauss.norm * exp(-x*x/2) / pow(energy, CrabIndex);
	/*cor *= calc_Be_trans(energy, additional_Be_thick);*/
	/*cor *= pow(energy, PowerCorrect);*/
	/*cor *= Correction_of_trans;*/
	return cor;
}

double
gis_eff(energy, correct_flag)
double energy;
int correct_flag;
/* correct_flag should be set to zero if the correction is to be made,
   and one otherwise - kaa */
{
	double eff;
	eff = calc_PET_trans(energy, 2*PET_thick, 2*Al_thick);
	eff *= calc_Be_trans(energy, Be_thick);
	eff *= 1 - calc_Gas_trans(energy, Drift_Region, He_pressure, Xe_pressure);
	eff *= ThermalShield_mesh_trans;
	eff *= PlasmaShield_mesh_trans;
	eff *= BeSupport_mesh_trans;
        if ( correct_flag == 0 ) {
            eff *= gis_eff_correction(energy);
	  }
	return eff;
}

#if 0
static int
mm2ch(xmm, ymm, xch, ych)
double xmm, ymm;
double *xch, *ych;
{
	double rmm, rch;
	rmm = sqrt(xmm*xmm+ymm*ymm);
	if ( 0.0 == rmm ) {
		*xch = gMX;
		*ych = gMY;
	} else {
		rch = 110.0 * sin( rmm/30.0 * M_PI/2.0 );
		*xch = xmm * rch / rmm + gMX;
		unless ( -0.5 <= *xch && *xch < gNX-0.5 ) return -1;
		*ych = ymm * rch / rmm + gMY;
		unless ( -0.5 <= *ych && *ych < gNY-0.5 ) return -1;
	}
	return 0;
}
#endif

static int
ch2mm(xch, ych, xmm, ymm)
double xch, ych;
double *xmm, *ymm;
{
	double rmm, rch;
	xch -= gMX;
	ych -= gMY;
	rch = sqrt(xch*xch+ych*ych);
	if ( 0. == rch ) {
		*xmm = *ymm = 0.0;
	} else {
		rmm = rch / 110.0;
		unless ( -1 <= rmm && rmm <= 1 ) return -1;
		rmm = 30.0 * 2 / M_PI * asin(rmm);
		*xmm = xch * rmm / rch;
		*ymm = ych * rmm / rch;
	}
	return 0;
}

#define NoEscape	{ 0.0, 0.0, 0.0, 0.0, 0.0 }

/* Y.Ueda 0.6975 keV x0.99,
   0.9705 keV -> 17.3(=18.2*0.95) to consider edge jump
   data between above two are interpolated with power law*/
static struct Cal {
	double Ex, ph, Eres, phkappa, posres, escape_percent[Nesc];
} Cal[] = {

/* 0.3-1.25 from the experiments of KovarB+zs937 */
/* 1.25-3.69 from the experiments of KovarB+zs941 */
/* 3.69- from the experiments of FM sensor */

	{ 0.30,   0.30,   25.7, 0.130,  2.21, NoEscape },
	{ 0.68,   0.68,   17.1, 0.067,  1.47, NoEscape },
	{ 0.6965, 0.6965, 16.9, 0.063,  1.45, NoEscape },
	{ 0.6975, 0.6975, 20.5, 0.304,  1.68, NoEscape },
	{ 0.77,   0.77,   19.4844, 0.203,  1.60, NoEscape },
	{ 0.85,   0.85,   18.5195, 0.130,  1.49, NoEscape },
	{ 0.93,   0.93,   17.6831, 0.100,  1.32, NoEscape },
	{ 0.9705, 0.9705, 17.3, 0.090,  1.32, NoEscape },
	{ 0.9715, 0.9715, 18.2, 0.135,  1.34, NoEscape },
	{ 1.01,   1.01,   17.5, 0.115,  1.17, NoEscape },
	{ 1.1565, 1.1565, 16.4, 0.095,  1.01, NoEscape },
	{ 1.1575, 1.1575, 16.6, 0.100,  1.02, NoEscape },
	{ 1.25,   1.25,   16.0, 0.085,  0.98, NoEscape },
	{ 1.49,   1.49,   14.8, 0.0521, 0.89, NoEscape },
	{ 1.74,   1.74,   13.5, 0.0351, 0.82, NoEscape },
	{ 2.31,   2.31,   11.5, 0.0173, 0.75, NoEscape },
	{ 2.62,   2.62,   10.8, 0.0130, 0.70, NoEscape },
	{ 3.69,   3.69,   9.20, 0.00650,0.60, NoEscape },
	{ 4.51,   4.51,   8.20, 0.00433,0.52, NoEscape },
	{ 4.785, 4.785,   8.00, 0.004,  0.51, NoEscape },
	{ 4.786, 4.721,   8.80, 0.0083, 0.56, { 1.0,  0.0,  0.0,  0.0,  0.0  } },
	{ 4.950, 4.884,   8.60, 0.008,  0.55, { 1.0,  0.0,  0.0,  0.0,  0.0  } },
	{ 5.105, 5.037,   8.45, 0.0073, 0.535,{ 1.0,  0.0,  0.0,  0.0,  0.0  } },
	{ 5.106, 5.031,   8.45, 0.010,  0.535,{ 1.0,  0.5,  0.2,  0.2,  0.0  } },
	{ 5.410, 5.335,   8.00, 0.009,  0.525,{ 1.0,  0.5,  0.2,  0.2,  0.0  } },
	{ 5.453, 5.379,   7.97, 0.009,  0.523,{ 1.0,  0.5,  0.18, 0.2,  0.0  } },
	{ 5.454, 5.377,   7.97, 0.009,  0.523,{ 1.0,  0.5,  0.18, 0.2,  0.05 } },
	{ 5.90,   5.84,   7.70, 0.008,  0.50, { 1.0,  0.5,  0.16, 0.2,  0.05 } },
	{ 6.40,   6.35,   7.30, 0.0075, 0.49, { 0.93, 0.4,  0.13, 0.25, 0.05 } },
	{ 6.92,   6.88,   7.05, 0.007,  0.47, { 0.88, 0.28, 0.1,  0.33, 0.05 } },
	{ 7.47,   7.44,   6.80, 0.0065, 0.46, { 0.81, 0.23, 0.085,0.4,  0.05 } },
	{ 8.04,   8.02,   6.50, 0.006,  0.45, { 0.76, 0.14, 0.07, 0.45, 0.05 } },
	{ 8.63,   8.62,   6.20, 0.006,  0.44, { 0.7,  0.15, 0.06, 0.43, 0.05 } },
	{ 9.87,   9.87,   5.75, 0.006,  0.42, { 0.7,  0.15, 0.06, 0.43, 0.05 } },
	{ 11.91, 11.91,   5.25, 0.006,  0.42, { 0.7,  0.15, 0.06, 0.43, 0.05 } },
	{ 14.92, 14.92,   4.70, 0.006,  0.46, { 0.7,  0.15, 0.06, 0.43, 0.05 } },
	{ 17.46, 17.46,   4.25, 0.006,  0.51, { 0.7,  0.15, 0.06, 0.43, 0.05 } }
};

#define Ncal	(sizeof(Cal)/sizeof(Cal[0]))

static int i_Cal;
static double t_E, s_E;

static void
set_cal_data(Ex)
double Ex;
{
	double ts;
	for (i_Cal = Ncal-1; 0 < i_Cal; i_Cal--) {
		if ( Cal[i_Cal].Ex < Ex ) break;
	}
	ts = log(Cal[i_Cal+1].Ex/Cal[i_Cal].Ex);
	t_E = log(Ex/Cal[i_Cal].Ex) / ts;
	s_E = log(Cal[i_Cal+1].Ex/Ex) / ts;
}

static double
get_ph(Ex)
double Ex;
{
	double ph;
	ph = exp( s_E*log(Cal[i_Cal].ph) + t_E * log(Cal[i_Cal+1].ph) );
	ph *= 1.005; /* Ueda adjusted for O-bunpu with tau=0.00 */
	return ph / 5.83;		/* 5.83 = 5.9 - 0.07(70eV jump) */
}

static double
get_Eres(Ex)
double Ex;
{
	double Eres;
	Eres = exp( s_E*log(Cal[i_Cal].Eres) + t_E*log(Cal[i_Cal+1].Eres) );
	return Eres / Mn_Eres0;
}

static double
get_phkappa(Ex)
double Ex;
{
	double kappa;
#if 0
	kappa = exp( s_E*log(Cal[i_Cal].phkappa) + t_E*log(Cal[i_Cal+1].phkappa) );
	return kappa / Al_kappa0;
#else
	kappa = calc_matter_tau("Xe", Ex, 1.0) / 50000 * 0.6 * 1.15;
	/* Y.Ueda *1.15 */
	return kappa;
#endif
}

static double
get_phtau(Ex)
double Ex;
{
	return 0.00; /* Y.Ueda changed */
}

static double
get_posres(Ex)
double Ex;
{
	double posres;
	posres = exp( s_E*log(Cal[i_Cal].posres) + t_E*log(Cal[i_Cal+1].posres) );
	return posres / Mn_posres0;
}

static void
get_escape(Ex, esc_rate)
double Ex;
double *esc_rate;
{
	int i;
	for (i = 0; i < Nesc; i++) {
		if ( 0.0 == Cal[i_Cal].escape_percent[i] ) {
			esc_rate[i] = 0.0;
		} else {
			esc_rate[i] = 0.01 *
				exp(   s_E * log(Cal[i_Cal].escape_percent[i])
					 + t_E * log(Cal[i_Cal+1].escape_percent[i]) );
		}
	}
}

static void
xy_xyres(xmm, ymm, dx, dy)
double xmm, ymm;
double *dx, *dy;
{
	double rr, dum;
	rr = xmm*xmm + ymm*ymm;
	if( rr < 15.0*15.0 ) {
		(*dx) = (*dy) = 1.0;
	} else {
		dum = ( sqrt(rr) - 15 ) / 7.0;
		(*dx) = (*dy) = 1.0 + dum*dum*0.5;
	}
}

static double
xy_Eres(xmm, ymm)
double xmm, ymm;
{
	double rr, dum;
	rr = xmm*xmm + ymm*ymm;
	if ( rr < 20.0*20.0 ) return 1.0;
	dum = ( sqrt(rr) - 20.0 ) / 2.0;
	return 1.0 + dum*dum*0.1;
}

static double
xy_gain(xmm, ymm)
double xmm, ymm;
{
/*	return ( 700 - 0.0005*squ(xmm*xmm+ymm*ymm) ) / 700; */
	return 1.0;
}

#define NPARA	30
#define FWHM	2.35482004503094938202313865291939927549477137877164107704
#define SQRT_2PI	2.50662827463100050241
#define XeL3E	4.781
#define XeL2E	5.102
#define XeL1E	5.451
#define XeLa	4.110
#define XeLb1	4.416
#define XeLb2	4.719
#define XeLb3	4.509
#define XeLc	5.039
/*
#define XeLaRate	1.0e-2
#define XeLb1Rate	5.0e-3
#define XeLb2Rate	3.0e-4
#define XeLb3Rate	2.0e-3
#define XeLcRate	5.0e-4
*/

typedef struct {
	struct {
		double ch, wd, kappa, tau;
	} ph;
	struct {
		double rate, ch, wd;
	} es[Nesc];
} Ex_Response;

typedef struct {
	struct {
		double ch, wd;
		struct {
			double wd;
		} es[Nesc];
	} x, y;
} xy_Response;

static void
local_correct(xmm, ymm, Ex_Re, xy_Re)
double xmm, ymm;
Ex_Response *Ex_Re;
xy_Response *xy_Re;
{
	int i;
	struct {
		double gain, Eres, xres, yres;
	} cor;
	
	cor.gain = xy_gain(xmm, ymm);
	cor.Eres = xy_Eres(xmm, ymm);
	xy_xyres(xmm, ymm, &cor.xres, &cor.yres);
	Ex_Re->ph.ch *= cor.gain;
	Ex_Re->ph.wd *= cor.gain * cor.Eres;
	xy_Re->x.wd *= cor.xres;
	xy_Re->y.wd *= cor.yres;
	for (i = 0; i < Nesc; i++) {
		if ( 0 < Ex_Re->es[i].rate ) {
			Ex_Re->es[i].ch *= cor.gain;
			Ex_Re->es[i].wd *= cor.gain * cor.Eres;
			xy_Re->x.es[i].wd *= cor.xres;
			xy_Re->y.es[i].wd *= cor.yres;
		}
	}
}

#define min(x,y)  (((x)<(y))?(x):(y))
#define ESCAPE

double
calc_obunpu(N, No, s, k)
double N, No, s, k;
{
	static double KIZAMI1 = 50;		/* 0 から 1 を何分割するか */
	static double KIZAMI2 = 5;		/* 1σを何分割するか */
	double sum, it, dit, sit, eit, mu, dmu, smu, emu, spow, epow;
	if ( k < 0 ) k = -k;
	sum = 0.0;
	dit = 1.0 / KIZAMI1;
	eit = 0.0;
	epow = 1.0;
	if ( 5*s < No - N ) {
		dmu = 1.0 / KIZAMI2;
		emu = 100;
		until ( 1.0 == eit ) {
			sit = eit;
			eit += dit;
			smu = emu;
			emu = ( N/eit - No ) / s;
			if ( emu < smu - dmu ) {
				if ( emu*smu < 0.0 || smu*smu < 9.0 || emu*emu < 9.0 ) {
					emu = smu - dmu;
					eit = N / ( s*emu + No );
				}
			}
			if ( 1 < eit ) eit = 1.0;
			spow = epow;
			epow = pow(1-eit, k);
			it = ( sit + eit ) / 2.0;
			if ( 0 == it ) continue;
			mu = ( N/it - No ) / s;
			sum -= exp(-mu*mu/2) / it * ( epow - spow );
		}
	} else {
		until ( 1.0 == eit ) {
			sit = eit;
			eit += dit;
			if ( 1 < eit ) eit = 1.0;
			spow = epow;
			epow = pow(1-eit, k);
			it = ( sit + eit ) / 2.0;
			if ( 0 == it ) continue;
			mu = ( N/it - No ) / s;
			sum -= exp(-mu*mu/2) / it * ( epow - spow );
		}
	}
	return sum/SQRT_2PI/s;	/* sqrt(2*M_PI) */
}

static double
calc_o2bunpu(N, No, s, k, r0)
double N, No, s, k, r0;
{
	static double KIZAMI1 = 50;		/* 0 から 1 を何分割するか */
	static double KIZAMI2 = 5;		/* 1σを何分割するか */
	double sum, it, dit, sit, eit, mu, dmu, smu, emu, spow, epow, ratio;
	if ( 0 == r0 ) return calc_obunpu(N, No, s, k);
	if ( k < 0 ) k = -k;
	sum = 0.0;
	dit = 1.0 / KIZAMI1;
	eit = 0.0;
	epow = 1.0;
	if ( 5*s < No - N ) {
		dmu = 1.0 / KIZAMI2;
		emu = 100;
		until ( 1.0 == eit ) {
			sit = eit;
			eit += dit;
			smu = emu;
			if ( 1 < eit ) {
				eit = 1.0;
			} else {
				ratio = 1 - erfcc(-log(1-eit)/r0)/2;
				emu = ( ratio*N/eit - No ) / s;
				if ( emu < smu - dmu ) {
					if ( emu*smu < 0.0 || smu*smu < 9.0 || emu*emu < 9.0 ) {
						emu = smu - dmu;
						eit = ratio*N / ( s*emu + No );
						if ( 1 < eit ) eit = 1.0;
					}
				}
			}
			spow = epow;
			epow = pow(1-eit, k);
			it = ( sit + eit ) / 2;
			if ( 0 == it ) continue;
			ratio = 1 - 0.5*erfcc(-log(1-it)/r0);
			mu = ( ratio*N/it - No ) / s;
			sum -= exp(-mu*mu/2) / it * ( epow - spow );
		}
	} else {
		until ( 1.0 == eit ) {
			sit = eit;
			eit += dit;
			if ( 1 < eit ) eit = 1.0;
			spow = epow;
			epow = pow(1-eit, k);
			it = ( sit + eit ) / 2;
			if ( 0 == it ) continue;
			ratio = 1 - erfcc(-log(1-it)/r0)/2;
			mu = ( ratio*N/it - No ) / s;
			sum -= exp(-mu*mu/2) / it * ( epow - spow );
		}
	}
	return sum/SQRT_2PI/s;	/* sqrt(2*M_PI) */
}

static double
gi_spec0(Ex_Re, ne0, E0)
Ex_Response *Ex_Re;
int ne0;
double *E0;
{
	static double E[gNE2];
	struct { double nm, ch, wd; } esc[Nesc];
	double x, phch, phwd, kappa, tau, totc, net;
	int i, j;
	
	for (i = 0; i < gNE2; i++) E[i] = 0;
	phch = Ex_Re->ph.ch;
	phwd = Ex_Re->ph.wd / FWHM;
	kappa = Ex_Re->ph.kappa;
	tau = Ex_Re->ph.tau;
	for (i = 0; i < Nesc; i++) {
		esc[i].ch = Ex_Re->es[i].ch;
		esc[i].wd = Ex_Re->es[i].wd / FWHM;
		if ( 0 == Ex_Re->es[i].rate ) {
			esc[i].nm = 0;
		} else {
			esc[i].nm = Ex_Re->es[i].rate / esc[i].wd / SQRT_2PI;
		}
	}
	for (i = PH_Ld; i < gNE2; i++) {
		E[i] = calc_o2bunpu(i, phch, phwd, kappa, tau);
		if ( E[i] < 1e-20 ) break;
	}
/* gradient 1/2 up to 1/2 peak v39l Y.Ueda */
/* only for > 2.5 keV */
	if(phch > 500/5.9*2.5)
	{
		int halfch;
		halfch = phch / 2;
		for (i = PH_Ld; i < halfch; i++) {
			E[i] = ( E[i] + E[halfch] ) / 2;
		}
	}
	
#ifdef ESCAPE
	for (i = 0; i < phch; i++) {
		for (j = 0; j < Nesc; j++) {
			if ( 0.0 < esc[j].nm ) {
				x = ( i - esc[j].ch ) / esc[j].wd;
				x = x*x;
				if ( x < 100 ) {	/* far */
					E[i] += esc[j].nm * exp(-x/2);
				}
			}
		}
	}
#endif
	totc = net = 0.0;
	for (i = 0; i < gNE; i++) net += E[i];
	if ( 0 == net ) return 0;
	for (totc = net; i < gNE2; i++) totc += E[i];
	if ( 0 == totc ) return 0;
	net /= totc;
	for (i = 0; i < ne0; i++) {
		E0[i] = E[i] / totc;
	}
	return net;
}

static void
get_Ex_Respo(Ex, p)
double Ex;
Ex_Response *p;
{
	int i;
	double phch, phwd, esc_rate[Nesc];
	
	p->ph.ch = phch = get_ph(Ex) * PH_Mn;
	p->ph.wd = phwd = Mn_Eres/100.0 * p->ph.ch * get_Eres(Ex);
	p->ph.kappa = get_phkappa(Ex);
	p->ph.tau = get_phtau(Ex);
	get_escape(Ex, esc_rate);
	for (i = 0; i < Nesc; i++) p->es[i].rate = 0.0;
	if ( XeL3E < Ex ) {						/* 1st escape */
		p->es[0].ch = ( Ex - XeLa ) / Ex * phch;
		p->es[0].wd = phwd / phch * sqrt( Ex/(Ex-XeLa) ) * p->es[0].ch;
		p->es[0].rate = esc_rate[0];
		if ( XeL2E < Ex ) {					/* 2nd - 4th  escape */
			p->es[1].ch = ( Ex - XeLb1 ) / Ex * phch;
			p->es[1].wd = phwd / phch * sqrt( Ex/(Ex-XeLb1) ) * p->es[1].ch;
			p->es[1].rate = esc_rate[1];
			p->es[2].ch = ( Ex - XeLb2 ) / Ex * phch;
			p->es[2].wd = phwd / phch * sqrt( Ex/(Ex-XeLb2) ) * p->es[2].ch;
			p->es[2].rate = esc_rate[2];
			p->es[3].ch = ( Ex - XeLb3 ) / Ex * phch;
			p->es[3].wd = phwd / phch * sqrt( Ex/(Ex-XeLb3) ) * p->es[3].ch;
			p->es[3].rate = esc_rate[3];
			if ( XeL1E < Ex ) {						/* 5th escape */
				p->es[4].ch = ( Ex - XeLc ) / Ex * phch;
				p->es[4].wd = phwd / phch * sqrt( Ex/(Ex-XeLc) ) * p->es[4].ch;
				p->es[4].rate = esc_rate[4];
			}
		}
	}
}

static void
get_xy_Respo(Ex, p)
double Ex;
xy_Response *p;
{
	p->x.wd = p->y.wd = Mn_posres * get_posres(Ex);
	if ( XeL3E < Ex ) {
		p->x.es[0].wd = p->y.es[0].wd = p->x.wd * sqrt( Ex/(Ex-XeLa) );
		if ( XeL2E < Ex ) {
			p->x.es[1].wd = p->y.es[1].wd = p->x.wd * sqrt( Ex/(Ex-XeLb1) );
			p->x.es[2].wd = p->y.es[2].wd = p->x.wd * sqrt( Ex/(Ex-XeLb2) );
			p->x.es[3].wd = p->y.es[3].wd = p->x.wd * sqrt( Ex/(Ex-XeLb3) );
			if ( XeL1E < Ex ) {
				p->x.es[4].wd = p->y.es[4].wd = p->x.wd * sqrt( Ex/(Ex-XeLc) );
			}
		}
	}
}

static double oldEkev = -1.0;
static double oldxmm = -9999, oldymm = -9999;
static struct {
	Ex_Response Ex;
	xy_Response xy;
	double parallax;
} Re;

static void
check_if_new_energy(xmm, ymm, Ekev)
double xmm, ymm, Ekev;
{
	double mfre;
	if ( oldEkev != Ekev ) {
		oldxmm = xmm;
		oldymm = ymm;
		oldEkev = Ekev;
		set_cal_data(Ekev);
		get_Ex_Respo(Ekev, &Re.Ex);
		get_xy_Respo(Ekev, &Re.xy);
		local_correct(xmm, ymm, &Re.Ex, &Re.xy);
		mfre = calc_Gas_mean_free_path(Ekev, He_pressure, Xe_pressure) * 10;
		Re.parallax = FWHM * sin(2.0*M_PI/180) * mfre;
		Re.parallax *= Re.parallax;
	} else if ( oldxmm != xmm || oldymm != ymm ) {
		oldxmm = xmm;
		oldymm = ymm;
		local_correct(xmm, ymm, &Re.Ex, &Re.xy);
	}
}

void
gis_total_spectrum(xmm, ymm, Ekev, E0)
double xmm, ymm, Ekev, E0[gNE];
{
	if ( Ekev <= 0.0 || 15.0 < Ekev ) {
		int i;
		for (i = 0; i < gNE; i++) E0[i] = 0;
		return;
	}
	check_if_new_energy(xmm, ymm, Ekev);
	gi_spec0(&Re.Ex, gNE, E0);
}

double
gis_psf(xmm, ymm, Ekev, xoff, yoff)
double xmm, ymm, Ekev, xoff, yoff;
{
	int i;
	double xx, yy, sgx, sgy, norm, dum, flux;
	if ( Ekev <= 0.0 || 15.0 < Ekev ) {
		return 0;
	}
	check_if_new_energy(xmm, ymm, Ekev);
	xx = xoff * FWHM;
	xx *= xx;
	yy = yoff * FWHM;
	yy *= yy;
	sgx = squ(Re.xy.x.wd) + Re.parallax;
	sgy = squ(Re.xy.y.wd) + Re.parallax;
	norm = 1;
	flux = 0;
	dum = xx/sgx + yy/sgy;
	if ( dum < 20 ) {
		flux = exp(-dum/2.0) / sqrt(sgx*sgy);
	}
#ifdef ESCAPE
	for (i = 0; i < Nesc; i++) {
		if ( 0.0 < Re.Ex.es[i].rate ) {
			sgx = squ(Re.xy.x.es[i].wd) + Re.parallax;
			sgy = squ(Re.xy.y.es[i].wd) + Re.parallax;
			norm += Re.Ex.es[i].rate;
			dum = xx/sgx + yy/sgy;
			if ( dum < 20 ) {
				flux += Re.Ex.es[i].rate * exp(-dum/2.0) / sqrt(sgx*sgy);
			}
		}
	}
#endif
	return flux / 2.0 / M_PI * FWHM * FWHM / norm;
}

int
gis_psf_sigma(xmm, ymm, Ekev, ratio, sgx, sgy)
double xmm, ymm, Ekev, *ratio, *sgx, *sgy;
{
	int i, n;
	if ( Ekev <= 0.0 || 15.0 < Ekev ) {
		return 0;
	}
	check_if_new_energy(xmm, ymm, Ekev);
	n = 1;
	*ratio++ = 1.0;
	*sgx++ = sqrt(squ(Re.xy.x.wd) + Re.parallax) / FWHM;
	*sgy++ = sqrt(squ(Re.xy.y.wd) + Re.parallax) / FWHM;
#ifdef ESCAPE
	for (i = 0; i < Nesc; i++) {
		if ( 0.0 < Re.Ex.es[i].rate ) {
			*sgx++ = sqrt(squ(Re.xy.x.es[i].wd) + Re.parallax) / FWHM;
			*sgy++ = sqrt(squ(Re.xy.y.es[i].wd) + Re.parallax) / FWHM;
			*ratio++ = Re.Ex.es[i].rate;
			n++;
		}
	}
#endif
	return n;
}

double
gis_efficiency(xmm, ymm, Ekev)
double xmm, ymm, Ekev;
{
	double eff;
	eff = gis_eff(Ekev);
	return eff;
}

double
gis_elements_trans(energy, plasma, be, gas)
double energy, *plasma, *be, *gas;
{
	*plasma = calc_PET_trans(energy, 2*PET_thick, 2*Al_thick);
	*be = calc_Be_trans(energy, Be_thick);
	*gas = calc_Gas_trans(energy, 0.1, He_pressure, Xe_pressure);
	return gis_eff(energy);
}

double
gis_kappa(energy, kappa, Xe_sigma, Eres)
double energy, *kappa, *Xe_sigma, *Eres;
{
	double ph;
	set_cal_data(energy);
	*kappa = get_phkappa(energy);
	*Xe_sigma = calc_matter_tau("Xe", energy, 1.0);
	ph = get_ph(energy);
	*Eres = Mn_Eres/100.0 * get_Eres(energy);
	return get_ph(energy);
}

