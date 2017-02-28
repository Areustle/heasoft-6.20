/* * gisres.c * *	GIS response function		Y.Fukazawa, Y.Ishisaki
*								fukazawa@miranda.phys.s.u-tokyo.ac.jp
*								ishisaki@miranda.phys.s.u-tokyo.ac.jp
* * * Version 3.2					94/5/15
*					position check in check_if_new_energy()
*					gis_psf_sigma() for SimASCA 
*                                       PH-E relation little correction arount 6-8keV 
* 
* Version 3.1					94/2/22 
* remove absorption filter of Be 1.3um 
* 
* Version 3.0					94/2/15
*					gaussian filter E.W. 12eV, sigma=0.2keV at 1.9keV
*					absorption filter of Be 1.3um
*					Xe-L edge energy jump 50,10,10eV -> 65,7,3eV 
*					try 6 value for O-tail kappa
* E-cut-off will be changed to 1-0.5*exp()
*					Xe L edge energy in efficiency table fix 
* 
* Version 2.6					94/1/19
*					O-tail parameters are returned to exp. value 
*					a little change of value is performed 
* E-cut-off will be changed to 1-0.8*exp() 
* 
* Version 2.5					93/12/16
*					Static definition of local functions 
*					Ueda supply Be transmission function 
* 
* Version 2.4					93/12/01
*					Experimental escape ratio for XeLb[1-3] and XeLc 
* 
* Version 2.3					93/10/31
*					O-tail E-cut-off calculation bug fix 
* 
* Version 2.2					93/09/05
*					O-tail parameter was corrected to 6kV value 
*					Moreover it was corrected so that Crab was well fitted 
*
* Version 2.1					93/08/29
*					XRT team supply PET transmission 
*					XRT parallax * 
* Version 2.0					93/08/23
*					new low energy experimental values 
*					Xe M-edge absorption jump 
*					artificial EDGE in 1.14keV
*					O-tail calculation till 0.3keV
* 
* Version 1.5					93/05/13
*					correct Xe thickness
*					new Myler thickness by Tawara
*					new functions	gis_total_spectrum()
*							gis_point_spread_function()
*							gis_efficiency()
* 
* Version 1.0					93/03/13
*					use experimental values 
* 
* functions 
* 
* void set_cal_data(double Ex) : set calibration data at Ex(energy) 
* (This should be called first) 
* double gis_eff(double energy) : return gis_efficiency with Be,Plasma_shield 
* Thermal_Shiled,Xe 
* (This should be called second) 
* double get_ph(void) : get ph channel 
* double get_Eres(void) : get energy resolution 
* double get_phkappa(void) : get O-bunpu parameter 
* double get_escape(double rate[]) : get escape ratio 
* double get_posres(void) : get position resolution * 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <XSFunctions/trns.h>
#include "cfortran.h"

char gisres_version[] = "gisres version 3.2";

#define GISINFO	void

#ifdef __cplusplus
extern "C" {
#endif

double gis_psf(double xmm, double ymm, double Ekev, double xoff, double yoff);
double gis_eff(double energy, int correct_flag);

#ifdef __cplusplus
}
#endif


/*#define EXP*/ /* if defined, O-tail exponential cut off will be considered */
#define EXP2 /* if defined, O-tail exponential cut off2 will be considered */
/*#define EDGE*/  /* if defined, 1.14kev(tau=0.2) edge will be included */
/*#define LDHIT*/ /* if you want to get LDHIT c/s */

#define gNE		1024	/* number of PH channels */
#define gNX		256		/* number of XP channels */
#define gNY		256		/* number of YP channels */
#define gMX		120		/* XP center channel */
#define gMY		120		/* YP center channel */
#define gNE2	2048	/* number of PH channels */
#define MnKa	5.8942	/* Energy of Mn K-alpha */

#define PI              3.1415926535897932385

double	PH_Mn = 500.0;
#define PH_Offset	0.0
#define PH_Ld		20.0

/* for HVH = 6kV */
#define Mn_Res		0.08	/* PH resolution [%] at 5.9keV */
#define Mn_posres0	0.50	/* position [mm] resulution at 5.9keV for HV 8kV*/
#define Mn_posres	0.60	/* position [mm] resulution at 5.9keV for HV 6kV*/
/*#define Al_kappa0	0.042 */ /* O_bunpu kappra at 1.49keV */
#define Al_kappa0	0.050  /* O_bunpu kappra at 1.49keV for HV 8kV */
#define Al_kappa	(Al_kappa0*1.1547)  /* O_bunpu kappra at 1.49keV for 6kV */
#define Kappa_correct 1.0   /* experimental value will be multiplied by this */
#define AEXP        0.5     /* O-tail cut off amplitude 1-AEXP*exp() */
/*#define AEXP        1.0   */ /* O-tail cut off amplitude 1-AEXP*exp() */
/*#define AEXP        0.0   */ /* O-tail cut off amplitude 1-AEXP*exp() */

double Be_thick = (105*10e-6);		/* (cm) Be 10.5 um */
#define PET_thick	(100*540e-9)	/* (cm) PET(Mylar) C10H8O4 */
#define Al_thick	(100*37e-9)		/* (cm) Al of alminized PET */
#define ThermalShield_mesh_trans	0.942
#define PlasmaShield_mesh_trans		0.90
#define BeSupport_mesh_trans		0.87
#define Xe_pressure	1.152	/* (atm) at 273.15 K */
#define He_pressure	0.048	/* (atm) at 273.15 K */
#define Drift_Region	1.0	/* cm */
#define Scinti_Region	1.5	/* cm */

#define unless(a)	if(!(a))
#define until(a)	while(!(a))
#define squ(x)	((x)*(x))

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
		{  0.90, 7794.4 },
		{  0.9705, 6877.4 },
		{  0.9715, 9169.8 },
		{  1.0, 7794.3 },
		{  1.1565, 6877.4 },
	   	{  1.1575, 7335.9 },
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

#define NPARA	30
#define FWHM	2.35482004503094938202313865291939927549477137877164107704
#define Nesc	5
#define XeL3E	4.781
#define XeL2E	5.102
#define XeL1E	5.451
#define XeLa	4.110
#define XeLb1	4.416
#define XeLb2	4.719
#define XeLb3	4.509
#define XeLc	5.039
#define XeLaRate	1.0e-2
#define XeLb1Rate	5.0e-3
#define XeLb2Rate	3.0e-4
#define XeLb3Rate	2.0e-3
#define XeLcRate	5.0e-4

/*
 * R[0]: expected count
 * R[1]: peak channel
 * R[2]: peak width
 * R[3]: x width
 * R[4]: y width
 * R[1+i*5]: i_th escape peak channel(La)
 * R[2+i*5]: i_th escape peak width
 * R[3+i*5]: i_th escape peak rate
 * R[4+i*5]: i_th escape x width
 * R[5+i*5]: i_th escape y width
 */

struct Ex_Response {
	double 	ph_ch, ph_wd, ph_kappa;
	double	es_ch[Nesc], es_wd[Nesc], es_rate[Nesc];
};

struct xy_Response {
	double 	x_ch, x_wd, y_ch, y_wd, es_x_wd[Nesc], es_y_wd[Nesc];
};



static void
set_matter(struct absorption_data *matter, char *name, int windo, double thick)
{
	int j, m;
        size_t i(0);
	matter->element = name;
	matter->windo = windo;		/* windo 0:gas 1:Solid */
	matter->thick = thick;		/* thick は、cm であたえよ */
	for (; i < sizeof(absorpm2)/sizeof(absorpm2[0]); i++) {
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
calc_eff(int nn, struct absorption_data *matter, int *o, double *n, double energy)
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
calc_matter_tau(char *element, double energy, double thick)
{
	size_t	i,id;
	double	sigma, s_E, t_E;

	id=0;
	for(id=0;id<sizeof(cs_data)/sizeof(cs_data[0]);id++) {
		if(strstr(cs_data[id].mat, element)) break;
	}
	if(id==sizeof(cs_data)/sizeof(cs_data[0])) return 0.0;
	for(i=0;i<sizeof(cs_data[id].cross_section)/sizeof(cs_data[id].cross_section[0]);i++) {
		if( cs_data[id].cross_section[i+1].ee==0.0 ) {
		  return 0.0;
		}
		if( energy < cs_data[id].cross_section[i].ee || energy >= cs_data[id].cross_section[i+1].ee ) continue;
		t_E = log(energy/cs_data[id].cross_section[i].ee) / log(cs_data[id].cross_section[i+1].ee/cs_data[id].cross_section[i].ee);
		s_E = log(cs_data[id].cross_section[i+1].ee/energy) / log(cs_data[id].cross_section[i+1].ee/cs_data[id].cross_section[i].ee);
		sigma = exp( s_E * log(cs_data[id].cross_section[i].cs) + t_E * log(cs_data[id].cross_section[i+1].cs) );
		return sigma * thick;
	}
	return 0.0;
}

static double
calc_trans(int nn, struct absorption_data *matter, double energy)
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
sigmabe(double energy)
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
calc_Be_trans(double energy, double tBe)
{
#if 0
	static int first_time = 1;
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


static double
calc_PET_trans(double energy, double tPET, double tAl)
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
calc_Gas_trans(double energy, double tGas, double pHe, double pXe)
{
	static int first_time = 1;
#if 0
	static struct absorption_data matter[2];
	double trans;
	if ( first_time ) {
		first_time = 0;
		set_matter(&matter[0], (char *)"He", 1, pHe*tGas);
		set_matter(&matter[1], (char *)"Xe", 1, pXe*tGas);
	}
	matter[0].thick = pHe*tGas;
	matter[1].thick = pXe*tGas;
	trans = calc_trans(2, matter, energy);
#else
	static struct absorption_data matter[1];
	double trans;
	if ( first_time ) {
		first_time = 0;
		set_matter(&matter[0], (char *)"He", 1, pHe*tGas);
	}
	matter[0].thick = pHe*tGas;
	trans = calc_trans(1, matter, energy);
	trans *= exp( - calc_matter_tau((char *)"Xe", energy, pXe*tGas*131.3/22.4e3) );
#endif
	return trans;
}

static double
calc_Gas_mean_free_path(double energy, double pHe, double pXe)
{
	double trans, depth;
        depth = 1.0;
	trans = calc_Gas_trans(energy, depth, pHe, pXe);
        if ( trans == 0.0 ) {
	  return 0.0;
        }
	return -1/log(trans);
}

double
gis_eff_correction(double energy)
{
	/*static double PowerCorrect = -0.02;*/
	static double CrabIndex = -2.09;
	//static double additional_Be_thick = 1.3 * 1.0e-4;		/* um -> cm */
	static struct {
		double E0, Sigma, EW, norm;
	} Gauss = {
		1.90, 0.20, 0.012, 0
	};
	double cor, x;
	if ( 0.0 == Gauss.norm ) {
		Gauss.norm = Gauss.EW * pow(Gauss.E0, CrabIndex) / sqrt(2*PI) / Gauss.Sigma;
	}
	x = ( energy - Gauss.E0 ) / Gauss.Sigma;
	cor = 1 + Gauss.norm * exp(-x*x/2) / pow(energy, CrabIndex);
	/*cor *= calc_Be_trans(energy, additional_Be_thick);*/
	/*cor *= pow(energy, PowerCorrect);*/
	return cor;
}

double
gis_eff(double energy, int correct_flag)


/* correct_flag should be set to zero if the correction is to be made,
   and one otherwise */
{
	double eff;
	eff = calc_PET_trans(energy, 2*PET_thick, 2*Al_thick);
	eff *= calc_Be_trans(energy, Be_thick);
#ifndef LDHIT
	eff *= 1 - calc_Gas_trans(energy, Drift_Region, He_pressure, Xe_pressure);
#else
	trans = 1 - calc_Gas_trans(energy, Drift_Region, He_pressure, Xe_pressure);
	eff *= trans + ( 1.0 - trans ) * ( 1.0 - calc_Gas_trans(energy, Scinti_Region, He_pressure, Xe_pressure) ) * 0.94;
#endif
	eff *= ThermalShield_mesh_trans * PlasmaShield_mesh_trans * BeSupport_mesh_trans;
#ifdef EDGE
	if(energy > 1.14) {
		eff *= exp( -0.2 * pow(1.14/energy,3.0) );
	}
#endif
        if ( correct_flag == 0 ) {
	  eff *= gis_eff_correction(energy);
	}
	return eff;
}

#if 0
static int
mm2ch(double xmm,double  ymm,double*  xch,double*  ych)
{
	double rmm, rch;
	rmm = sqrt(xmm*xmm+ymm*ymm);
	if ( 0.0 == rmm ) {
		*xch = gMX;
		*ych = gMY;
	} else {
		rch = 110.0 * sin( rmm/30.0 * PI/2.0 );
		*xch = xmm * rch / rmm + gMX;
		unless ( -0.5 <= *xch && *xch < gNX-0.5 ) return -1;
		*ych = ymm * rch / rmm + gMY;
		unless ( -0.5 <= *ych && *ych < gNY-0.5 ) return -1;
	}
	return 0;
}
#endif

static int
ch2mm(double xch, double ych, double *xmm, double *ymm)
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
		rmm = 30.0 * 2 / PI * asin(rmm);
		*xmm = xch * rmm / rch;
		*ymm = ych * rmm / rch;
	}
	return 0;
}

#define Ncal 36

static struct Cal {
	double Ex, ph, Eres, phkappa, posres, Esc_rate[5];
} Cal[] = {
/* ver1.0 value */
/*	{ 0.70, 0.70, 19.5, 0.26, 1.31 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.93, 0.93, 16.5, 0.12, 1.14 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.97, 0.97, 17.2, 0.11, 1.11 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.971, 0.971, 18.1, 0.125, 1.11 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
*/
/* 0.3-1.25 from the experiments of KovarB+zs937 */
/* 1.25-3.69 from the experiments of KovarB+zs941 */
/* 3.69- from the experiments of FM sensor */

/* trial value */
/*	{ 0.30, 0.30, 25.7, 0.900, 2.21 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.68, 0.68, 17.1, 0.760, 1.47 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6965, 0.6965, 16.9, 0.750, 1.45 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6975, 0.6975, 20.7, 0.904, 1.68 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.77, 0.77, 19.7, 0.650, 1.60 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.85, 0.85, 19.3, 0.185, 1.49 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.93, 0.93, 18.5, 0.102, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9705, 0.9705, 18.1, 0.102, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9715, 0.9715, 18.2, 0.121, 1.34 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.01, 1.01, 17.5, 0.109, 1.27 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1565, 1.1565, 16.4, 0.076, 1.01 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1575, 1.1575, 16.6, 0.100, 1.02 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.25, 1.25, 16.0, 0.070, 0.98 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.49, 1.49, 14.8, Al_kappa0, 0.89 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.74, 1.74, 13.5, 0.029, 0.82 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
*/
/* experiment value */
/*	{ 0.30, 0.30, 25.7, 0.130, 2.21 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.68, 0.68, 17.1, 0.067, 1.47 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6965, 0.6965, 16.9, 0.063, 1.45 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6975, 0.6975, 20.7, 0.304, 1.68 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.77, 0.77, 19.7, 0.233, 1.60 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.85, 0.85, 19.3, 0.150, 1.49 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.93, 0.93, 18.5, 0.115, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9705, 0.9705, 18.1, 0.102, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9715, 0.9715, 18.2, 0.121, 1.34 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.01, 1.01, 17.5, 0.109, 1.27 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1565, 1.1565, 16.4, 0.076, 1.01 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1575, 1.1575, 16.6, 0.078, 1.02 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.25, 1.25, 16.0, 0.070, 0.98 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.49, 1.49, 14.8, 0.042, 0.89 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.74, 1.74, 13.5, 0.029, 0.82 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
*/
/* trial value 2 */
/*	{ 0.30, 0.30, 25.7, 0.130, 2.21 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.68, 0.68, 17.1, 0.067, 1.47 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6965, 0.6965, 16.9, 0.063, 1.45 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6975, 0.6975, 20.7, 0.304, 1.68 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.77, 0.77, 19.7, 0.203, 1.60 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.85, 0.85, 19.3, 0.130, 1.49 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.93, 0.93, 18.5, 0.100, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9705, 0.9705, 18.1, 0.090, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9715, 0.9715, 18.2, 0.135, 1.34 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.01, 1.01, 17.5, 0.115, 1.17 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1565, 1.1565, 16.4, 0.095, 1.01 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1575, 1.1575, 16.6, 0.100, 1.02 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.25, 1.25, 16.0, 0.085, 0.98 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.49, 1.49, 14.8, 0.055, 0.89 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.74, 1.74, 13.5, 0.023, 0.82 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
*/
/* trial value 5 */
/*	{ 0.30, 0.30, 25.7, 0.130, 2.21 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.68, 0.68, 17.1, 0.067, 1.47 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6965, 0.6965, 16.9, 0.063, 1.45 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6975, 0.6975, 20.7, 0.304, 1.68 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.77, 0.77, 19.7, 0.203, 1.60 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.85, 0.85, 19.3, 0.130, 1.49 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.93, 0.93, 18.5, 0.100, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9705, 0.9705, 18.1, 0.090, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9715, 0.9715, 18.2, 0.135, 1.34 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.01, 1.01, 17.5, 0.115, 1.17 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1565, 1.1565, 16.4, 0.095, 1.01 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1575, 1.1575, 16.6, 0.100, 1.02 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.25, 1.25, 16.0, 0.085, 0.98 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.49, 1.49, 14.8, 0.050, 0.89 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.74, 1.74, 13.5, 0.029, 0.82 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
*/
/* trial value 6 */
	{ 0.30, 0.30, 25.7, 0.130, 2.21 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.68, 0.68, 17.1, 0.067, 1.47 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6965, 0.6965, 16.9, 0.063, 1.45 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.6975, 0.6975, 20.7, 0.304, 1.68 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.77, 0.77, 19.7, 0.203, 1.60 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.85, 0.85, 19.3, 0.130, 1.49 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.93, 0.93, 18.5, 0.100, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9705, 0.9705, 18.1, 0.090, 1.32 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 0.9715, 0.9715, 18.2, 0.135, 1.34 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.01, 1.01, 17.5, 0.115, 1.17 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1565, 1.1565, 16.4, 0.095, 1.01 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.1575, 1.1575, 16.6, 0.100, 1.02 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.25, 1.25, 16.0, 0.085, 0.98 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.49, 1.49, 14.8, 0.0521, 0.89 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 1.74, 1.74, 13.5, 0.0351, 0.82 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 2.31, 2.31, 11.5, 0.0173, 0.75 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 2.62, 2.62, 10.8, 0.0130, 0.70 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 3.69, 3.69, 9.20, 0.00650, 0.60 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 4.51, 4.51, 8.20, 0.00433, 0.52 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },

/*	{ 2.31, 2.31, 11.5, 0.016, 0.75 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 2.62, 2.62, 10.8, 0.012, 0.70 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 3.69, 3.69, 9.20, 0.007, 0.60 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 4.51, 4.51, 8.20, 0.005, 0.52 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
*/
/* trial value 7 */
	{ 4.785, 4.785, 8.00, 0.004, 0.51 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 4.786, 4.721, 8.80, 0.0083, 0.56 ,{ 1.0e-2, 0.0, 0.0, 0.0, 0.0 } },
	{ 4.950, 4.884, 8.60, 0.008, 0.55 ,{ 1.0e-2, 0.0, 0.0, 0.0, 0.0 } },
	{ 5.105, 5.037, 8.45, 0.0073, 0.535 ,{ 1.0e-2, 0.0, 0.0, 0.0, 0.0 } },
	{ 5.106, 5.031, 8.45, 0.010, 0.535 ,{ 1.0e-2, 5.0e-3, 2.0e-3, 2.0e-3, 0.0 } },
	{ 5.410, 5.335, 8.00, 0.009, 0.525 ,{ 1.0e-2, 5.0e-3, 2.0e-3, 2.0e-3, 0.0 } },
	{ 5.453, 5.379, 7.97, 0.009, 0.523 ,{ 1.0e-2, 5.0e-3, 1.8e-3, 2.0e-3, 0.0 } },
	{ 5.454, 5.377, 7.97, 0.009, 0.523 ,{ 1.0e-2, 5.0e-3, 1.8e-3, 2.0e-3, 5.0e-4 } }, 
	{ 5.90, 5.84, 7.70, 0.008, 0.50 ,{ 1.0e-2, 5.0e-3, 1.6e-3, 2.0e-3, 5.0e-4 } },
	{ 6.40, 6.35, 7.30, 0.0075, 0.49 ,{ 9.3e-3, 4.0e-3, 1.3e-3, 2.5e-3, 5.0e-4 } },
	{ 6.92, 6.88, 7.05, 0.007, 0.47 ,{ 8.8e-3, 2.8e-3, 1.0e-3, 3.3e-3, 5.0e-4 } },
	{ 7.47, 7.44, 6.80, 0.0065, 0.46 ,{ 8.1e-3, 2.3e-3, 8.5e-4, 4.0e-3, 5.0e-4 } },
	{ 8.04, 8.02, 6.50, 0.006, 0.45 ,{ 7.6e-3, 1.4e-3, 7.0e-4, 4.5e-3, 5.0e-4 } },
	{ 8.63, 8.62, 6.20, 0.006, 0.44 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } },
	{ 9.87, 9.87, 5.75, 0.006, 0.42 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } },
	{ 11.91, 11.91, 5.25, 0.006, 0.42 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } },
	{ 14.92, 14.92, 4.70, 0.006, 0.46 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } },
	{ 17.46, 17.46, 4.25, 0.006, 0.51 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } }

/*	{ 4.785, 4.785, 8.00, 0.004, 0.51 ,{ 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ 4.786, 4.736, 8.80, 0.0083, 0.56 ,{ 1.0e-2, 0.0, 0.0, 0.0, 0.0 } },
	{ 4.95, 4.90, 8.60, 0.008, 0.55 ,{ 1.0e-2, 0.0, 0.0, 0.0, 0.0 } },
	{ 5.105, 5.055, 8.45, 0.0073, 0.535 ,{ 1.0e-2, 0.0, 0.0, 0.0, 0.0 } },
	{ 5.106, 5.046, 8.45, 0.010, 0.535 ,{ 1.0e-2, 5.0e-3, 2.0e-3, 2.0e-3, 0.0 } },
	{ 5.41, 5.35, 8.00, 0.009, 0.525 ,{ 1.0e-2, 5.0e-3, 2.0e-3, 2.0e-3, 0.0 } },
	{ 5.453, 5.393, 7.97, 0.009, 0.523 ,{ 1.0e-2, 5.0e-3, 1.8e-3, 2.0e-3, 0.0 } },
	{ 5.454, 5.384, 7.97, 0.009, 0.523 ,{ 1.0e-2, 5.0e-3, 1.8e-3, 2.0e-3, 5.0e-4 } }, 
	{ 5.90, 5.84, 7.70, 0.008, 0.50 ,{ 1.0e-2, 5.0e-3, 1.6e-3, 2.0e-3, 5.0e-4 } },
	{ 6.40, 6.35, 7.30, 0.0075, 0.49 ,{ 9.3e-3, 4.0e-3, 1.3e-3, 2.5e-3, 5.0e-4 } },
	{ 6.92, 6.88, 7.05, 0.007, 0.47 ,{ 8.8e-3, 2.8e-3, 1.0e-3, 3.3e-3, 5.0e-4 } },
	{ 7.47, 7.44, 6.80, 0.0065, 0.46 ,{ 8.1e-3, 2.3e-3, 8.5e-4, 4.0e-3, 5.0e-4 } },
	{ 8.04, 8.02, 6.50, 0.006, 0.45 ,{ 7.6e-3, 1.4e-3, 7.0e-4, 4.5e-3, 5.0e-4 } },
	{ 8.63, 8.62, 6.20, 0.006, 0.44 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } },
	{ 9.87, 9.87, 5.75, 0.006, 0.42 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } },
	{ 11.91, 11.91, 5.25, 0.006, 0.42 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } },
	{ 14.92, 14.92, 4.70, 0.006, 0.46 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } },
	{ 17.46, 17.46, 4.25, 0.006, 0.51 ,{ 7.0e-3, 1.5e-3, 6.0e-4, 4.3e-3, 5.0e-4 } }
*/
};

static int i_Cal;
static double t_E,s_E;

static void
set_cal_data(double Ex)
{
	for (i_Cal = Ncal-1; 0 < i_Cal; i_Cal--) {
		if ( Cal[i_Cal].Ex < Ex ) break;
	}
	t_E=log(Ex/Cal[i_Cal].Ex)/log(Cal[i_Cal+1].Ex/Cal[i_Cal].Ex);
	s_E=log(Cal[i_Cal+1].Ex/Ex)/log(Cal[i_Cal+1].Ex/Cal[i_Cal].Ex);
}

static double
get_ph(void)
{
	/* 5.83 = 5.9 - 0.07(70eV jump) */
	return exp( s_E * log(Cal[i_Cal].ph) + t_E * log(Cal[i_Cal+1].ph) )/5.83;
}

static double
get_Eres(void)
{
	return exp(s_E*log(Cal[i_Cal].Eres) + t_E * log(Cal[i_Cal+1].Eres) )/7.70;
}

static double
get_phkappa(void)
{
	return exp( s_E*log(Cal[i_Cal].phkappa) + t_E*log(Cal[i_Cal+1].phkappa) )/Al_kappa0 * Kappa_correct;
}

static double
get_posres(void)
{
	return exp(s_E*log(Cal[i_Cal].posres) + t_E*log(Cal[i_Cal+1].posres))/Mn_posres0;
}

static void
get_escape(double *esc_rate)
{
	int i;

	for(i=0;i<5;i++){
		if ( 0.0 == Cal[i_Cal].Esc_rate[i] ) {
			esc_rate[i]=0.0;
		} else {
			esc_rate[i] = exp( s_E * log(Cal[i_Cal].Esc_rate[i]) + t_E * log(Cal[i_Cal+1].Esc_rate[i]) );
		}
	}
}

static void
xy_xyres(double xmm, double ymm, double *dx, double *dy)
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
xy_Eres(double xmm, double ymm)
{
	double rr, dum;
	rr = xmm*xmm + ymm*ymm;
	if ( rr < 20.0*20.0 ) return 1.0;
	dum = ( sqrt(rr) - 20.0 ) / 2.0;
	return 1.0 + dum*dum*0.1;
}

static double
xy_gain(double xmm, double ymm)
{
/*	return ( 700 - 0.0005*squ(xmm*xmm+ymm*ymm) ) / 700; */
	return 1.0;
}



static void
local_correct(double xmm, double ymm, double *R, struct Ex_Response *Ex_Re, struct xy_Response *xy_Re)
{
	int i;
	double correct[4];

	correct[0] = xy_gain(xmm, ymm);
	correct[1] = xy_Eres(xmm, ymm);
	xy_xyres(xmm, ymm, &correct[2], &correct[3]);
	R[1] = Ex_Re->ph_ch * correct[0];
	R[2] = Ex_Re->ph_wd * correct[0] * correct[1];
	R[3] = xy_Re->x_wd * correct[2];
	R[4] = xy_Re->y_wd * correct[3];
	for(i=0;i<Nesc;i++){
		if(Ex_Re->es_rate[i]==0.0){
			R[5+i*5] = 0.0;
			R[6+i*5] = 0.0;
			R[7+i*5] = Ex_Re->es_rate[i];
			R[8+i*5] = 0.0;
			R[9+i*5] = 0.0;
		} else {
			R[5+i*5] = Ex_Re->es_ch[i] * correct[0];
			R[6+i*5] = Ex_Re->es_wd[i] * correct[0] * correct[1];
			R[7+i*5] = Ex_Re->es_rate[i];
			R[8+i*5] = xy_Re->es_x_wd[i] * correct[2];
			R[9+i*5] = xy_Re->es_y_wd[i] * correct[3];
		}
	}
}

static double E[gNE2];

void
gi_spec(double *Rxy, double *E0)
{
	int  i;
	for (i=0; i < gNE; i++) E0[i] = E[i];
}

#define min(x,y)  (((x)<(y))?(x):(y))
#define ESCAPE

static double
calc_obunpu(double N, double No, double s, double k)
{
	static double KIZAMI1 = 100;		/* 0 から 1 を何分割するか */
	/*static double KIZAMI2 = 10;	 1σを何分割するか */
	double sum, it, dit, sit, eit, sq, /*dsq, ssq, esq, */spow, epow;

	k = fabs(k);
	sum = 0.0;
	dit = 1.0 / KIZAMI1;
#if 0
	dsq = 1.0 / KIZAMI2;
	esq = 1.0e99;
#endif
	eit = 0.0;
	epow = 1.0;
	until ( 1.0 == eit ) {
		sit = eit;
		eit = min(1.0, eit+dit);
#if 0
		ssq = esq;
		esq = ( N/eit - No ) / s;
		if ( esq*ssq < 0.0 || ssq*ssq < 9.0 || esq*esq < 9.0 ) {
			unless ( 0.0 == N ) {
				esq = min(3.0, ssq - dsq);
				eit = min(eit, N/(s*esq+No));
			}
		}
#endif
		spow = epow;
		epow = pow(1-eit, k);
		it = ( sit + eit ) / 2.0;
		sq = ( N/it - No ) / s;
		sum -= exp(-sq*sq/2.0) / it * ( epow - spow );
	}
	return sum/2.50662827463100050241/s;	/* sqrt(2*PI) */
}

static double
gi_spec0(double *R, double kappa)
{
	static double eswd[Nesc], esnm[Nesc];
	int i, j, Eend;
	static double x, norm, phwd, totc, net, Ocut, Oend;
	for (i = 0; i < gNE2; i++) E[i] = 0;
	phwd = R[2]/FWHM;
	for(i=0;i<Nesc;i++) {
		eswd[i] = R[6+i*5]/FWHM;
		if ( 0 == eswd[i] ) {
			esnm[i] = 0;
		} else {
			esnm[i] = R[7+i*5] / eswd[i] / sqrt(2.0*PI);
		}
	}
#ifdef EXP
/* 0.3keV 以下では、O 分布の計算がうまくいかない */
	Ocut = PH_Mn * 0.05;
	if(Ocut < R[1]*0.25) Ocut = R[1]*0.25;
	Oend = PH_Mn * 0.05;
	Eend = min( gNE2, (int)R[1]*2 );
	for (i = (int)Ocut; i < Eend; i++) {
		E[i] = calc_obunpu(i, R[1], phwd, kappa);
		E[i] *= 1.0 - exp(-(i-Oend)/Ocut);
	}
	norm = 1.0 - exp(-(Ocut-Oend)/Ocut);
	for (i = (int)Oend; i < (int)Ocut; i++) {
		E[i] = E[(int)Ocut] * ( 1.0 - exp(-(i-Oend)/Ocut) ) / norm;
	}
#elif defined EXP2
/* 0.3keV 以下では、O 分布の計算がうまくいかない */
	Ocut = PH_Mn * 0.05;
	if(Ocut < R[1]*0.25) Ocut = R[1]*0.25;
	Oend = PH_Mn * 0.05;
	Eend = min( gNE2, (int)R[1]*2 );
	for (i = (int)Ocut; i < Eend; i++) {
		E[i] = calc_obunpu(i, R[1], phwd, kappa);
		E[i] *= 1.0 - AEXP*exp(-(i-Oend)/Ocut);
	}
	norm = 1.0 - AEXP*exp(-(Ocut-Oend)/Ocut);
	for (i = (int)Oend; i < (int)Ocut; i++) {
		E[i] = E[(int)Ocut] * ( 1.0 - AEXP*exp(-(i-Oend)/Ocut) ) / norm;
	}
#else
	Oend = PH_Mn * 0.05;
	if(Ocut < R[1]*0.25) Ocut = R[1]*0.25;
	Eend = min( gNE2, (int)R[1]*2 );
	for (i = (int)Ocut; i < Eend; i++) {
		E[i] = calc_obunpu(i, R[1], phwd, kappa);
	}
	for (i = (int)Oend; i < (int)Ocut; i++) {
		E[i] = E[(int)Ocut];
	}
#endif
#ifdef ESCAPE
	for (i = 0; i < (int)Eend; i++) {
		for(j=0;j<Nesc;j++) {
			if ( 0.0 != esnm[j] ) {
				x = ( i - R[5+j*5] ) / eswd[j];
				E[i] += esnm[j] * exp(-x*x/2);
			}
		}
	}
#endif
	totc = net = 0.0;
	for (i = 0; i < gNE2; i++) totc += E[i];
	if ( 0 == totc ) return 0;
	for (i = 0; i < gNE; i++) net += E[i];
	net /= totc;
	for (i = 0; i < gNE; i++) E[i] /= totc;
	return net;
}

static double
gi_xy_spec(double *R, double *(*Rxy)[256], double xmm, double ymm)
{
	int  i,j,k;
	double x, y, dum, rr, eswd[Nesc][2], esnm[Nesc], mmnm, mmwd[2], totc;

	mmwd[0] = R[3]/FWHM;
	mmwd[1] = R[4]/FWHM;
	if ( 0 == mmwd[0] || 0 == mmwd[1] ) {
		mmnm = 0;
	} else {
		mmnm = 1.0 / mmwd[0] / mmwd[1] / (2.0*PI);
	}
	for(i=0;i<Nesc;i++) {
		eswd[i][0] = R[8+i*5]/FWHM;
		eswd[i][1] = R[9+i*5]/FWHM;
		if ( 0 == eswd[i][0] || 0 == eswd[i][1] ) {
			esnm[i] = 0;
		} else {
			esnm[i] = R[7+i*5] / eswd[i][0] / eswd[i][1] / (2.0*PI);
		}
	}
	for(i=0;i<gNX;i++) {
		for(j=0;j<gNY;j++) {
			if ( ch2mm(i, j, &x, &y) ) continue;
			rr = sqrt( squ(x-xmm) + squ(y-ymm) );
			dum = squ((x-xmm)/R[3]*FWHM) + squ((y-ymm)/R[4]*FWHM);
			if ( rr > 100.0 ) {					/* far */
				continue;
			} else {
				*Rxy[i][j] = exp(-dum/2.0) * mmnm;
			}
#ifdef ESCAPE
			for(k=0;k<Nesc;k++) {
				if ( 0.0 == R[7+k*5] ) continue;
				dum = squ((x-xmm)/R[8+5*k]*FWHM) + squ((y-ymm)/R[9+5*k]*FWHM);
				if ( rr > 100.0 ) {					/* far */
					continue;
				} else {
					*Rxy[i][j] += exp(-dum/2.0) * esnm[k];
				}
			}
#endif
#ifdef DEBAG
			} else if ( rr > 16.0 ) {			/* middle */
				*Rxy[i][j] = exp(-dum/2.0);
			} else {							/* near */
				int ii, jj;
				for (ii = 0; ii < 5; ii++) {
					for (jj = 0; jj < 5; jj++) {
						if ( ch2mm(i-.5+.2*ii+.1, j-.5+.2*jj+.1, &x, &y)){
							continue;
						}
						dum = squ(x-xmm)/squ(xwd) + squ(y-ymm)/squ(ywd);
						*Rxy[i][j] += exp(-dum/2.0) / 25.0;
					}
				}
			}
#endif
		}
	}
	totc = 0.0;
	for (i = 0; i < gNX; i++)
		for (j = 0; j < gNY; j++) totc += *Rxy[i][j];
	if ( 0 == totc ) return 0;
	for (i = 0; i < gNX; i++)
		for (j = 0; j < gNY; j++) *Rxy[i][j] /= totc;
	return totc;
}


/*
 * 0:success, -1:fail
/ */
int
gi_allocRxy(double *(*Rxy)[256])
{
	int i;
	for (i = 0; i < gNX*gNY; i++) {
/*		Rxy[0][i] = (double*)malloc( sizeof(double)*NPARA ); */
		Rxy[0][i] = (double*)malloc( sizeof(double) );
		if ( NULL == Rxy[0][i] ) {
			while (i) {
				free(Rxy[0][i-1]);
				i--;
			}
			return -1;
		}
	}
	return 0;
}

static void
get_Ex_Respo(double Ex, struct Ex_Response *Ex_Re)
{
	int i;
	double esc_rate[Nesc];

	Ex_Re->ph_ch = get_ph() * PH_Mn;
	Ex_Re->ph_wd = Mn_Res * Ex_Re->ph_ch * get_Eres();
	Ex_Re->ph_kappa = Al_kappa * get_phkappa();
	get_escape(esc_rate);
	for(i=0;i<Nesc;i++) Ex_Re->es_rate[i] = 0.0;
	if ( XeL3E < Ex ) {						/* 1st escape */
		Ex_Re->es_ch[0] = ( Ex - XeLa ) / Ex * Ex_Re->ph_ch;
		Ex_Re->es_wd[0] = Ex_Re->ph_wd / Ex_Re->ph_ch * sqrt( Ex/(Ex-XeLa) ) * Ex_Re->es_ch[0];
		Ex_Re->es_rate[0] = esc_rate[0];
		if ( XeL2E < Ex ) {						/* 2nd - 4th  escape */
			Ex_Re->es_ch[1] = ( Ex - XeLb1 ) / Ex * Ex_Re->ph_ch;
			Ex_Re->es_wd[1] = Ex_Re->ph_wd / Ex_Re->ph_ch * sqrt( Ex/(Ex-XeLb1) ) * Ex_Re->es_ch[1];
			Ex_Re->es_rate[1] = esc_rate[1];
			Ex_Re->es_ch[2] = ( Ex - XeLb2 ) / Ex * Ex_Re->ph_ch;
			Ex_Re->es_wd[2] = Ex_Re->ph_wd / Ex_Re->ph_ch * sqrt( Ex/(Ex-XeLb2) ) * Ex_Re->es_ch[2];
			Ex_Re->es_rate[2] = esc_rate[2];
			Ex_Re->es_ch[3] = ( Ex - XeLb3 ) / Ex * Ex_Re->ph_ch;
			Ex_Re->es_wd[3] = Ex_Re->ph_wd / Ex_Re->ph_ch * sqrt( Ex/(Ex-XeLb3) ) * Ex_Re->es_ch[3];
			Ex_Re->es_rate[3] = esc_rate[3];
			if ( XeL1E < Ex ) {						/* 5th escape */
				Ex_Re->es_ch[4] = ( Ex - XeLc ) / Ex * Ex_Re->ph_ch;
				Ex_Re->es_wd[4] = Ex_Re->ph_wd / Ex_Re->ph_ch * sqrt( Ex/(Ex-XeLc) ) * Ex_Re->es_ch[4];
				Ex_Re->es_rate[4] = esc_rate[4];
			}
		}
	}
}

static void
get_xy_Respo(double Ex, struct xy_Response *xy_Re)
{
	xy_Re->x_wd = xy_Re->y_wd = Mn_posres * get_posres();
	if ( XeL3E < Ex ) {
		xy_Re->es_x_wd[0] = xy_Re->es_y_wd[0] = xy_Re->x_wd * sqrt( Ex/(Ex-XeLa) );
		if ( XeL2E < Ex ) {
			xy_Re->es_x_wd[1] = xy_Re->es_y_wd[1] = xy_Re->x_wd * sqrt( Ex/(Ex-XeLb1) );
			xy_Re->es_x_wd[2] = xy_Re->es_y_wd[2] = xy_Re->x_wd * sqrt( Ex/(Ex-XeLb2) );
			xy_Re->es_x_wd[3] = xy_Re->es_y_wd[3] = xy_Re->x_wd * sqrt( Ex/(Ex-XeLb3) );
			if ( XeL1E < Ex ) {
				xy_Re->es_x_wd[4] = xy_Re->es_y_wd[4] = xy_Re->x_wd * sqrt( Ex/(Ex-XeLc) );
			}
		}
	}
}

void
gi_response(double xmm, double ymm, double Ekev, double *(*Rxy)[256], void *ginfo, void *time)
{
	int i;
	struct Ex_Response Ex_Re;
	struct xy_Response xy_Re;
	double eff, R[NPARA];
	for (i = 0; i < gNX*gNY; i++) *Rxy[0][i] = 0.0;
	if ( Ekev <= 0.0 || 15.0 < Ekev ) return;
	R[0]=1.0;
	set_cal_data(Ekev);
	get_Ex_Respo(Ekev, &Ex_Re);
	get_xy_Respo(Ekev, &xy_Re);
	local_correct(xmm, ymm, R, &Ex_Re, &xy_Re);
	gi_xy_spec(R, Rxy, xmm, ymm);
	gi_spec0(R, Ex_Re.ph_kappa);
	eff = gis_eff(Ekev, 0);
	for (i = 0; i < gNX*gNY; i++) *Rxy[0][i] *= eff;	/* normalize */
}

static double oldEkev = -1.0;
static double oldxmm = -9999, oldymm = -9999;
static struct Ex_Response Ex_Re;
static struct xy_Response xy_Re;
static double R[NPARA];
static double parallax;

static void
check_if_new_energy(double xmm, double ymm, double Ekev)
{
	double mfre;
	if ( oldEkev != Ekev ) {
		oldxmm = xmm;
		oldymm = ymm;
		oldEkev = Ekev;
		set_cal_data(Ekev);
		get_Ex_Respo(Ekev, &Ex_Re);
		get_xy_Respo(Ekev, &xy_Re);
		local_correct(xmm, ymm, R, &Ex_Re, &xy_Re);
		mfre = calc_Gas_mean_free_path(Ekev, He_pressure, Xe_pressure) * 10;
		parallax = FWHM * sin(2.0*PI/180) * mfre;
		parallax *= parallax;
	} else if ( oldxmm != xmm || oldymm != ymm ) {
		oldxmm = xmm;
		oldymm = ymm;
		local_correct(xmm, ymm, R, &Ex_Re, &xy_Re);
	}
}	

void
gis_total_spectrum(double xmm, double ymm, double Ekev, double *E0)
{
	if ( Ekev <= 0.0 || 15.0 < Ekev ) {
		int i;
		for (i = 0; i < gNE; i++) E0[i] = 0;
		return;
	}
	check_if_new_energy(xmm, ymm, Ekev);
	gi_spec0(R, Ex_Re.ph_kappa);
	memcpy(E0, E, gNE*sizeof(*E0));
}

double
gis_psf(double xmm, double ymm, double Ekev, double xoff, double yoff)
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
	sgx = R[3]*R[3] + parallax;
	sgy = R[4]*R[4] + parallax;
	norm = 1;
	flux = 0;
	dum = xx/sgx + yy/sgy;
	if ( dum < 20 ) {
		flux = exp(-dum/2.0) / sqrt(sgx*sgy);
	}
#ifdef ESCAPE
	for (i = 0; i < Nesc; i++) {
		double *Rp = &R[5*i];
		if ( 0.0 == Rp[7] ) continue;
		sgx = Rp[8]*Rp[8] + parallax;
		sgy = Rp[9]*Rp[9] + parallax;
		norm += Rp[7];
		dum = xx/sgx + yy/sgy;
		if ( dum < 20 ) {
			flux += Rp[7] * exp(-dum/2.0) / sqrt(sgx*sgy);
		}
	}
#endif
	return flux / 2.0 / PI * FWHM * FWHM / norm;
}

int
gis_psf_sigma(double xmm, double ymm, double Ekev, double *ratio, double *sgx, double *sgy)
{
	int i, n;
	if ( Ekev <= 0.0 || 15.0 < Ekev ) {
		return 0;
	}
	check_if_new_energy(xmm, ymm, Ekev);
	n = 1;
	*ratio++ = 1.0;
	*sgx++ = sqrt(R[3]*R[3] + parallax) / FWHM;
	*sgy++ = sqrt(R[4]*R[4] + parallax) / FWHM;
#ifdef ESCAPE
	for (i = 0; i < Nesc; i++) {
		double *Rp = &R[5*i];
		if ( 0.0 == Rp[7] ) continue;
		*sgx++ = sqrt(Rp[8]*Rp[8] + parallax) / FWHM;
		*sgy++ = sqrt(Rp[9]*Rp[9] + parallax) / FWHM;
		*ratio++ = Rp[7];
		n++;
	}
#endif
	return n;
}

double
gis_efficiency(double xmm, double ymm, double Ekev)
{
	double eff;
	eff = gis_eff(Ekev, 0);
	return eff;
}

double
gis_elements_trans(double energy, double *plasma, double *be, double *gas)
{
        double depth;
	*plasma = calc_PET_trans(energy, 2*PET_thick, 2*Al_thick);
	*be = calc_Be_trans(energy, Be_thick);
        depth = 0.1;
	*gas = calc_Gas_trans(energy, depth, He_pressure, Xe_pressure);
	return gis_eff(energy, 0);
}

double
gis_kappa(double energy, double *kappa, double *Xe_sigma, double *Eres)
{
	double ph;
	set_cal_data(energy);
	*kappa = Al_kappa * get_phkappa();
	*Xe_sigma = calc_matter_tau((char *)"Xe", energy, 1.0);
	ph = get_ph();
	*Eres = Mn_Res * get_Eres();
	return get_ph();
}

