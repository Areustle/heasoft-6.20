/* $Id: aste_rand.h,v 1.5 2006/08/26 15:20:57 ishisaki Exp $ */
/*******************************************************************
*	hrndm.h: Random number following specified distribution
*
*	2006/08/26	Y.ISHISAKI	version 1.83
*		add Hrndm1D**(), Hrndm2D**()
********************************************************************/

#ifndef _ASTE_RAND_H_
#define _ASTE_RAND_H_

struct Hrndm1 {
	int ne, ni, ipos;
	unsigned short *index;
	double *ear, model[1];
};

struct Hrndm2 {
	int nx, ny;
	float xmi, xma, ymi, yma;
	struct Hrndm1 *yhm;
	struct Hrndm1 *xhm[1];
};

struct Hrndm1D {
	int ne, ni, ipos;
	unsigned short *index;
	double *ear, model[1];
};

struct Hrndm2D {
	int nx, ny;
	double xmi, xma, ymi, yma;
	double xmami, ymami;
	struct Hrndm1D *yhm;
	struct Hrndm1D *xhm[1];
};

struct HrndmRMF {
	struct HrndmRMFindex {
		double norm, offs;
		int nbody;	/* in fact, sizeof(body)-1 */
		int *body;	/* 0-nbody */
	} index;
	int ne, detchans;
	struct HrndmRMFrmf {
		struct { double lo, hi; } e;
		int f_chan, n_chan;
		float *mat;
	} *rmf;
	struct HrndmRMFebounds {
		int *channel;
		double *e_min, *e_max;
	} ebounds;
};

#ifdef __cplusplus
extern "C" {
#endif

int aste_rndseed(void);
void aste_rndlcini(int irseed);
void aste_irndlc(int np, int *iran);
void aste_rndtsini(int irseed);
double aste_drndts(void);
void aste_drndtsn(int np, double *dran);
void aste_drndtsn_skip(int np);
void aste_drndtsn_skipd(double np);
double aste_drndtsn_gen(void);
double aste_drndtsg(void);

struct Hrndm1 *Hrndm1_init(int ne, float *ear, float *photar, int ni);
void Hrndm1_free(struct Hrndm1 *hm);
double Hrndm1(double pos, struct Hrndm1 *hm);

struct Hrndm1D *Hrndm1D_init(int ne, double *ear, double *photar, int ni);
void Hrndm1D_free(struct Hrndm1D *hm);
double Hrndm1D(double pos, struct Hrndm1D *hm);

struct Hrndm2 *Hrndm2_init(float *image, int nx, double xmi, double xma, int ny, double ymi, double yma);
void Hrndm2_free(struct Hrndm2 *hm2);
void Hrndm2(double pos[2], struct Hrndm2 *hm2);

struct Hrndm2D *Hrndm2D_init(double *image, int nx, double xmi, double xma, int ny, double ymi, double yma);
void Hrndm2D_free(struct Hrndm2D *hm2);
void Hrndm2D(double pos[2], struct Hrndm2D *hm2);

struct HrndmRMF *HrndmRMF_init(char *rmffile);
void HrndmRMF_free(struct HrndmRMF *hmf);
int HrndmRMF(double energy, double pos, struct HrndmRMF *hmf, double *pi, double *efficiency);

double HrndmB(int hid, .../* double *r_x, double *r_y */);
void HrndmB_free(int hid);

#ifdef __cplusplus
}
#endif

#endif
