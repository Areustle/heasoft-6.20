#ifndef UVOTINTEG_CLIP_H
#define UVOTINTEG_CLIP_H


typedef struct
{
	double * data;    /* input/output: data to sigma clip */
	int count;        /* input/output: number of elements of data to read */

	double nsigma;    /* input: how many sigma outliers to clip */
	int maxiter;      /* input: if > 0, constrains number of iterations */

	int niter;        /* output: actual number of iterations */
	double mean;      /* output: mean of unclipped data */
	double sigma;     /* output: sigma of unclipped data */

} SigmaClip;


int sigma_clip (SigmaClip * task);
void add_clip_data (SigmaClip * task, double data);


#endif
