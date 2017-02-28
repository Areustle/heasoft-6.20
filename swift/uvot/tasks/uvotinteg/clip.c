/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotinteg/clip.c,v $
 * $Revision: 1.1 $
 * $Date: 2008/03/26 19:54:25 $
 *
 * $Log: clip.c,v $
 * Revision 1.1  2008/03/26 19:54:25  rwiegand
 * Started implementing MEAN operation and sigma clipping, but not ready
 * for public use.
 *
 */

#include <math.h>
#ifdef DEBUG_SIGMA_CLIP
#include <stdio.h>
#endif /* DEBUG_SIGMA_CLIP */

#include "clip.h"
#include "report.h"


#ifdef DEBUG_SIGMA_CLIP
static void
print_data (const double * data, int count)
{
	int i;
	for (i = 0; i < count; ++i)
		printf("%d: %.3f\n", i, data[i]);
}
#endif /* DEBUG_SIGMA_CLIP */


void
add_clip_data (SigmaClip * task, double x)
{
	task->data[task->count] = x;
	++task->count;
}


static int
sigma_clip_aux (SigmaClip * task)
{
	int i, result, count1;
	double sum, sum2;

	sum = sum2 = 0;

#ifdef DEBUG_SIGMA_CLIP
	print_data(task->data, task->count);
#endif /* DEBUG_SIGMA_CLIP */

	for (i = 0; i < task->count; ++i) {
		sum += task->data[i];
		sum2 += task->data[i] * task->data[i];
	}

	task->mean = sum / task->count;
	task->sigma = sqrt(sum2 / task->count - task->mean * task->mean);
#ifdef DEBUG_SIGMA_CLIP
	printf("sigma_clip_aux: sum=%.3f sum2=%.3f mean=%.3f sigma=%.3f\n",
			sum, sum2, task->mean, task->sigma);
#endif /* DEBUG_SIGMA_CLIP */

	count1 = 0;
	for (i = 0; i < task->count; ++i) {
		if (fabs(task->data[i] - task->mean) <= task->nsigma * task->sigma)
			task->data[count1++] = task->data[i];
		/* else data[i] is clipped */
#ifdef DEBUG_SIGMA_CLIP
		else
			printf("clipping %d %.3f\n", i, task->data[i]);
#endif /* DEBUG_SIGMA_CLIP */
	}

	++task->niter;
#ifdef DEBUG_SIGMA_CLIP
	printf("sigma_clip_aux: iteration %d: count=%d count1=%d\n",
			task->niter, task->count, count1);
#endif /* DEBUG_SIGMA_CLIP */

	if (count1 == task->count)
		result = 0;
	else {
		task->count = count1;
		if (task->niter == task->maxiter)
			result = 0;
		else if (task->count == 1) {
			task->mean = task->data[0];
			task->sigma = 0;
			result = 0;
		}
		else
			result = sigma_clip_aux(task);
	}

	return result;
}


int
sigma_clip (SigmaClip * task)
{
	int result = 0;

	task->niter = 0;

	if (task->count < 1) {
		result = 1;
		report_warning("sigma_clip: invalid count %d\n", task->count);
	}
	else if (task->nsigma < 1) {
		result = 1;
		report_warning("sigma_clip: invalid sigma %f\n", task->sigma);
	}
	else
		result = sigma_clip_aux(task);

	return result;
}


#ifdef DEBUG_SIGMA_CLIP
static int
sigma_clip_test (double * data, int count)
{
	int result;
	SigmaClip task = { 0 };
	task.data = data;
	task.count = count;
	task.maxiter = 0;
	task.nsigma = 1.5;

	printf("sigma_clip_test: %d values nsigma=%.1f maxiter=%d\n",
			count, task.nsigma, task.maxiter);

	result = sigma_clip(&task);
	printf("result=%d\n", result);
	printf("\tcount=%d\n", task.count);
	printf("\tmean=%.3f\n", task.mean);
	printf("\tsigma=%.3f\n", task.sigma);
	printf("\tniter=%d\n", task.niter);

	print_data(task.data, task.count);
	
}
#endif /* DEBUG_SIGMA_CLIP */



#ifdef TEST_SIGMA_CLIP

#include <stdlib.h>

#define N_ELEMENTS(array) (sizeof(array)/sizeof(array[0]))

int main (int argc, char * argv[])
{
	int i;
	double data0[] = { 1, 2, 3, 4 };
	double data1[20];

	if (argc > 1)
		srand48(time(0));
	else
		srand48(0);

	sigma_clip_test(data0, N_ELEMENTS(data0));

	for (i = 0; i < N_ELEMENTS(data1); ++i)
		data1[i] = drand48();
	sigma_clip_test(data1, N_ELEMENTS(data1));

	return 0;
}

#endif /* TEST_SIGMA_CLIP */
