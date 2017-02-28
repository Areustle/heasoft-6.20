
#include <math.h>
#include <stdlib.h>

#include "headas.h"
#include "param.h"
#include "image.h"


typedef struct InterRecord InterRecord;
struct InterRecord
{
	float x, y, z;
	InterRecord *next;
};


typedef struct InterDelta InterDelta;
struct InterDelta
{
	int dx, dy;
};


typedef struct InterTable InterTable;
struct InterTable
{
	int dimenx, dimeny;
	InterRecord ***grid;
	int nDeltas;
	InterDelta *delta;
	double scale;
};


typedef struct InterBorder InterBorder;
struct InterBorder
{
	int dimenx, dimeny;
	int null;
	double ipad;
	double opad;
	int *minx;
	int *maxx;
	int *miny;
	int *maxy;
	double lastx;
	double lasty;
};


typedef struct InterState InterState;
struct InterState
{
	int x0, y0;
	InterRecord *probe;
	double probeDistance;
	double minDist2, maxDist2;
	int foundAny, foundBetter;
	double scale;
};


static int compareDeltas (const void *v1, const void *v2)
{
	InterDelta *n1 = (InterDelta *) v1;
	InterDelta *n2 = (InterDelta *) v2;

	int d1 = n1->dx * n1->dx + n1->dy * n1->dy;
	int d2 = n2->dx * n2->dx + n2->dy * n2->dy;

	if (d1 != d2)
		return d1 - d2;

	/* the rest is just for stable ordering */
	if (n1->dx != n2->dx)
		return n1->dx - n2->dx;

	return n1->dy - n2->dy;
}


static void initializeInterTable (InterTable *table,
		IMAGE* original, PARAM* param)
{
	int i, j;
	double x, y;


	/************************************
	* allocate InterRecord lookup table
	************************************/
	table->grid = (InterRecord***)
			calloc(table->dimeny, sizeof(InterRecord**));

	for (j = 0; j < table->dimeny; ++j)
		table->grid[j] = (InterRecord**)
				calloc(table->dimenx, sizeof(InterRecord*));

	/************************************
	* populate InterRecord lookup table
	************************************/
	for (j = 0; j < original->dimeny; ++j) {
		for (i = 0; i < original->dimenx; ++i) {
			int ihat, jhat;
			applyComboXform(param->combo, &x, &y, i, j);
			ihat = (int) floor(x);
			jhat = (int) floor(y);
			if (ihat >= 0 && ihat < table->dimenx
					&& jhat >= 0 && jhat < table->dimeny) {
				InterRecord *tmp, *existing;
				tmp = calloc(1, sizeof(InterRecord));
				tmp->x = ihat;
				tmp->y = jhat;
				tmp->z = (float) getImagePixel(original, i, j);
				existing = table->grid[jhat][ihat];
				table->grid[jhat][ihat] = tmp;
				if (existing)
					tmp->next = existing;
			}
		}
	}

	headas_chat(4, "built known grid\n");

	{
		double *rot, tmp;
		int index, range, count, maxDist2;
		table->scale = 0;
		for (i = 0; i < 2; ++i) {
			rot = param->combo->trans->rot[i];
			tmp = fabs(rot[0]) + fabs(rot[1]);
			if (tmp > table->scale)
				table->scale = tmp;
		}

		headas_chat(4, "scale=%f\n", table->scale);

		range = (int) (2.5 + table->scale);
		if (range < 3)
			range = 3;
		count = 2 * range + 1;
		count *= count;
		table->delta = (InterDelta *) calloc(count, sizeof(InterDelta));
		index = 0;
		for (j = -range; j <= range; ++j) {
			for (i = -range; i <= range; ++i) {
				InterDelta *p = &table->delta[index++];
				p->dx = i;
				p->dy = j;
			}
		}

		qsort(table->delta, count, sizeof(InterDelta), compareDeltas);

		/* only keep deltas which are in (rough) circle */
		maxDist2 = range * range;
		for (i = 0; i < count; ++i) {
				InterDelta *p = &table->delta[i];
				if (p->dx * p->dx + p->dy * p->dy > maxDist2)
					break;
		}

		table->nDeltas = i;
		headas_chat(4, "checking up to %d neighbors\n", table->nDeltas);

#if 0
		for (i = 0; i < table->nDeltas; ++i) {
			InterDelta *p = &table->delta[i];
			printf("delta[%d] is %d, %d\n", i, p->dx, p->dy);
		}
#endif
	}
}


static void destroyInterTable (InterTable *table)
{
	headas_chat(5, "destroyInterTable: not fully implemented\n");
	free(table->delta);
}



static void updateBorder2 (InterBorder *border, int x, int y)
{
	if (y >= 0 && y < border->dimeny) {
		if (border->minx[y] == border->null || border->minx[y] > x)
			border->minx[y] = x;

		if (border->maxx[y] == border->null || border->maxx[y] < x)
			border->maxx[y] = x;
	}

	if (x >= 0 && x < border->dimenx) {
		if (border->miny[x] == border->null || border->miny[x] > y)
			border->miny[x] = y;

		if (border->maxy[x] == border->null || border->maxy[x] < y)
			border->maxy[x] = y;
	}
}



#define UIMIN(a,b) (((a)<(b)) ? a : b)
#define UIMAX(a,b) (((a)>(b)) ? a : b)

static void updateBorder1 (InterBorder *border, int x, int y)
{
	int i, j;
	int x0, x1, y0, y1;

	updateBorder2(border, x, y);

	if (border->lastx != border->null) {
		if (abs(x - border->lastx) + abs(y - border->lasty) > 1) {
			x0 = UIMIN(border->lastx, x);
			x1 = UIMAX(border->lastx, x);
			y0 = UIMIN(border->lasty, y);
			y1 = UIMAX(border->lasty, y);
			for (i = x0; i <= x1; ++i)
				for (j = y0; j <= y1; ++j)
					updateBorder2(border, i, j);
		}
	}

	border->lastx = x;
	border->lasty = y;
}


static void updateBorder (InterBorder *border, PARAM *param, double x, double y)
{
	double ox, oy;
	int x0, x1, y0, y1;

	applyComboXform(param->combo, &ox, &oy, x, y);

	/* apply 0.5 offset since we are about to floor everything */
	ox += 0.5;
	oy += 0.5;

	if (fabs(border->opad) < 1e-3) {
		x0 = (int) floor(ox);
		y0 = (int) floor(oy);
		updateBorder1(border, x0, y0);
	}
	else {
		x0 = (int) floor(ox - border->opad);
		x1 = (int) floor(ox + border->opad);
		y0 = (int) floor(oy - border->opad);
		y1 = (int) floor(oy + border->opad);

		updateBorder1(border, x0, y0);
		updateBorder1(border, x1, y0);
		updateBorder1(border, x1, y1);
		updateBorder1(border, x0, y1);
	}
}


static void initializeBorder (InterBorder *border, IMAGE *original, PARAM *param)
{
	int i;
	double x, y;
	double x0, x1, y0, y1;

	border->null = -123456789;
	border->lastx = border->null;
	border->dimenx = param->dimenx;
	border->dimeny = param->dimeny;

	border->minx = (int*) malloc(param->dimeny * sizeof(int));
	border->maxx = (int*) malloc(param->dimeny * sizeof(int));
	for (i = 0; i < param->dimeny; ++i)
		border->minx[i] = border->maxx[i] = border->null;

	border->miny = (int*) malloc(param->dimenx * sizeof(int));
	border->maxy = (int*) malloc(param->dimenx * sizeof(int));
	for (i = 0; i < param->dimenx; ++i)
		border->miny[i] = border->maxy[i] = border->null;

	x0 = -0.5 - border->ipad;
	x1 = original->dimenx - 0.5 + border->ipad;
	y0 = -0.5 - border->ipad;
	y1 = original->dimeny - 0.5 + border->ipad;

	/* walk the bottom left to right */
	for (x = x0; x < x1; x += 1)
		updateBorder(border, param, x, y0);

	/* walk up the right edge */
	for (y = y0; y < y1; y += 1)
		updateBorder(border, param, x1, y);
	
	/* walk the top right to left */
	for (x = x1; x > x0; x -= 1)
		updateBorder(border, param, x, y1);

	/* walk down the left edge */
	for (y = y1; y > y0; y -= 1)
		updateBorder(border, param, x0, y);

	headas_chat(4, "established border\n");
}


static InterRecord *checkIfBetter (InterRecord *old, InterState *state)
{
	if (!old || state->probeDistance < hypot(old->x - state->x0, old->y - state->y0)) {
		++state->foundAny;
		++state->foundBetter;
		if (state->minDist2 < 1) {
			state->minDist2 = 1.5 + state->probeDistance;
			state->minDist2 *= state->minDist2;
		}
		if (state->maxDist2 < 1) {
			state->maxDist2 = state->scale + 1 + state->probeDistance;
			state->maxDist2 *= state->maxDist2;
		}
		return state->probe;
	}
	return old;
}


static double interpolateNeighbors (InterTable *table, int x0, int y0,
		PARAM *param)
{
	int done = 0;
	int x, y;
	double weightSum = 0, zSum = 0;
	InterRecord *nw = 0, *ne = 0, *se = 0, *sw = 0;
	int i;
	InterDelta *delta = 0;
	int deltaDist2;
	InterRecord *probe;

	InterState state = { 0 };

	state.x0 = x0;
	state.y0 = y0;
	state.scale = table->scale;

	i = -1;
	while (!done) {

		++i;

		if (i == table->nDeltas)
			break;

		delta = &table->delta[i];
		deltaDist2 = delta->dx * delta->dx + delta->dy * delta->dy;
		if (state.foundAny && deltaDist2 > state.maxDist2)
			break;

		if (state.foundAny && deltaDist2 > state.minDist2)
			if (nw && ne && se && sw) {
				done = 1;
				break;
			}

		x = x0 + delta->dx;
		y = y0 + delta->dy;

		if (x < 0 || x >= table->dimenx || y < 0 || y >= table->dimeny)
			continue;

		state.foundBetter = 0;

		probe = table->grid[y][x];
		while (probe) {
			state.probe = probe;
			state.probeDistance = hypot(probe->x - x0, probe->y - y0);
			if (probe->x <= x0) {
				if (probe->y <= y0)
					sw = checkIfBetter(sw, &state);
				else
					nw = checkIfBetter(nw, &state);
			}
			else {
				if (probe->y <= y0)
					se = checkIfBetter(se, &state);
				else
					ne = checkIfBetter(ne, &state);
			}
			probe = probe->next;
		}
	}

	{
		int count = 0;
		InterRecord *toAverage[4];
		if (nw) toAverage[count++] = nw;
		if (ne) toAverage[count++] = ne;
		if (se) toAverage[count++] = se;
		if (sw) toAverage[count++] = sw;

#define OUTSIDE_VALUE 0
		if (count == 0)
			return OUTSIDE_VALUE;

		for (i = 0; i < count; ++i) {
			double d;
			InterRecord *record = toAverage[i];

			if (isnan(record->z))
				return OUTSIDE_VALUE;

			d = hypot(record->x - x0, record->y - y0);

			if (d < param->interEpsilon)
				return record->z;
			else {
				double weight = pow(d, param->interAlpha);
				weightSum += weight;
				zSum += weight * record->z;
			}
		}
	}

	return zSum / weightSum;
}


/********************************************************************************
*
********************************************************************************/
void transform_by_interalpha(IMAGE* original, IMAGE* transformed, PARAM* param) {
	int i, j;
	int minx, maxx, miny, maxy;
	double value;

	InterBorder border = { 0 };
	InterTable table = { 0 };

	headas_chat(3, "initializing interpolation\n");

#define CHECK_BORDER 1
#if CHECK_BORDER
	initializeBorder(&border, original, param);
#endif

	table.dimenx = transformed->dimenx;
	table.dimeny = transformed->dimeny;
	initializeInterTable(&table, original, param);

	headas_chat(3, "interpolating each output pixel\n");

	/***************************
	* transform all the pixels *
	***************************/
	for (j = 0; j < transformed->dimeny; ++j) {

		headas_chat(5, "Transforming row %d\n", j);

#define CHECK_BORDER 1
#if CHECK_BORDER
		minx = border.minx[j];
		maxx = border.maxx[j];

		if (minx == border.null)
			continue;
#endif

		for (i = 0; i < transformed->dimenx; ++i) {

#if CHECK_BORDER
			miny = border.miny[i];
			maxy = border.maxy[i];

			if (miny == border.null)
				continue;

			if (i < minx || i > maxx || j < miny || j > maxy)
				continue;
#endif

			/************************************************
			* write the interpolated value to the new image *
			************************************************/
			value = interpolateNeighbors(&table, i, j, param);
			setImagePixel(transformed, i, j, value);

#if 0
			headas_chat(5, "setting %g %g to value=%g\n", i, j, value);
#endif
		}
	}

	destroyInterTable(&table);

} /* end of transform_by_forwardinterpolate function */

