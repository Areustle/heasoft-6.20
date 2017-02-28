/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotexpmap/instmap.c,v $
 * $Revision: 1.8 $
 * $Date: 2007/06/21 15:44:13 $
 *
 * $Log: instmap.c,v $
 * Revision 1.8  2007/06/21 15:44:13  rwiegand
 * Implemented mask radius parameter since corners of detector do not collect
 * counts.
 *
 * Revision 1.7  2007/05/03 15:50:27  rwiegand
 * Added MASK mode.
 *
 * Revision 1.6  2005/09/13 18:23:31  rwiegand
 * Yet another refinement to determining interior pixels.
 *
 * Revision 1.5  2005/07/18 12:53:44  rwiegand
 * Was using wrong pixel overlap when applying bad pixels.  Address floating
 * point precision when scaling exposure map.
 *
 * Revision 1.4  2005/07/13 20:09:59  rwiegand
 * Reimplemented update_interior.
 *
 * Revision 1.3  2005/07/01 18:39:18  rwiegand
 * Failure in bounds checking was leading to memory being stomped.
 *
 * Revision 1.2  2004/11/17 21:04:52  rwiegand
 * Rewrote and applied limits to min/max expressions when updating interior.
 *
 * Revision 1.1  2004/11/01 16:23:14  rwiegand
 * Reimplemented to only transform boundaries instead of every pixel.  Also
 * made output map flat.
 *
 * Revision 1.5  2004/10/29 22:25:08  wiegand
 * Output dimensions set by input.  Deal with case of input image being
 * transformed outside output.  Force more interior pixels to unity.
 *
 * Revision 1.4  2004/10/27 19:10:52  wiegand
 * Instrument map implementation.
 *
 */

/*
 *	start with a list of points in one frame
 *	convert to a list of points in a different frame
 *	find a point such that none is lower
 *		things below it must be OUTSIDE
 *	walk the converted list finding the bounding side and value
 *		[at CORNERS the bounding side changes]
 *	walk the converted list finding overlaps for the quadrilaterals
 *		bounded by the converted list and the bounding side
 *		and updating the minima/maxima for each row/column
 *	walk the region defined by the minima/maxima for each row/column
 *		adding 1 to the area.  this will make negative OUTSIDE
 *		areas zero or more and strictly INSIDE areas exactly 1.
 */



#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#include "instmap.h"
#include "overlap.h"
#include "genimage.h"
#include "report.h"
#include "uvotquality.h"


#define DEBUG_OVERLAP   0x100
#define DEBUG_INTERIOR  0x200


#define MIN2(x,y) (x<y?x:y)
#define MAX2(x,y) (x>y?x:y)



/* same calculation as librew/point_area2 */
static double edge_point_area2 (
		const EdgePoint * a, const EdgePoint * b, const EdgePoint * c)
{
	double area = ((c->x - b->x) * (a->y - b->y))
		- ((a->x - b->x) * (c->y - b->y));
	return area;
}


static void
get_edge_quad (Task * task, Quad * q, EdgePoint * from, EdgePoint * to)
{
	double extreme;

	switch (task->side) {

		case BOTTOM:
			extreme = ceil(MAX2(from->y, to->y)) + 0.5;
			q->a.x = from->x;
			q->a.y = extreme;
			q->b.x = from->x;
			q->b.y = from->y;
			q->c.x = to->x;
			q->c.y = to->y;
			q->d.x = to->x;
			q->d.y = extreme;
			task->sign = to->x > from-> x ? 1 : -1;
			break;

		case RIGHT:
			extreme = floor(MIN2(from->x, to->x)) - 0.5;
			q->a.x = extreme;
			q->a.y = from->y;
			q->b.x = from->x;
			q->b.y = from->y;
			q->c.x = to->x;
			q->c.y = to->y;
			q->d.x = extreme;
			q->d.y = to->y;
			task->sign = to->y > from-> y ? 1 : -1;
			break;

		case TOP:
			extreme = floor(MIN2(to->y, from->y)) - 0.5;
			q->a.x = to->x;
			q->a.y = extreme;
			q->b.x = from->x;
			q->b.y = extreme;
			q->c.x = from->x;
			q->c.y = from->y;
			q->d.x = to->x;
			q->d.y = to->y;
			task->sign = to->x < from-> x ? 1 : -1;
			break;

		case LEFT:
			extreme = ceil(MAX2(to->x, from->x)) + 0.5;
			q->a.x = to->x;
			q->a.y = to->y;
			q->b.x = extreme;
			q->b.y = to->y;
			q->c.x = extreme;
			q->c.y = from->y;
			q->d.x = from->x;
			q->d.y = from->y;
			task->sign = to->y < from-> y ? 1 : -1;
			break;
	}
}


static void
get_inverted_quad (Task * task, Quad * q, EdgePoint * from, EdgePoint * to)
{
	EdgePoint obverse;

	switch (task->side) {

		case BOTTOM:
			obverse.x = ceil(MAX2(from->x, to->x)) + 0.5;
			obverse.y = ceil(MAX2(to->y, to->next->y)) + 0.5;
			break;

		case RIGHT:
			obverse.x = floor(MIN2(to->x, to->next->x)) - 0.5;
			obverse.y = ceil(MAX2(from->y, to->y)) + 0.5;
			break;

		case TOP:
			obverse.x = floor(MIN2(from->x, to->x)) - 0.5;
			obverse.y = floor(MIN2(to->y, to->next->y)) - 0.5;
			break;

		case LEFT:
			obverse.x = ceil(MAX2(to->x, to->next->x)) + 0.5;
			obverse.y = floor(MIN2(from->y, to->y)) - 0.5;
			break;
	}

	q->a.x = to->x;
	q->a.y = to->y;
	q->b.x = obverse.x;
	q->b.y = to->y;
	q->c.x = obverse.x;
	q->c.y = obverse.y;
	q->d.x = to->x;
	q->d.y = obverse.y;

	task->sign = -1;
}



static void iterate_tool_overlap (int x, int y, OverlapState * state)
{
	int code;
	float z;
	Task * task = (Task *) state->user;
	FImage * map = task->map;
	if (debug_test(DEBUG_OVERLAP))
		report_debug("callback: x=%d y=%d area=%f sign=%d weight=%f\n",
				x, y, task->area, task->sign, state->weight);
	code = fimage_get(map, x, y, &z);
	if (code) {
		/* transformed outside of map */

		if (y < 0 || y >= map->height)
			; /* y out of bounds */
		else {
			if (x < task->xmin[y])
				task->xmin[y] = x;
			if (x > task->xmax[y])
				task->xmax[y] = x;
		}

		if (x < 0 || x >= map->width)
			; /* x out of bounds */
		else {
			if (y < task->ymin[x])
				task->ymin[x] = y;
			if (y > task->ymax[x])
				task->ymax[x] = y;
		}
	}
	else if (z != map->null) {
		float delta = (float) (task->sign * state->output);
		z += delta;
		if (debug_test(DEBUG_OVERLAP))
			report_debug("incrementing x=%d y=%d area by %f to %f\n",
							x, y, delta, z);
		code = fimage_set(map, x, y, z);
		if (code)
			report_warning("error updating map at x=%d y=%d to %f [%d]",
							x, y, z, code);
		else {
			/* update row/column limits */
			if (x < task->xmin[y])
				task->xmin[y] = x;
			if (x > task->xmax[y])
				task->xmax[y] = x;

			if (y < task->ymin[x])
				task->ymin[x] = y;
			if (y > task->ymax[x])
				task->ymax[x] = y;
		}
	}
}


static void apply_quad (Task * task, const Quad * q)
{
	task->area = fabs(quad_area(q));
	iterate_overlap(q, iterate_tool_overlap, task);
}


static void
update_interior_aux (Task * task, int y, int from, int limit)
{
	int x;
	float zdelta = -1;
	float zlast = 0;
	FImage * map = task->map;
	int * ymin = task->ymin;
	int * ymax = task->ymax;
	int delta = from < limit ? 1 : -1;
	int inside = 0;
	if (from < 0) {
		from = 0;
		zlast = 1;
	}
	else if (from >= map->width) {
		from = map->width - 1;
		zlast = 1;
	}

	for (x = from; x != limit; x += delta) {

		if (y > ymin[x] && y < ymax[x]) {

			float z, za, zb;

			if (inside) {
				fimage_set(map, x, y, 1);
				continue;
			}

			fimage_get(map, x, y, &z);

			if (zdelta < 0)
				zdelta = z;
			if (z <= zlast + zdelta / 2)
				z = 1;

			if (z < 1) {
				/* given pixels
				 * 	a
				 * 	  p
				 * 	b
				 * ---> delta
				 * p is inside if a>0 and b>0
				 */
				fimage_get(map, x - delta, y - 1, &za);
				fimage_get(map, x - delta, y + 1, &zb);
				if (za > 0 && zb > 0) {
					inside = 1;
					z = 1;
					if (debug_test(DEBUG_INTERIOR))
						report_debug("%d,%d is inside since za=%f, zb=%f\n",
									x, y, za, zb);
				}
			}

			fimage_set(map, x, y, z);

			zdelta = z - zlast;
			zlast = z;
			if (debug_test(DEBUG_INTERIOR))
				report_debug("updated interior point x=%d y=%d\n", x, y);
		}
	}
}


static int update_interior (Task * task)
{
	int y;
	FImage * map = task->map;

	for (y = 0; y < map->height; ++y) {
		int xmin, xmax, middle;
		xmin = task->xmin[y];
		xmax = task->xmax[y];
		if (xmin != POS_VOID && xmax > 0) {
			if (xmax > xmin + 1) {
				middle = xmin < 0 ? xmax / 2 : (xmin + xmax) / 2;
				update_interior_aux(task, y, xmin, middle + 1);
				update_interior_aux(task, y, xmax, middle);
			}
		}
	}

	return 0;
}


static void apply_transform (COMBOXFORM * xform, double * px, double * py,
			double x, double y)
{
#if 1

	applyComboXform(xform, px, py, x, y);

#elif 0

static int n = 0;
	++n;
	*px = x + 0.1 * cos(n);
	*py = y + 0.1 * sin(n);

#else

	*px = x;
	*py = y;

#endif

}


static void apply_transform_aux (Task * task, EdgePoint * p, double x, double y)
{
	apply_transform(task->xform, &p->x, &p->y, x, y);
}



static void iterate_quality_aux (int x, int y, OverlapState * state)
{
	Task * task = (Task *) state->user;
	FImage * map = task->map;
	float z;

	int code = fimage_get(map, x, y, &z);
	if (code)
		report_error("iterate_quality_aux: unable to get x=%d y=%d\n", x, y);

	else if (task->qualval == QUALITY_DEAD) {
		float zhat = z - state->output;
report_verbose("decrementing %d,%d exposure to %f for DEAD pixel\n", x, y, zhat);
		fimage_set(map, x, y, zhat);
	}

	else /* mark output pixels touched by this input as bad */
		fimage_set(map, x, y, map->null);
}


static int iterate_quality (SImage * image, IIState * state)
{
	int x = state->x;
	int y = state->y;
	Task * task = (Task *) state->user;

	int code = simage_get(image, x, y, &task->qualval);
	if (code)
		report_error("unable to get quality at x=%d y=%d\n", x, y);

	else if (task->qualval != QUALITY_GOOD) {
		/* update output pixels touched by this input pixel */
		Quad q;

		apply_transform(task->xform, &q.a.x, &q.a.y, x - 0.5, y - 0.5);
		apply_transform(task->xform, &q.b.x, &q.b.y, x + 0.5, y - 0.5);
		apply_transform(task->xform, &q.c.x, &q.c.y, x + 0.5, y + 0.5);
		apply_transform(task->xform, &q.d.x, &q.d.y, x - 0.5, y + 0.5);

		iterate_overlap(&q, iterate_quality_aux, task);
	}

	return 0;
}


static int update_quality (Task * task)
{
	int code = 0;

#if COMMENT
	If this task were updated to include iterating over an attitude,
	this could be optimized to collect the list of bad pixels first
	then just iterate over them instead of the entire quality map.
#endif

	IIState state = { 0 };
	state.user = task;

	simage_iterate(task->quality, iterate_quality, &state);

	return code;
}



static void build_oriented_boundary (Task * task)
{
	int i;
	task->size = 2 * (task->nx + task->ny);
	task->boundary = calloc(task->size, sizeof(EdgePoint));

	{
		int count = 0;
		double dx, dy;
		SImage * quality = task->quality;

		dx = ((double) quality->width) / task->nx;
		dy = ((double) quality->height) / task->ny;

		task->corner[0] = &task->boundary[count];
		for (i = 0; i < task->nx; ++i)
			apply_transform_aux(task,
					&task->boundary[count++],
					i * dx - 0.5, - 0.5);

		task->corner[1] = &task->boundary[count];
		for (i = 0; i < task->ny; ++i)
			apply_transform_aux(task,
					&task->boundary[count++],
					quality->width - 0.5, i * dy - 0.5);

		task->corner[2] = &task->boundary[count];
		for (i = 0; i < task->nx; ++i)
			apply_transform_aux(task,
					&task->boundary[count++],
					quality->width - i * dx - 0.5, quality->height - 0.5);

		task->corner[3] = &task->boundary[count];
		for (i = 0; i < task->ny; ++i)
			apply_transform_aux(task,
					&task->boundary[count++],
					- 0.5, quality->height - i * dy - 0.5);
	}

	{
		FImage * map = task->map;

		task->xmin = malloc(map->height * sizeof(int));
		task->xmax = malloc(map->height * sizeof(int));
		task->ymin = malloc(map->width * sizeof(int));
		task->ymax = malloc(map->width * sizeof(int));
		for (i = 0; i < map->height; ++i) {
			task->xmin[i] = POS_VOID;
			task->xmax[i] = -POS_VOID;
		}
		for (i = 0; i < map->width; ++i) {
			task->ymin[i] = POS_VOID;
			task->ymax[i] = -POS_VOID;
		}
	}

	{
		EdgePoint ** corner = task->corner;

		/* determine orientation */
		int ccw = edge_point_area2(corner[0], corner[1], corner[2]) > 0;

		/* link boundary */
		EdgePoint * last = task->boundary + task->size - 1;
		for (i = 0; i < task->size; ++i) {
			EdgePoint * pi = task->boundary + i;
			if (ccw)
				pi->prev = last, last->next = pi;
			else
				pi->next = last, last->prev = pi;
			last = pi;
		}
	}

	{
		/* set bound directions */
		double dx, dy;
		EdgePoint * c0 = task->corner[0];
		EdgePoint * c1 = task->corner[1];
		dx = c1->x - c0->x;
		dy = c1->y - c0->y;
		if (fabs(dx) > fabs(dy)) {
			if (dx > 0)
				c0->setside = BOTTOM;
			else
				c0->setside = TOP;
		}
		else {
			if (dx < 0)
				c0->setside = RIGHT;
			else
				c0->setside = LEFT;
		}
		for (i = 1; i < NSIDES; ++i)
			task->corner[i]->setside = 1 + ((c0->setside + i - 1) % NSIDES);
	}
}


static int iterate_scale_by_exposure (FImage * map, IIState * state)
{
	Task * task = (Task *) state->user;
	int x = state->x;
	int y = state->y;
	float z;
	int code = fimage_get(map, x, y, &z);
	if (code)
		report_error("iterate_scale_by_exposure: unable to fetch x=%d y=%d\n",
						x, y);

	else if (z != map->null) {
		/* floating point error is < 1e-6 */
		if (z < 1e-6)
			z = 0;
		fimage_set(map, x, y, (float) (z * task->exposure));

	}

	return 0;
}


static int
update_mask (Task * task)
{
	int x, y;
	int xmin, xmax, ymin, ymax;

	for (y = 0; y < task->mask->height; ++y) {
		xmin = MAX2(0, task->xmin[y] + task->dxmax);
		xmax = MIN2(task->mask->width-1, task->xmax[y] + task->dxmin);
		if (xmin <= xmax) {
			for (x = xmin; x <= xmax; ++x) {
				ymin = task->ymin[x] + task->dymax;
				ymax = task->ymax[x] + task->dymin;
				if (y >= ymin && y <= ymax)
					simage_set(task->mask, x, y, 1);
			}
		}
	}

	if (task->maskRadius > 0) {
		double dx, dy, pixsep;
		for (y = 0; y < task->mask->height; ++y) {
			for (x = 0; x < task->mask->width; ++x) {
				dx = x - task->crpix1d;
				dy = y - task->crpix2d;
				pixsep = sqrt(dx*dx + dy*dy);
				if (pixsep > task->maskRadius)
					simage_set(task->mask, x, y, 0);
			}
		}
	}

	return 0;
}



int solve (Task * task)
{
	int code = 0;
	EdgePoint * p;
	EdgePoint * limit;

	task->map->null = -1000;
			/* the null value has to be different than any
				achieved during the integration */

	build_oriented_boundary(task);

	task->side = SIDE_VOID;

	limit = task->corner[0];
	p = limit;
	do {
		Quad q;

		if (p->setside) {
			task->side = p->setside;
			get_inverted_quad(task, &q, p->prev, p);
			apply_quad(task, &q);
		}

		get_edge_quad(task, &q, p, p->next);
		apply_quad(task, &q);

		p = p->next;

	} while (p != limit);

	if (task->mask) {
		update_mask(task);
	}
	else {
		update_interior(task);

		update_quality(task);

		{
			IIState state = { 0 };
			state.user = task;
			fimage_iterate(task->map, iterate_scale_by_exposure, &state);
		}
	}

	return code;
}



