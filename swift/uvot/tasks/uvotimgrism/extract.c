#include <math.h>
#include <stdio.h>

#include "extract.h"
#include "genimage.h"
#include "overlap.h"



typedef struct
{
	DImage * source;
	double sum;
	double weight;

} Integrator;


static void
integrate_counts (int i, int j, OverlapState * state)
{
	double value;
	Integrator * o = (Integrator *) state->user;

	if (dimage_get(o->source, i, j, &value)) {
		/* off source */
		return;
	}
	else {
		double delta = state->weight * value;
		o->weight += state->weight;
		o->sum += delta;
	}
}


static int EXTRACTION_CHATTER = 0;


int extract_rectangle (Extraction * e)
{
	int code = 0;

	int i, j;
	double cosangle, sinangle;
	Integrator integrator;

	integrator.source = e->from;

	cosangle = cos(e->angle);
	sinangle = sin(e->angle);

if (EXTRACTION_CHATTER)
	printf("parameters\n\tx0 %f\n\ty0 %f\n"
		"\tangle %f\n\tcosangle %f\n\tsinangle %f\n",
		e->x0, e->y0, e->angle, cosangle, sinangle);

	for (i = 0; i < e->to->width; ++i) {

		for (j = 0; j < e->to->height; ++j) {

			double value;
			Quad quad;
			double xa;
			double ya;

			/* determine the coordinates of the corresponding
			 * box in the source image
			 */
			xa = e->x0 + i * cosangle - j * sinangle;
			ya = e->y0 + i * sinangle + j * cosangle;

if (EXTRACTION_CHATTER > 1)
	printf("extract_rectangle: i=%d, j=%d\n\txa=%.3f, ya=%.3f\n",
		i, j, xa, ya);

			quad.a.x = xa;
			quad.a.y = ya;

			quad.b.x = xa + cosangle;
			quad.b.y = ya + sinangle;

			quad.c.x = xa + cosangle - sinangle;
			quad.c.y = ya + sinangle + cosangle;

			quad.d.x = xa - sinangle;
			quad.d.y = ya + cosangle;

			integrator.sum = 0;
			integrator.weight = 0;
			iterate_overlap(&quad, integrate_counts, &integrator);

			if (integrator.weight < e->overlap)
				value = e->null;
			else
				value = integrator.sum;

			dimage_set(e->to, i, j, value);
		}
	}

	return code;
}

