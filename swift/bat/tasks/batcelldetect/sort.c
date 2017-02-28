#include <fitsio.h>
#include <math.h>
#include <string.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batcelldetect.h"

/* 
 * batcelldetect
 * Sorting I/O routines
 * 
 *
 * C. Markwardt 
 *
 * $Id: sort.c,v 1.2 2009/06/10 02:01:29 craigm Exp $
 */


/* Global variables for sorting */
#define MAX_SORT_FUNC 20
typedef int (*compare_func)(int *,int *);

/* Global data to be sorted */
struct source_struct *sort_sources = 0;

/* Global description of sort functions, to be applied in order */
compare_func sort_functions[MAX_SORT_FUNC] = {0};
int negate_sort_functions[MAX_SORT_FUNC] = {0};
int n_sort_functions = 0;

int sort_func(int *a, int *b)
{
  int i, status;
  for (i=0; i<n_sort_functions; i++) {
    if (sort_functions[i] == 0) continue;
    status = (*sort_functions[i])(a, b);
    if (status != 0) return (negate_sort_functions[i])?(-status):(+status);
  }
  return 0;
}

int sort_known(int *a, int *b)
{
  double x = sort_sources[*a].precat;
  double y = sort_sources[*b].precat;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_ra(int *a, int *b)
{
  double x = sort_sources[*a].ra_corr;
  double y = sort_sources[*b].ra_corr;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_dec(int *a, int *b)
{
  double x = sort_sources[*a].dec_corr;
  double y = sort_sources[*b].dec_corr;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_imx(int *a, int *b)
{
  double x = sort_sources[*a].imx_corr;
  double y = sort_sources[*b].imx_corr;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_imy(int *a, int *b)
{
  double x = sort_sources[*a].imy_corr;
  double y = sort_sources[*b].imy_corr;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_snr(int *a, int *b)
{
  double x = sort_sources[*a].snr;
  double y = sort_sources[*b].snr;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_pcode(int *a, int *b)
{
  double x = sort_sources[*a].pcode;
  double y = sort_sources[*b].pcode;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_catnum(int *a, int *b)
{
  double x = sort_sources[*a].sourceid;
  double y = sort_sources[*b].sourceid;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}


int sort_flux(int *a, int *b)
{
  double x = sort_sources[*a].bestflux;
  double y = sort_sources[*b].bestflux;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_theta(int *a, int *b)
{
  double x = sort_sources[*a].theta;
  double y = sort_sources[*b].theta;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}


int sort_phi(int *a, int *b)
{
  double x = sort_sources[*a].phi;
  double y = sort_sources[*b].phi;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}

int sort_snr_unknown(int *a, int *b)
{
  double x = sort_sources[*a].precat ? (-1) : sort_sources[*a].snr;
  double y = sort_sources[*b].precat ? (-1) : sort_sources[*b].snr;

  if (x < y) return (-1);
  else if (x > y) return (+1);
  else return 0;
}


int sort_name(int *a, int *b)
{
  return strcasecmp(sort_sources[*a].name, sort_sources[*b].name);
}

int parse_sort_expression(char *expr, 
			  struct source_struct *sources, int nsources,
			  int **result,
			  int *status)
{
  struct { char *name; compare_func f; } lookup[] = {
    {"KNOWN",     &sort_known},
    {"RA_OBJ",    &sort_ra},
    {"DEC_OBJ",   &sort_dec},
    {"RA",        &sort_ra},
    {"DEC",       &sort_dec},
    {"IMX",       &sort_imx},
    {"IMY",       &sort_imy},
    {"SNR",       &sort_snr},
    {"PCODEFR",   &sort_pcode},
    {"CATNUM",    &sort_catnum},
    {"SOURCE_ID", &sort_catnum},
    {"RATE",      &sort_flux},
    {"COUNTS",    &sort_flux},
    {"FLUX",      &sort_flux},
    {"THETA",     &sort_theta},
    {"PHI",       &sort_phi},
    {"SNR/UNKNOWN", &sort_snr_unknown},
    {"NAME",      &sort_name} };
  int nfunc = sizeof(lookup)/sizeof(lookup[0]);
  char **keys = 0;
  int i, j, k;
  int *p = 0;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return (*status);
  if (result == 0) return (*status = NULL_INPUT_PTR);

  /* Make default sort order */
  p = (int *) malloc(sizeof(int)*nsources);
  if (p == 0) {
    fprintf(stderr, "ERROR: could not allocate memory for sort operation\n");
    return (*status = MEMORY_ALLOCATION);
  }
  for (i=0; i<nsources; i++) p[i] = i;

  /* Separate the sort keys */
  n_sort_functions = 0;
  keys = expand_item_list(expr, &n_sort_functions, ',', 1, 1, 0, status);
  if (keys == 0 || *status != 0) {
    fprintf(stderr, "ERROR: could not parse 'sortcolumns' parameter\n");
    return (*status = COL_NOT_FOUND);
  }
  if (n_sort_functions == 0) {
    return (*status);
  }

  /* Loop through each sort key and parse it */
  for (i=0; i<n_sort_functions; i++) {
    if (i >= MAX_SORT_FUNC) {
      fprintf(stderr, 
	      "WARNING: too many sort keys were specified (max = %d).\n",
	      MAX_SORT_FUNC);
      break;
    }

    /* Default is ascending */
    sort_functions[i] = 0;
    negate_sort_functions[i] = 0;

    j = 0;
    if (keys[i][0] == '-') {
      negate_sort_functions[i] = 1;
      j ++;
    } else if (keys[i][0] == '+') {
      negate_sort_functions[i] = 0;
      j ++;
    }

    /* Find the sort key */
    for (k=0; k<nfunc;k++) {
      if (strcasecmp(&(keys[i][j]), lookup[k].name) == 0) {
	sort_functions[i] = lookup[k].f;
	break;
      }
    }
    if (k == nfunc) {
      fprintf(stderr, "WARNING: could not find sort key '%s' (ignoring)\n", &(keys[i][j]));
    }
  }

  /* No longer need this storage */
  free(keys);

  sort_sources = sources;
  qsort(p, nsources, sizeof(p[0]), &sort_func);

  (*result) = p;
  return *status;
}
