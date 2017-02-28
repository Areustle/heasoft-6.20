#ifndef PARAM_H
#define PARAM_H

#include <stdio.h>
#include "coordfits.h"

#define FILENAME_DIMEN 128

typedef struct {

char filename[FILENAME_DIMEN];
TELDEF* teldef;

char outfile[FILENAME_DIMEN];
FILE* fp;

int frame_level; /* detector coords to align with page */
int label_raw; /* should the Raw coordinates be labeled? */


double bbox_x0;
double bbox_y0;
double bbox_x1;
double bbox_y1;

double width;
double margin;

XFORM2D* det2ps;

int max_grid_lines;

char font[FILENAME_DIMEN];
double font_leeway;
double arrow_length;

int draw_scx;
int draw_scy;
int draw_scz;

double sc_length;

double sc_originx;
double sc_originy;

double min_length;

int history;

} PARAM;


/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void);


/******************************************************************************
*******************************************************************************
* free all the memory associated with the PARAM structure.
* note this also closes the outfile 
******************************************************************************/
void destroyParam(PARAM* param);

#endif /* PARAM_H */
