#include "param.h"

/***************************************************************************
****************************************************************************
* write the PostScript header stuff
***************************************************************************/
void postscript_preamble(PARAM* param);

/***************************************************************************
****************************************************************************
* write stuff which goes at the end of the PostScript file
***************************************************************************/
void postscript_trailer(PARAM* param);

/***************************************************************************
****************************************************************************
* write the PostScript header stuff
***************************************************************************/
void draw_line(PARAM* param, double x0, double y0, double x1, double y1);

/***************************************************************************
****************************************************************************
* write a comment in the PostScript code
***************************************************************************/
void write_comment(PARAM* param, char* comment);

/***************************************************************************
****************************************************************************
* set a rectangular clipping path arounf the given bounding box.
***************************************************************************/
void begin_clipping(PARAM* param, double x0, double y0, double x1, double y1);

/***************************************************************************
****************************************************************************
* quit using a clipping path - this is really just a grestore - to it
* assumes gsave/grestore pairs are properly nested
***************************************************************************/
void end_clipping(PARAM* param);

/***************************************************************************
****************************************************************************
* label a set of axies
***************************************************************************/
void label_axes(PARAM* param, double x0, double y0, 
                               double x1, double y1, 
                               double x2, double y2, 
                 char* name_x, char* name_y, int inside );

/*************************************************************************
**************************************************************************
* place a label at given detector coordinates 
*************************************************************************/
void generic_label(PARAM* param, double x, double y, char* spacing, char* text);


/*************************************************************************
**************************************************************************
* add a title to the plot
*************************************************************************/
void add_title(PARAM* param, char* title);
