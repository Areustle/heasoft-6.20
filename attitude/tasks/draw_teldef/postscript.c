/****************************************************************************
* this file contains a number of routines for drawing in PostScript
****************************************************************************/
#include <math.h>
#include "postscript.h"

/***************************************************************************
****************************************************************************
* write the PostScript header stuff
***************************************************************************/
void postscript_preamble(PARAM* param) {

FILE* fp;

fp=param->fp;

fprintf(fp,"%%!PS-Adobe-3.0 EPSF-3.0\n");
fprintf(fp,"%%%%Title: %s from %s\n",param->outfile, param->filename);
fprintf(fp,"%%%%Creator: draw_teldef FTOOL\n");

fprintf(fp,"%%%%BoundingBox: %d %d %d %d\n",
        (int)(param->bbox_x0 +.5 ),
        (int)(param->bbox_y0 +.5 ),
        (int)(param->bbox_x1 +.5 ),
        (int)(param->bbox_y1 +.5 ) );

fprintf(fp,"%%%%Pages: 1\n");
fprintf(fp,"%%%%End Comments\n");
fprintf(fp,"%%%%End Prolog\n");
fprintf(fp,"%%%%Page: 1 1\n");

fprintf(fp,"/origstate save def\n");
fprintf(fp,"10 dict begin\n");

/*******************************************************
* procedure for showing a string on a white background *
*******************************************************/
fprintf(fp,"/showonwhite {\n");
fprintf(fp,"dup\n");
fprintf(fp,"gsave\n");
fprintf(fp,"true charpath flattenpath pathbbox\n");
fprintf(fp,"%g add 4 1 roll %g add 4 1 roll %g sub 4 1 roll %g sub 4 1 roll\n",
        param->font_leeway,param->font_leeway,
        param->font_leeway,param->font_leeway );
fprintf(fp,"exch 2 copy 5 4 roll 5 index 1 index\n");
fprintf(fp,"newpath moveto lineto  lineto lineto closepath\n");
fprintf(fp,"1 setgray fill\n");
fprintf(fp,"grestore\n");
fprintf(fp,"show } bind def\n");

/****************************************************************
* procedures for placing text with respect to the current point *
****************************************************************/
fprintf(fp,"/below {\n");
fprintf(fp,"dup gsave true charpath flattenpath pathbbox grestore\n");
fprintf(fp,"4 1 roll pop pop pop\n");
fprintf(fp,"currentpoint exch pop sub %g add currentlinewidth 2 mul add neg\n",
        param->font_leeway);
fprintf(fp,"%g currentlinewidth 2 mul add exch rmoveto } bind def\n",
        param->font_leeway);


fprintf(fp,"/ontop {\n");
fprintf(fp,"dup gsave true charpath flattenpath pathbbox grestore\n");
fprintf(fp,"4 1 roll pop pop pop\n");
fprintf(fp,"currentpoint exch pop sub neg\n");
fprintf(fp,"0 exch 2 div rmoveto } bind def\n");



fprintf(fp,"/above {\n");
fprintf(fp,"%g currentlinewidth 2 mul add\n",param->font_leeway);
fprintf(fp,"%g currentlinewidth 2 mul add rmoveto } bind def\n",
        param->font_leeway);

fprintf(fp,"/center {\n");
fprintf(fp,"dup stringwidth pop 2 div neg 0 rmoveto } bind def\n");


fprintf(fp,"/oheightspace {\n");
fprintf(fp,"gsave (O) true charpath flattenpath pathbbox grestore\n");
fprintf(fp,"exch pop exch sub exch pop %g add 0 rmoveto } bind def\n",
        param->font_leeway);

/**********************************************
* procedure to show text followed by an arrow *
**********************************************/
fprintf(fp,"/textandarrow {\n");
fprintf(fp,"exch dup showonwhite\n");
fprintf(fp,"gsave true charpath flattenpath pathbbox grestore\n");
fprintf(fp,"4 1 roll pop pop pop\n");
fprintf(fp,"currentpoint exch pop sub 2 div\n");
fprintf(fp,"dup 0 exch  rmoveto\n");
fprintf(fp,"exch 0 rlineto\n");
fprintf(fp,"dup neg 1 index rmoveto\n");
fprintf(fp,"dup 1 index neg rlineto\n");
fprintf(fp,"neg dup rlineto\n");
fprintf(fp,"stroke } bind def\n");


/***************
* set the font *
***************/
fprintf(fp,"/%s exch findfont exch scalefont setfont\n",param->font);

/*
fprintf(fp,"/fontsize exch def\n");

fprintf(fp,"currentfont /FontBBox get dup\n");
fprintf(fp,"3 get 1000 div fontsize mul %g add /fontup exch def\n",
        param->font_leeway);
fprintf(fp,"1 get 1000 div fontsize mul %g add /fontdn exch def\n",
        param->font_leeway);
*/



} /* end of postscript_preamble function */

/***************************************************************************
****************************************************************************
* write stuff which goes at the end of the PostScript file
***************************************************************************/
void postscript_trailer(PARAM* param) {

FILE* fp;

fp=param->fp;

fprintf(fp,"showpage\n");
fprintf(fp,"end\n");
fprintf(fp,"origstate restore\n");
fprintf(fp,"%%%%Trailer\n");

} /* end of postscript_trailer function */

/***************************************************************************
****************************************************************************
* write the PostScript header stuff
***************************************************************************/
void draw_line(PARAM* param, double x0, double y0, double x1, double y1) {

double psx0, psy0;
double psx1, psy1;


applyXform2dToContinuousCoords(param->det2ps,&psx0,&psy0,x0,y0);
applyXform2dToContinuousCoords(param->det2ps,&psx1,&psy1,x1,y1);

fprintf(param->fp,"%g %g moveto %g %g lineto stroke\n",psx0,psy0,psx1,psy1);

} /* end of draw_line function */

/***************************************************************************
****************************************************************************
* write a comment in the PostScript code
***************************************************************************/
void write_comment(PARAM* param, char* comment) {

fprintf(param->fp,"\n");
fprintf(param->fp,"%% %s\n",comment);

}

/***************************************************************************
****************************************************************************
* set a rectangular clipping path arounf the given bounding box.
***************************************************************************/
void begin_clipping(PARAM* param, double x0, double y0, double x1, double y1) {

double psx0, psy0;
double psx1, psy1;

applyXform2dToContinuousCoords(param->det2ps,&psx0,&psy0,x0,y0);
applyXform2dToContinuousCoords(param->det2ps,&psx1,&psy1,x1,y1);

fprintf(param->fp,"gsave %% begin clipping\n");
fprintf(param->fp,"%g %g moveto %g %g lineto %g %g lineto %g %g lineto\n",
        psx0,psy0, psx1,psy0, psx1,psy1, psx0,psy1);
fprintf(param->fp,"closepath clip\n");

} /* end of begin_clipping function */

/***************************************************************************
****************************************************************************
* quit using a clipping path - this is really just a grestore - to it
* assumes gsave/grestore pairs are properly nested
***************************************************************************/
void end_clipping(PARAM* param) {

fprintf(param->fp,"grestore %% end clipping\n");

} /* end of end_clipping function */


/***************************************************************************
****************************************************************************
* label a set of axies
***************************************************************************/
void label_axes(PARAM* param, double x0, double y0, 
                               double x1, double y1, 
                               double x2, double y2, 
                 char* name_x, char* name_y, int inside) {

double psx0, psy0;
double psx1, psy1;
double psx2, psy2;
double dx1, dx2, dy1, dy2;
double length1, length2;

int clockwise;
char *above[]= {"below", "above"};
char *space[]= {"", "oheightspace"};
int isAbove;

double arrow_length;

FILE* fp;

fp=param->fp;

/************************************
* convert to PostScript coordinates *
************************************/
applyXform2dToContinuousCoords(param->det2ps,&psx0,&psy0,x0,y0);
applyXform2dToContinuousCoords(param->det2ps,&psx1,&psy1,x1,y1);
applyXform2dToContinuousCoords(param->det2ps,&psx2,&psy2,x2,y2);

/******************************************
* right or left handed coordinate system? *
******************************************/
dx1=psx1-psx0;
dx2=psx2-psx0;
dy1=psy1-psy0;
dy2=psy2-psy0;
clockwise=( dx1*dy2-dx2*dy1 < 0 );

/**************************************************************
* set arrow shaft length to be a fraction of the smaller axis *
**************************************************************/
length1=sqrt(dx1*dx1+dy1*dy1);
length2=sqrt(dx2*dx2+dy2*dy2);

if(length2>length1) arrow_length=length1*param->arrow_length;
else                arrow_length=length2*param->arrow_length;

/***************
* X axis label *
***************/
fprintf(fp,"gsave\n");
fprintf(fp,"%g %g translate %g %g atan rotate\n", 
        psx0, psy0, psy1-psy0, psx1-psx0);

isAbove=((!clockwise &&  inside) || ( clockwise && !inside)   );
fprintf(fp,"0 0 moveto (%s) %s %s %g textandarrow\n",
        name_x, above[isAbove], space[inside], arrow_length );

fprintf(fp,"grestore\n");

/***************
* Y axis label *
***************/
fprintf(fp,"gsave\n");
fprintf(fp,"%g %g translate %g %g atan rotate\n", 
        psx0, psy0, psy2-psy0, psx2-psx0);

isAbove=!isAbove;
fprintf(fp,"0 0 moveto (%s) %s %s %g textandarrow\n",
        name_y, above[isAbove], space[inside], arrow_length );

fprintf(fp,"grestore\n");




} /* end of label_axies function */


/*************************************************************************
**************************************************************************
* place a label at given detector coordinates 
*************************************************************************/
void generic_label(PARAM* param, double x, double y, char* spacing,
                   char* text) {

double psx, psy;

applyXform2dToContinuousCoords(param->det2ps,&psx,&psy,x,y);

fprintf(param->fp,"%g %g moveto  (%s) %s showonwhite\n",
        psx, psy, text, spacing);

} /* end of  generic_label function */


/*************************************************************************
**************************************************************************
* add a title to the plot
*************************************************************************/
void add_title(PARAM* param, char* title) {

double psx, psy;


applyXform2dToContinuousCoords(param->det2ps,&psx,&psy,
                               param->teldef->det[param->frame_level]->center_x,
                               param->teldef->det[param->frame_level]->max_y);
psy=(psy+param->bbox_y1)*0.5;


fprintf(param->fp,"%g %g moveto  (%s) above center showonwhite\n",
        psx, psy, title);

} /* end of add_title function */
