/************************************************************************

           File: PowGrid.c

       Function: PowDrawGridLines

      Tcl Usage: powDrawGridLines gn canvas useWCS xTickScal yTickScal \
                                  color xNumTicks yNumTicks dash doGrid

    Description: Locate appropriate tick positions on the indicated
                 graph and, if requested, draw a grid connecting them.
                 Originally written in TCL, the routines are so
                 computationally intense, especially when using
                 celestial coordinate system near the poles, that they
                 were converted to C to speed up the calculations.

         Author: Peter D. Wilson, 12/97-1/98

************************************************************************/
#include "pow.h"

      /*****  Additional structures/data types  *****/

typedef enum { top=0, right, bottom, left, none } SideVal;
char *SideStr[] = {"top", "rgt", "bot", "lft", "none"};

typedef struct {
   double x,y;
} Point;

typedef struct {
   Point   scrnPt;
   Point   imgPt;
   double  coeff[4];
   SideVal side;
} GridPt;

      /*****  Function Prototypes in PowGrid.c  *****/

int   PowDrawGridLines( ClientData clientData, Tcl_Interp *interp, 
			int argc, char *argv[] );
int   CreateGridPts( PowGraph *graph, int zoomed, const char *graphType, int xCount, int yCount, 
                     Point BotLeft_real, Point BotLeft, Point TopRgt, GridPt **rtnGrid );
int   GetTicks( int nGrid, GridPt Grid[], int useWCS, char *tickScal[2],
		int numTicks[2], double **ticks, char **axis );
int   GetTics( double a1, double a2, int nlabel, int maxlabels,
	       char *tickScal, double *list );
int   PtBtwnPts( Point pt, Point pt1, Point pt2, char fixed );
Point CalcXY( PowGraph *graph, Point pt, GridPt *G, int zoomed, const char *graphType, int xCount, int yCount );
Point SolveXY( double Val, char axis, GridPt *G);
void CalcCoeff( PowGraph *graph, GridPt *G1, GridPt *G2, GridPt *G3, int zoomed, const char *graphType, int xCount, int yCount );
void changeListOrder(double *list, int n);

int CanvToGraph( PowGraph *graph, Point Pt0, Point pt, Point *Pt, SideVal sd );
int GraphToCanv( PowGraph *graph, int zoomed, const char *graphType, int xCount, int yCount, 
                 Point Pt0, Point Pt, Point Pt0_real, Point *pt );

/*****************************************************************/

int PowDrawGridLines(ClientData clientData, Tcl_Interp *interp, 
		     int argc, char *argv[])
{
   char *gn, *canvas, *color, *dash;
   const char **list;
   char *tickScal[2];
   int useWCS, doGrid;
   Point bbox_ll, bbox_ur, BotLft_real, IntSects[16], GridSegs[100];
   Point BotLft, TopLft, TopRgt, BotRgt;
   Point Pt, pt;
   int nGrid, nPts, nSegs;
   GridPt *Grid;
   int nTicks, numTicks[2], NumPts;
   double *TickList;
   char *TickAxis, *result, cmdLine[1024], ignore;
   SideVal Sides[16];
   int i, j, k, flag, offmap, resultLen;
   PowGraph *graph;
   int len;
   char *idxStr;
   const char *graphType;
   int zoomed;
   int xCount, yCount;

   for (i=0; i < 16; i++) {
       Sides[i] = none;
   }

   if( argc != 10 ) {
      Tcl_SetResult(interp, "usage: powDrawGridLines gn canvas xTickScal yTickScal color xNumTicks yNumTicks dash doGrid", TCL_VOLATILE);
      return TCL_ERROR;
   }

   /********************************************************/
   /*  Convert parameter arguments to useable C variables  */
   /********************************************************/

   gn        =       argv[1];
   canvas    =       argv[2];
   tickScal[0] =     argv[3];
   tickScal[1] =     argv[4];
   color     =       argv[5];
   Tcl_GetInt(interp,argv[6],numTicks+0);
   Tcl_GetInt(interp,argv[7],numTicks+1);
   dash      =       argv[8];
   Tcl_GetBoolean(interp,argv[9],&doGrid);

   graph  = PowFindGraph( gn );
   if( graph==NULL ) {
      Tcl_SetResult(interp,"Couldn't find graph to adorn", TCL_VOLATILE);
      return TCL_ERROR;
   }
   useWCS = (graph->WCS.type[0]!='\0');

   /***********************************/
   /*  Get the bounding box of graph  */
   /***********************************/

   sprintf(cmdLine, "%s coords %sbox", canvas, gn );
   if( Tcl_Eval(interp,cmdLine)!=TCL_OK ) {
      Tcl_SetResult(interp,"Couldn't get bounding box", TCL_VOLATILE);
      return TCL_ERROR;
   }
   strncpy(cmdLine,Tcl_GetStringResult(interp),256);
   Tcl_SplitList(interp,cmdLine,&i,&list);

   len    = strlen(gn)+15;
   idxStr = (char *) ckalloc( len*sizeof(char) );
   sprintf(idxStr, "%s,%s", "graphType", gn);
   graphType = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
   ckfree(idxStr);

   len    = strlen(gn)+15;
   idxStr = (char *) ckalloc( len*sizeof(char) );
   sprintf(idxStr, "%s,%s", "zoomed", gn);
   zoomed = atoi(Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY));
   ckfree(idxStr);

   xCount = atoi(Tcl_GetVar2(interp,"xCount",gn,TCL_GLOBAL_ONLY));
   yCount = atoi(Tcl_GetVar2(interp,"yCount",gn,TCL_GLOBAL_ONLY));

   Tcl_GetDouble(interp,list[0],&(bbox_ll.x));
   Tcl_GetDouble(interp,list[1],&(bbox_ur.y));
   Tcl_GetDouble(interp,list[2],&(bbox_ur.x));
   Tcl_GetDouble(interp,list[3],&(bbox_ll.y));

   /* Chai 06/29/2007: 
      We are not actually fliping the coordinates on the canvas. If tk allows this, then there is
      no need to do the following. What the logic below is to trick pow to think that the point on 
      the canvas has been flipped. The xCount and yCount indicate if the graph has been flipped 
      before. So if X has been previously flipped, the next flipping occurs on Y, the logic inside
      ..Count % 2 will make sure the information on previous flip still exists. */

   /* Chai 07/19/2007:
      When reverse axis for a plot with WCS, the CDELT value was changed (i.e. * -1.0) in pow.tcl 
      powFlipImage routine and reset in powResetWcsStructure C code. That is enough to flip the axis so there is
      no need to recalculate the value. Just call routines in this file the normal way. */

   if ( (graph->WCS.type[0] == '\0' && zoomed == 0) && strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
      /* previous flip */
      Tcl_GetDouble(interp,list[0],&(bbox_ur.x));
      Tcl_GetDouble(interp,list[2],&(bbox_ll.x));
   }

   if ( (graph->WCS.type[0] == '\0' && zoomed == 0) && strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
      /* previous flip */
      Tcl_GetDouble(interp,list[1],&(bbox_ll.y));
      Tcl_GetDouble(interp,list[3],&(bbox_ur.y));
   }

   BotLft.x = bbox_ll.x;
   BotLft.y = bbox_ll.y;

   TopLft.x = bbox_ll.x;
   TopLft.y = bbox_ur.y;

   TopRgt.x = bbox_ur.x;
   TopRgt.y = bbox_ur.y;

   BotRgt.x = bbox_ur.x;
   BotRgt.y = bbox_ll.y;

   Tcl_GetDouble(interp,list[0],&(BotLft_real.x));
   Tcl_GetDouble(interp,list[3],&(BotLft_real.y));

   ckfree((char *) list);

   resultLen = 1024;
   result    = ckalloc(sizeof(char)*resultLen);
   result[0] = 0;

   /*************************************************/
   /*  Create an array of grid points around graph  */
   /*  and use it to obtain the list of tick marks  */
   /*************************************************/

   nGrid  = CreateGridPts(graph,zoomed, graphType,xCount,yCount,BotLft_real, BotLft,TopRgt,&Grid);
   nTicks = GetTicks(nGrid,Grid,useWCS,tickScal,numTicks,&TickList,&TickAxis);

   for( i=0; i<nTicks; i++ ) {

      /******************************************************/
      /*  Locate where each tick mark is located around     */
      /*  the graph's bounding box, as defined by the Grid  */
      /******************************************************/

      if( TickAxis[i]=='x' ) { Pt.x = TickList[i]; ignore='y';}
      else                   { Pt.y = TickList[i]; ignore='x';}
      
      for( nPts=0,j=0; j<nGrid-1; j++ ) {
	 if( Grid[j].side!=none && PtBtwnPts( Pt, Grid[j].imgPt, Grid[j+1].imgPt, ignore ) ) {
	    IntSects[nPts] = SolveXY( TickList[i], TickAxis[i], Grid+j);
	    Sides[nPts] = Grid[j].side;
	    nPts++;
         }
      }

      /****************************************************************/
      /*  Test the number of intersections found for the tick mark    */
      /*  and make corrections if necessary (e.g., pole is in graph)  */
      /****************************************************************/

      if( nPts<2 && !useWCS ) continue;
      if( nPts<2 ) {
	 if( TickAxis[i]=='y' ) {
	    Pt.x = 0.0;
	    GraphToCanv(graph,zoomed, graphType,xCount,yCount,BotLft,Pt,BotLft_real,&pt);
	    if( PtBtwnPts( pt, BotLft, TopRgt, 'n' ) ) {
	       IntSects[0].x = 0.0;
	       IntSects[0].y = TickList[i];
	       Sides[0] = none;
	       IntSects[1].x = 360.0;
	       IntSects[1].y = TickList[i];
	       Sides[1] = none;
	       nPts = 2;
	    }
	 } else if( TickAxis[i]=='x' && nPts==1 ) {
            Pt.y=90.0;
            if( GraphToCanv(graph,zoomed, graphType,xCount,yCount,BotLft,Pt,BotLft_real,&pt)!=TCL_OK )
               Pt.y=-90.0;
	    IntSects[1]=Pt;
	    Sides[1]=none;
	    nPts++;
	 }
      }
      
      /**********************************************************/
      /*  Follow grid line across graph.  Because with WCS,     */
      /*  right ascension is cyclic, make sure we are moving    */
      /*  in the right direction.  Reverse if necessary.        */
      /*  Intersections in and out of graph should be adjacent  */
      /**********************************************************/

      for( j=0,flag=0; j<nPts; j+=2 ) {
	 if( doGrid ) {
	    if( useWCS ) {
	       NumPts = 3+(int)(nPts/6) 
		  + (int)(fabs(IntSects[j+1].x-IntSects[j].x)/20.0);
	       for( k=0,nSegs=0; k<=NumPts; k++ ) {
		  Pt.x = k*IntSects[j+1].x + (NumPts-k)*IntSects[j].x;
		  Pt.x /= NumPts;
		  Pt.y = k*IntSects[j+1].y + (NumPts-k)*IntSects[j].y;
		  Pt.y /= NumPts;
		  offmap = GraphToCanv(graph,zoomed, graphType,xCount,yCount,BotLft,Pt,BotLft_real,&pt);
		  if( k>0 && k<NumPts ) {

		     /*******************************************/
                     /*  Test whether point is in graph's bbox  */
		     /*******************************************/

		     if( offmap || !PtBtwnPts(pt, BotLft, TopRgt, 'n') ) {
			if( IntSects[j].x<IntSects[j+1].x )
			   IntSects[j].x += 360.0;
			else IntSects[j].x -= 360.0;

			if( flag==0 ) {
			   /******************************/
			   /*  Redo grid line but going  */
			   /*  in opposite direction     */
			   /******************************/
			   flag = 1;
			   k = -1;
			   nSegs = 0;
			   NumPts = 3+(int)(nPts/6) 
			      + (int)(fabs(IntSects[j+1].x-IntSects[j].x)/20.0);
			   continue;
			} else if( flag==1 ) {
			   /********************************************/
			   /*  Reversing direction didn't work.  So,   */
			   /*  Redo grid line but now put first point  */
			   /*  at end of intersection list, phase      */ 
			   /*  shifting the pattern of in/out of ticks */
			   /********************************************/
			   flag = 2;
			   IntSects[nPts] = IntSects[0];
			   Sides[nPts] = Sides[0];
			   nPts++;
			   j--;
			   nSegs = 0;
			   break;
			} else {
			   /*************************************************/
			   /*  Nothing worked.  Something screwy going on.  */
			   /*  Ignore this grid line and hope for the best. */
			   /*************************************************/
			   nSegs = flag = 0;
			   break;
			   /*
			   Tcl_SetResult(interp, 
					 "Major flub drawing grid lines!?!\n",
					 TCL_VOLATILE);
			   return TCL_ERROR;
			   */
			}
		     }
		  }
		  GridSegs[nSegs] = pt;
		  nSegs++;
	       }
	    } else {
	       /*********************************************************/
	       /*  Linear coordinates, so just need one pair of points  */
	       /*********************************************************/
	       GraphToCanv(graph,zoomed, graphType,xCount,yCount,BotLft,IntSects[0],BotLft_real,&pt);
	       GridSegs[0] = pt;
	       GraphToCanv(graph,zoomed, graphType,xCount,yCount,BotLft,IntSects[1],BotLft_real,&pt);
	       GridSegs[1] = pt;
	       nSegs = 2;
	    }

	    /**********************************************************/
	    /*  Convert line points into TCL list and draw grid line  */
	    /**********************************************************/
	    if( nSegs>=2 ) {
	       char *line,*cmd;
	       int loc=0;

	       line = ckalloc(sizeof(char)*nSegs*60);
	       for( k=0; k<nSegs; k++ ) {
		  sprintf(line+loc,"%25.16g %25.16g ",
			  GridSegs[k].x, GridSegs[k].y );
		  loc += strlen(line+loc);
	       }
	       /*  Make sure we allocate more than enough space  */
	       cmd = ckalloc(sizeof(char)*( loc+200+2*strlen(gn) ));
	       sprintf(cmd,
		       "%s create line %s -fill %s -dash \"%s\" ",
		       canvas, line, color, dash);
	       sprintf(cmd+strlen(cmd),
		       "-tags \"%s %sline %sgrid\"",
		       gn, gn, gn );
	       if( Tcl_Eval(interp,cmd)!=TCL_OK ) {
		  Tcl_SetResult(interp,	"Couldn't draw grid line",
				TCL_VOLATILE);
		  return TCL_ERROR;
	       }
	       flag=0;
	       ckfree(line);
	       ckfree(cmd);
	    }
	 }
	 if( !flag ) {
	    /*****************************************************/
	    /*  Tick was good (ie, line did draw this loop), so  */
	    /*  append it to the result string to be sent back   */
	    /*  to the calling TCL routine.                      */
	    /*****************************************************/
	    sprintf(cmdLine,"%.16g %.16g %.16g %c %s ",
		    IntSects[j].x, IntSects[j].y, TickList[i], TickAxis[i],
		    SideStr[Sides[j]] );
	    sprintf(cmdLine+strlen(cmdLine),"%.16g %.16g %.16g %c %s ",
		    IntSects[j+1].x, IntSects[j+1].y, TickList[i], TickAxis[i],
		    SideStr[Sides[j+1]] );
	    k = strlen(cmdLine);
	    if( resultLen <= (int)(k+strlen(result)))
	       result = ckrealloc(result, sizeof(char)*(resultLen+=k+k));
	    strcat(result,cmdLine);
	 }
      }
   }
   
   ckfree( (char*)TickList);
   ckfree( (char*)TickAxis);
   ckfree( (char*)Grid);

   Tcl_SetResult(interp,result,TCL_DYNAMIC);
   return TCL_OK;
}

#define InitGridPt(Grid,xx,yy,sd,which)   (Grid).scrnPt.x=xx; \
                                          (Grid).scrnPt.y=yy; \
				          (Grid).side=sd;     \
                                          if (which == 'r') { \
				             CanvToGraph(graph,BotLft_real,(Grid).scrnPt,\
                                                         &((Grid).imgPt), sd); \
                                          } else { \
				             CanvToGraph(graph,BotLft,(Grid).scrnPt,\
                                                         &((Grid).imgPt), sd); \
                                          } \

int CreateGridPts(PowGraph *graph, int zoomed, const char *graphType, int xCount, int yCount, 
                  Point BotLft_real, Point BotLft, Point TopRgt, GridPt **rtnGrid)
{
   GridPt *Grid;
   int nPts,nGrds,*GridOrder,start;
   int i,j;
   char which;

   /******************************************************/
   /*  Initialize corners of Grid with the bounding box  */
   /******************************************************/

   nGrds     = 25;
   Grid      = (GridPt *)ckalloc( nGrds*sizeof(GridPt) );
   GridOrder = (int    *)ckalloc( nGrds*sizeof(int) );
 
   which = 'c';

/*
   if ( graph->WCS.type[0] != '\0' && strcmp(graphType, "binary") == 0 && (xCount % 2 != 0 || yCount % 2 != 0)) {
      which = 'r';
   }
*/

   InitGridPt(Grid[0],BotLft.x,BotLft.y,left,which);
   InitGridPt(Grid[1],BotLft.x,TopRgt.y,top,which);
   InitGridPt(Grid[2],TopRgt.x,TopRgt.y,right,which);
   InitGridPt(Grid[3],TopRgt.x,BotLft.y,bottom,which);
   InitGridPt(Grid[4],BotLft.x,BotLft.y,none,which);

   for (i=0;i<4;i++) {
      GridOrder[i]=i;
      CalcCoeff(graph, Grid+i,Grid+i+1,NULL,zoomed,graphType,xCount,yCount);
   }

   GridOrder[4] = 4;
   nPts  = 5;
   start = 0;

   /**************************************************************************/
   /*  No need to look for extra points around graph if using linear coords  */
   /**************************************************************************/

   if( graph->WCS.type[0] ) {
      Point midpt,testpt,testPt;
      GridPt *currGrid,*newlGrid,*newrGrid,*nextGrid;
      double diff,dist,bnds;
   
      for( i=0; i<nPts-1; i++ ) {
	 currGrid = Grid + GridOrder[i];
	 nextGrid = Grid + GridOrder[i+1];
	 newlGrid = Grid + nPts;
	 
	 /************************************************************/
	 /*  Test how accurate the Grid array is in calculating the  */
	 /*  location of the midpoint between two grid points        */
	 /************************************************************/

	 midpt.x = 0.5*( currGrid->scrnPt.x + nextGrid->scrnPt.x );
	 midpt.y = 0.5*( currGrid->scrnPt.y + nextGrid->scrnPt.y );

	 InitGridPt(*newlGrid,midpt.x,midpt.y,currGrid->side,which);
	 
/*
         PowPixToPos( midpt.x, midpt.y, &graph->WCS, &testPt.x, &testPt.y );
         PowPosToPix( testPt.x, testPt.y, &graph->WCS, &testpt.x, &testpt.y );
*/
         testPt = CalcXY( graph, midpt, currGrid, zoomed, graphType, xCount, yCount );
         GraphToCanv(graph, zoomed, graphType, xCount, yCount, BotLft,testPt,BotLft_real, &testpt);

	 diff = fabs(midpt.x-testpt.x) + fabs(midpt.y-testpt.y);
	 dist = fabs(currGrid->scrnPt.x-nextGrid->scrnPt.x)
              + fabs(currGrid->scrnPt.y-nextGrid->scrnPt.y);
	 
	 if( diff>4.0 && dist>4.0 ) {
           /* Chai: 07/20/2007: These codes will not be used anymore after WCS lib routines are installed. */
	    /*************************************************/
	    /*  Not too good, must add a new point to array  */
	    /*************************************************/
	    CalcCoeff(graph, currGrid,newlGrid,NULL,zoomed,graphType,xCount,yCount);
	    CalcCoeff(graph, newlGrid,nextGrid,NULL,zoomed,graphType,xCount,yCount);
	    
	    if( dist<=9.0 &&
		( fabs(currGrid->coeff[0])>40.0 
		  || fabs(newlGrid->coeff[0])>40.0 ) ) {

	       /***************************************************/
	       /*  Seems the problem is that we are crossing the  */
	       /*  0/360 position of right ascension.  Create an  */
               /*  extra point to eliminate discontinuity.        */
	       /***************************************************/

	       newrGrid = newlGrid+1;
	       if( fabs(currGrid->coeff[0])>40.0 ) {

                  bnds = (currGrid->imgPt.x < newlGrid->imgPt.x) ? 360.0 : 0.0;
                  newlGrid->imgPt = SolveXY( bnds, 'x', newlGrid);
                  GraphToCanv( graph, zoomed, graphType,xCount,yCount,BotLft, newlGrid->imgPt, BotLft_real,
                               &newlGrid->scrnPt );
                  *newrGrid = *newlGrid;
                  newlGrid->imgPt.x = 360.0 - bnds;

	       } else {

                  bnds = (newlGrid->imgPt.x > nextGrid->imgPt.x) ? 360.0 : 0.0;
                  newlGrid->imgPt = SolveXY( bnds, 'x', currGrid);
                  GraphToCanv( graph, zoomed, graphType,xCount,yCount,BotLft, newlGrid->imgPt, BotLft_real,
                               &newlGrid->scrnPt );
                  *newrGrid = *newlGrid;
                  newrGrid->imgPt.x = 360.0 - bnds;

	       }
               CalcCoeff(graph, currGrid,newlGrid,NULL,zoomed,graphType,xCount,yCount);
               CalcCoeff(graph, newrGrid,nextGrid,NULL,zoomed,graphType,xCount,yCount);
	       newlGrid->side = none;
	       for (j=nPts-1; j>i; j--)
		  GridOrder[j+2]=GridOrder[j];
	       GridOrder[i+1] = nPts;
	       GridOrder[i+2] = nPts+1;
	       nPts += 2;
	       start = (i+=2);
	    } else {
	       CalcCoeff(graph, currGrid,newlGrid,nextGrid,zoomed,graphType,xCount,yCount);
	       for (j=nPts-1; j>i; j--)
		  GridOrder[j+1]=GridOrder[j];
	       GridOrder[i+1]=nPts;
	       nPts++;
	       i--;
	    }
	    if( nPts+3>nGrds ) {
	       nGrds    += 25;
	       Grid      = (GridPt *)ckrealloc( (char *)Grid,
						nGrds*sizeof(GridPt) );
	       GridOrder = (int    *)ckrealloc( (char *)GridOrder,
						nGrds*sizeof(int) );
	    }
	 } else {
	    CalcCoeff(graph, currGrid,newlGrid,nextGrid,zoomed,graphType,xCount,yCount);
	 }
      }
   }
   
   /*************************************************************************/
   /*  Copy grid points in order to the return grid array.  Use the         */
   /*  0/360 discontinuity as the start/end of array if it was encountered  */
   /*************************************************************************/

   *rtnGrid = (GridPt *)ckalloc( nPts*sizeof(GridPt) );
   for( i=start,j=0; i<nPts; i++,j++ )
      (*rtnGrid)[j] = Grid[ GridOrder[i] ];
   if( start )
      for( i=0,j--,nPts--; i<start; i++,j++ )
	 (*rtnGrid)[j] = Grid[ GridOrder[i] ];

   ckfree( (char *)Grid );
   ckfree( (char *)GridOrder );
   return nPts;
}

int GetTicks(int nGrid, GridPt Grid[], int useWCS, char *tickScal[2],
	     int numTicks[2], double **ticks, char **axis)
{
   double minX,minY,maxX,maxY;
   double xlist[100],ylist[100];
   int i,nx,ny,n,nBreaks=0;

   /*************************************************/
   /*  Locate the min and max values of each graph  */
   /*  axis, including the poles if necessary       */
   /*************************************************/

   minX=maxX=Grid[0].imgPt.x;
   minY=maxY=Grid[0].imgPt.y;
   for (i=1;i<nGrid;i++) {
      if(      Grid[i].imgPt.x < minX ) minX = Grid[i].imgPt.x;
      else if( Grid[i].imgPt.x > maxX ) maxX = Grid[i].imgPt.x;
      if(      Grid[i].imgPt.y < minY ) minY = Grid[i].imgPt.y;
      else if( Grid[i].imgPt.y > maxY ) maxY = Grid[i].imgPt.y;
      /*  Note that the 0-element will never be a discontinuity  */
      if( useWCS
          && ( Grid[i].imgPt.x<0.000001 || Grid[i].imgPt.x>359.999999 )
          && Grid[i].side==none ) nBreaks++;
   }

   if( nBreaks % 2 ) {
      /*  Crossed 0/360 boundary odd times, so must have a pole present  */
      if( (maxY+minY)>1.0 ) maxY=90.0;
      else if( (maxY+minY)<-1.0 ) minY=-90.0;
      nBreaks = 0;
   } else if( nBreaks ) {
      /*  Crossed 0/360 boundary even times, so must find -180,180 min/max  */
      double val;

      minX = maxX = 0.0;
      for( i=0; i<nGrid; i++ ) {
         val = Grid[i].imgPt.x;
         if( val>180.0 ) val -= 360.0;
         if(      val < minX ) minX = val;
         else if( val > maxX ) maxX = val;
      }
   }

   /**************************************************************************/
   /*  Get the values of the tick marks and copy them to the returned array  */
   /**************************************************************************/

   nx = GetTics( minX, maxX, numTicks[0], 100, tickScal[0], xlist );
   ny = GetTics( minY, maxY, numTicks[1], 100, tickScal[1], ylist );

   n = nx+ny;
   *ticks = (double *)ckalloc(sizeof(double)*n);
   *axis = (char *)ckalloc(sizeof(char)*n);

   for(i=0;i<nx;i++) {
      if( nBreaks && xlist[i]<0.0 )
         (*ticks)[i] = xlist[i]+360.0;
      else
         (*ticks)[i] = xlist[i];
      (*axis)[i]='x';
   }

   for(;i<n;i++) {
      (*ticks)[i] = ylist[i-nx];
      (*axis)[i]='y';
   }
   return n;
}

int PowGetTics(ClientData clientData, Tcl_Interp *interp, 
	       int argc, char *argv[])
{
   double min, max, ticks[100];
   int nlabels, nFnd, i, loc;
   char *list, *tickScal;

   if( argc != 5 ) {
      Tcl_SetResult(interp, "usage: powGetTics min max nlabels tickScal", TCL_VOLATILE);
      return TCL_ERROR;
   }

   /********************************************************/
   /*  Convert parameter arguments to useable C variables  */
   /********************************************************/

   Tcl_GetDouble(interp,argv[1],&min);
   Tcl_GetDouble(interp,argv[2],&max);
   Tcl_GetInt   (interp,argv[3],&nlabels);
   tickScal = argv[4];

   nFnd = GetTics( min, max, nlabels, 100, tickScal, ticks );

   list = (char *)ckalloc( 20*nFnd * sizeof(char) );
   if( !list ) {
      Tcl_SetResult(interp, "Memory allocation failure in powGetTics", TCL_VOLATILE);
      return TCL_ERROR;
   }

   loc = 0;
   for( i=0; i<nFnd; i++ ) {
      sprintf(list+loc, "%.16g ", ticks[i] );
      loc += strlen(list+loc);
   }
   Tcl_SetResult(interp, list, TCL_DYNAMIC);
   return(TCL_OK);
}

int GetTics( double a1, double a2, int nlabel, int maxlabels,
	     char *tickScal, double *list )
{
   /* shamelessly ripped from Mongo (and modified for base 60) */

   int num, iexp, off, n;
   double adiff, diff, amant, step, value;

   if( nlabel==0 ) return 0;

   n=0;

   if( a1 == a2 ) {
      list[0] = a1;
      return 1;
   }

   /* order is irrelevant, so make a1 the min value */
   if( a1>a2 ) {
      value = a1;
      a1 = a2;
      a2 = value;
   }
   adiff = a2 - a1;
   a1 -= 1e-6 * adiff;
   a2 += 1e-6 * adiff;

   /*  Identify the scaling method  */

   if(        !strcmp("ra",tickScal) ) {

      diff  = log10( (adiff/15.0)/nlabel ) / log10(60.0);
      iexp  = (int)floor(diff);
      amant = diff - iexp;
      if( iexp<-2 ) {
         /*  This is the sub-second level... resort to base 10/3600 steps  */

         diff  = log10( (adiff*3600.0/15.0)/nlabel );
         iexp  = (int)floor(diff);
         amant = diff - iexp;
         if     ( amant < .15 ) num =  1;
         else if( amant < .50 ) num =  2;
         else if( amant < .85 ) num =  5;
         else                   num = 10;
         step = num * pow(10.0, (double)iexp) * 15.0 / 3600.0;

      } else if( iexp<0 ) {
	 if     ( amant < .10 ) num =  1;
	 else if( amant < .21 ) num =  2;
	 else if( amant < .30 ) num =  3;
	 else if( amant < .36 ) num =  4;
	 else if( amant < .43 ) num =  5;
	 else if( amant < .46 ) num =  6;
	 else if( amant < .60 ) num = 10;
	 else if( amant < .70 ) num = 15;
	 else if( amant < .79 ) num = 20;
	 else if( amant < .92 ) num = 30;
	 else                   num = 60;
         step = num * pow(60.0, (double)iexp) * 15.0;
      } else {
	 if     ( amant < .10 ) num =  1;
	 else if( amant < .20 ) num =  2;
	 else if( amant < .30 ) num =  3;
	 else if( amant < .40 ) num =  4;
	 else if( amant < .46 ) num =  6;
	 else if( amant < .55 ) num =  8;
	 else                   num = 12;
         step = num * pow(60.0, (double)iexp) * 15.0;
      }

   } else if( !strcmp("dec",tickScal) ) {

      diff  = log10( adiff/nlabel ) / log10(60.0);
      iexp  = (int)floor(diff);
      amant = diff - iexp;
      if( iexp<-2 ) {
         /*  This is the sub-second level... resort to base 10/3600 steps  */

         diff  = log10( (adiff*3600.0)/nlabel );
         iexp  = (int)floor(diff);
         amant = diff - iexp;
         if     ( amant < .15 ) num =  1;
         else if( amant < .50 ) num =  2;
         else if( amant < .85 ) num =  5;
         else                   num = 10;
         step = num * pow(10.0, (double)iexp) / 3600.0;
      } else {
         if     ( amant < .10 ) num =  1;
         else if( amant < .21 ) num =  2;
         else if( amant < .30 ) num =  3;
         else if( amant < .36 ) num =  4;
         else if( amant < .43 ) num =  5;
         else if( amant < .46 ) num =  6;
         else if( amant < .60 ) num = 10;
         else if( amant < .70 ) num = 15;
         else if( amant < .79 ) num = 20;
         else if( amant < .92 ) num = 30;
         else                   num = 60;
         step = num * pow(60.0, (double)iexp);
      }

   } else if( !strcmp("log",tickScal) ) {
      static int logTicks[][10] = {
	 { 1, 10 },
	 { 1, 3, 10 },
	 { 1, 2, 5, 10 },
	 { 1, 2, 4, 6, 10 },
	 { 1, 2, 4, 6, 8, 10 },
	 { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }
      };
      double logDiff, base;
      int i, idx;

      if( fabs(a1)>300 || fabs(a2)>300 ) {
	 return 0;
      }
      base    = pow(10.0, floor(a1) );
      a1      = pow(10.0,a1);
      a2      = pow(10.0,a2);
      logDiff = adiff / nlabel;

      if( logDiff < 0.15 ) {

	 value = a1;
	 do {
	    diff  = log10( value*pow(10.0, logDiff) - value );
	    iexp  = (int)floor(diff);
	    amant = diff - iexp;
	    if     ( amant < .10 ) num =  1;
	    else if( amant < .45 ) num =  2;
	    else if( amant < .80 ) num =  5;
	    else   { iexp++;       num =  1; }
	    
	    base = pow(10.0, (double)iexp);
	    step = num * base;
	    
	    off  = (int)floor( value / step ) + 1;
	    do {
	       value = step * off++;
	       if( value>=a1 && value<=a2 ) {
		  list[n++] = log10(value);
	       }
	       if( ((int)(value/base))%10 == 0 ) break;
	    } while( value<=a2 && n<maxlabels );
	    
	    if( value>a2 ) return n;
	    
	 } while(1);
      }

      if     ( logDiff < 0.19 ) idx = 5;  /*  1, 2, 3, 4, 5, ...  */
      else if( logDiff < 0.24 ) idx = 4;  /*  1, 2, 4, 6, 8       */
      else if( logDiff < 0.30 ) idx = 3;  /*  1, 2, 4, 6          */
      else if( logDiff < 0.45 ) idx = 2;  /*  1, 2, 5             */
      else if( logDiff < 0.75 ) idx = 1;  /*  1, 3                */
      else                      idx = 0;  /*  1                   */
      if( logDiff > 1.8 )
	 step = pow(10.0, floor(logDiff+0.2) );
      else
	 step = 10.0;
			       
      i = 0;
      do {
	 do {
	    value = logTicks[idx][i] * base;
	    if( value>=a1 && value <=a2 ) {
	       list[n++] = log10(value);
	    }
	 } while( logTicks[idx][i++]<10 );
	 base *= step;
	 i = 1;
      } while( value<a2 );

      return n;
   
   } else {  /*  Default to linear  */

      diff  = log10(adiff / nlabel);
      iexp  = (int)floor(diff);
      amant = diff - iexp;
      if     ( amant < .15 ) num =  1;
      else if( amant < .50 ) num =  2;
      else if( amant < .85 ) num =  5;
      else                   num = 10;
      step = num * pow(10.0, (double)iexp);

   }

   off = (int)floor(a1 / step);
   value = step * (off+1);
    
   do {
      /*  Handle near-zero case  */
      if( value != 0.0 && fabs(step/value)>1000.0 ) value = 0.0;

      list[n++] = value;
      value += step;
   } while( value<=a2 && n<maxlabels );

   return n;
}

void changeListOrder(double *list, int n) 
{
     double tmpList[100];
     int i, j;

     j = n - 1;
     for (i = 0; i < n; i++) {
         tmpList[j--] = list[i];  
     }

     for (i = 0; i < n; i++) {
         list[i] = tmpList[i];       
     }
     return;
}

int CanvToGraph( PowGraph *graph, Point Pt0, Point pt, Point *Pt, SideVal sd )
{
   int errFlag;
   int len;
   char *idxStr;
   const char *graphType;
   int zoomed;
   int xCount, yCount;

   len    = strlen(graph->graph_name)+15;
   idxStr = (char *) ckalloc( len*sizeof(char) );
   sprintf(idxStr, "%s,%s", "graphType", graph->graph_name);
   graphType = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
   ckfree(idxStr);

   len    = strlen(graph->graph_name)+15;
   idxStr = (char *) ckalloc( len*sizeof(char) );
   sprintf(idxStr, "%s,%s", "zoomed", graph->graph_name);
   zoomed = atoi(Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY));
   ckfree(idxStr);

   xCount = atoi(Tcl_GetVar2(interp,"xCount",graph->graph_name,TCL_GLOBAL_ONLY));
   yCount = atoi(Tcl_GetVar2(interp,"yCount",graph->graph_name,TCL_GLOBAL_ONLY));

   /* when zoom, the flipping of axis is already done. So right X at zoomed = 0 will now be at left X */
   if ( graph->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
      if ( zoomed == 0 ) {
         pt.x = (pt.x - Pt0.x) / graph->xmagstep;
      } else {
         pt.x = (Pt0.x - pt.x) / graph->xmagstep;
      }
   } else {
      pt.x = (pt.x - Pt0.x) / graph->xmagstep;
   }

   if ( graph->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
      if ( zoomed == 0 ) {
         pt.y = (Pt0.y - pt.y) / graph->ymagstep;
      } else {
         pt.y = (pt.y - Pt0.y) / graph->ymagstep;
      }
   } else {
      pt.y = (Pt0.y - pt.y) / graph->ymagstep;
   }
   
   errFlag = PowPixToPos( pt.x, pt.y, &graph->WCS, &Pt->x, &Pt->y );
   return(errFlag);
}

int GraphToCanv( PowGraph *graph, int zoomed, const char *graphType, int xCount, int yCount, 
                 Point Pt0, Point Pt, Point Pt0_real, Point *pt )
{
   int errFlag;

   errFlag = PowPosToPix( Pt.x, Pt.y, &graph->WCS, &pt->x, &pt->y );

/* WCS Flag ? */
   if ( graph->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
      if ( zoomed == 0 ) {
         pt->x = Pt0.x + pt->x * graph->xmagstep;
      } else {
         pt->x = Pt0.x + (-1.0 * pt->x) * graph->xmagstep;
      }
   } else {
      pt->x = Pt0.x + pt->x * graph->xmagstep;
   }

   if ( graph->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
      if ( zoomed == 0 ) {
         pt->y = Pt0.y - pt->y * graph->ymagstep;
      } else {
         pt->y = Pt0.y - (-1.0 * pt->y) * graph->ymagstep;
      }
   } else {
      pt->y = Pt0.y - pt->y * graph->ymagstep;
   }
   return(errFlag);
}

int PtBtwnPts(Point pt, Point pt1, Point pt2, char fixed)
{
   double x, x1, x2, y, y1, y2;

   if( fixed!='x' && fixed!='l' && fixed!='r' ) {
      x  = pt.x;
      x1 = pt1.x;
      x2 = pt2.x;
      if( x1<x2 ) {
	 if( x<x1 || x>=x2 ) return 0;
      } else {
	 if( x<x2 || x>=x1 ) return 0;
      }
   }

   if( fixed!='y' && fixed!='t' && fixed!='b' ) {
      y  = pt.y;
      y1 = pt1.y;
      y2 = pt2.y;
      if( y1<y2 ) {
	 if( y<y1 || y>=y2 ) return 0;
      } else {
	 if( y<y2 || y>=y1 ) return 0;
      }
   }

   return 1;
}

Point CalcXY ( PowGraph *graph, Point pt, GridPt *G, int zoomed, const char *graphType, int xCount, int yCount )
{

   /* Chai: 07/19/2007: This function is no longer used. */
   Point XY;
   double ds;
   int errFlag = 0;

   /*********************************************************************/
   /*  Calculate the graph cordinates of pt using the info in GridPt G  */
   /*********************************************************************/

   errFlag = PowPixToPos( pt.x, pt.y, &graph->WCS, &XY.x, &XY.y );

   if( G->side==left || G->side==right ) {
      if ( strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
         ds = pt.y - G->scrnPt.y;
      } else {
         ds = G->scrnPt.y - pt.y;
      }
   } else {
      if ( graph->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
         ds = G->scrnPt.x - pt.x;
      } else {
         ds = pt.x - G->scrnPt.x;
      }
   }
   
   XY.x = G->imgPt.x + G->coeff[0]*ds + G->coeff[2]*ds*ds;
   XY.y = G->imgPt.y + G->coeff[1]*ds + G->coeff[3]*ds*ds;
   
   return XY;
}

Point SolveXY ( double Val, char axis, GridPt *G)
{
   double a,b,c,ds,ds1,ds2,quad;
   Point XY;

   /*************************************************************/
   /*  Calculate the full graph coordinates of where the given  */
   /*  tick value, Val, intersects the graph's bounding box.    */
   /*************************************************************/

   if( axis=='x' ) {
      a = G->coeff[2];
      b = G->coeff[0];
      c = G->imgPt.x-Val;
   } else {
      a = G->coeff[3];
      b = G->coeff[1];
      c = G->imgPt.y-Val;
   }

   if( a==0.0 ) ds = -c/b;
   else {
      quad = sqrt(b*b-4.0*a*c);
      ds1  = (-b-quad)/(2.0*a);
      ds2  = (-b+quad)/(2.0*a);
      if( ds1>ds2 ) { ds = ds1; ds1 = ds2; ds2 = ds; }
      
      if( G->side==top || G->side==right ) {
	 if( ds1<0.0 ) ds = ds2;
	 else          ds = ds1;
      } else {
	 if( ds2>0.0 ) ds = ds1;
	 else          ds = ds2;
      }
   }

   if( axis=='x' ) {
      XY.x = Val;
      XY.y = G->imgPt.y + G->coeff[1]*ds + G->coeff[3]*ds*ds;
   } else {
      XY.x = G->imgPt.x + G->coeff[0]*ds + G->coeff[2]*ds*ds;
      XY.y = Val;
   }

   return XY;
}

void CalcCoeff( PowGraph *graph, GridPt *G1, GridPt *G2, GridPt *G3, int zoomed, const char *graphType, int xCount, int yCount )
{
   double dX,dY,ds;
   double dX2,dY2;

   /********************************************************************/
   /*  Calculate the linear or 2nd-order polynomial coefficients which */
   /*  fit the dependency of graph coordinates on canvas coordinates   */
   /********************************************************************/

   if( G1->side==left || G1->side==right ) {
      ds = G2->scrnPt.y - G1->scrnPt.y;
   } else {
      ds = G2->scrnPt.x - G1->scrnPt.x;
   }

   dX = G2->imgPt.x - G1->imgPt.x;
   dY = G2->imgPt.y - G1->imgPt.y;
   
   if( G3==NULL ) {
      G1->coeff[0] = dX/ds;
      G1->coeff[1] = dY/ds;
      G1->coeff[2] = G1->coeff[3] = 0.0;
   } else {
      dX2 = G3->imgPt.x - G1->imgPt.x;
      dY2 = G3->imgPt.y - G1->imgPt.y;
      G1->coeff[0] = (4.0*dX-dX2)/(ds+ds);
      G1->coeff[1] = (4.0*dY-dY2)/(ds+ds);
      G1->coeff[2] = (dX2-2.0*dX)/(2.0*ds*ds);
      G1->coeff[3] = (dY2-2.0*dY)/(2.0*ds*ds);
    }
}


/***********************************************************************/

#define MAXCONTOURS 50

typedef struct {
   int xdim, ydim;
   double **rows;
   char *usedGrid;
   long nPts, nAlloc;
   double *X, *Y;
} Contours;   

int BuildContours( int nCntrs, double *levels,
		   int xdim, int ydim, double *image,
		   int *nPts, double **X, double **Y );
int TraceContour ( Contours *Info, double cntr,
		   int xCell, int yCell, SideVal side);


int PowCreateContour(ClientData clientData, Tcl_Interp *interp, 
		     int argc, char *argv[])
{
   char *contour, *image;
   double *imgData, levels[MAXCONTOURS], *lvlPtr;
   double xfrac, yfrac;
   double *X, *Y;
   int nPts;
   int nContours, xdim, ydim, res, status=TCL_OK;
   int i, j, xbnds, ybnds;
   long nelem, elem;
   PowImage *img;
   const char **list;

   if( argc != 5 ) {
      Tcl_SetResult(interp, "usage: powCreateContour contour image levels res", TCL_VOLATILE);
      return TCL_ERROR;
   }

   /********************************************************/
   /*  Convert parameter arguments to useable C variables  */
   /********************************************************/

   contour      =       argv[1];
   image        =       argv[2];
   Tcl_GetInt   (interp,argv[4],&res);
   if( res<1 ) res = 1;

   if( Tcl_SplitList(interp, argv[3], &nContours, &list)!=TCL_OK) {
      Tcl_SetResult(interp, "Contour levels not a valid list", TCL_VOLATILE);
      return TCL_ERROR;
   }
   if( nContours > MAXCONTOURS ) {
      Tcl_SetResult(interp, "Too many levels selected", TCL_VOLATILE);
      ckfree( (char*)list );
      return TCL_ERROR;
   }

   lvlPtr = levels;
   for( i=0; i<nContours; i++ ) {
      if( Tcl_GetDouble(interp,list[i],lvlPtr) == TCL_OK )
	 lvlPtr++;
      else
	 printf("Couldn't interpret contour line #%d. Skipping.\n",i+1);
   }
   ckfree( (char*)list );
   nContours = lvlPtr - levels;

   img = PowFindImage( image );
   if( img==NULL ) {
      Tcl_SetResult(interp, "Could not find requested image", TCL_VOLATILE);
      return TCL_ERROR;
   }

   xdim = (img->width  + res - 1) / res;
   ydim = (img->height + res - 1) / res;
   nelem = xdim * ydim;
   imgData = (double*)ckalloc( nelem*sizeof(double) );
   if( !imgData ) {
      Tcl_SetResult(interp, "Could not allocate memory for image",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }


   /*  Rescale image to desired contour resolution  */

   for( elem=0; elem<nelem; elem++ ) imgData[elem] = 0.0;
   xbnds = img->width  - img->width%res;
   ybnds = img->height - img->height%res;
   yfrac = 1.0/res;
   for( j=0; j<img->height; j++ ) {

      if( j == ybnds )
	 yfrac = 1.0/(img->height - ybnds);

      xfrac = 1.0/res;
      for( i=0; i<img->width; i++ ) {

	 if( i == xbnds )
	    xfrac = 1.0/(img->width - xbnds);

	 imgData[(j/res)*xdim+(i/res)] += xfrac * yfrac *
	    PowExtractDatum( img->dataptr, j*(img->width)+i );
      }

   }

   status = BuildContours( nContours, levels, xdim, ydim, imgData,
			   &nPts, &X, &Y);

   if( !status ) {

      /*  Must use pointers to pass data to PowCreateXXX...  ICK!!!  */

      int datatype = DOUBLE_DATA;
      int length = nPts;
      int copy = 1;
      int offset = 0;
      int sLen;
      char str1[256], str2[256];

      for( elem = 0; elem<nPts; elem++ ) {
	 if( X[elem]==DBL_MAX ) continue;
	 X[elem] = X[elem]*res + 0.5*(res-1) + 1;
	 Y[elem] = Y[elem]*res + 0.5*(res-1) + 1;
         if( img->WCS.type[0]=='\0' ) {
            X[elem] = (X[elem] - 0.5) * img->xinc + img->xorigin;
            Y[elem] = (Y[elem] - 0.5) * img->yinc + img->yorigin;
         }
      }

      sLen = strlen(contour);
      if( sLen>245 ) sLen=245;
      strncpy(str1,contour,sLen); str1[sLen]='\0';
      strncpy(str2,contour,sLen); str2[sLen]='\0';

      strcpy(str1+sLen,"_Xdata");
      strcpy(str2+sLen,"_Xvec");
      PowCreateData( str1, X, &datatype, &length, &copy, &status );
      PowCreateVector( str2, str1, &offset, &length, "NULL", &status );
      ckfree( (char *)X );

      strcpy(str1+sLen,"_Ydata");
      strcpy(str2+sLen,"_Yvec");
      PowCreateData( str1, Y, &datatype, &length, &copy, &status );
      PowCreateVector( str2, str1, &offset, &length, "NULL", &status );
      ckfree( (char *)Y );

      strcpy(str1+sLen,"_Xvec");
      PowCreateCurve( contour, str1, NULL, str2, NULL, NULL, NULL,
		      &status );
   }

   ckfree( (char *)imgData );

   if( status )
      Tcl_SetResult(interp, "Unable to build contours", TCL_VOLATILE);

   return status;
}


int BuildContours( int nCntrs, double *levels,
		   int xdim, int ydim, double *image,
		   int *nPts, double **X, double **Y )
{
   int i, j, c, status = TCL_OK;
   double cntour;
   long nelem, elem;
   Contours Info;

   Info.xdim   = xdim;
   Info.ydim   = ydim;
   Info.nPts   = 6;
   Info.nAlloc = 2000;
   Info.X      = (double*)ckalloc( Info.nAlloc * sizeof(double) );
   Info.Y      = (double*)ckalloc( Info.nAlloc * sizeof(double) );
   if( !(Info.X && Info.Y) )
      return TCL_ERROR;

   Info.X[0] =         Info.Y[0] = 0.0;
   Info.X[1] = 0.0;    Info.Y[1] = ydim-1;
   Info.X[2] = xdim-1; Info.Y[2] = ydim-1;
   Info.X[3] = xdim-1; Info.Y[3] = 0.0;
   Info.X[4] = 0.0;    Info.Y[4] = 0.0;
   Info.X[5] =         Info.Y[5] = DBL_MAX;

   nelem = xdim*ydim;
   Info.usedGrid = (char *)ckalloc( nelem*sizeof(char) );
   if( ! Info.usedGrid ) {
      ckfree( (char *)Info.X );
      ckfree( (char *)Info.Y );
      return TCL_ERROR;
   }
   Info.rows = (double **)ckalloc( ydim * sizeof(double*) );
   for( j=0; j<ydim; j++ ) Info.rows[j] = image + j*xdim;

   for( c=0; c<nCntrs && !status; c++ ) {
      cntour = levels[c];
      for( elem=0; elem<nelem; elem++ ) Info.usedGrid[elem]=0;

      /*  Search outer edge  */

      /*  Search top  */
      for( j=0, i=0; i<xdim-1 && !status; i++ )
	 if( Info.rows[j][i]<cntour && cntour<=Info.rows[j][i+1] ) {
	    status = TraceContour( &Info, cntour, i, j, top );
	 }

      /*  Search right  */
      for( j=0; j<ydim-1 && !status; j++ )
	 if( Info.rows[j][i]<cntour && cntour<=Info.rows[j+1][i] ) {
	    status = TraceContour( &Info, cntour, i-1, j, right );
	 }

      /*  mottob hcraeS */
      for( i--; i>=0 && !status; i-- )
	 if( Info.rows[j][i+1]<cntour && cntour<=Info.rows[j][i] ) {
	    status = TraceContour( &Info, cntour, i, j-1, bottom );
	 }

      /*  tfel hcraeS  */
      for( i=0, j--; j>=0 && !status; j-- )
	 if( Info.rows[j+1][i]<cntour && cntour<=Info.rows[j][i] ) {
	    status = TraceContour( &Info, cntour, i, j, left );
	 }


      /*  Search each row of the image  */

      for( j=1; j<ydim-1 && !status; j++ )
	 for( i=0; i<xdim-1 && !status; i++ ) {
	    if( ! Info.usedGrid[j*xdim + i] &&
		Info.rows[j][i]<cntour && cntour<=Info.rows[j][i+1] ) {
	       status = TraceContour( &Info, cntour, i, j, top );
	    }
	 }

   }

   ckfree( (char *)Info.usedGrid );
   ckfree( (char *)Info.rows );
   *X = Info.X;
   *Y = Info.Y;
   *nPts = Info.nPts;
   return( status );
}

int TraceContour( Contours *Info, double cntr,
		  int xCell, int yCell, SideVal side)
{
   int i, j, done, flag, init, npts;
   double X, Y, a, b, c, d;
   double *ptr;
   SideVal origSide;

   i = xCell;
   j = yCell;
   origSide = side;

   init = 1;
   npts = Info->nPts;
   done = (i<0 || i>=Info->xdim-1 || j<0 && j>=Info->ydim-1);
   while( !done ) {
      flag = 0;
      a = Info->rows[j][i];
      b = Info->rows[j][i+1];
      c = Info->rows[j+1][i+1];
      d = Info->rows[j+1][i];

      if( init ) {

	 init = 0;
	 switch( side ) {
	 case top:
	    X = (cntr-a) / (b-a) + i;
	    Y = j;
	    break;
	 case right:
	    X = i+1;
	    Y = (cntr-b) / (c-b) + j;
	    break;
	 case bottom:
	    X = (cntr-c) / (d-c) + i;
	    Y = j+1;
	    break;
	 case left:
	    X = i;
	    Y = (cntr-a) / (d-a) + j;
	    break;
	 }

      } else {

	 if( side==top ) Info->usedGrid[j*Info->xdim + i] = 1;

	 do {
	    if( ++side == none ) side = top;
	    switch( side ) {
	    case top:
	       if( a>=cntr && cntr>b ) {
		  flag = 1;
		  X = (cntr-a) / (b-a) + i;
		  Y = j;
		  j--;
	       }
	       break;
	    case right:
	       if( b>=cntr && cntr>c ) {
		  flag = 1;
		  X = i+1;
		  Y = (cntr-b) / (c-b) + j;
		  i++;
	       }
	       break;
	    case bottom:
	       if( c>=cntr && cntr>d ) {
		  flag = 1;
		  X = (cntr-d) / (c-d) + i;
		  Y = j+1;
		  j++;
	       }
	       break;
	    case left:
	       if( d>=cntr && cntr>a ) {
		  flag = 1;
		  X = i;
		  Y = (cntr-a) / (d-a) + j;
		  i--;
	       }
	       break;
	    }
	 } while (!flag);

	 if( ++side == none ) side = top;
	 if( ++side == none ) side = top;
	 if( i==xCell && j==yCell && side==origSide ) done = 1;
	 if( i<0 || i>=Info->xdim-1 || j<0 || j>=Info->ydim-1 )
	    done = 1;
      }

      /*  Make sure there are at least 2 more Pts available to allocate  */

      if( npts+2 > Info->nAlloc ) {
	 ptr = (double*)ckrealloc( (char *)Info->X,
				   (Info->nAlloc+1000) * sizeof(double) );
	 if( ptr ) {
	    Info->X = ptr;
	    ptr = (double*)ckrealloc( (char *)Info->Y,
				      (Info->nAlloc+1000) * sizeof(double) );
	    if( ptr ) Info->Y = ptr;
	 }
	 if( !ptr )
	    return TCL_ERROR;
	 Info->nAlloc += 1000;
      }

      Info->X[ npts ] = X;
      Info->Y[ npts ] = Y;
      npts++;
      if( done ) {
	 Info->X[ npts ] = DBL_MAX;
	 Info->Y[ npts ] = DBL_MAX;
	 npts++;
      }

   }
   Info->nPts = npts;
   return TCL_OK;
}
