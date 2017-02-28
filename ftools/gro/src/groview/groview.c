
#include "groview.h"

 void groview()
 {
  fitsfile  *fp;
  float hourSource, minuSource, degSource, minSource;
  float DdegSource, DminSource;
  int   i,j;
  float secSource, decSource, raSource, fltnull = 0, DsecSource;
  char  data_dir[81], *egretFlg[400], *primTarget[400], *secTarget[400],c[2];
  char  show_OSSE[3],show_EGRET[3],show_COMPTEL[3],show_BATSE[3];
  float *raX=NULL, *decX=NULL, *raZ=NULL, *decZ=NULL, *tjd=NULL;
  float *scLong=NULL,*scLat=NULL,*scAng=NULL,*batse=NULL, incThresh=0;
  float *sx=NULL, *sy=NULL, *sz=NULL, *SpaceCor=NULL, *R=NULL;
  long nrows, *vp=NULL;
  char all_VPs[4];

  /* get data from vp_list.fits */
  getData(&tjd,&raZ, &decZ,&raX,&decX,&nrows,data_dir, &vp, egretFlg,
          primTarget,secTarget, all_VPs);

  /* get source  Ra & Dec */
     getSource(&raSource, &decSource);
   
  /* get inc_thresh */
     getAngThresh(&incThresh);
 
  /* get gro view */
     getGroView(&raZ, &decZ, &raSource, &decSource, &nrows, &scLong,
		&scLat, &raX, &decX, &scAng, &sx, &sy, &sz);
  /* get sc_z & source angle */
     getAng(&raSource,&decSource,&raZ,&decZ,&scAng, &nrows);

  /*get view flag */
     getViewFlag(show_OSSE, show_EGRET, show_COMPTEL, show_BATSE);
          
     printf("\n==============================================\n");
   
     for (i=0;i<nrows;i++) 
       {
         
         if ((incThresh - scAng[i]/RAD)<0.0) i=i+1;  
	 printf("Viewing Period %d\n",vp[i]);
         printf("Spacecratf Z-Axis RA & DEC: %6.3f, %6.3f\n",raZ[i],decZ[i]);
	 SpaceCor = (float *) malloc((long)(3) * sizeof(float));
	 SpaceCor[0] = sx[i];
	 SpaceCor[1] = sy[i];
	 SpaceCor[2] = sz[i];
	 printf("Angle between Source and SC_Z: %6.3f\n",scAng[i]/RAD);
	 printf("----------------\n");
	 printf("VP Statistics\n");
	 printf("----------------\n");
	 /* display OSSE Target */
         if (strncmp(show_OSSE,"y",1)==0 ||
            strncmp(show_OSSE,"Y",1)==0 ) 
	   {
            printf("OSSE Primary Target  :  %s\n",primTarget[i]);
            printf("OSSE Secondary Target: %s\n",secTarget[i]); 
	   }

	 /* get batse view */  
	 getBatseView(&SpaceCor, &batse);

         if (strncmp(show_BATSE,"y",1)==0 ||
            strncmp(show_BATSE,"Y",1)==0 ) 
	   {
	     printf("BATSE Direction Cosines:\n");   
	     j=0;
	     printf("B[%d]=%6.3f B[%d]=%6.3f B[%d]=%6.3f\n",
		    j,batse[j],j+1,batse[j+1],
		j+2,batse[j+2] );
	     j=3;
	     printf("B[%d]=%6.3f B[%d]=%6.3f  B[%d]=%6.3f\n",
		    j,batse[j],j+1,batse[j+1],
		j+2,batse[j+2] );
	     j=6;
	     printf("B[%d]=%6.3f  B[%d]=%6.3f \n",
		    j,batse[j],j+1,batse[j+1] );
	   }

	 /* get COMPTEL Responses: */
	 CmpView(&SpaceCor, &R);
         if (strncmp(show_COMPTEL,"y",1)==0 ||
            strncmp(show_COMPTEL,"Y",1)==0 ) 
	   {
	     printf("COMPTEL Responses: Efficiency\n");
	     printf("Case 1:  1275 KeV, no phi-bar restriction:  %6.2f\n",
		    R[0]);
	     printf("Case 2:  1275 KeV, standard:                %6.2f\n",
		    R[1]);
	   }

	 /* get egretview */
	 EgretView(&SpaceCor, &R);
         if (strncmp(show_EGRET,"y",1)==0 ||
            strncmp(show_EGRET,"Y",1)==0 ) 
	   {
	     if (strncmp(egretFlg[i],"a",1)==0 || 
		 strncmp(egretFlg[i],"d",1)==0 ) 
	       {
		 printf("EGRET off!\n");
		 goto step2;
	       }
	     printf("EGRET Responses: Effective Area\n");
         
	     if (strncmp(egretFlg[i],"c",1) ==0)
	       {
		 printf("only the central telescopes active:\n");
		 printf("100 Mev:  %8.1f\n", R[0]);
		 printf("1   Gev:  %8.1f\n", R[1]);
	       }
	     if (strncmp(egretFlg[i],"x",1) ==0)
	       {
		 printf("standard:\n");   
		 printf("100 Mev                            %8.1f\n", R[2]);
		 printf("1 Gev                              %8.1f\n", R[3]);
	       }
	   }
       step2:
	 printf("==============================================\n\n");
       }

}

