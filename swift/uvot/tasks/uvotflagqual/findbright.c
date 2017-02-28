/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotflagqual/findbright.c,v $
 * $Revision: 1.6 $
 * $Date: 2010/07/07 02:54:42 $
 *
 * $Log: findbright.c,v $
 * Revision 1.6  2010/07/07 02:54:42  rwiegand
 * Increased precision and checked for null pixels to get consistent results.
 *
 * Revision 1.5  2010/06/21 20:27:12  rwiegand
 * Updated with new version from Vladimir: Source detection algorithm is improved
 * (splitting overlapping sources).
 *
 * Revision 1.4  2010/04/29 21:50:58  rwiegand
 * Use a structure for the detected sources and 0-based indexing.
 *
 * Revision 1.3  2010/04/28 21:51:06  rwiegand
 * Vladimir provided a new version which operates on each extension and updates
 * quality flagging (halo rings, filter dependence).
 *
 * Revision 1.2  2010/04/05 20:56:49  rwiegand
 * Purged commented out code.
 *
 * Revision 1.1  2010/04/05 20:20:55  rwiegand
 * Saving formatted version of Vladimir Yershov changes:
 * VNY 2009/11/22 Introduced the subroutine flagHaloRings, which should replace
 * the routine flagCentralEnhancementRegion (using the same bit 5 for flagging)
 * VNY 2009/12/21 The task is now detecting bright sources by itself and does
 * not require the input source list. So, the corresponding input parameter is
 * removed.
 * VNY 2010/03/21 (flagqual2.c) isolated single pixels are removed from the
 * source map used for source detection, which is needed for dealing with
 * short-exposures made with the UVW2-filter.
 */

#include <stdlib.h>
#include <math.h>

#include "report.h"
#include "uvotfile.h"
#include "uvotquality.h"
#include "flagqual.h"


typedef struct
{
	int id;
	float x;
	float y;
	float radius;
	float value;
        float sourceSizeFromMask;
} FindSource;


#ifdef REQUIREMENTS
>******************************************************************************
> Compute the background level
>******************************************************************************
>
> test
>.
>
>[bunch of code]

#endif /* REQUIREMENTS */

float computePositiveFraction(int nx, int ny, float za01[2048][2048])
{
	float code = 0;
	float sum1, sum2;

	int il, iu, jl, ju, i, j;

	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;

	sum1 = 0.0; /* total number of pixels*/
	sum2 = 0.0; /* positive pixels */

	for (i = il; i <= iu; i++) {
		for (j = jl; j <= ju; j++) {
			sum1 += 1;
			if (za01[i][j] > 0)
				sum2 += 1;
		}
	}
	if (sum1 > 1) {
		sum2 /= sum1;
	}

	code = sum2;

	return code;
} /* end computePositiveFraction*/


float computeAverage(int nx, int ny, float image[2048][2048])
{
	float code = 0;
	double faver, naver;

	int il, iu, jl, ju, i, j;


	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;

	naver = 0.0; /* total number of pixels*/
	faver = 0.0; /* sum of the pixel values */


	for (i = il; i <= iu; i++) {
		for (j = jl; j <= ju; j++) {
			naver += 1;
			faver += image[i][j];
		}
	}
	if (naver > 1) {
		faver /= naver;
	}
	code = faver;
	return code;
} /* end computeAverage*/


static int
writeFloatImage (const char *path, float image[2048][2048])
{
	int code = 0;
	int i,j, nx, ny;
	float z;
	FImage smapImage = { 0 };
	ImageIO io = { 0 };

	smapImage.null = -1;
	smapImage.width=2048;
	smapImage.height=2048;
	nx = 2047;
	ny = 2047;
	/* Allocate smapImage */
	if (!code) {
		report_verbose(" allocating the source map image \n");
		code = fimage_allocate(&smapImage, smapImage.width, smapImage.height);
		if (code)
		  report_error("Unable to allocate smapImage image; code=[%d]\n",
			       code);
		else {
		  report_verbose("allocated source map image\n");
		}
	}
	if (!code) {
	  report_verbose(" setting source map image to zero \n");
	  code = fimage_set_constant(&smapImage, 0);
	}
	for (i = 0; i <= nx; ++i) {
	  for (j = 0; j <= ny; ++j) {
	    z = image[i][j];
	    fimage_set_relative(&smapImage, i, j, z);
	  }
	}

	code = fimage_write(&smapImage, path, &io);
	

	return code;
} /* end writeFloatImage */


int checkAnnularStructures(int nx, int ny, float image[2048][2048],
		float outputImage[2048][2048], float average, int binx)
{
	int nfound = 0;
	float rin, rin2, rout, rout2;
	static float auxImage[2048][2048];
	static float auxImage2[2048][2048];
	/*static float auxImage3[2048][2048];*/
	int il, iu, jl, ju, i, j, i1, j1, i2, j2;
	float dist;
	int isize;
	int binx1;
	float sum1, sum2, sum;
	float area0, area1,area2, area3;

#define kAnnulus 41
#define kAnnulus2xp1 83
	static float templ[kAnnulus2xp1][kAnnulus2xp1]; /* Annulus template */
	static float templ2[kAnnulus2xp1][kAnnulus2xp1];/* Thicker annulus template*/
        float dx,dy;
	float aver0, aver1, aver2, aver3, naver0, naver1, naver2, naver3;

	float aver0a[4];
	float aver1a[4];
	float aver2a[4];
	float aver3a[4];

	float naver0a[4];
	float naver1a[4];
	float naver2a[4];
	float naver3a[4];

	float diff23[4];
	float diff23a, diff23b, diff12;
	int flagdiff;

	float dav0;
	float dav1;
	float dav2;
	float dav3;

	int dil[4], dir[4];
	int djl[4], djr[4];

	int isect, iaux;
	float averAux;
	float avertot, navertot, threshold;

	int wsize=100;
	int wl,wr,wd,wu;
	int flag0, flag1, flag2, flag2strong, hsize, vsize;
	float xsrc, ysrc;
	int isrc,jsrc;
	/*int code2=0;*/
	float daver, sigma0;
	int nOutOfBounds[10] = { 0 };
       

	report_verbose(" checking for possible smoke rings   ... \n");
	/*printf("L183 average=%f \n", average);*/

	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;

	binx1=binx;
	if (binx1<1) binx1=1;

	rin=10.0/binx1;
	rout=28.0/binx1;
	rin2=(rout+rin)/2.0;
        /*rout2=sqrt(2.0*(rout*rout-rin*rin));*/
	rout2=36.0/binx1;

	wsize=4*rin;

	

	/*printf("L200 rin=%f  rout=%f rout2=%f \n", rin, rout, rout2);*/

	dil[0]=-kAnnulus; dir[0]=0;
	djl[0]=-kAnnulus; djr[0]=0;

	dil[1]=-kAnnulus; dir[1]=0;
	djl[1]=0; djr[1]=kAnnulus;

	dil[2]=0; dir[2]=kAnnulus;
	djl[2]=0; djr[2]=kAnnulus;

	dil[3]=0; dir[3]=kAnnulus;
	djl[3]=-kAnnulus; djr[3]=0;
	
        /* Fill in the annulus templates*/
	sum=0.0;
	area0=0.0;
	area1=0.0;
	area2=0.0;
	area3=0.0;
        for (i=kAnnulus-kAnnulus;i<=kAnnulus+kAnnulus;i++){
	  for (j=kAnnulus-kAnnulus;j<=kAnnulus+kAnnulus;j++){
	    templ[i][j]=-1.0;
	    templ2[i][j]=-1.0;	
	    sum+=1;
	    dx=(float)(i-kAnnulus);
	    dy=(float)(j-kAnnulus);
	    dist=sqrt(dx*dx+dy*dy);
	    
	    if (dist<=rin-1.0){
	      templ[i][j]=0.0;
	      area0+=1.0;
	    }
	    else{
	      if ((dist>rin+1.0) && (dist <=rin2-1.0)){
	        templ[i][j]=1.0;
		area1+=1.0;
	      }
	      else{
		if ((dist>rin2+1.0) && (dist <=rout-1.0)){
		  templ[i][j]=2.0;
		  area2+=1.0;
		}
		else{
		  if ((dist>rout+1.0) && (dist<=rout2-1.0)){
		    templ[i][j]=3.0;
		    area3+=1.0;
		  }
		}
	      }
	    }

	    /* Fill in the thicker annulus*/
	    if (dist<=rin-2.0/binx){
	      templ2[i][j]=0.0;
	    }
	    else{
	      if ((dist>rin-2.0/binx) && (dist <=rout+6.0/binx)){
	        templ2[i][j]=1.0;
	      }
	    }
  
	  }
	}
	 
	isize=(int) rout/2.0;

	for (i = il; i <= iu; i++) {
	  for (j = jl; j <= ju; j++) {
	    outputImage[i][j]=0.0;
	    auxImage[i][j]=0.0;
	    auxImage2[i][j]=0.0;
	    /*auxImage3[i][j]=image[i][j];*/
	  }
	}

	/*Place the annulus in the image for testing */
	/*
	i=1180; j=560;
	for (i1=i-kAnnulus;i1<=i+kAnnulus;i1++){
	  for (j1=j-kAnnulus;j1<=j+kAnnulus;j1++){
	    i2=i1-i+kAnnulus;
	    j2=j1-j+kAnnulus;
	    if (templ[i2][j2]==0.0){ 
	      auxImage3[i1][j1]=1.0;
	    }
	    if (templ[i2][j2]==1.0){
	      auxImage3[i1][j1]=40.0;
	    }
	    if (templ[i2][j2]==2.0){
	      auxImage3[i1][j1]=45;
	    }
	    if (templ[i2][j2]==3.0){
	      auxImage3[i1][j1]=5;
	    }
	  }
	}
	*/
	/*
	printf("L337 dumping the test annulus .......\n");
	code2 = writeFloatImage("ann0a_image2map.fits", auxImage3);
	printf("L339 ......... auxImage3 .\n");
	*/

	avertot=0.0;
	navertot=0.0;
	for (i = il; i <= iu; i+=2) {
	  for (j = jl; j <= ju; j+=2) {

	    /*if (i>1038 && i<1166 && j>496 && j<622){*/

	    /* four sectors */
	    for (isect=0;isect<=3;isect++){
	      aver0a[isect]=0.0;
	      naver0a[isect]=0.0;
	      aver1a[isect]=0.0;
	      naver1a[isect]=0.0;
	      aver2a[isect]=0.0;
	      naver2a[isect]=0.0;
	      aver3a[isect]=0.0;
	      naver3a[isect]=0.0;

	      for (i1=i+dil[isect];i1<=i+dir[isect];i1+=2){
		for (j1=j+djl[isect];j1<=j+djr[isect];j1+=2){
		  i2=i1-i+kAnnulus;
		  j2=j1-j+kAnnulus;

			if (i1 < 0 || i1 > nx || j1 < 0 || j1 > ny) {
				++nOutOfBounds[0];
				/* printf("checkAnnularStructures: i1=%d j1=%d out of bounds\n", i1, j1); */
				continue;
			}
			if (i2 < 0 || i2 >= kAnnulus2xp1 || j2 < 0 || j2 >= kAnnulus2xp1) {
				++nOutOfBounds[1];
				/* printf("checkAnnularStructures: i2=%d j2=%d out of bounds\n", i2, j2); */
				continue;
			}
		  
		  if (templ[i2][j2]==0.0){ 
		    aver0a[isect]+=image[i1][j1];
		    naver0a[isect]+=1.0;
		  }
		  if (templ[i2][j2]==1.0){
		    aver1a[isect]+=image[i1][j1];
		    naver1a[isect]+=1.0;
		  }
		  if (templ[i2][j2]==2.0){
		    aver2a[isect]+=image[i1][j1];
		    naver2a[isect]+=1.0;
		  }
		  if (templ[i2][j2]==3.0){
		    aver3a[isect]+=image[i1][j1];
		    naver3a[isect]+=1.0;
		  }
		}
	      }
	      aver3a[isect]/=naver3a[isect];
	      aver2a[isect]/=naver2a[isect];
	      aver1a[isect]/=naver1a[isect];
	      aver0a[isect]/=naver0a[isect];
	    }/* for isect=0 */


	    /* remove the max value of one of the sectors */
	    for (isect=0;isect<=2;isect++){
	      for (iaux=isect+1;iaux<=3;iaux++){
		if (aver0a[isect]>aver0a[iaux]){
		  averAux=aver0a[isect];
		  aver0a[isect]=aver0a[iaux];
		  aver0a[iaux]=averAux;
		}
		if (aver1a[isect]>aver1a[iaux]){
		  averAux=aver1a[isect];
		  aver1a[isect]=aver1a[iaux];
		  aver1a[iaux]=averAux;
		}
		if (aver2a[isect]>aver2a[iaux]){
		  averAux=aver2a[isect];
		  aver2a[isect]=aver2a[iaux];
		  aver2a[iaux]=averAux;
		}
		if (aver3a[isect]>aver3a[iaux]){
		  averAux=aver3a[isect];
		  aver3a[isect]=aver3a[iaux];
		  aver3a[iaux]=averAux;
		}
	      }
	    }


	    sum1=0.0;
	    sum2=0.0;
	    sum=0.0;

	    aver0=0.0;
	    aver1=0.0;
	    aver2=0.0;
	    aver3=0.0;
	    naver0=0.0;
	    naver1=0.0;
	    naver2=0.0;
	    naver3=0.0;

	    flag2=0;
	    flag1=0;
	    flag0=0;

	    for (isect=0;isect<=3;isect++){
	      aver0+=aver0a[isect];
	      naver0+=naver0a[isect];

	    }
	   
	    aver0/=4.0;
	    dav0=0;
	    for (isect=0;isect<=3;isect++){
	      dav0+=(aver0a[isect]-aver0)*(aver0a[isect]-aver0);
	    }
	    dav0=sqrt(dav0/3.0);
	    if (dav0>3.5*sqrt(average))
	      flag0=1; /* the dispersion in the inner circle is too high*/

	    sigma0=sqrt(aver0);

	    for (isect=0;isect<=2;isect++){
	      if (! (aver1a[isect]>aver0a[isect] 
		     && aver2a[isect]>aver0a[isect]
		     && aver1a[isect]>aver3a[isect]
		     && aver2a[isect]>aver3a[isect]
		     && aver1a[isect]>3.0*average
		     && aver2a[isect]>3.0*average) )
		flag1=1; /* the condition for this pixel belonging to an annulus is not met*/

	      aver1+=aver1a[isect];
	      naver1+=naver1a[isect];
	      
	      aver2+=aver2a[isect];
	      naver2+=naver2a[isect];
	      
	      aver3+=aver3a[isect];
	      naver3+=naver3a[isect];
	      
	    }


	    aver1/=3.0;
	    aver2/=3.0;
	    aver3/=3.0;

	    
	    diff12=abs(aver1-aver2);


	    dav1=0;
	    dav2=0;
	    dav3=0;

	    flagdiff=0;

	    for (isect=0;isect<=3;isect++){
	      dav1+=(aver1a[isect]-aver1)*(aver1a[isect]-aver1);
	      dav2+=(aver2a[isect]-aver2)*(aver2a[isect]-aver2);
	      dav3+=(aver3a[isect]-aver3)*(aver3a[isect]-aver3);


	      diff23[isect]=((aver1a[isect]+aver2a[isect])/2.0-(aver0a[isect]+aver3a[isect])/2.0);

	      /*
	      if (dav0>0.0) diff23[isect]/=dav0;
	      if (diff12>0.0) diff23[isect]/=diff12;
	      */

	      if (!(aver1a[isect]>aver0a[isect])) flagdiff+=1; 
	      if (!(aver2a[isect]>aver0a[isect])) flagdiff+=1; 
 	      if (!(aver1a[isect]>aver3a[isect])) flagdiff+=1;
	      if (!(aver2a[isect]>aver3a[isect])) flagdiff+=1; 
	      /* the condition of levels is not met */
	      
	    }


	    dav1=sqrt(dav1/2.0);
	    dav2=sqrt(dav2/2.0);
	    dav3=sqrt(dav3/2.0);

	    daver=(dav0+dav1+dav2+dav3)/4.0;

	    if (dav1>3.5*sigma0)
	      flag2=1; /* the condition of uniformity of the anuulus is not met */

	    /*if ((i==1096 && j==558) || (i==1104 && j==594)){*/

	    /*
	    if ((i==536 && j==444) || (i==656 && j==490)){
	      printf("L501 i=%d j=%d dav1=%f sigma0=%f 3.5sgma0=%f flag2=%d \n", 
		     i,j,dav1, sigma0, 3.5*sigma0, flag2);
	    }	    
	    */


	    if ((flagdiff+flag2+flag1+flag0<3))
	      diff23a=(diff23[0]+diff23[1]+diff23[2]+diff23[3])/4.0;
	    else
	      diff23a=0.0;
	    /*if ((i==1096 && j==558) || (i==1104 && j==594)){*/
	    /*if ((i==1074 && j==638) || (i==1312 && j==574)){*/

	    /*
	    if ((i==536 && j==444) || (i==656 && j==490)){
	      printf("--------------\n");
	      printf("L526 initial diff23a=%f, sigma0=%f daver=%f \n", diff23a, sigma0, daver);
	    }      
	    */
	    

	    if (diff23a>3.0*sigma0){
	      if (daver>0.0) diff23a/=daver;
	    }
	    else
	      diff23a=0.0;

	    /*if ((i==1096 && j==558) || (i==1104 && j==594)){*/
	    /*if ((i==1074 && j==638) || (i==1312 && j==574)){*/

	    /*
	    if ((i==536 && j==444) || (i==656 && j==490)){
	      printf("L531 normalised diff23a=%f \n", diff23a);
	      printf("L538 sigma0=%f \n", sigma0);
	    } 
	    */

     	    

	    diff23b=(diff23[0]+diff23[1]+diff23[2]+diff23[3])/4.0;
	    /*if ((i==1096 && j==558) || (i==1104 && j==594)){*/
	    /*if ((i==1074 && j==638) || (i==1312 && j==574)){*/

	    /*
	    if ((i==536 && j==444) || (i==656 && j==490)){
	      printf("L363 average=%f \n", average);
	      printf("L364 i=%d j=%d  aver0=%f aver1=%f aver2=%f aver3=%f \n", i,j,aver0, aver1,aver2,aver3);
	      printf("--------------\n");
	      printf("L366    1)     aver01=%f aver11=%f aver21=%f aver31=%f diff23=%f \n", 
		     aver0a[0], aver1a[0],aver2a[0],aver3a[0], diff23[0]);
	      printf("L367    2)     aver02=%f aver12=%f aver22=%f aver32=%f diff23=%f \n", 
		     aver0a[1], aver1a[1],aver2a[1],aver3a[1], diff23[1]);
	      printf("L368    3)     aver03=%f aver13=%f aver23=%f aver33=%f diff23=%f \n", 
		     aver0a[2], aver1a[2],aver2a[2],aver3a[2], diff23[2]);
	      printf("L369    4)     aver04=%f aver14=%f aver24=%f aver34=%f diff23=%f \n", 
		     aver0a[3], aver1a[3],aver2a[3],aver3a[3], diff23[3]);
	      printf("--------------\n");
	      printf("L371 dav0=%f dav1=%f dav2=%f dav3=%f \n",dav0, dav1,dav2,dav3);
	      printf("L436 diff23a=%f another=%f \n", diff23a, diff23b);
	      printf("L533 flagdiff=%d flag0=%d flag1=%d flag2=%d \n", flagdiff, flag0, flag1, flag2);
	      printf("L545 daver=%f \n", daver); 
	    }
	    
	    */



	    /*
	    if (aver1>aver0 && aver2>aver0 
		&& aver1>aver3 && aver2>aver3 
		&& aver1>3.0*average && aver2>3.0*average 
		&& flag0==0 && flag1==0 && flag2==0 && flagdiff==0){
	    */
	    if (flagdiff+flag0+flag1+flag2<3){
	      /* max between aver3 and aver1*/

	      /*if (aver3<aver1) aver3=aver1;*/
 
	      /* max between the deviations dav1, dav2, dav3*/
	      /*if (dav1<dav2) dav1=dav2;*/
	      /*if (dav1<dav3) dav1=dav3;*/
	      /* Average between dav1 and dav2*/
	      /*dav1=(dav1+dav2)/2.0;*/

	      
	      
	      sum=diff23a;
	     
	      /*
	      if (aver1 >0.0)
		sum=aver2/aver1;
	      else
		sum=0.0;
	      */

	      /*sum=(aver2-aver1);*/
	      /*if (aver3>0.0) sum/=aver3;*/
	      /*if (aver1>0.0) sum/=aver1;*/


	      /*sum/=dav1;*/
	      
	      /*if ((i==1096 && j==558) || (i==1104 && j==594)){*/
	      /*if ((i==1074 && j==638) || (i==1312 && j==574)){*/

	      /*
	      if ((i==536 && j==444) || (i==656 && j==490)){
		printf("L412 dav0=%f dav1=%f aver2= %f aver3= %f sum=%f \n", dav0, dav1, aver2, aver3, sum);
		printf("L470 diff23a=%f \n", diff23a);
	      }
	      */

	    }
	    else
	      sum=0.0;
	    
	    /* test 2010-06-02 */
	    /*sum=diff23a;*/

	    /*outputImage[i][j]=sum;*/
	    auxImage[i][j]=sum;
	    auxImage2[i][j]=sum;
	    
	    if (sum>0.0){
	      avertot+=sum;
	      navertot+=1.0;
	    }
 	    /*if ((i==1096 && j==558) || (i==1104 && j==594)){*/
	    /*if ((i==1074 && j==638) || (i==1312 && j==574)){*/
	    /*
	    if ((i==536 && j==444) || (i==656 && j==490)){
	      printf("L491 avertot=%f navertot=%f \n", avertot, navertot); 
	    }		
	      
	    */
	      /*}*/ 

	  }/* for j */
	}/* for i */

		
	
	if (navertot>1.0) avertot/=navertot;
	/*
	printf("L501 avertot=%f \n", avertot);
	printf("L502 navertot=%f avertot=%f \n", navertot, avertot);
	*/

	/*threshold=avertot*3.0;*/
	threshold=avertot+sqrt(avertot);

	/*printf("L418 removing the pixels below threshold  %f ... \n", threshold);*/
	
	/*
	printf("L425 threshold=%f \n", threshold);
	*/

	/*
	printf("L431 dumping the annular image .......\n");
	code2 = writeFloatImage("ann0_image2map.fits", auxImage);
	printf("L433 ......... auxImage .\n");
	*/	

	/* Find the coordinates of the source centres (if they are found) */
	for (j = jl; j <= ju; j+=2) {
	  for (i = il; i <= iu; i+=2) {

	    /*
	    if (i==1092 && j==550){
	      printf("..................................\n");
	      printf("L537 i=%d j=%d auxImage=%f auxImage+1=%f \n", i,j,auxImage[i][j], auxImage[i+1][j]);
	    }
	    */

	    if (auxImage[i][j]>0.0 && auxImage2[i][j]>0.0){
	      	      

	      /* Check whether the found source is large enough */
	      wd=j; /* down-limit of the source */
	      wl=i; /* left-limit of the source */
	      /* Find the left-limit of the source  */
	      flag1=0;
	      flag2=0;
	      flag2strong=0;
	      for (i1=i-2; i1>=i-wsize;i1-=2){
		
		if (i1>1){
		  for (j1=j;j1<=j+wsize;j1++){
		    if (j1<=ny){
		      if (auxImage[i1][j1]>0.0){ 
			flag1=1;
			if (auxImage[i1][j1]>threshold) flag2+=1;
			if (auxImage[i1][j1]>2.0*threshold) flag2strong+=1;
		      }
		      if (auxImage[i1-2][j1]>0.0){ 
			flag1=1;
			if (auxImage[i1-2][j1]>threshold) flag2+=1;
			if (auxImage[i1-2][j1]>2.0*threshold) flag2strong+=1;
		      }
		    }
		    else break;
		  }/* for j1*/
		  if (flag1==1){ /* The left edge of the source is not yet found  */
		    flag1=0;
		  }
		  else break; /* the left edge of the source is found */
		}
		else break;
	      } /* for i1 */
	      wl=i1;
	      
	      /*printf("L479 left edge wl= %d \n", wl);*/
	      

	      /* Find the right-limit of the source  */
	      wr=i;
	      flag1=0;
	      for (i1=i+2; i1<=i+wsize;i1+=2){
		
		if (i1<nx-1){
		  for (j1=j;j1<=j+wsize;j1++){
		    if (j1<=ny){
		      if (auxImage[i1][j1]>0.0){
			flag1=1;
			if (auxImage[i1][j1]>threshold) flag2+=1;
			if (auxImage[i1][j1]>2.0*threshold) flag2strong+=1;
		      }
		      if (auxImage[i1+2][j1]>0.0){ 
			flag1=1;
			if (auxImage[i1+2][j1]>threshold) flag2+=1;
			if (auxImage[i1+2][j1]>2.0*threshold) flag2strong+=1;
		      }
		    }
		    else break;
		  }/* for j1*/
		  if (flag1==1){ /* The right edge of the source is not yet found  */
		    flag1=0;
		  }
		  else break; /* the right edge of the source is found */
		}
		else break;
	      } /* for i1 */
	      wr=i1;
	      
	      /*printf("L500 right edge wr= %d \n", wr);*/
	      

	      /* Find the upper-limit of the source  */
	      wu=j;
	      flag1=0;
	      for (j1=j+2; j1<=j+wsize;j1+=2){
		
		if (j1<ny-1){
		  for (i1=i-wsize;i1<=i+wsize;i1++){
		    if (i1>0 && i1<nx){
		      if (auxImage[i1][j1]>0.0){
			flag1=1;
			if (auxImage[i1][j1]>threshold) flag2+=1;
			if (auxImage[i1][j1]>2.0*threshold) flag2strong+=1;
		      }
		      if (auxImage[i1][j1+2]>0.0){
			flag1=1;
			if (auxImage[i1][j1+2]>threshold) flag2+=1;
			if (auxImage[i1][j1+2]>2.0*threshold) flag2strong+=1;
		      }
		    }
		    else break;
		  }/* for i1*/
		  if (flag1==1){ /* The upper edge of the source is not yet found  */
		    flag1=0;
		  }
		  else break; /* the upper edge of the source is found */
		}
		else break;
	      } /* for j1 */
	      wu=j1;
	      
	      /*printf("L525 the upper edge wu= %d \n", wu);*/
	      
	      hsize=wr-wl;
	      vsize=wu-wd;

	      /*
	      if (flag2>0 || flag2strong>0){
		printf("-------------------------------------\n");
		printf("L637 i=%d j=%d hsize=%d vsize=%d flag2=%d rin=%f\n", i, j, hsize, vsize, flag2, rin);
		printf("L638 3rin=%f flag2=%d flag2strong=%d \n", 3.0*rin, flag2, flag2strong);
		printf(" wl=%d wr=%d   wd=%d  wu=%d \n", wl,wr,wd,wu);
		printf("L770 threshold=%f avertot=%f \n", threshold, avertot);
	      }
	      */

	      /* remove the checked pixels from auxImage2*/
	      for (i1=wl; i1<=wr;i1++){
		for (j1=wd;j1<=wu; j1++){
		  auxImage2[i1][j1]=0.0;
		}
	      }

	      
	      /* The condition to decide whether the source is an annulus or not*/
	      if ((hsize<=3*rin && vsize<=3*rin && hsize>=rin/2 && vsize>rin/2 && flag2>(int)rin/3)
		  || (hsize<=3*rin && vsize<=3*rin && flag2strong>0 )){

		/*
		printf("------------------------------------ possible annulus: -------\n");
		printf("L648 i=%d j=%d hsize=%d vsize=%d rin=%f \n", i,j, hsize, vsize, rin);
		*/


		/* Additionally, check whether the perimeter around the source is null */
		flag1=0;
		for (i1=wl-1;i1<=wr+1; i1++){
		  if (i1 >=0 && i1<=nx && wd-1>=0 && wu+1<=ny){
		    if (auxImage[i1][wd-1]>0.0)
		      flag1=1;
		    if (auxImage[i1][wu+1]>0.0)
		      flag1=1;
		  }
		  else
		    flag1=1;
		}
		for (j1=wd-1;j1<=wu+1; j1++){
		  if (j1 >=0 && j1<=ny && wl-1>=0 && wr+1<=nx){
		    if (auxImage[wl-1][j1]>0.0)
		      flag1=1;
		    if (auxImage[wu+1][j1]>0.0)
		      flag1=1;
		  }
		  else
		    flag1=1;
		}		


		if (flag1==0){

		  /*
		  printf("----------------------------------------------\n");
		  printf("L436 i=%d j=%d auxImage=%f \n", i,j, auxImage[i][j]);
		  printf("L630 wl=%d  wr=%d   wd=%d  wu=%d \n",wl,wr,wd,wu); 
		  printf("L528 hsize=%d  vsize=%d rin=%f \n", hsize, vsize, rin);
		  */

		  /* Calculate the x-coordinates of the source */
		  xsrc=(float)((wl+wr)/2);
		  sum=0.0;
		  sum2=0.0;
		  for (i1=wl;i1<=wr;i1++){
		    dx=(float)i1;
		    sum1=0.0;
		    for (j1=wd;j1<=wu;j1++){
		      sum1+=auxImage[i1][j1];
		    }/* for j1 */
		    sum+=sum1*dx;
		    sum2+=sum1;
		  }/* for i1 */
		  if (sum2>0.0) xsrc=sum/sum2;
		  
		  /* Calculate the y-coordinates of the source */
		  ysrc=(float)((wd+wu)/2);
		  sum=0.0;
		  sum2=0.0;
		  sum1=0.0;
		  for (j1=wd;j1<=wu;j1++){
		    dy=(float)j1;
		    sum1=0.0;
		    for (i1=wl;i1<=wr;i1++){
		      sum1+=auxImage[i1][j1];
		    }/* for i1 */
		    sum+=sum1*dy;
		    sum2+=sum1;
		  }/* for j1 */
		  if (sum2>0.0) ysrc=sum/sum2;

		  /*
		  printf("L563 xsrc=%f ysrc=%f \n", xsrc, ysrc);
		  */

		  isrc=(int)(xsrc+0.5);
		  jsrc=(int)(ysrc+0.5);
		  

		  /* Check whether the source is really an annulus */
		  flagdiff=0;
		  for (isect=0;isect<=3;isect++){
		    aver0a[isect]=0.0;
		    naver0a[isect]=0.0;
		    aver1a[isect]=0.0;
		    naver1a[isect]=0.0;
		    aver2a[isect]=0.0;
		    naver2a[isect]=0.0;
		    aver3a[isect]=0.0;
		    naver3a[isect]=0.0;
		    
		    for (i1=isrc+dil[isect]; i1<=isrc+dir[isect]; i1+=2){
		      if (i1>=0 && i1 <=nx){
			for (j1=jsrc+djl[isect];j1<jsrc+djr[isect]; j1+=2){
			  if (j1>=0 && j1 <=ny){
			    i2=i1-isrc+kAnnulus;
			    j2=j1-jsrc+kAnnulus;
			    
			    if (templ[i2][j2]==0){
			      aver0a[isect]+=image[i1][j1];
			      naver0a[isect]+=1.0;
			    }
			    
			    if (templ[i2][j2]==1){
			      aver1a[isect]+=image[i1][j1];
			      naver1a[isect]+=1.0;
			    }
			    if (templ[i2][j2]==2){
			      aver2a[isect]+=image[i1][j1];
			      naver2a[isect]+=1.0;
			    }			  
			  }
			}/* for j1*/
		      }
		    }/* for i1*/
		    
		    if (naver0a[isect]>1.0) aver0a[isect]/=naver0a[isect];
		    if (naver1a[isect]>1.0) aver1a[isect]/=naver1a[isect];
		    if (naver2a[isect]>1.0) aver2a[isect]/=naver2a[isect];
		    
		    if ((2.0*aver0a[isect])>aver1a[isect]) flagdiff=1;
		    if ((2.0*aver0a[isect])>aver2a[isect]) flagdiff=1;

		    /*		    
		    printf("L713 isect=%d aver0a=%f aver1a=%f aver2a=%f \n", 
			   isect, aver0a[isect], aver1a[isect], aver2a[isect]);
		    */

		  }/* for isect */
		  
		  /*		  
		  if (flagdiff==1) 
		    printf("L723 NOT an annulus \n");
		  else
		    printf("L908 IS an annulus \n");
		  
		  printf("-----------------------\n");
		  */

		  /* The source is acceptable: put it onto the map */
		  if (flagdiff==0){
		    nfound+=1;
		    
		    report_verbose("(o) possible annulus x=%f y=%f \n", xsrc, ysrc);
		    

		    /* Copy the annulus mask to the output image */
		    for (i1=isrc-kAnnulus;i1<=isrc+kAnnulus;i1++){
		      for (j1=jsrc-kAnnulus;j1<=jsrc+kAnnulus;j1++){

				if (i1 < 0 || i1 > nx || j1 < 0 || j1 > ny) {
					/* i1 or j1 out of bounds; cannot set a flag in outputImage */
					++nOutOfBounds[2];
					continue;
				}

				auxImage[i1][j1]=0.0;

				i2=i1-isrc+kAnnulus;
				j2=j1-jsrc+kAnnulus;

				if (i2 < 0 || i2 >= kAnnulus2xp1 || j2 < 0 || j2 >= kAnnulus2xp1) {
					/* i2 or j2 out of bounds, cannot dereference templ2 */
					++nOutOfBounds[3];
					continue;
				}
			
			if (templ2[i2][j2]==1.0){
			  outputImage[i1][j1]=1.0;
			}
			
		      }/*  for j1 */
		    }/* for i1 */
		    
		  }/* if flagdiff==0*/

		} /* if flag1==0*/
	      }/* if hsize */
	      
	    }/* if outputImage */
	  }/* for i */
	}/* for j */

	report_verbose("finished searching for annular (smoke ring) structures   \n");

	for (i = 0; i < sizeof(nOutOfBounds) / sizeof(nOutOfBounds[0]); ++i) {
		if (nOutOfBounds[i] > 0)
			report_verbose("nOutOfBounds[%d]=%d\n", i, nOutOfBounds[i]);
	}

	return nfound;
} /* end checkAnnularStructures*/


int maskAnnularStructures(int nx, int ny, float image[2048][2048], float annImage[2048][2048],
		float average, int binx)
{
	int code = 0;

	int binx1;
	int il, iu, jl, ju, i, j, i1, j1;

	float v[4];
	float vaux;

	float aver1, naver1;
	

	report_verbose(" masking annular (smoke-ring) structures  ... \n");

	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;

	binx1=binx;
	if (binx1<1) binx1=1;


	for (i = il; i <= iu; i++) {
	  for (j = jl; j <= ju; j++) {
	    if (annImage[i][j] >0){

	      /* Pick up the left image value */
	      v[0]=-1.0;
	      for (i1=i-1;i1>=i-82;i1--){
		if (i1>=0){
		  if (annImage[i1][j]==0.0){
		    v[0]=image[i1][j];
		    break;
		  }
		}
		else break;
	      }/* for i1 */
	
	      /* Pick up the right image value */
	      v[1]=-1.0;
	      for (i1=i+1;i1<=i+82;i1++){
		if (i1<=nx){
		  if (annImage[i1][j]==0.0){
		    v[1]=image[i1][j];
		    break;
		  }
		}
		else break;
	      }/* for i1 */
	
	      /* Pick up the down value */
	      v[2]=-1.0;
	      for (j1=j-1;j1>=j-82;j1--){
		if (j1>=0){
		  if (annImage[i][j1]==0.0){
		    v[2]=image[i][j1];
		    break;
		  }
		}
		else break;
	      }/* for j1 */

	      /* Pick up the upper image value */
	      v[3]=-1.0;
	      for (j1=j+1;j1<=j+82;j1++){
		if (j1<=ny){
		  if (annImage[i][j1]==0.0){
		    v[1]=image[i][j1];
		    break;
		  }
		}
		else break;
	      }/* for j1 */

	      /* Sort the above four values to get rid of the maximal value */
	      for (i1=0; i1<=2; i1++){
		for (j1=i1+1;j1<=3;j1++){
		  if (v[i1]>v[j1]){
		    vaux=v[i1];
		    v[i1]=v[j1];
		    v[j1]=vaux;
		  }
		}
	      }

	      /* get the average of the above four values excluding the last one (maximum) */
	      aver1=0.0;
	      naver1=0.0;
	      for (i1=0;i1<=2;i1++){
		if (v[i1]>0.0){
		  aver1+=v[i1];
		  naver1+=1.0;
		}
	      }
	      if (naver1 > 0.0) 
		aver1/=naver1;
	      else
		aver1=average;

	      image[i][j]=aver1;

	    }
	  }/* for j */
	}/* for i */



	report_verbose("finished masking annular structures  \n");

	return code;
} /* end maskAnnularStructures*/


/* Fill in the zero-valued gaps within the mod-8 pattern*/
int fillMod8gaps(int nx, int ny, float image[2048][2048],  float average, int ishift)
{
	int code = 0;
	float shiftSquared;
	static float auxImage[2048][2048];
	int il, iu, jl, ju, i, j, i1, j1;

	int count1, count2, ishift2;

	float threshold, threshold2, max1;
	float  aver1, naver1, average2;
	float v[4];

	report_verbose(" filling in the gaps within the mod8-pattern ... \n");
	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;

	ishift2=ishift*2;
	shiftSquared = (float) ishift;
	shiftSquared *= shiftSquared;
	if (shiftSquared < 1.0)
		shiftSquared = 1.0;

	average2=average;
	if (average<1.5) average2=1.5;

	/*
	printf("L781 image average= %5.1f \n", average);
	*/

	threshold=average2*1000.0;
	threshold2=average2+100.0;

	report_verbose("filling in zero-valued gaps between bright pixels   \n");
	/* filling in the zero-valued gaps between bright pixels  */
	for (i = il; i <= iu; i++) {
	  for (j = jl; j <= ju; j++) {
	    auxImage[i][j]=image[i][j];

	    if (image[i][j]==0.0){

	      count1=0;
	      count2=0;
	      max1=0.0;

	      /* checking the horizontal line*/

	      /* Pick up the left image value */
	      v[0]=-1.0;
	      for (i1=i-1;i1>=i-ishift2;i1--){
		if (i1>=0){
		  if (image[i1][j]>threshold2){
		    v[0]=image[i1][j];
		    count1+=1;
		    break;
		  }
		}
		else break;
	      }/* for i1 */

	      /* Pick up the right image value */
	      v[1]=-1.0;
	      for (i1=i+1;i1<=i+ishift2;i1++){
		if (i1<=nx){
		  if (image[i1][j]>threshold2){
		    v[1]=image[i1][j];
		    count1+=1;
		    break;
		  }
		}
		else break;
	      }/* for i1 */

	      /* Pick up the down value */
	      v[2]=-1.0;
	      for (j1=j-1;j1>=j-ishift2;j1--){
		if (j1>=0){
		  if (image[i][j1]>threshold2){
		    v[2]=image[i][j1];
		    count1+=1;
		    break;
		  }
		}
		else break;
	      }/* for j1 */

	      /* Pick up the upper image value */
	      v[3]=-1.0;
	      for (j1=j+1;j1<=j+ishift2;j1++){
		if (j1<=ny){
		  if (image[i][j1]>threshold2){
		    v[1]=image[i][j1];
		    count1+=1;
		    break;
		  }
		}
		else break;
	      }/* for j1 */

	      /* get the average of the above four values excluding the last one (maximum) */
	      aver1=0.0;
	      naver1=0.0;
	      for (i1=0;i1<=2;i1++){
		if (v[i1]>0.0){
		  aver1+=v[i1];
		  naver1+=1.0;
		}
	      }
	      if (naver1 > 0.0) 
		aver1/=naver1;
	      else
		aver1=average2;


	      /**************************************/
	      /*
	      for (i1=i-ishift2; i1<=i+ishift2;i1++){
		if (i1 >=0 && i1 <= nx){
		  if (image[i1][j]>threshold){
		    count1+=1;
		    if (image[i1][j]>max1) max1=image[i1][j];
		  }
		}
	      }
	      */
	      /* checking the vertical line */
	      /*
	      for (j1=j-ishift2; j1<=j+ishift2;j1++){
		if (j1 >=0 && j1 <= ny){
		  if (image[i][j1]>threshold){
		    count1+=1;
		    if (image[i][j1]>max1) max1=image[i][j1];
		  }
		}
	      }
	      */

	      /* very bright pixels are found near zero: filling the gap */
	      /*if (count1>0) auxImage[i][j]=max1;*/
	      if (count1>2) auxImage[i][j]=aver1;

	    } /* image==0*/
	  } /* for j*/
	} /* for i*/


	/******************************/
	/* Check the same once again */
	/*****************************/


	for (i = il; i <= iu; i++) {
	  for (j = jl; j <= ju; j++) {
	    image[i][j]=auxImage[i][j];

	    if (auxImage[i][j]==0){

	      count1=0;
	      count2=0;
	      max1=0.0;

	      /* checking the horizontal line*/

	      /* Pick up the left image value */
	      v[0]=-1.0;
	      for (i1=i-1;i1>=i-ishift2;i1--){
		if (i1>=0){
		  if (auxImage[i1][j]>threshold2){
		    v[0]=auxImage[i1][j];
		    count1+=1;
		    break;
		  }
		}
		else break;
	      }/* for i1 */

	      /* Pick up the right image value */
	      v[1]=-1.0;
	      for (i1=i+1;i1<=i+ishift2;i1++){
		if (i1<=nx){
		  if (auxImage[i1][j]>threshold2){
		    v[1]=auxImage[i1][j];
		    count1+=1;
		    break;
		  }
		}
		else break;
	      }/* for i1 */

	      /* Pick up the down value */
	      v[2]=-1.0;
	      for (j1=j-1;j1>=j-ishift2;j1--){
		if (j1>=0){
		  if (auxImage[i][j1]>threshold2){
		    v[2]=auxImage[i][j1];
		    count1+=1;
		    break;
		  }
		}
		else break;
	      }/* for j1 */

	      /* Pick up the upper image value */
	      v[3]=-1.0;
	      for (j1=j+1;j1<=j+ishift2;j1++){
		if (j1<=ny){
		  if (auxImage[i][j1]>threshold2){
		    v[1]=auxImage[i][j1];
		    count1+=1;
		    break;
		  }
		}
		else break;
	      }/* for j1 */


	      /* get the average of the above four values excluding the last one (maximum) */
	      aver1=0.0;
	      naver1=0.0;
	      for (i1=0;i1<=2;i1++){
		if (v[i1]>0.0){
		  aver1+=v[i1];
		  naver1+=1.0;
		}
	      }
	      if (naver1 > 0.0) 
		aver1/=naver1;
	      else
		aver1=average2;


	      /***********************************/
	      /*
	      for (i1=i-ishift2; i1<=i+ishift2;i1++){
		if (i1 >=0 && i1 <= nx){
		  if (auxImage[i1][j]>threshold){
		    count1+=1;
		    if (auxImage[i1][j]>max1) max1=auxImage[i1][j];
		  }
		}
	      }
	      */
	      /* checking the vertical line */
	      /*
	      for (j1=j-ishift2; j1<=j+ishift2;j1++){
		if (j1 >=0 && j1 <= ny){
		  if (auxImage[i][j1]>threshold){
		    count1+=1;
		    if (auxImage[i][j1]>max1) max1=auxImage[i][j1];
		  }
		}
	      }
	      */
	      /* very bright pixels are found near zero: filling the gap */
	      if (count1>2) image[i][j]=aver1;

	    } /* image==0*/
	  } /* for j*/
	} /* for i*/

	/*	
	printf("L224 dumping the source mask image .......\n");
	{
	  code = writeFloatImage("smask_image2map.fits", auxImage);
	  printf("L227 ......... srcMaskImage .\n");
	}
	*/

	report_verbose("finished filling in the gaps in the mod8-pattern \n");

	return code;
} /* end fillMod8gaps*/


int enhanceImage(int nx, int ny, float image[2048][2048],
		float outputImage[2048][2048], int ishift)
{
	int code = 0;
	float rk, rkmax;
	float shiftSquared;
	/*static float auxImage[2048][2048];*/
	int il, iu, jl, ju, i, j, i1, j1;
	int ishift2;
	
	report_verbose(" enhancing the image ... \n");
	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;
	ishift2=ishift*2;
	shiftSquared = (float) ishift;
	shiftSquared *= shiftSquared;
	if (shiftSquared < 1.0)
		shiftSquared = 1.0;


	report_verbose("spreading pixels across the %d x %d squares \n", ishift*2,ishift*2);
	for (i = il; i <= iu; i+=2) {
	  for (j = jl; j <= ju; j+=2) {
	    rk = 0.0;
	    rkmax = 0;
	    outputImage[i][j] = 0;

            /* BOB: added this test for predictability */
	    if (image[i][j] < 0)
              {
		outputImage[i][j] = image[i][j];
		continue;
	      }

	    for (i1 = -ishift; i1 < ishift; i1++) {
	      for (j1 = -ishift; j1 < ishift; j1++) {
		if (i + i1 >= il && i + i1 <= iu) {
		  if (j + j1 >= jl && j + j1 <= ju) {
		    if (image[i + i1][j + j1] > 0.0) {
		      rk += 1;
		      if (image[i + i1][j + j1] > rkmax)
			rkmax = image[i + i1][j + j1];
		    }
		  }
		}
	      }
	    }
	    outputImage[i][j] = image[i][j] + rk * rkmax / shiftSquared;
	  }
	}

	report_verbose("spreading by 2 pixels  \n");
	for (i = il; i < iu; i+=2) {
	  for (j = jl; j < ju; j+=2) {
	    outputImage[i+1][j] = outputImage[i][j];
	    outputImage[i][j+1] = outputImage[i][j];
	    outputImage[i+1][j+1] = outputImage[i][j];
	  }
	}


	report_verbose("finished enhancing the image \n");

	return code;
} /* end enhanceImage*/

/***************************************************************************/
/* This subroutine should remove the isolated pixels prom the source mask*/
/* That is, if the values of all of the pixels around are zero then */
/* the pixel is considered to be isolated */
/*************************************************************************/
int removeIsolatedPixels(int nx, int ny, float image[2048][2048],
		int maxNeighbours)
{
	int code = 0;

	static float auxImage[2048][2048];
	int i, j, i1, j1;
	int neighbours;

	for (i = 0; i <= nx; i++) {
		for (j = 0; j <= ny; j++) {
			neighbours = 0;
			if (image[i][j] > 0.0) {
				/* Check the nearest pixels around */
				for (i1 = i - 1; i1 <= i + 1; i1++) {
					for (j1 = j - 1; j1 <= j + 1; j1++) {
						if (i1 == i && j1 == j) {
							/* do nothing */
						}
						else {
							/* Check whether the value is positive */
							if (i1 >= 0 && i1 <= nx) {
								if (j1 >= 0 && j1 <= ny) {
									if (image[i1][j1] > 0) {
										neighbours += 1;
									}
								}
							}
						}
					}
				}
			}
			/* If there were some neighbours then keep the pixel*/
			if (neighbours >= maxNeighbours)
				auxImage[i][j] = image[i][j];
			else
				auxImage[i][j] = 0;
		}
	}
	/* Pass the result to the input image */
	for (i = 0; i <= nx; i++) {
		for (j = 0; j <= ny; j++) {
			image[i][j] = auxImage[i][j];
		}
	}

	return code;
} /* end removeIsolatedPixels*/


/* Create source mask for the normal image */
int createMaskNormal(int nx, int ny, float image[2048][2048],
		float srcMaskImage[2048][2048], float average, 
		int ishift, int binx)
{
  int code = 0;
  float z,maxvalue, minvalue;
  double faver, naver;
  int il, iu, jl, ju, i, j;/*, i1,j1;*/
  int imax, jmax;
 
  /*static float auxImage[2048][2048];*/
  /*int sum1, sum2;*/
  int  maxNeighbours;
  /*int code2=0;*/

  float threshold;

  report_verbose(" createing a source mask from the original image ... \n");
  /*
  printf("L1530 average= %f \n", average);
  */

  il = 0;
  iu = nx;
  jl = 0;
  ju = ny;
  
  maxvalue=-1.0E10;
  minvalue=1.0E10;
  imax=-1;
  jmax=-1;
  naver = 0.0; /* total number of pixels*/
  faver = 0.0; /* sum of the pixel values */

  for (i = il; i <= iu; i++) {
    for (j = jl; j <= ju; j++) {
      naver += 1;
      faver += image[i][j];
      if (image[i][j]>maxvalue){
	maxvalue=image[i][j];
	imax=i; jmax=j;
      }
      if (image[i][j]<minvalue) minvalue=image[i][j];
    }
  }
  if (naver > 1) {
    faver /= naver;
  }


  threshold=10.0*faver+0.1*(maxvalue-10.0*faver);
  /*threshold=faver+10.0*sqrt(faver);*/
  /*
  printf("L1562 faver= %5.1f   maxvalue=%5.1f   minvalue=%5.1f threshold=%5.1f \n", 
	 faver, maxvalue, minvalue, threshold);
  printf("L1564 imax=%d jmax=%d \n", imax, jmax);
  */


  /* Create the source mask image */
  for (i = il; i <= iu; ++i) {
    for (j = jl; j <= ju; ++j) {
      z = image[i][j];
      if (z > threshold)
	srcMaskImage[i][j] = 1;
      else
	srcMaskImage[i][j] = 0;
    }
  }

  /* remove isolated pixels */

  maxNeighbours = 3;

  /*   
  printf("L1607 dumping the normal source mask image .......\n"); 
  code = writeFloatImage("nmask_image2map.fits", auxImage);
  printf("L1609 ......... auxImage .\n");
  */
  

  report_verbose("finished creating the source mask  from the normal image \n");

  return code;
} /* end createMaskNormal*/


/* Create source mask for the enhanced image */
int createMaskEnhanced(int nx, int ny, float enhImage[2048][2048],
		float srcMaskImage[2048][2048], float * averEnh, int ishift)
{
  int code = 0;
  float z,maxvalue, minvalue;
  double faver, naver;
  int il, iu, jl, ju, i, j;/*, i1,j1;*/
  int imax, jmax;
  int checkLength;
  static float auxImage[2048][2048];
  int sum1, sum2;
  int  maxNeighbours;
  int code2=0;

  report_verbose(" createing a source mask from the enhanced image ... \n");
  /*printf("L226 ishift= %d \n", ishift);*/
  checkLength=ishift*2;

  il = 0;
  iu = nx;
  jl = 0;
  ju = ny;
  
  maxvalue=-1.0E10;
  minvalue=1.0E10;
  imax=-1;
  jmax=-1;
  naver = 0.0; /* total number of pixels*/
  faver = 0.0; /* sum of the pixel values */

  for (i = il; i <= iu; i++) {
    for (j = jl; j <= ju; j++) {
      naver += 1;
      faver += enhImage[i][j];
      if (enhImage[i][j]>maxvalue){
	maxvalue=enhImage[i][j];
	imax=i; jmax=j;
      }
      if (enhImage[i][j]<minvalue) minvalue=enhImage[i][j];
    }
  }
  if (naver > 1) {
    faver /= naver;
  }


  /*
  printf("L1008 averEnh= %5.1f   maxvalue=%5.1f   minvalue=%5.1f \n", 
	 faver, maxvalue, minvalue);
  printf("L1010 imax=%d jmax=%d \n", imax, jmax);
  */  

  /*
  printf("L1012 dumping the enhanced image .......\n"); 
  code = writeFloatImage("enh_image2map.fits", enhImage);
  printf("L1014 ......... enhImage .\n");
  */

  /* recompute the average taking into account only moderately bright pixels*/
  
  naver = 0.0;
  faver = 0.0;

  for (i = il; i <= iu; i++) {
    for (j = jl; j <= ju; j++) {
      if (enhImage[i][j] < 0.5*maxvalue){
	naver += 1;
	faver += enhImage[i][j];
      }
    }
  }
  if (naver > 1) faver /= naver;

  *averEnh=faver;
  /*
  printf("L434 recomputed averEnh= %5.1f  \n", *averEnh);
  */
  /* Create the source mask image */
  for (i = il; i <= iu; ++i) {
    for (j = jl; j <= ju; ++j) {
      z = enhImage[i][j];
      if (z > 6.0 * *averEnh)
	/*if (z > 0.5 * maxvalue)*/
	auxImage[i][j] = 1;
      else
	auxImage[i][j] = 0;
    }
  }

  /* remove isolated pixels */
  maxNeighbours = 3;
  code2 = removeIsolatedPixels(nx, ny, auxImage, maxNeighbours);

  /*  
  printf("L433 dumping the source mask image .......\n"); 
  code = writeFloatImage("smask_image2map.fits", auxImage);
  printf("L436 ......... srcMaskImage .\n");
  */
  

  /* fill in the gaps in the mask */
  
  for (i = il; i <= iu; ++i) {
    for (j = jl; j <= ju; ++j) {
      if (auxImage[i][j] > 0){
	srcMaskImage[i][j]=auxImage[i][j];
      }
      else{
	
	sum1=0;
	sum2=0;
	srcMaskImage[i][j]=0;
	/*
	for (i1=i-checkLength;i1<=i+checkLength;i1++){
	  if (i1>=0 && i1<=nx){
	    if (auxImage[i1][j]>0) sum1+=1;
	  }
	} 
	for (j1=j-checkLength;j1<=j+checkLength;j1++){
	  if (j1>=0 && j1<=ny){
	    if (auxImage[i][j1]>0) sum2+=1;
	  }
	}

	if (sum1>0 && sum2 >0) srcMaskImage[i][j]=1;
	*/

      }
    }
  }

  /*
  printf("L467 dumping the source mask image .......\n");
  {
    code = writeFloatImage("smask_filled2map.fits", srcMaskImage);
    printf("L470 ......... srcMaskImage .\n");
  }
  */

  report_verbose("finished creating the source mask  from the enhanced image \n");

  return code;
} /* end createMaskEnhanced*/



/* Create source mask for the enhanced image */
int createMaskEnhanced2(int nx, int ny, float enhImage[2048][2048],
		float srcMaskImage[2048][2048], float * averEnh, int ishift)
{
  int code = 0;
  float z,maxvalue, minvalue;
  double faver, naver, faver2,naver2;
  int il, iu, jl, ju, i, j;/*, i1,j1;*/
  int imax, jmax;
  int checkLength;
  static float auxImage[2048][2048];
  static float auxImage2[2048][2048];
  int sum1, sum2;
  int  maxNeighbours;
  int code2=0;
  float threshold1, threshold2;

  report_verbose(" createing a source mask from the enhanced image ... \n");
  /*printf("L226 ishift= %d \n", ishift);*/
  checkLength=ishift*2;

  il = 0;
  iu = nx;
  jl = 0;
  ju = ny;
  
  maxvalue=-1.0E10;
  minvalue=1.0E10;
  imax=-1;
  jmax=-1;
  naver = 0.0; /* total number of pixels*/
  faver = 0.0; /* sum of the pixel values */

  for (i = il; i <= iu; i++) {
    for (j = jl; j <= ju; j++) {
      naver += 1;
      faver += enhImage[i][j];
      if (enhImage[i][j]>maxvalue){
	maxvalue=enhImage[i][j];
	imax=i; jmax=j;
      }
      if (enhImage[i][j]<minvalue) minvalue=enhImage[i][j];
    }
  }
  if (naver > 1) {
    faver /= naver;
  }


  /*
  printf("L1154 averEnh= %5.1f   maxvalue=%5.1f   minvalue=%5.1f \n", 
	 faver, maxvalue, minvalue);
  printf("L1156 imax=%d jmax=%d \n", imax, jmax);
  */

  /*
  printf("L1012 dumping the enhanced image .......\n"); 
  code = writeFloatImage("enh_image2map.fits", enhImage);
  printf("L1014 ......... enhImage .\n");
  */

  /* recompute the average taking into account only moderately bright pixels*/
  
  naver = 0.0;
  faver = 0.0;


  for (i = il; i <= iu; i++) {
    for (j = jl; j <= ju; j++) {
      if (enhImage[i][j] < 0.5*maxvalue){
	naver += 1;
	faver += enhImage[i][j];
      }
    }
  }
  if (naver > 1) faver /= naver;

  *averEnh=faver;
  /*  
  printf("L1181 recomputed (moderate) averEnh= %5.1f  \n", *averEnh);
  */
  threshold1=6.0*sqrt(faver);
  /*
  printf("L1186 threshold1=%f 6*faver=%f \n", threshold1, 6.0*faver);
  */
  faver2=0.0;
  naver2=0.0;

  /* Create the source mask image */
  for (i = il; i <= iu; ++i) {
    for (j = jl; j <= ju; ++j) {
      z = enhImage[i][j];
      if (z > 6.0 * *averEnh)
	auxImage[i][j] = 1;
      else{
	auxImage[i][j] = 0;
	faver2+=z;
	naver2+=1.0;
      }
    }
  }
  /*
  printf("L1204 faver2=%f naver2=%f \n", faver2, naver2);
  */
  if (naver2>1.0) faver2/=naver2;
  /*
  printf("L1200 average of the pixels below the threshold %f faver2= %f \n", 6.0*faver, faver2);
  */
  threshold2=6.0*sqrt(faver2);
  /*
  printf("L1203 threshold2=%f \n", threshold2);
  */
  /***************************** test *********************************/
  /* Create an extra source map for fainter sources */
  for (i = il; i <= iu; ++i) {
    for (j = jl; j <= ju; ++j) {
      z = enhImage[i][j];
      if (z < 6.0 * *averEnh && z> threshold2)
	auxImage2[i][j] = 1;
      else{
	auxImage2[i][j] = 0;
      }
    }
  }

  /*
  printf("L1219 dumping the faint source mask image-2 .......\n"); 
  code = writeFloatImage("smask2_image2map.fits", auxImage2);
  printf("L1221 ......... auxImage2 .\n");
  */

  /***************************** test *********************************/

  /* remove isolated pixels */
  maxNeighbours = 3;
  code2 = removeIsolatedPixels(nx, ny, auxImage, maxNeighbours);

  /*  
  printf("L433 dumping the source mask image .......\n"); 
  code = writeFloatImage("smask_image2map.fits", auxImage);
  printf("L436 ......... srcMaskImage .\n");
  */
  

  /* fill in the gaps in the mask */
  
  for (i = il; i <= iu; ++i) {
    for (j = jl; j <= ju; ++j) {
      if (auxImage[i][j] > 0){
	srcMaskImage[i][j]=auxImage[i][j];
      }
      else{
	
	sum1=0;
	sum2=0;
	srcMaskImage[i][j]=0;
	/*
	for (i1=i-checkLength;i1<=i+checkLength;i1++){
	  if (i1>=0 && i1<=nx){
	    if (auxImage[i1][j]>0) sum1+=1;
	  }
	} 
	for (j1=j-checkLength;j1<=j+checkLength;j1++){
	  if (j1>=0 && j1<=ny){
	    if (auxImage[i][j1]>0) sum2+=1;
	  }
	}

	if (sum1>0 && sum2 >0) srcMaskImage[i][j]=1;
	*/

      }
    }
  }

  /*
  printf("L467 dumping the source mask image .......\n");
  {
    code = writeFloatImage("smask_filled2map.fits", srcMaskImage);
    printf("L470 ......... srcMaskImage .\n");
  }
  */

  report_verbose("finished creating the source mask  from the enhanced image \n");

  return code;
} /* end createMaskEnhanced2*/


/***********************************************************************
 * checkIntersectingColumns should remove the pixels masked in the centres
 * of the symmetric three-column patterns (which appear because of  
 * intersecting squares/rectangles after image enhancing)
 **********************************************************************/
int checkIntersectingColumns(int nx, int ny, float image[2048][2048],
		int binx)
{
  int code = 0;
  
  static float auxImage[2048][2048];
  int i, j, i1, j1, ileft, iright, jleft, jright;
  int sideLimit, sideGap, verticalLimit;
  int ndownleft, nupright;
  int nupleft, ndownright;
  int patternFlag1=0;
  int patternFlag2=0;
  int patternDistance1, patternDistance2;

  verticalLimit=60/binx;
  sideLimit=20/binx;
  sideGap=6/binx;

  for (j = 0; j <= ny; j++) {
    for (i = 0; i <= nx; i++) { 
      auxImage[i][j]=0.0;
    }
  }

  for (j = 0; j <= ny; j++) {
    for (i = 0; i <= nx; i++) { 
      if (image[i][j] > 0.0) {

	/* Check for the presence of the symmetric pattern for the left-down anf right-up columns */
	patternFlag1=0;
	patternDistance1=0;
	for (i1 = sideGap; i1 <= sideLimit; i1++) {
	  ileft=i-i1;
	  iright=i+i1;
	  
	  
	  ndownleft=0;
	  nupright=0;
	  for (j1 = 1; j1 <= verticalLimit; j1++) {	    
	    jleft=j-j1;
	    jright=j+j1;
	    if (jleft>=0){
	      if (ileft>=0){
		if (image[ileft][jleft]>0){
		  ndownleft+=1;
		}
	      }
	      if (ileft-1>=0){
		if (image[ileft-1][jleft]>0){
		  ndownleft+=1;
		}
	      }
	      if (ileft-2>=0){
		if (image[ileft-2][jleft]>0){
		  ndownleft+=1;
		}
	      }

	    } /* if jleft1 */
	    
	    if (jright<=ny){

	      if (iright<=nx){
		if (image[iright][jright]>0){
		  nupright+=1;
		}
	      }
	      if (iright+1<=nx){
		if (image[iright+1][jright]>0){
		  nupright+=1;
		}
	      }
	      if (iright+2<=nx){
		if (image[iright+2][jright]>0){
		  nupright+=1;
		}
	      }

	    } /* if jright */
	    
	  } /* for j1*/
	  if (ndownleft>2 && nupright>2){ /* then the masked pixel belong to the pattern-1 */
	    patternFlag1=1;
	    patternDistance1=i1+1;
	    break;
	  }
	  
	} /* for i1*/

	/* Make the same checking for the left-up anf right-down columns */
	patternFlag2=0;
	patternDistance2=0;
	for (i1 = sideGap; i1 <= sideLimit; i1++) {
	  ileft=i-i1;
	  iright=i+i1;
	  
	  
	  nupleft=0;
	  ndownright=0;
	  for (j1 = 1; j1 <= verticalLimit; j1++) {	    
	    jleft=j+j1;
	    jright=j-j1;
	    if (jleft<=ny){
	      if (ileft>=0){
		if (image[ileft][jleft]>0){
		  nupleft+=1;
		}
	      }
	      if (ileft-1>=0){
		if (image[ileft-1][jleft]>0){
		  nupleft+=1;
		}
	      }
	      if (ileft-2>=0){
		if (image[ileft-2][jleft]>0){
		  nupleft+=1;
		}
	      }

	    } /* if jleft */
	    
	    if (jright>=0){

	      if (iright<=nx){
		if (image[iright][jright]>0){
		  ndownright+=1;
		}
	      }
	      if (iright+1<=nx){
		if (image[iright+1][jright]>0){
		  ndownright+=1;
		}
	      }
	      if (iright+2<=nx){
		if (image[iright+2][jright]>0){
		  ndownright+=1;
		}
	      }
	    } /* if jright */
	    
	  } /* for j1*/
	  if (nupleft>2 && ndownright>2){ /* then the masked pixel belong to the pattern-2 */
	    patternFlag2=1;
	    patternDistance2=i1+1;
	    break;
	  }
	  
	} /* for i1*/


	/* Flag the pixels identified as belonging to the pattern */
	if (patternFlag1==1){ /*down-left to up-right direction of the pattern*/
	  auxImage[i][j]=0;
	  auxImage[i-patternDistance1][j]=3;
	  auxImage[i+patternDistance1][j]=3;	  
	}
	else{
	  if(patternFlag2==1){ /* up-left to down-right direction of the pattern */
	    auxImage[i][j]=0;
	    auxImage[i-patternDistance2][j]=5;
	    auxImage[i+patternDistance2][j]=5;
	  }
	  else{
	    auxImage[i][j]=image[i][j];
	  }
	}


      } /* if image >0 */
    } /* for i */
  } /* for j */


  /* Pass the result to the input image */
  for (i = 0; i <= nx; i++) {
    for (j = 0; j <= ny; j++) {
      image[i][j] = auxImage[i][j];
    }
  }

  return code;
} /* end checkIntersectingColumns*/





/* Find the centres of the vertical lines corresponding to the bright sources */
int findSplittedCentres(int nx, int ny, float image[2048][2048],
		int binx)
{
  int code = 0;
  
  static float auxImage[2048][2048];
  int i, j, i1, j1, j2, ileft, iright, jdown, jup, jmiddle;
  int verticalLimit, verticalLimit2, sizeLimit;
  int verticalFlag;
  int horizontalLimit;
  int icolumn1, icolumn2, icolumnAver;
  int icolumn1_0;

  /*verticalLimit=160/binx;*/
  verticalLimit=200/binx;
  verticalLimit2=20/binx;
  horizontalLimit=10/binx;
  sizeLimit=4/binx;

  for (j = 0; j <= ny; j++) {
    for (i = 0; i <= nx; i++) {
      auxImage[i][j]=0;
    }
  }

  for (j = 0; j <= ny; j++) {
    for (i = 0; i <= nx; i++) {
     
      if (image[i][j] > 0.0) {
	jdown=j;
	icolumn1_0=i;
	icolumn1=i;
	icolumn2=i;
	icolumnAver=i;
	ileft=icolumn1-horizontalLimit;
	iright=icolumn1+horizontalLimit;

	/************************/
	/*auxImage[icolumn1][jdown]=3;*/
	/************************/
	image[icolumn1][jdown]=0;
	jup=-1;
	/* Check for the pixels upwards */
	for (j1 = 1; j1 <= verticalLimit; j1++) {
	  if (j+j1<ny){
	    verticalFlag=0;
	    for (j2=0;j2<=verticalLimit2; j2++){
	      if (j+j1+j2<ny){
		for (i1=ileft;i1<=iright;i1++){
		  if (i1>=0 && i1 <=nx){
		    if (image[i1][j+j1+j2]>0){
		      verticalFlag=1;
		      icolumn2=i1;
		    }
		  }
		} /* for i1 */
	      }
	      else{
		jup=j+j1+j2;
		/************************/
		/*auxImage[icolumn1][jup]=5;*/ 
		/************************/
		break; /* achieved the upper edge of the image */
	      }
	    } /* for j2 */


	    if (verticalFlag==0){
	      /* possible upper end of the line is found at j1*/

	      jup=j+j1; 
	      /************************/
	      /*auxImage[icolumn1][jup]=7;*/
	      /************************/
	      break;
	    }
	    else { /*vertical edge in not found: erase the checked pixels*/
	      for (i1=ileft;i1<=iright;i1++){
		if (i1>=0 && i1 <=nx){
		  image[i1][j+j1]=0;
		}
	      } /* for i1 */

	      if (icolumn2 != icolumn1){ 
		/*icolumnAver=(icolumn1+icolumn2)/2;*/
		icolumnAver=(icolumn1_0+icolumn2)/2;
		/*icolumn1=icolumn2;*/
		icolumn1=icolumnAver;
		ileft=icolumn1-horizontalLimit;
		iright=icolumn1+horizontalLimit;		
	      }

	      /************************/
	      /*auxImage[icolumn1][j+j1]=1;*/
	      /************************/
	    } /* verticalFlag==0 */
	  }
	  else { /* achieved the upper edge of the image */
	    jup=ny;
	    /************************/    
	    /*auxImage[icolumn1][jup]=9;*/
	    /************************/
	    break;
	  }
	} /* for j1 */
	/* Flag the middle point of the line */
	if (jup >= ny-2) /* then the source is likely to be closer to the upper edge of the image */
	  jmiddle=jup-(jup-jdown)/3;
	else
	  jmiddle=(jdown+jup)/2;
	if (jup-jdown>sizeLimit)
	  auxImage[icolumnAver][jmiddle]=jup-jdown;/*10;*/
  
      } /* if image >0 */
    } /* for i */
  } /* for j */


  /* Pass the result to the input image */
  for (i = 0; i <= nx; i++) {
    for (j = 0; j <= ny; j++) {
      image[i][j] = auxImage[i][j];
    }
  }

  return code;
} /* end findSplittedCentres */



/***************************************************************************/
/* This routine should unsplit the touching rectangles in the mask         */
/* in order to avoid mistaking two neighbour sources for one bright source */
/*                                                                         */
/*************************************************************************/
int splitOverlappingSources(int nx, int ny, float image[2048][2048],
		int binx, int hdu)
{
	int code = 0;

	static float auxImage[2048][2048];
	int i, j, i1;
	int sideLimit, sideLength, k1,k2;
	int maxNeighbours = 2;
	int code2 = 0;

	sideLimit=160/binx;

	for (j = 0; j <= ny; j++) {
	  for (i = 0; i <= nx; i++) {
	    auxImage[i][j]=0.0;
	  }
	}

	/* Check the vertical edges */
	for (j = 0; j <= ny; j++) {
	  for (i = 0; i <= nx; i++) {
	    if (image[i][j] > 0.0) {
	      k1=i; /* left edge of the square */
	      k2=i; /* right edge of the square */
	      /* Check the limits of the vertical sides of the squares */
	      for (i1 = i + 1; i1 <= i + sideLimit; i1++) {
		if (i1 >= nx) {
		  /* do nothing */
		  k2=nx;
		  break;
		}
		else {
		  /* Check whether the value is positive */  
		  if (image[i1][j] > 0 && image[i1+1][j]==0) { /* found the right edge */
		    k2=i1;
		    break;
		  }    
		}
	      } /*for loop */
	      sideLength=k2-k1;
	      /* If the length is larger than 2 set the source to be at the half-length from the edge */
	      if (sideLength >1){
		auxImage[i+sideLength/2][j] = image[i][j];
		i=i+sideLength;
	      } 
	    }
	  }
	}
	
	/*
	printf("L1405 dumping the vertical edge mask image .......\n");	
	code = writeFloatImage("dump_image2map.fits", auxImage);
	printf("L1408 ......... auxImage  (2) .\n");
	*/
	
	
	code2 = removeIsolatedPixels(nx, ny, auxImage, maxNeighbours);

	/*
	printf("L1415 dumping the vertical edge mask image .......\n");
	code = writeFloatImage("dump0_image2map.fits", auxImage);
	printf("L1417 ......... auxImage  (2) .\n");
	*/

	/*
	 * Check whether some of the rectangles were intersecting with each other 
	 */

	/*printf("L663 checking intersecting columns \n");*/
	code2=checkIntersectingColumns(nx, ny, auxImage, binx);

	/*
	printf("L1428 dumping after check intersecting columns  .......\n");	
	code2 = writeFloatImage("dump1_image2map.fits", auxImage);
	printf("L1430 ......... auxImage .\n");
	*/

	/*printf("L849 finding the splitted centres \n");*/
	code2=findSplittedCentres(nx, ny, auxImage, binx);

	/*
	printf("L1437 dumping after find splitted centres  .......\n");	
	code2 = writeFloatImage("dump2_image2map.fits", auxImage);
	printf("L1439 ......... aux .\n");
	*/

	/* Pass the result to the input image */
	for (i = 0; i <= nx; i++) {
	  for (j = 0; j <= ny; j++) {
	    image[i][j] = auxImage[i][j];
	  }
	}

	return code;
} /* end splitOverlappingSources*/


int checkConnectedPixels(float origImage[2048][2048], float image[2048][2048],
		float imaskImage[2048][2048], int xMin, int xMax, int yMin, int yMax,
		int i, int j, int label, float *sumX, float *sumY, float *sumValue,
		float *counts, int verbFlag, int currentNpix, int nestedLevel)
{
	int nPixels, nPixels0, nPixels1;
	int k, l;

	nPixels0 = 0;
	nPixels1 = 0;
	nPixels = 0;
	nestedLevel += 1;

	if (currentNpix > 10000) {
		nPixels = 0;
		return nPixels;
	}

	/*if (verbFlag == 1 && nestedLevel <50){
	 printf("L160 i= %d  j= %d  label= %d\n", i, j, label);
	 }*/

	for (k = i - 1; k <= i + 1; k++) {
		for (l = j - 1; l <= j + 1; l++) {
			if (k >= xMin && k <= xMax && l >= yMin && l <= yMax) {
				if (imaskImage[k][l] > 0.0) {
					nPixels0 += 1;
					currentNpix += 1;
					imaskImage[k][l] = -(float) label;
					if (image[k][l] > 0.0) {
						*sumValue += image[k][l];
						*sumX += (float) k * image[k][l];
						*sumY += (float) l * image[k][l];
						*counts += origImage[k][l];
					} /* image >0 */

					/* Apply the same procedure checkConnectedPixels recursively */
					nPixels1 = checkConnectedPixels(origImage, image,
							imaskImage, xMin, xMax, yMin, yMax, k, l, label,
							&(*sumX), &(*sumY), &(*sumValue), &(*counts),
							verbFlag, currentNpix, nestedLevel);
					nPixels0 += nPixels1;

				} /* if imaskImage */
			} /* if k */
		} /* for l */
	} /* for k*/
	nestedLevel -= 1;
	nPixels = nPixels0;
	return nPixels;
} /* end checkConnectedPixels */

/********************************************************************/
/* Extract the source counts from the standard 12-pix radius circle */
/* centered at the pixel with the coordinates (xsource,ysource)     */
/********************************************************************/
int get12pixRegionCounts(int nx, int ny, float origImage[2048][2048],
	      int binx, float xsource, float ysource, float sourceSizeFromMask)
{
  int nCounts = 0;
  
  int i, j, isrc, jsrc;
  int extractionRadius, extractionRadius2;
  float dx,dy,dist;
  float max1, standardRegion;

  extractionRadius=12/binx;
  extractionRadius2=(int)sourceSizeFromMask/2;
  standardRegion=3.14159*(float)extractionRadius*(float)extractionRadius;

  isrc=(int) xsource+0.5;
  jsrc=(int) ysource+0.5;
  /*printf("L937 i=%d  j=%d  sourceSizeFromMask=%f \n",isrc, jsrc, sourceSizeFromMask);*/
  if (sourceSizeFromMask<(float)(4*extractionRadius)){
    for (i=isrc-extractionRadius;i<=isrc+extractionRadius;i++){
      if (i>=0 && i<=nx){
	for (j=jsrc-extractionRadius;j<=jsrc+extractionRadius;j++){
	  if (j>=0 && j<=ny){
	    /* Compute the distance from the central source pixel */
	    dx=(float) i-isrc;
	    dy=(float) j-jsrc;
	    dist=sqrt(dx*dx+dy*dy);
	    if (dist <=(float)extractionRadius){/* The pixels is inside of the extraction circle */
	      nCounts+=origImage[i][j];
	    }
	  }/* if j>= 0*/
	} /* for j */
      } /* if i >=0 */
    } /* for i*/
  } /* if */
  else{ /* extract counts from a larger region */
    max1=-1.0E10;
    for (i=isrc-extractionRadius2;i<=isrc+extractionRadius2;i++){
      if (i>=0 && i<=nx){
	for (j=jsrc-extractionRadius2;j<=jsrc+extractionRadius2;j++){
	  if (j>=0 && j<=ny){
	    if (origImage[i][j]>max1) max1=origImage[i][j];
	    nCounts+=origImage[i][j];
	  }
	}
      }
    } /* for i*/    
    /*nCounts=max1*standardRegion;*/
  }/* else */
	
  return nCounts;
} /* end get12pixCounts*/

/**********************************************************************/
/* Converting the source mask (srcMaskImage) to the output source map */
/* image (task->smapImage) by extending the source regions from 1 pix */
/* to the standard 12-pix-radius circular regions */
/**********************************************************************/
int createSourceMap(Task * task, int nx, int ny, float origImage[2048][2048],
			 float srcMaskImage[2048][2048], int binx, float background, int hdu)
{
  int code = 0;
  
  static float auxImage[2048][2048];
  int i, j, isrc, jsrc, i1, j1;
  int extractionRadius;
  float dx,dy,dist;
  float z;
  float roundedBkg;
  int smoothingRadius;
  float aver, naver;
  float maxVal, mod8max;
  int mod8window;
  /*int code2=0;*/

  extractionRadius=12/binx;
  smoothingRadius=4/binx;

  roundedBkg=ceil(background)*3.0;
  /*
  printf("L743 background= %f (ceil) roundedBkg= %f \n", background, roundedBkg);
  */
  
  /* set the source map to a constant value */
  for (isrc=0;isrc<=nx;isrc++){
    for (jsrc=0;jsrc<=ny;jsrc++){
      /*z=roundedBkg;*/
      /*z=0.0;
	fimage_set_relative(task->smapImage,isrc,jsrc,z);*/
      auxImage[isrc][jsrc]=roundedBkg;
    }
  }

  /*
  if (hdu==3){
    printf("L1597 dumping srcMaskImage  ... hdu= %d \n", hdu);	
    code2 = writeFloatImage("hdu3_image2map.fits", srcMaskImage);
    printf("L1599 ......... srcMaskImage .\n");
  }
  */

  for (isrc=0;isrc<=nx;isrc++){
    for (jsrc=0;jsrc<=ny;jsrc++){

      /* If the source is larger than 2xExtraction radius then pass it into the map */
      /*if (srcMaskImage[isrc][jsrc] >  2*extractionRadius){*/
      if (srcMaskImage[isrc][jsrc] >  0.0){
	/* Expand the source to the 12-pix circle*/
	/*
	printf("---------------\n");
	printf("L1647 srcMaskImage isrc= %d  jsrc=%d value=%f \n", isrc, jsrc, srcMaskImage[isrc][jsrc]);
	*/
	/* If the value of the mask exceeds 4extractionRadius then */
	/* the source was likely mod-8 corrupted. In this case the source */
	/* map pixels should be set to the maximal value found within the*/
	/* square area with the side equal to the value of the mask */
	if ((int)srcMaskImage[isrc][jsrc]>4*extractionRadius){
	  /*
	  printf("L1681 the value %f  exceeds 4*extractionRadius (%d) \n",
		 srcMaskImage[isrc][jsrc], 4*extractionRadius);
	  */ 
	  mod8window=(int)srcMaskImage[isrc][jsrc]/2.0;
	  /* search for the maximal value */
	  mod8max=-1.0E10;
	  for (i=isrc-mod8window;i<=isrc+mod8window;i++){
	    if (i>=0 && i<=nx){
	      for (j=jsrc-mod8window;j<=jsrc+mod8window;j++){
		if (j>=0 && j<=ny){
		  if (origImage[i][j]>mod8max) mod8max=origImage[i][j];
		}
	      }/* for j*/
	    }
	  }/* for i*/
	  /*printf("L1696 mod8max=%f \n", mod8max);*/
	  /* Fill in the source region with the found mod8max value */
	  for (i=isrc-extractionRadius;i<=isrc+extractionRadius;i++){
	    if (i>=0 && i<=nx){
	      for (j=jsrc-extractionRadius;j<=jsrc+extractionRadius;j++){
		if (j>=0 && j<=ny){
		  /* Compute the distance from the central source pixel */
		  dx=(float) i-isrc;
		  dy=(float) j-jsrc;
		  dist=sqrt(dx*dx+dy*dy);
		  if (dist <=(float)extractionRadius){/* The pixels is inside of the extraction circle */
		    auxImage[i][j]=mod8max;
		  }
		}
	      }
	    }
	  }
	}/* if srcMaskImage > 4 extractionRadius */	      
	else{
	  for (i=isrc-extractionRadius;i<=isrc+extractionRadius;i++){
	    if (i>=0 && i<=nx){
	      for (j=jsrc-extractionRadius;j<=jsrc+extractionRadius;j++){
		if (j>=0 && j<=ny){
		  /* Compute the distance from the central source pixel */
		  dx=(float) i-isrc;
		  dy=(float) j-jsrc;
		  dist=sqrt(dx*dx+dy*dy);
		  if (dist <=(float)extractionRadius){/* The pixels is inside of the extraction circle */
		    z=origImage[i][j];
		    if (z>roundedBkg)
		      /*fimage_set_relative(task->smapImage,i,j,z);*/
		      auxImage[i][j]=z;
		    else{
		      if (dist <= smoothingRadius){/* smooth the source counts*/
			/* in order to reduce the mod-8 noise of the bright sources */
			aver=0.0;
			naver=0.0;
			maxVal=-100.0;
			/* first compute the average value for the cross centered at (i,j)*/
			for (i1=i-smoothingRadius;i1<i+smoothingRadius; i1++){
			  if (i1>=0 && i1 <=nx && i1 != i){
			    if (origImage[i1][j] > 6.0*roundedBkg){
			      aver+=origImage[i1][j];
			      naver+=1.0;
			      if (origImage[i1][j] > maxVal)
				maxVal=origImage[i1][j];
			    }
			  } 
			} /* for i1 */
			for (j1=j-smoothingRadius;j1<j+smoothingRadius; j1++){
			  if (j1>=0 && j1 <=ny && j1 != j){
			    if (origImage[i][j1] > 6.0*roundedBkg){
			      aver+=origImage[i][j1];
			      naver+=1.0;
			      if (origImage[i][j1] > maxVal)
				maxVal=origImage[i][j1];
			    }
			  } 
			} /* for i1 */
			if (naver >1.0)
			  aver/=naver;

			/* assign the average value to the pixels of the cross */
			/*fimage_set_relative(task->smapImage,i,j,aver);*/
			/*fimage_set_relative(task->smapImage,i,j,maxVal);*/
			
			if (aver > roundedBkg){
			  for (i1=i-smoothingRadius;i1<i+smoothingRadius; i1++){
			    if (i1>=0 && i1 <=nx){
			      /*fimage_set_relative(task->smapImage,i1,j,aver);*/
			      auxImage[i1][j]=aver;
			    } 
			  } 
			  for (j1=j-smoothingRadius;j1<j+smoothingRadius; j1++){
			    if (j1>=0 && j1 <=ny){
			      /*fimage_set_relative(task->smapImage,i,j1,aver);*/
			      auxImage[i][j1]=aver;
			    } 
			  } 
			} /* if aver > roundedBkg */


		      }
		    } /* else (if z>roundedBkg)*/
		  }
		}
	      } /* for j*/
	    }
	  }  /* for i*/
	}/* else if srcMaskImage > 4 extractionRadius */
      } /* if srcMaskImage>0*/
    } /* for jsrc */
  } /* for isrc */

  /*
  if (hdu==3){
    printf("L1711 dumping after find splitted centres  ... hdu= %d \n", hdu);	
    code2 = writeFloatImage("hdu3_image2map.fits", auxImage);
    printf("L1713 ......... aux .\n");
  }
  */

  /* Smooth the output image */
  report_verbose(" smoothing the source map image \n");
  for (isrc=0;isrc<=nx;isrc++){
    for (jsrc=0;jsrc<=ny;jsrc++){
      
      aver=0.0;
      naver=0.0;
      for (i=isrc-smoothingRadius/2;i<=isrc+smoothingRadius/2;i++){
	for (j=jsrc-smoothingRadius/2;j<=jsrc+smoothingRadius/2;j++){
	  if (i>=0 && i<=nx){
	    if(j>=0 && j<=ny){
	      aver+=auxImage[i][j];
	      naver+=1.0;
	    }
	  }
	}
      }
      if (naver >1.0)
	aver /= naver;
      
      /*aver=auxImage[isrc][jsrc];*/
      fimage_set_relative(task->smapImage,isrc,jsrc,aver);
      /*srcMaskImage[isrc][jsrc]=aver;*/
    }
  }

	
  return code;
} /* end createSourceMap*/

int correctBrightSourceCoordinates(Task * task, int nx, int ny, float origImage[2048][2048],
			 float srcMaskImage[2048][2048],  float srcMaskImage0[2048][2048], 
			 int binx, float background, int hdu)
{
  int code = 0;
  
  static float auxImage[2048][2048];
  int i, j, isrc, jsrc;
  int extractionRadius;
  float x,y, sumx, sumv;
  float roundedBkg;
  int smoothingRadius;
  int mod8window;
  int inew, jnew, inew0, jnew0;
  int count0;
  float sum1x, sum2x, sum1y, sum2y;

  /*
  printf("---------------------------\n");
  printf("L2688 correcting the source mask coordinates \n");
  */
  extractionRadius=12/binx;
  smoothingRadius=4/binx;

  roundedBkg=ceil(background)*3.0;

  for (isrc=0;isrc<=nx;isrc++){
    for (jsrc=0;jsrc<=ny;jsrc++){
      auxImage[isrc][jsrc]=0.0;
    }
  }
  
  for (isrc=0;isrc<=nx;isrc++){
    for (jsrc=0;jsrc<=ny;jsrc++){
      if (srcMaskImage[isrc][jsrc] >  0.0){

	mod8window=(int)srcMaskImage[isrc][jsrc]/2.0;
	/* First check the vicinity at srcMaskImage0 */
	/*
	if (isrc==848 && jsrc==450){
	  printf("L2709 isrc=%d jsrc=%d val=%f \n", isrc, jsrc, srcMaskImage[isrc][jsrc]); 
	}
	*/
	count0=0;
	sum1x=0.0;
	sum2x=0.0;
	sum1y=0.0;
	sum2y=0.0;
	inew0=-1;
	jnew0=-1;
	inew=-1;
	jnew=-1;
	for (i=isrc-mod8window;i<=isrc+mod8window;i++){
	  if (i>=0 && i<=nx){
	    for (j=jsrc-mod8window;j<=jsrc+mod8window;j++){
	      if (j>=0 && j<=ny){
		if (srcMaskImage0[i][j]>0)
		  count0+=1;
		sum1x+=origImage[i][j]*(float)i;
		sum2x+=origImage[i][j];
		sum1y+=origImage[i][j]*(float)j;
		sum2y+=origImage[i][j];
	      }
	    }/* for j*/
	  }
	}/* for i*/

	if (count0>0){
	  if (sum2x>1.0) sum1x/=sum2x;
	  if (sum2y>1.0) sum1y/=sum2y;
	  inew0=(int)sum1x+0.5;
	  jnew0=(int)sum1y+0.5;
	  /*
	  printf("L2717 isrc=%d jsrc=%d val=%f count0=%d inew0=%d jnew0=%d\n", 
		 isrc, jsrc, srcMaskImage[isrc][jsrc], count0, inew0, jnew0);
	  */
	}

	/* If the value of the mask exceeds 4extractionRadius then */
	/* the source was likely mod-8 corrupted.  */
	if ((int)srcMaskImage[isrc][jsrc]>4*extractionRadius){

	  /* compute the x-coordinate */
	  sumx=0.0;
	  sumv=0.0;
	  for (i=isrc-mod8window;i<=isrc+mod8window;i++){
	    if (i>=0 && i<=nx){
	      x=(float)i;
	      for (j=jsrc-mod8window;j<=jsrc+mod8window;j++){
		if (j>=0 && j<=ny){
		  sumx+=origImage[i][j]*x;
		  sumv+=origImage[i][j];
		}
	      }/* for j*/
	    }
	  }/* for i*/
	  if (sumv>1.0)
	    x=sumx/sumv;
	  else
	    x=(float)isrc;


	  /* compute the y-coordinate */
	  sumx=0.0;
	  sumv=0.0;

	  for (j=jsrc-mod8window;j<=jsrc+mod8window;j++){
	    if (j>=0 && j<=ny){
	      y=(float)j;
	      for (i=isrc-mod8window;i<=isrc+mod8window;i++){
		if (i>=0 && i<=nx){
		  sumx+=origImage[i][j]*y;
		  sumv+=origImage[i][j];
		}
	      }/* for i*/
	    }
	  }/* for j*/
	  if (sumv>1.0)
	    y=sumx/sumv;
	  else
	    y=(float)jsrc;

	  inew=(int)(x+0.5);
	  jnew=(int)(y+0.5);

	}/* if srcMaskImage > 4 extractionRadius */	      

	if (inew0>0 && jnew0>0){
	  auxImage[inew0][jnew0]=srcMaskImage[isrc][jsrc];
	}
	else{
	  if (inew>0 && jnew>0)
	    auxImage[inew][jnew]=srcMaskImage[isrc][jsrc];	  
	  else
	    auxImage[isrc][jsrc]=srcMaskImage[isrc][jsrc];
	}
      } /* if srcMaskImage>0*/

    } /* for jsrc */
  } /* for isrc */


  for (isrc=0;isrc<=nx;isrc++){
    for (jsrc=0;jsrc<=ny;jsrc++){
      srcMaskImage[isrc][jsrc]=auxImage[isrc][jsrc];
    }
  }
	
  return code;
} /* end correctBrightSourceCoordinates*/

int getMaskCentres(int nx, int ny, float origImage[2048][2048],
		float image[2048][2048], float outputImage[2048][2048], int ishift,
		FindSource sources[], int nSourcesMax, int binx)
{
	int nSources = 0;
	int xMin, xMax, yMin, yMax, i, j;
	float sumX, sumY;
	float sumValue = 0;
	int nPixels, nPixels0, currentNpix, verbFlag, nestedLevel;
	static float auxImage[2048][2048];
	int nAux = 0;
	float counts;
	int nExcess = 0;
	int standardRegionCounts=0;
	float xsource, ysource;
	float sourceSizeFromMask;

	FindSource *psource = 0;

	xMin = 0;
	xMax = nx;
	yMin = 0;
	yMax = ny;
	/* .......................*/
	/* Initialise the auxiliar images */
	for (i = xMin; i <= xMax; i++) {
		for (j = yMin; j <= yMax; j++) {
			auxImage[i][j] = image[i][j];
			outputImage[i][j] = image[i][j];
		}
	}

	sumValue = 0;
	sourceSizeFromMask=0;
	/* Find sources (assuming the pixels of each source are connected together)  */
	nSources = 0;
	nAux = 0;
	for (i = xMin; i <= xMax; i++) {
		for (j = yMin; j <= yMax; j++) {
			if (outputImage[i][j] > 0) {
			        sourceSizeFromMask=outputImage[i][j];
				psource = &sources[nSources];

				/* A source is found */
				if (nSources < nSourcesMax)
					nSources += 1;
				else
					++nExcess;

				sumX = 0;
				sumY = 0;
				sumValue = 0;
				counts = origImage[i][j];

				outputImage[i][j] = -nSources;
				nPixels = 1; /* The number of connected pixels for this source */

				sumX += (float) i * auxImage[i][j];
				sumY += (float) j * auxImage[i][j];
				sumValue += auxImage[i][j];

				currentNpix = 0;
				verbFlag = 0;
				/*if (nSources==1 && nAux ==0) verbFlag=1;*/
				nestedLevel = 0;

				/* Use a recursive subroutine to find the connected pixels */
				nPixels0 = checkConnectedPixels(origImage, auxImage,
						outputImage, xMin, xMax, yMin, yMax, i, j, nSources,
						&sumX, &sumY, &sumValue, &counts, verbFlag,
						currentNpix, nestedLevel);

				nPixels += nPixels0;

				if (sumValue > 1.0E-6){
				  xsource=sumX / sumValue;
				  ysource=sumY / sumValue;
				}
				else{
				  xsource=(float) i;
				  ysource=(float) j;
				}

				psource->id = nSources;
				psource->x = xsource;
				psource->y = ysource;
				psource->sourceSizeFromMask=sourceSizeFromMask;

				/********************************************************************/
				/* The source-mask subroutine in the task version of 2010.05.08     */
				/* goves the regions of one-pixel radius for each source.           */
				/* So, the source counts have to be re-extracted using              */
				/* the standard 12-pixel radius circle                              */
				/********************************************************************/
				standardRegionCounts=get12pixRegionCounts(nx, ny, origImage, binx, 
						       xsource,ysource, sourceSizeFromMask);
				
				psource->radius = 0.5 * sqrt((float) nPixels);
				psource->value = (float) standardRegionCounts;
				/*
				printf("L2269 nSources=%d x=%f y=%f counts=%d \n", 
				       nSources, xsource, ysource, standardRegionCounts);
				*/

			} /* if src */
		} /* for j */
	} /* for i */

	report_verbose("finished bright source detection, nSources=%d\n", nSources);

	if (nExcess > 0)
		report_warning("exceeded source array space- merged %d last sources into a single source\n", nExcess);

	return nSources;
} /* end getMaskCentres*/


int computeBackground(Task * task)
{
	int code = 0;

	int binx, dx, nx1, ny1, nx, ny;
	int i, i0, il, iu;
	int j, j0, jl, ju;

	float z, fmax, null;
	double faver, naver;

	float z0 = 0;

	binx = task->binx;
	dx = 4 / binx;
	nx1 = task->rawImage->width;
	ny1 = task->rawImage->height;
	nx = task->rawImage->width - 1;
	ny = task->rawImage->height - 1;
	null = task->rawImage->null;

	i0 = (int) nx1 / 2;
	j0 = (int) ny1 / 2;

	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;

	fmax = -1000;
	faver = 0;
	naver = 0;
	for (i = il; i <= iu; ++i) {
		for (j = jl; j <= ju; ++j) {
			z = fimage_get_relative(task->rawImage, i, j);
			faver += z;
			naver++;
			fimage_set_relative(task->smapImage, i, j, z);
			if (z > fmax)
				fmax = z;
		}
	}
	if (naver > 1) {
		faver /= naver;
	}
	printf("L936 I) faver=%f \n ", faver);
	/* remove everything that is larger than the average */
	for (i = il; i <= iu; ++i) {
		for (j = jl; j <= ju; ++j) {
			z = fimage_get_relative(task->rawImage, i, j);
			if (z > faver)
				fimage_set_relative(task->smapImage, i, j, z0);
		}
	}

	/* Recompute the average*/
	fmax = -1000;
	faver = 0;
	naver = 0;
	for (i = il; i <= iu; ++i) {
		for (j = jl; j <= ju; ++j) {
			z = fimage_get_relative(task->smapImage, i, j);
			if (z > 0) {
				faver += z;
				naver++;
				if (z > fmax)
					fmax = z;
			}
		}
	}
	if (naver > 1) {
		faver /= naver;
	}
        printf("L963 II) image average faver=%f \n", faver);
	

	return code;
} /* end computeBackground*/


int findSources(Task * task, int hdu)
{
	int code = 0;

	int binx, dx, nx1, ny1, nx, ny;
	int i, i0, il, iu;
	int j, j0, jl, ju;
	float z, z1, fmax, null;
	double faver, naver;

	int code2 = 0;

	static float origImage[2048][2048];
	static float srcMaskImage0[2048][2048];
	static float srcMaskImage[2048][2048];
	static float enhImage[2048][2048];
	static float srcImage[2048][2048];
	/*static float auxImage[2048][2048];*/
	static float annImage[2048][2048]; /* For storing the smoke-ring mask */

	float positiveFraction = 0;
	int ishift;
	float averEnh = 0;

	FindSource sources[2000];
	int nSourcesMax = sizeof(sources) / sizeof(FindSource);
	int nSources = 0;
	int maxNeighbours = 1;
	int nBrightSources = 0;

	const Constants * k = task->constants;

	binx = task->binx;
	dx = 4 / binx;
	ishift = 16 / binx; /* The width of the sub-window for image enhancement */
	task->smoothSize = ishift;

	nx1 = task->rawImage->width;
	ny1 = task->rawImage->height;
	nx = task->rawImage->width - 1;
	ny = task->rawImage->height - 1;
	null = task->rawImage->null;

	i0 = (int) nx1 / 2;
	j0 = (int) ny1 / 2;

	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;

	/*printf("L1982 --------------> hd= %d \n", hdu);*/

	/* Fill in the original image array and calculate the image average */
	fmax = -1000;
	faver = 0;
	naver = 0;
	for (i = il; i <= iu; ++i) {
		for (j = jl; j <= ju; ++j) {
			z = fimage_get_relative(task->rawImage, i, j);
			if (z>0){
			  faver += z;
			  naver++;
			}
			fimage_set_relative(task->smapImage, i, j, z);
			if (z > fmax)
				fmax = z;
			origImage[i][j] = z;
		}
	}
	if (naver > 1)
		faver /= naver;

	/* Compute the fraction of non-zero pixels */
	positiveFraction = computePositiveFraction(nx, ny, origImage);

	report_verbose("fraction on the non-zero pixels = %E \n", positiveFraction);

	/*****************************************************/
	/* Check for the presence of annulus-like structures */
	/******************************************************/
        code2 = checkAnnularStructures(nx, ny, origImage, annImage, faver, binx);

	report_verbose(" the number of annular (smoke-ring) features found:  %d \n", code2);

	/*
	printf("L2628 dumping the annular masked image .......\n");
	code = writeFloatImage("ann1_image2map.fits", annImage);
	printf("L2630 ......... annImage .\n");
	*/

	if (code2)
	  code2 = maskAnnularStructures(nx, ny, origImage, annImage, faver, binx);

		
	/*	
	printf("L2036 dumping the annular masked image .......\n");
	code = writeFloatImage("ann2_image2map.fits", origImage);
	printf("L2038 ......... enhImage .\n");
	*/
	

	code2 = fillMod8gaps(nx, ny, origImage, faver, ishift);
	
	/*
	printf("L2476 dumping the gap-filled image .......\n");	 
	code = writeFloatImage("gaps_image2map.fits", origImage);
	printf("L2478 ......... origImage .\n");
	*/

	code2=createMaskNormal(nx, ny, origImage,srcMaskImage0, faver, ishift, binx);
	/*
	printf("L3144 dumping the normal source mask image .......\n");
	code = writeFloatImage("smask0_image2map.fits", srcMaskImage0);
	printf("L3146 ......... srcMaskImage0 .\n");
	*/


	/* Enhance the image to ensure that the images with short 
	 exposures and with many zero-valued pixels are processed properly */
	code2 = enhanceImage(nx, ny, origImage, enhImage, ishift);

	/*	
	printf("L2366 dumping the enhanced image .......\n");	 
	code = writeFloatImage("enh_image2map.fits", enhImage);
	printf("L2368 ......... enhImage .\n");
	*/


	/*------------------------------------------*/	
	/* Create the source mask image             */
	/*------------------------------------------*/
	code2=createMaskEnhanced(nx, ny, enhImage,srcMaskImage,&averEnh,ishift);
	/*code2=createMaskEnhanced2(nx, ny, enhImage,srcMaskImage,&averEnh,ishift);*/

	/*
	printf("L1205 average of the enhanced image= %5.1f \n", averEnh);
	*/

	/* the result  (srcMaskImage) is the source mask for bright sources*/
	
	/*	
	printf("L2172 dumping the source mask image .......\n");
	code = writeFloatImage("smask_image2map.fits", srcMaskImage);
	printf("L2174 ......... srcMaskImage .\n");
	*/

	report_verbose(" storing the enhanced image for further extended source detection  \n");
	/* Store the enhanced image for further use in the extended source detection */
	for (i = il; i <= iu; ++i) {
		for (j = jl; j <= ju; ++j) {
		  if (srcMaskImage[i][j]){
		    z=-2;
		    fimage_set_relative(task->extendedImage, i, j, z);
		  }
		  else{
		    z = enhImage[i][j];
		    fimage_set_relative(task->extendedImage, i, j, z);
		  }
		}
	}
	

	/* Recompute the average for the pixels below the threshold averEnh*/
	fmax = -1000;
	faver = 0;
	naver = 0;
	for (i = il; i <= iu; ++i) {
		for (j = jl; j <= ju; ++j) {
			z = fimage_get_relative(task->smapImage, i, j);
			z1 = enhImage[i][j];
			if (z1 < averEnh) {
			  faver += z;
			  naver++;
			  if (z > fmax)
			    fmax = z;
			}
		}
	}
	if (naver > 1) {
		faver /= naver;
	}
	/*
	printf("L1099 bkg average after masking faver= %f \n ", faver);
	*/

	/* Remove isolated pixels from the mask*/
	maxNeighbours = 3;
	code2 = removeIsolatedPixels(nx, ny, srcMaskImage, maxNeighbours);
	
	/*
	printf("L2104 dumping the source mask image (no isolated pixels) ...hdu= %d ..\n", hdu);	
	code = writeFloatImage("noisolated_image2map.fits", srcMaskImage);
	printf("L2107 ......... srcMaskImage .\n");
	*/
		

	report_verbose("splitting overlapping sources  \n");
	/*************************************************************/
	/* Split those masked rectangles that overlap with each other */
	code2 = splitOverlappingSources(nx, ny, srcMaskImage, binx, hdu);

	/*		
	printf("L2647 dumping the split-square source mask image ..hdu= %d \n", hdu);
	code = writeFloatImage("split_image2map.fits", srcMaskImage);
	printf("L2649 ......... srcMaskImage (1) .\n");
	*/

	/* Check the correctness of the brightest source coordinates */
	code2=correctBrightSourceCoordinates(task, nx, ny, origImage, srcMaskImage, 
					     srcMaskImage0, binx, faver, hdu);

	
	/*		
	printf("L2655 dumping the more accurate source mask image ..hdu= %d \n", hdu);
	code = writeFloatImage("split2_image2map.fits", srcMaskImage);
	printf("L2657 ......... srcMaskImage (1) .\n");
	*/


	/* Use the obtained source mask to create the output source map */
	code2=createSourceMap(task, nx, ny, origImage, srcMaskImage, binx, faver, hdu);

	/*
	printf("L2129 dumping the split-square source mask image ..hdu= %d \n", hdu);
	code = writeFloatImage("split3_image2map.fits", srcMaskImage);
	printf("L2131 ......... srcMaskImage (2) .\n");
	*/

	/* removing everything that is larger than the 2*enhanced average */
	/* (this image will be used to detect the vertical readout streaks) */
	for (i = il; i <= iu; ++i) {
		for (j = jl; j <= ju; ++j) {
			z = fimage_get_relative(task->rawImage, i, j);
			z1 = enhImage[i][j];
			if (z1 < 1.0 * averEnh) {
			  /*auxImage[i][j] = z;*/
			  fimage_set_relative(task->vstreakImage, i, j, z);
			}
			else {
			  /*auxImage[i][j] = -2.0;*/
			  z=-2.0;
			  fimage_set_relative(task->vstreakImage, i, j, z);
			}
		}
	}


	/* Find the centres of the sources covered with the bright source mask srcMaskImage
	 (the original image is origImage)*/
	nSources = getMaskCentres(nx, ny, origImage, srcMaskImage, srcImage,
			ishift, sources, nSourcesMax, binx);

	report_verbose("the nuumber of found sources = %d \n", nSources);
	
	/*
	printf("--------------------\n");
	for (i = 0; i < nSources; ++i) {
		FindSource *s = &sources[i];
		printf("L524 i=%2d x= %5.1f y= %5.1f r= %5.1f value= %5.1f \n",
				s->id, s->x, s->y, s->radius, s->value);
	}
	*/


	/* Allocate the memory for the bright source list */

	task->nSources = nSources;
	task->source = calloc(nSources, sizeof(Source));

	/* replace the old source list with the new one */

	for (i = 0; i < nSources; i++) {
		FindSource *s = &sources[i];
		/*
			Bob: Aren't xRaw and xImage only the same if the binning is 1x1
			and WINDOWX0 is 0?
		*/
		task->source[i].xRaw = s->x;
		task->source[i].yRaw = s->y;
		task->source[i].xImage = s->x;
		task->source[i].yImage = s->y;
		task->source[i].majorAxis_imagePixels = s->radius;
		task->source[i].minorAxis_imagePixels = s->radius;
		task->source[i].rawCounts = s->value;
		task->source[i].rawRate = s->value / task->exposure;
	}

	/*
	report_verbose("---------------------------------------\n");
	report_verbose("Sources with count rates > 0 counts/s: \n");
	report_verbose("---------------------------------------\n");
	for (i = 0; i < nSources; i++)
	  report_verbose(
			 " i= %4d    x= %7.1f    y= %7.1f    rate= %7.1f\n",
			 i, task->source[i].xRaw, task->source[i].yRaw,
			 task->source[i].rawRate);
	report_verbose("---------------------------------------\n");
	*/

	nBrightSources = 0;
	for (i = 0; i < nSources; i++) {
	  if (task->source[i].rawRate > 30.0)
	    nBrightSources += 1;
	}

	if (nBrightSources > 0) {
	        report_verbose("---------------------------------------\n");
		report_verbose("Sources with count rates > 30 counts/s: \n");
		report_verbose("---------------------------------------\n");
		for (i = 0; i < nSources; i++)
		   {
			if (task->source[i].rawRate > 30.0)
		  
				report_verbose(
						" i= %4d    x= %7.1f    y= %7.1f    rate= %7.1f\n",
						i, task->source[i].xRaw, task->source[i].yRaw,
						task->source[i].rawRate);
		   }
		report_verbose("---------------------------------------\n");
	}
	else {
		report_verbose("Sources with rates > 60 counts/s:  ..... NONE \n");
	}


	/* Do the same for the old source list */

	for (i = 0; i < nSources; i++) {
		double scaled_imagePixels;
		double pointExtendedScale = 1;

		Source * s = &task->source[i];
		s->id = i + 1;

		/* the image was smeared by 16 pixels, so reduce the source sizes*/
		s->minorAxis_imagePixels /= 8;
		s->majorAxis_imagePixels /= 8;
		scaled_imagePixels = s->minorAxis_imagePixels * pointExtendedScale;
		s->isPointSource = scaled_imagePixels < k->psf_fwhm_imagePixels;
	}

	return code;
} /* end findSources*/


/* Find vertical readout streaks by the horizontal-shift-and-subtract method */

int checkBrightSources(Task * task, int hdu)
{
	int code = 0;
	/*
	report_verbose("computing background \n");
	
	if (!code)
		code = computeBackground(task);
	*/
	report_verbose("trying to find bright sources \n");
	if (!code)
		code = findSources(task, hdu);

	return code;
}

