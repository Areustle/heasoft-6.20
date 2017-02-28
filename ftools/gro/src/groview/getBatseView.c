#include "groview.h"
void getBatseView(float **SpaceCor, float **B)
{
     float Btmp[8], *S=NULL;
     int i;
     static float B0[3] = {0.578520,  0.574750,  0.578780 }; /*!BASTE LAD*/
     static float B1[3] = {0.576830,  0.576900, -0.578310 }; /*orientations,*/
     static float B2[3] = { 0.576860, -0.578030,  0.577150};  /*!in SC frame*/
     static float B3[3] = { 0.577260, -0.578030, -0.577150}; 
     static float B4[3] = { -0.577750,  0.575240,  0.579060}; 
     static float B5[3] = { -0.576690,  0.579470, -0.575890}; 
     static float B6[3] = { -0.575970, -0.576450,  0.579610}; 
     static float B7[3] = { -0.577010, -0.577720, -0.577320};     
     long ndim = 3;

     /* COMPUTE DOT PRODUCTS, i.e. DIRECTION COSINES FOR BATSE LAD's*/

     Btmp[0]  =  getDotProd(SpaceCor, B0, &ndim);
     Btmp[1]  =  getDotProd(SpaceCor, B1, &ndim);
     Btmp[2]  =  getDotProd(SpaceCor, B2, &ndim);
     Btmp[3]  =  getDotProd(SpaceCor, B3, &ndim);
     Btmp[4]  =  getDotProd(SpaceCor, B4, &ndim);
     Btmp[5]  =  getDotProd(SpaceCor, B5, &ndim);
     Btmp[6]  =  getDotProd(SpaceCor, B6, &ndim);
     Btmp[7]  =  getDotProd(SpaceCor, B7, &ndim);

	  /* ASSUME ZERO FOR <= 90 DEGREE INCIDENCE*/
     *B = (float *) malloc((long)(8) * sizeof(float));
     
	(*B)[0] = Btmp[4];     /* q'n'd fix to coord-inversion */
	(*B)[1] = Btmp[5];     /* problem*/
	(*B)[2] = Btmp[6];
	(*B)[3] = Btmp[7];
	(*B)[4] = Btmp[0];
	(*B)[5] = Btmp[1];
	(*B)[6] = Btmp[2];
	(*B)[7] = Btmp[3]; 
}
