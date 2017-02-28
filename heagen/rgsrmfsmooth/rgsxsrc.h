
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "exsource.h"

#define MMIN(a,b) ((a)<(b)?(a):(b))
#define MMAX(a,b) ((a)>(b)?(a):(b))

#define NCONV 8192
#define HC 12.398

void rgsxsrc
(float *ear,int ne,float *param,int ifl,float *photar, char *parfile);

void convlv(float data[],int n, float respns[], int m, int isign,float ans[]);
void four1(float data[], int nn, int isign);
void hunt(float xx[],int n, float x, int *jlo);
void nrerror(char error_text[]);
float *nrvector(long nl, long nh);
void free_nrvector(float *v, long nl, long nh);
void realft(float data[], int n, int isign);
void twofft(float data1[], float data2[], float fft1[], float fft2[], int n);


