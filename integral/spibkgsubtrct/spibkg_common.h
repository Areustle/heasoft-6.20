#ifndef SPIBKG_COMMON_H
#define SPIBKG_COMMON_H

#ifdef __cplusplus
#include <xsTypes.h>
#include <string>
#include <memory>
#include <XSutility.h>


const int maxBkgSpecs  = 15000;
const int maxSpecChans = 2048;

using namespace XSutility;

extern auto_array_ptr<auto_array_ptr<float> > bkg;
extern auto_array_ptr<int> sorted_indices;

int getNumSpectra(string fileName);
void initSpectralTemplates(const string& strFileName, int numFlux, 
			   int numRows, const Real* energy);
int initialize(const Real* energy, int Nflux);
size_t getRowNumber(int specNum);
#endif

#define       epsilon           0.000001 /* used to avoid div-by-zero's           */

void writebintable( );
void copyhdu( );
void selectrows( );
void readheader();
void readimage();
void readtable();
void printerror( int status);

int  get_params(char *, char *, char * , char *, int *);
int  get_spec_info( char *, int *, int * );
int  get_spec_dat( char *, float[], float[], float[], float[], int, int);
int  wrtPHAoutp( char *, float[], int, char * , int);

#endif
