#include "spibkg_common.h" //includes string.h and xsTypes.h
/* #include <XSContainer.h>  */
/* #include <XSModel/GlobalContainer/DataContainer.h> */
/* #include <DataContainer.h> */
#include <Error.h> 
#include <math.h>
#include <fitsio.h>
#include <iostream>
/* #include <Error/Error.h> */

using namespace std;
using namespace XSutility;

//int* sorted_indices = 0;
//Real** bkg=0;

auto_array_ptr<auto_array_ptr<float> > bkg;
auto_array_ptr<int> sorted_indices;

int getNumSpectra(string fileName)
{
    using namespace std;
    fileName += "[1]";

    fitsfile* file;
    int status = 0, nRows;

    fits_open_file(&file, fileName.c_str(), READONLY, &status);

    /*
    if(status)
	throw YellowAlert("Error opening file: " + fileName);
    */

    fits_read_key(file, TINT, "NAXIS2", &nRows, 0, &status);

    /*
    if(status)
	throw YellowAlert("Error reading from file: " + fileName);
    */

    fits_close_file(file, &status);

    return nRows;
}

void initSpectralTemplates(const string& strFileName, int numFlux, 
			   int numRows, const Real* energy)
{
    fitsfile* file;
    int status = 0, nullVal = 0;
    auto_array_ptr<float>
	bkgrndVals(new float[numFlux]),
	av_bkg(new float[numFlux]),
	sigma(new float[numFlux]);

    fill(av_bkg.get(), av_bkg.get() + numFlux, 0);
    fill(sigma.get(), sigma.get() + numFlux, 0);
    
    fits_open_file(&file, (strFileName + "[1]").c_str(), READONLY, &status);
    
    int i, j;

    //read background template values from the second column
    for(i = 0; i < numRows; ++i)
    {
	fits_read_col(file, TFLOAT, 2, i + 1, 1, numFlux, 
		      &nullVal, bkgrndVals.get(), 0, &status);

	/*
	if(status)
	    throw YellowAlert("Error reading column data from file: " + strFileName);
	*/

	for(j = 0; j < numFlux; ++j)
	    bkg[i][j] = bkgrndVals[j];
    }

    //form sum of background terms and average result
    for(i = 0; i < numFlux; ++i)
    {
	for(j = 0; j < numRows; ++j)
	    av_bkg[i] += bkg[j][i];

	av_bkg[i] /= numRows;
	sorted_indices[i] = i;
    }

    //compute standard deviation to identify most variable channels
    for(i = 0; i < numFlux; ++i)
	for(j = 0; j < numRows; ++j)
	  //	    sigma[i] += ((av_bkg[i] - bkg[j][i]) * 2);
	    sigma[i] += pow((av_bkg[i] - bkg[j][i]), 2);

    for(i = 0; i < numFlux; ++i)
    {
	sigma[i] = sqrt(sigma[i] / (numFlux - 1));

	Real calc = (energy[i - 1] + energy[i]) / 2;

	if(calc >= 1380 && calc < 1650)
	    sigma[i] = -1;
    }

    //apply a bubble sort algorithm to rank bins in terms of sigma
    for(j = 0; j < numFlux; ++j)
	for(i = 0; i < numFlux - 1; ++i)
	    if(sigma[i + 1] > sigma[i]) 
	    {
		Real tmp = sigma[i];
		sigma[i] = sigma[i + 1];
		sigma[i + 1] = tmp;

		tmp = sorted_indices[i];
		sorted_indices[i] = sorted_indices[i + 1];
		sorted_indices[i + 1] = (int)tmp;
	    }
}

int initialize(const Real* energy, int Nflux)
{
    int numRows;
    string strFileName;
    char* fileName;

    if(!(fileName = getenv("SPIBKG_INP_DAT")))
	strFileName = string("spibkg_inp_dat.fits");
    else
	strFileName = string(fileName);

    numRows = getNumSpectra(strFileName);

    if(numRows > maxBkgSpecs)
	cout << "-*** Warning: Too Many Spectra ***-" << endl;

    bkg.reset(new auto_array_ptr<float>[numRows]);

    for(int i = 0; i < numRows; ++i)
	bkg[i].reset(new float[Nflux]);

    sorted_indices.reset(new int[Nflux]);

    initSpectralTemplates(strFileName, Nflux, numRows, energy);
    
    return numRows;
}


size_t getRowNumber(int specNum)
{
  /*
   size_t rowNum = 0;
   if (!XSContainer::datasets->dataSetLookup(static_cast<size_t>(specNum),rowNum))
   {
      cout << "***Error - Cannot find spectrum number: " << specNum <<endl;      
   } 
   return rowNum;   
  */
  return 1;
}

