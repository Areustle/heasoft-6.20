/*****************************************************************************
* This is a sample program illustrating how to use the librdd.a library
* This annotated example should show you everything you will need 
* for general programming tasks. The library contains many more functions,
* but those are primarilly used internally.
*
* To compile this you will need to link to the CFITSIO library and to librdd.a
*
*****************************************************************************/
#include <rdd.h>

main() {


/***********************
* variable definitions *
***********************/
int sis=0; /* instruiment number 0, or 1 */
int ccd=1; /* CCD ID 0, 1, 2, or 3 */
int ccdmode=4; /* CCD clocking mode 1, 2, or 4 */

double time=1e8; /* ascatime in seconds */
double accuracy=1e-5; /* allowed error in numerical calculations */

char calfile[]={"sis_rdd_1999-06-24.fits"}; /* RDD calibration file */

int i; /* loop index */
double pha; /* PHA value */
double model; /* value of the model function */
double model2; /* RDD model of two combined pixels */

RDDPARAM* rdd; /* structure to hold RDD model parameters */

/****************************************************
* allocate memory for RDD model parameter structure *
****************************************************/
rdd=allocateRDDparam();

/**********************************************************
* read the RDD model parameters from the calibration file *
**********************************************************/
readRDDparam(rdd,calfile,accuracy,sis,ccd,ccdmode,time);

/***************************************************************************
* adjust the zero point of the distribution -
* The PHA=0 point of the distribution is generally arbitrary.
* This function sets the zero point in one of three ways:
* 
* RDD_PEAK_ZERO - sets the peak of the distribution to zero
* RDD_MEAN_ZERO - sets the mean PHA to zero
* RDD_4040_ZERO - sets the PHA mean between mean-40 and mean+40 to zero.
* This last options mimics the onboard dark level subtraction
***************************************************************************/
adjustZeroOfRDDmodel(rdd,RDD_PEAK_ZERO);

/***********************************************************************
* loop over a number of PHA values and tabulate the RDD model function *
***********************************************************************/
for(i=-60;i<=150;++i) {

    pha=(double)i;

    /********************************************************************
    * this function calculates the actual model function at a given PHA *
    ********************************************************************/
    model=calculateRDDmodel(rdd,pha);

    /***********************************************************
    * this is the RDD model function for the sum of two pixels *
    * This is just the RDD model convolved with itself.
    ***********************************************************/
    model2=multiPixelRDDmodel(rdd,pha,2);

    
    /********************
    * print the results *
    ********************/
    printf("%g %g %g\n", pha, model, model2);


} /* end of loop over PHA values */


/***********************************************************
* free the memory associated with the RDD model parameters *
***********************************************************/
destroyRDDparam(rdd);


exit(0);
}
