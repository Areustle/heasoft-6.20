#include "atFunctions.h"
#include "atError.h"
#include <stdio.h>
#include <string.h>

/*
*  test atSetElement3 to read orbital fits file with two added elements
*                     930128
*/

sample_()
{
    static char file[]= "orb_fitsfile";
    static AtTime time1 = { 1993,2,10,2,9,0,0. };
    double mjd0;
    int i, kchk = 0;

    printf("\n### Sample program for atSetElement2 ###\n");

    printf("Orbital Element File Name : %s\n",file);
    printf("\nTime for which the elements are requested :\n");
    printf("%5d yy",time1.yr); 
    printf("%5d mo",time1.mo); 
    printf("%5d dy",time1.dy); 
    printf("%5d hr",time1.hr); 
    printf("%5d mn",time1.mn); 
    printf("%5d sc",time1.sc); 
    printf("%10.2f ms\n",time1.ms); 

    atMJulian(&time1, &mjd0);

    printf("In MJD : %10lf\n", mjd0);

    atSetElement2(file, mjd0, kchk);
    
    printf("\nOrbital Element epoch\n");
    printf("%5d yr",atElement.itz.yr);
    printf("%5d mo",atElement.itz.mo);
    printf("%5d dy",atElement.itz.dy);
    printf("%5d hr",atElement.itz.hr);
    printf("%5d mn",atElement.itz.mn);
    printf("%5d sc",atElement.itz.sc);
    printf("%10.2f ms\n",atElement.itz.ms);
    printf("In MJD :  ");
    printf("%f\n", atElement.mjdz);
    printf("   orbital Elements\n");
    printf("   semiax %15.6e",atElement.semiax);
    printf("   eccent %15.6e",atElement.eccent);
    printf("   Aincln %15.6e\n",atElement.aincln);
    printf("   ragome %15.6e",atElement.ragome);
    printf("   saome  %15.6e",atElement.smaome);
    printf("   omean0 %15.6e\n",atElement.omean0);
    printf("   adot   %15.6e",atElement.adot);
    printf("   eccdot %15.6e",atElement.eccdot);
    printf("   aindot %15.6e\n",atElement.aindot);
    printf("   ragdot %15.6e",atElement.ragdot);
    printf("   smodot %15.6e\n",atElement.smodot);

}











