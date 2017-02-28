/** \file legacy_heasp.c                                                                                                                                  
    \brief HEADAS Simulator - contains legacy heasp routines in C
    \author  David Riethmiller                                                                                                                      
    \date $Date: 2015/02/09 14:53:08 $                                                                                                              
*/

#include "heasim.h"
#ifndef use_legacy_heasp
#include "Cheasp.h"       /* C-interface to C++ HEASP library */
#endif

#include "fitsio.h"       /* cfitsio defined constants */
#include "headas.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
//#include <xlocale.h>
#include <headas_rand.h>
#include <sys/time.h>
#include <string.h>



/* FUNCTION NAME: C_ReturnChannel                                                                 */
/*                                                                                                */
/* CALLING SEQUENCE:                                                                              */
/*    C_ReturnChannel(&rmf_struct, ebin_mid[ien], int_specout[ien], pi_chans);                    */
/*                                                                                                */
/* PURPOSE:                                                                                       */
/*    A C-language version of the C++ HEASP library routine, because the C/C++ interface          */
/*    conversion between struct and object was a major bottleneck in the code.  Returns the       */
/*    output PI channel from the input energy according to the RMF.                               */
/*                                                                                                */
/* INPUTS:                                                                                        */
/*   rmf, energy, NumberPhoton                                                                    */
/*                                                                                                */
/* OUTPUTS:                                                                                       */
/*   channel                                                                                      */
/*                                                                                                */
/* NOTES:                                                                                         */
/*   Modified from original version to use Mersenne Twister random generator.                     */
/*                                                                                                */
/* SUBROUTINES: MTseed()                                                                          */
/*                                                                                                */
/* CALLED BY:                                                                                     */
/*   spectra()                                                                                    */
/*                                                                                                */

void C_ReturnChannel(struct RMF * rmf, float energy, long NumberPhoton, long *channel, HDmt_state *MTstate){

    long upper, lower, middle, energybin;
    int i, j, igrp, ivec, ielt;
    float *sumresponse;
    float *RandomNumber;

    RandomNumber = (float *) calloc(NumberPhoton, sizeof(float));
    sumresponse = (float *) calloc(rmf->NumberChannels, sizeof(float));

    /* initialize the output array to -1s in the event that either the input energy is                                                              
       outside the response range or that the response does not sum to unity and events                                                             
       can all off the end of the channels. */

    for (i=0; i<NumberPhoton; i++) channel[i] = -1;

    lower = 0;
    upper = rmf->NumberEnergyBins-1;

    /* trap the case of the energy being outside the response range */
    if ( energy < rmf->LowEnergy[lower] || energy > rmf->HighEnergy[upper] ) return;

    /* find the energy bin associated with the input energy - assumes the energies are in increasing order */
    while ( upper - lower > 1 ) {
        middle = (upper + lower)/2;
        if ( energy < rmf->HighEnergy[middle] ) {
            upper = middle;
        } else {
            lower = middle;
        }
    }
    if ( energy > rmf->HighEnergy[lower] ) {
        energybin = upper;
    } else {
        energybin = lower;
    }
    /* generate an array of size channel each element of which is the integrated response up to and                                                 
       including that channel */

    for (i=0; i<rmf->NumberChannels; i++) sumresponse[i] = 0.;

    for (i=0; i<rmf->NumberGroups[energybin]; i++) {

        igrp = i + rmf->FirstGroup[energybin];
        ivec = rmf->FirstChannelGroup[igrp] - rmf->FirstChannel;
        ielt = rmf->FirstElement[igrp];

        for (j=0; j<rmf->NumberChannelGroups[igrp]; j++) {
            sumresponse[ivec+j] += rmf->Matrix[ielt+j];
        }
    }

    for (i=1; i<rmf->NumberChannels; i++) sumresponse[i] += sumresponse[i-1];

    /* generate random numbers between 0 and 1 */

    for (i=0; i<NumberPhoton; i++){
        RandomNumber[i] = (float) HDmt_drand(MTstate);
    }

    /* loop round the photons */

    for (i=0; i<NumberPhoton; i++) {
        /* find the array element containing this random number. note that we do                                                                    
           not assume that the total response sums to 1 - if the random number                                                                      
           exceeds the total response then we assume that the event fell off the                                                                    
           end of the channel array and return a -1 */

        lower = 0;
        upper = rmf->NumberChannels-1;
        if ( RandomNumber[i] <= sumresponse[upper] ) {
            while ( upper - lower > 1 ) {
                middle = (upper + lower)/2;
                if ( RandomNumber[i] < sumresponse[middle] ) {
                    upper = middle;
                } else {
                    lower = middle;
                }
            }
            if ( RandomNumber[i] > sumresponse[lower] ) {
                channel[i] = upper;
            } else {
                channel[i] = lower;
            }

            /* correct the channel number for the first channel number in use in the response matrix */

            channel[i] += rmf->FirstChannel;

        }

    }

    /* memory tidy-up */

    free(sumresponse);
    free(RandomNumber);

    return;
}





/* Read the MATRIX extension and return the result in the RMF structure
   Assumes that the file has been opened but not positioned at the correct
   extension */

int C_ReadRMFMatrix(fitsfile *fptr, long RMFnumber, struct RMF *rmf)
{
    int status=0;
    int status2=0;
    int colnum=0;
    int anynul=0;
    int nfound=0;
    int i, j, ipt, igrp;
    long nelt;
    float rtemp;
    long ltemp;
    char unit_keyword[20];

    /* Move to the correct RMF extension */

    fits_movnam_hdu(fptr, BINARY_TBL, "MATRIX", RMFnumber, &status);
    if (status) {
	status = 0;
	fits_clear_errmsg();
	fits_movnam_hdu(fptr, BINARY_TBL, "SPECRESP MATRIX", RMFnumber, &status);
    }
    if (!status) {
	headas_chat(5, "Found MATRIX or SPECRESP MATRIX extension...\n");
    } else {
	headas_chat(1, "***Cannot find MATRIX or SPECRESP MATRIX extension...\n");
	headas_chat(1, "   FITS status = %d\n", status);
	return(status);
    }

    /* Read the standard keywords and save the values */

    C_SP_read_key(fptr, TSTRING, "EXTNAME", rmf->RMFExtensionName, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "HDUCLAS3", rmf->RMFType, "UNKNOWN");
    
    C_SP_read_key(fptr, TSTRING, "CHANTYPE", rmf->ChannelType, "UNKNOWN");

    if (C_SP_read_key(fptr, TSTRING, "HDUVERS", rmf->RMFVersion, "UNKNOWN")) {
	if (C_SP_read_key(fptr, TSTRING, "HDUVERS2", rmf->RMFVersion, "UNKNOWN")) {
	    if (!C_SP_read_key(fptr, TSTRING, "RMFVERSN", rmf->RMFVersion, "UNKNOWN")) {
		strcpy(rmf->RMFVersion,"1.0.0");
	    }
	}
    }

    C_SP_read_key(fptr, TSTRING, "TELESCOP", rmf->Telescope, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "INSTRUME", rmf->Instrument, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "DETNAM", rmf->Detector, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "FILTER", rmf->Filter, "UNKNOWN");

    rtemp = 1.0;
    C_SP_read_key(fptr, TFLOAT, "EFFAREA", &rmf->AreaScaling, &rtemp); 

    rtemp = 1.0;
    C_SP_read_key(fptr, TFLOAT, "LO_THRES", &rmf->ResponseThreshold, &rtemp);

    ltemp = 0;
    status = C_SP_read_key(fptr, TLONG, "DETCHANS", &rmf->NumberChannels, &ltemp);
    if (status) return(status);

    status = C_SP_read_key(fptr, TLONG, "NAXIS2", &rmf->NumberEnergyBins, &ltemp);
    if (status) return(status);

    /* A couple of optional keywords - if they are present it saves us some calculation later */

    ltemp = 0;
    C_SP_read_key(fptr, TLONG, "NUMGRP", &rmf->NumberTotalGroups, &ltemp);
    C_SP_read_key(fptr, TLONG, "NUMELT", &rmf->NumberTotalElements, &ltemp);

    /* Get the start and stop energies for the bins */

    headas_chat(5, "Allocating %ld for energy bins\n", rmf->NumberEnergyBins);

    rmf->LowEnergy = (float *) malloc(rmf->NumberEnergyBins*sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "ENERG_LO", &colnum, &status);
    fits_read_col(fptr, TFLOAT, colnum, 1, 1, rmf->NumberEnergyBins, NULL, rmf->LowEnergy, &anynul, &status);

    /* inserted to match live C++ version - get EnergyUnits from ENERG_LO column. */
    sprintf(unit_keyword,"TUNIT%d",colnum);
    C_SP_read_key(fptr, TSTRING, unit_keyword, &rmf->EnergyUnits, "");

    /* also set RMFUnits - though not used for anything. */
    strcpy(rmf->RMFUnits,"");

    rmf->HighEnergy = (float *) malloc(rmf->NumberEnergyBins*sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "ENERG_HI", &colnum, &status);
    fits_read_col(fptr, TFLOAT, colnum, 1, 1, rmf->NumberEnergyBins, NULL, rmf->HighEnergy, &anynul, &status);

    /* Get the number of groups for each energy bin */

    rmf->NumberGroups = (long *) malloc(rmf->NumberEnergyBins*sizeof(long));
    fits_get_colnum(fptr, CASEINSEN, "N_GRP", &colnum, &status);
    fits_read_col(fptr, TLONG, colnum, 1, 1, rmf->NumberEnergyBins, NULL, rmf->NumberGroups, &anynul, &status);

    if (status) {
	fits_report_error(stderr, status);
	printf("\n");
	return(status);
    }

    /* Set up the FirstGroup array - note counts from 0 */

    rmf->FirstGroup = (long *) malloc(rmf->NumberEnergyBins*sizeof(long));
    igrp = 0;
    for (i=0; i<rmf->NumberEnergyBins; i++) {
	rmf->FirstGroup[i] = igrp;
	igrp += rmf->NumberGroups[i];
    }

    /* If the NUMGRP keyword was not read then sum up this column to calculate it */

    if ( rmf->NumberTotalGroups == 0 ) {
	for (i=0; i<rmf->NumberEnergyBins; i++) {
	    rmf->NumberTotalGroups += rmf->NumberGroups[i];
	}
	headas_chat(5, "Setting NumberTotalGroups to %ld\n", rmf->NumberTotalGroups);
    }
    /* Get the first channel for each group */

    headas_chat(5, "Allocating %ld for groups\n", rmf->NumberTotalGroups);

    rmf->FirstChannelGroup = (long *) malloc(rmf->NumberTotalGroups*sizeof(long));
    fits_get_colnum(fptr, CASEINSEN, "F_CHAN", &colnum, &status);
    ipt = 0;
    for (i=0; i<rmf->NumberEnergyBins; i++) {
	fits_read_col(fptr, TLONG, colnum, i+1, 1, rmf->NumberGroups[i], NULL, &rmf->FirstChannelGroup[ipt], &anynul, &status);
	ipt += rmf->NumberGroups[i];
    }

    fits_read_keys_lng(fptr, "TLMIN", colnum, 1, &rmf->FirstChannel, &nfound, &status);
    if (status || nfound==0) {
	rmf->FirstChannel = 1;
	headas_chat(5, "Failed to read TLMIN for F_CHAN column - setting FirstChannel to 1\n");
	status = 0;
	fits_clear_errmsg();
    } else {
	headas_chat(5, "TLMIN for F_CHAN column = %ld\n", rmf->FirstChannel);
    }

    /* Get the number of channels for each group */

    rmf->NumberChannelGroups = (long *) malloc(rmf->NumberTotalGroups*sizeof(long));
    fits_get_colnum(fptr, CASEINSEN, "N_CHAN", &colnum, &status);
    ipt = 0;
    for (i=0; i<rmf->NumberEnergyBins; i++) {
	fits_read_col(fptr, TLONG, colnum, i+1, 1, rmf->NumberGroups[i], NULL, &rmf->NumberChannelGroups[ipt], &anynul, &status);
	ipt += rmf->NumberGroups[i];
    }

    if (status) {
	fits_report_error(stderr, status);
	printf("\n");
	return(status);
    }

    /* If the NUMELT keyword was not read then sum up this column to calculate it */

    if ( rmf->NumberTotalElements == 0 ) {
	for (i=0; i<rmf->NumberTotalGroups; i++) {
	    rmf->NumberTotalElements += rmf->NumberChannelGroups[i];
	}
	headas_chat(5, "Setting NumberTotalElements to %ld\n", rmf->NumberTotalElements);
    }

    /* Get the memory for the FirstElement array pointing to the first element in the Matrix
       array for each response group */

    rmf->FirstElement = (long *) malloc(rmf->NumberTotalGroups*sizeof(long));

    /* Read the matrix information */

    headas_chat(5, "Allocating %ld for response elements\n", rmf->NumberTotalElements);

    rmf->Matrix = (float *) malloc(rmf->NumberTotalElements*sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "MATRIX", &colnum, &status);
    ipt = 0;
    igrp = 0;
    for (i=0; i<rmf->NumberEnergyBins; i++) {
	nelt = 0;
	for (j=0; j<rmf->NumberGroups[i]; j++) {
	    rmf->FirstElement[igrp] = nelt+ipt;
	    nelt += rmf->NumberChannelGroups[igrp];
	    igrp++;
	}
	fits_read_col(fptr, TFLOAT, colnum, i+1, 1, nelt, NULL, &rmf->Matrix[ipt], &anynul, &status);
	ipt += nelt;
    }

    /* Read the optional order information */

    status2 = 0;
    fits_write_errmark();
    rmf->isOrder = 0;
    fits_get_colnum(fptr, CASEINSEN, "ORDER", &colnum, &status2);
    fits_clear_errmark();
    if (!status2) {
	rmf->isOrder = 1;
	rmf->OrderGroup = (long *) malloc(rmf->NumberTotalGroups*sizeof(long));
	fits_read_col(fptr, TLONG, colnum, 1, 1, rmf->NumberTotalGroups, NULL, rmf->OrderGroup, &anynul, &status);
    }

    /* Check for a cfitsio error and if it occurred write diagnostic information */

    if (status) {
	fits_report_error(stderr, status);
	printf("\n");
    }

    return(status);

}




/* Read the EBOUNDS extension and return the result in the RMF structure
   Assumes that the file has been opened but not positioned at the correct
   extension */

int C_ReadRMFEbounds(fitsfile *fptr, long EBDnumber, struct RMF *rmf)
{
    int status=0;
    int colnum=0;
    int anynul=0;
    long ltemp;

    /* Move to the correct RMF extension */

    fits_movnam_hdu(fptr, BINARY_TBL, "EBOUNDS", EBDnumber, &status);

    headas_chat(5, "Found EBOUNDS extension...\n");

    /* Read the standard keywords and save the values */

    C_SP_read_key(fptr, TSTRING, "EXTNAME", rmf->EBDExtensionName, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "CHANTYPE", rmf->ChannelType, "UNKNOWN");

    if (C_SP_read_key(fptr, TSTRING, "HDUVERS", rmf->EBDVersion, "UNKNOWN")) {
	if (C_SP_read_key(fptr, TSTRING, "HDUVERS2", rmf->EBDVersion, "UNKNOWN")) {
	    if (!C_SP_read_key(fptr, TSTRING, "EBDVERSN", rmf->EBDVersion, "UNKNOWN")) {
		strcpy(rmf->EBDVersion,"1.0.0");
	    }
	}
    }

    C_SP_read_key(fptr, TSTRING, "TELESCOP", rmf->Telescope, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "INSTRUME", rmf->Instrument, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "FILTER", rmf->Filter, "UNKNOWN");

    /* ltemp = 0; */
    ltemp = rmf->NumberChannels;
    C_SP_read_key(fptr, TLONG, "DETCHANS", &rmf->NumberChannels, &ltemp);
    if (status) return(status);

    /* Get the start and stop energies for the channels */

    headas_chat(5, "Allocating %ld for channels\n", rmf->NumberChannels);

    rmf->ChannelLowEnergy = (float *) malloc(rmf->NumberChannels*sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "E_MIN", &colnum, &status);
    fits_read_col(fptr, TFLOAT, colnum, 1, 1, rmf->NumberChannels, NULL, rmf->ChannelLowEnergy, &anynul, &status);

    rmf->ChannelHighEnergy = (float *) malloc(rmf->NumberChannels*sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "E_MAX", &colnum, &status);
    fits_read_col(fptr, TFLOAT, colnum, 1, 1, rmf->NumberChannels, NULL, rmf->ChannelHighEnergy, &anynul, &status);

    /* Check for a cfitsio error and if it occurred write diagnostic information */

    if (status) {
	fits_report_error(stderr, status);
	printf("\n");
    }

    return(status);

}





/* Read the SPECRESP extension and return the result in the ARF structure
   Assumes that the file has been opened but not positioned at the correct
   extension */

int C_ReadARF(fitsfile *fptr, long ARFnumber, struct ARF *arf)
{
    int status=0;
    int colnum=0;
    int anynul=0;
    long ltemp;

    /* Move to the correct SPECRESP extension */

    fits_movnam_hdu(fptr, BINARY_TBL, "SPECRESP", ARFnumber, &status);
    if (!status) {
	headas_chat(5, "Found SPECRESP extension...\n");
    } else {
	headas_chat(1, "***Cannot find SPECRESP extension...\n");
	headas_chat(1, "   FITS status = %d\n", status);
	return(status);
    }

    /* Read the standard keywords and save the values */

    C_SP_read_key(fptr, TSTRING, "EXTNAME", arf->ARFExtensionName, "UNKNOWN");

    if (C_SP_read_key(fptr, TSTRING, "HDUVERS", arf->ARFVersion, "UNKNOWN"))
	C_SP_read_key(fptr, TSTRING, "HDUVERS2", arf->ARFVersion, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "TELESCOP", arf->Telescope, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "INSTRUME", arf->Instrument, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "DETNAM", arf->Detector, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "FILTER", arf->Filter, "UNKNOWN");

    status = C_SP_read_key(fptr, TLONG, "NAXIS2", &arf->NumberEnergyBins, &ltemp);
    if (status) return(status);

    /* Get the start and stop energies for the bins and the effective areas */

    headas_chat(5, "Allocating %ld for energy bins\n", arf->NumberEnergyBins);

    arf->LowEnergy = (float *) malloc(arf->NumberEnergyBins*sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "ENERG_LO", &colnum, &status);
    fits_read_col(fptr, TFLOAT, colnum, 1, 1, arf->NumberEnergyBins, NULL, arf->LowEnergy, &anynul, &status);

    arf->HighEnergy = (float *) malloc(arf->NumberEnergyBins*sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "ENERG_HI", &colnum, &status);
    fits_read_col(fptr, TFLOAT, colnum, 1, 1, arf->NumberEnergyBins, NULL, arf->HighEnergy, &anynul, &status);

    arf->EffArea = (float *) malloc(arf->NumberEnergyBins*sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "SPECRESP", &colnum, &status);
    fits_read_col(fptr, TFLOAT, colnum, 1, 1, arf->NumberEnergyBins, NULL, arf->EffArea, &anynul, &status);


    /* Check for a cfitsio error and if it occurred write diagnostic information */
    if (status) {
	fits_report_error(stderr, status);
	printf("\n");
    }

    return(status);

}


/* Read the Type I SPECTRUM extension and return the result in the PHA structure
   Assumes that the file has been opened but not positioned at the correct
   extension */

int C_ReadPHAtypeI(fitsfile *fptr, long PHAnumber, struct PHA *pha)
{
    int status=0;
    int colnum=0;
    int nfound=0;
    int i, itemp, ntotal;
    long ltemp, nrows;
    float rtemp;
    char *ctemp1[10];
    char *ctemp2[90];
    int ichan =0;

    /* Move to the correct SPECTRUM extension */

    fits_movnam_hdu(fptr, BINARY_TBL, "SPECTRUM", PHAnumber, &status);
    if (!status) {
	headas_chat(5, "Found SPECTRUM extension...\n");
    } else {
	headas_chat(1, "***Cannot find SPECTRUM extension...\n");
	return(status);
    }

    /* Read the standard keywords and save the values */

    C_SP_read_key(fptr, TSTRING, "CHANTYPE", pha->ChannelType, "UNKNOWN");

    if (C_SP_read_key(fptr, TSTRING, "HDUVERS", pha->PHAVersion, "UNKNOWN"))
	C_SP_read_key(fptr, TSTRING, "HDUVERS1", pha->PHAVersion, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "TELESCOP", pha->Telescope, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "INSTRUME", pha->Instrument, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "DETNAM", pha->Detector, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "FILTER", pha->Filter, "UNKNOWN");

    C_SP_read_key(fptr, TSTRING, "DATAMODE", pha->Datamode, "UNKNOWN");

    ltemp = 0;
    status = C_SP_read_key(fptr, TLONG, "DETCHANS", &pha->NumberChannels, &ltemp);
    if (status) return(status);

    C_SP_read_key(fptr, TSTRING, "HDUCLAS2", pha->Spectrumtype, "TOTAL");

    C_SP_read_key(fptr, TSTRING, "HDUCLAS3", pha->Datatype, "COUNT");

    C_SP_read_key(fptr, TSTRING, "RESPFILE", pha->ResponseFile, "NONE");

    C_SP_read_key(fptr, TSTRING, "ANCRFILE", pha->AncillaryFile, "NONE");

    C_SP_read_key(fptr, TSTRING, "BACKFILE", pha->BackgroundFile, "NONE");

    C_SP_read_key(fptr, TSTRING, "CORRFILE", pha->CorrectionFile, "NONE");

    rtemp = 1.0;
    C_SP_read_key(fptr, TFLOAT, "CORRSCAL", &pha->CorrectionScaling, &rtemp);

    rtemp = 0.0;
    C_SP_read_key(fptr, TFLOAT, "EXPOSURE", &pha->Exposure, &rtemp);

    itemp = 0;
    C_SP_read_key(fptr, TINT, "POISSERR", &pha->Poisserr, &itemp);

    /* Read the XFLT keywords  - this is a bit messy because it can't be done in 
       a single cfitsio call */

    for (i=0; i<9; i++) ctemp1[i] = (char *) malloc(FLEN_KEYWORD*sizeof(char));
    fits_read_keys_str(fptr, "XFLT000", 0, 10, ctemp1, &nfound, &status);
    ntotal = nfound;
    if (nfound == 10) {
	for (i=0; i<90; i++) ctemp2[i] = (char *) malloc(FLEN_KEYWORD*sizeof(char));
	fits_read_keys_str(fptr, "XFLT00", 10, 100, ctemp2, &nfound, &status);
	ntotal += nfound;
    }
    for (i=0; i<ntotal; i++) pha->XSPECFilter[i] = (char *) malloc(FLEN_KEYWORD*sizeof(char));
    for (i=0; i<(ntotal<10?ntotal:10); i++) strcpy(pha->XSPECFilter[i], ctemp1[i]);
    if (ntotal > 10) for (i=10; i<(100>ntotal?100:ntotal); i++) strcpy(pha->XSPECFilter[i], ctemp2[i-10]);
    for (i=0; i<9; i++) free(ctemp1[i]);
    if (ntotal > 10) for (i=0; i<90; i++) free(ctemp2[i]);

    /* Check for TLMIN set for the CHANNEL column */

    pha->FirstChannel = 1;
    fits_get_colnum(fptr, CASEINSEN, "CHANNEL", &colnum, &status);
    if (!status) fits_read_keys_lng(fptr, "TLMIN", colnum, 1, &pha->FirstChannel, &nfound, &status);
    status = 0;
    fits_clear_errmsg();

    /* Get the number of detector channels which may differ from the actual number of rows */
    C_SP_read_key(fptr, TINT, "DETCHANS", &pha->DetChans, 0);
    C_SP_read_key(fptr, TLONG, "NAXIS2", &nrows, &ltemp);

    /* Read the CHANNEL column - still have colnum from TLMIN check above */
    pha->Channel = (int *) calloc(pha->NumberChannels,sizeof(int));
    /*C_SP_read_col(fptr, TINT, "CHANNEL", nrows, pha->Channel);*/
    fits_read_col(fptr, TINT, colnum, 1, 1, nrows, NULL, pha->Channel, NULL, &status);
    if (status != 0){
	for (ichan=0; ichan < pha->NumberChannels; ichan++)
	    pha->Channel[ichan] = pha->FirstChannel + ichan;
	status = 0;
	fits_clear_errmsg();
    }

    /* Read the data and set datatype column appropriately */
    pha->Pha = (float *) calloc(pha->NumberChannels,sizeof(float));
    fits_get_colnum(fptr, CASEINSEN, "COUNTS", &colnum, &status);
    if (status == 0){
	fits_read_col(fptr, TFLOAT, colnum, 1, 1, nrows, NULL, pha->Pha, NULL, &status);
	if (status == 0) strcpy(pha->Datatype,"COUNT");
    } else {
	status = 0;
	fits_get_colnum(fptr, CASEINSEN, "RATE", &colnum, &status);
	if (status == 0){
	    fits_read_col(fptr, TFLOAT, colnum, 1, 1, nrows, NULL, pha->Pha, NULL, &status);
	    if (status == 0) strcpy(pha->Datatype,"RATE");
	} else {
	    printf("Input spectrum has no channels!\n");
	}
    }

    /* allocate StatError no matter what, even if we don't populate it */
    pha->StatError = (float *) calloc(pha->NumberChannels,sizeof(float));
    /* Read the statistical error if poisserr is false */
    if (!pha->Poisserr) {
	headas_chat(5, "Allocating %ld for StatError\n", pha->NumberChannels);
	C_SP_read_col(fptr, TFLOAT, "STAT_ERR", nrows, pha->StatError);
    }

    /* Read the systematic error if it is there otherwise set the array to 0 */

    headas_chat(5, "Allocating %ld for SysError\n", pha->NumberChannels);
    pha->SysError = (float *) calloc(pha->NumberChannels,sizeof(float));
    status = 0;
    fits_get_colnum(fptr, CASEINSEN, "SYS_ERR", &colnum, &status);
    if (status == 0){
	fits_read_col(fptr, TFLOAT, colnum, 1, 1, nrows, NULL, pha->SysError, NULL, &status);
    } else {
	for (i=0; i<nrows; i++) pha->SysError[i] = 0.0;
	status = 0;
    }

    /* Read the QUALITY - first check for a keyword then for a column */

    headas_chat(5, "Allocating %ld for Quality\n", pha->NumberChannels);
    pha->Quality = (int *) calloc(pha->NumberChannels,sizeof(int));
    C_SP_read_col(fptr, TINT, "QUALITY", nrows, pha->Quality);
  
    /* Read the GROUPING - first check for a keyword then for a column */

    headas_chat(5, "Allocating %ld for Grouping\n", pha->NumberChannels);
    pha->Grouping = (int *) calloc(pha->NumberChannels,sizeof(int));
    C_SP_read_col(fptr, TINT, "GROUPING", nrows, pha->Grouping);

    /* Read the AREASCAL - first check for a keyword then for a column */

    headas_chat(5, "Allocating %ld for AreaScaling\n", pha->NumberChannels);
    pha->AreaScaling = (float *) calloc(pha->NumberChannels,sizeof(float));
    C_SP_read_col(fptr, TFLOAT, "AREASCAL", nrows, pha->AreaScaling);

    /* Read the BACKSCAL - first check for a keyword then for a column */

    headas_chat(5, "Allocating %ld for BackScaling\n", pha->NumberChannels);
    pha->BackScaling = (float *) calloc(pha->NumberChannels,sizeof(float));
    C_SP_read_col(fptr, TFLOAT, "BACKSCAL", nrows, pha->BackScaling);
    



    return(status);

}


/******************************** C_SP_read_col **************************************/

int C_SP_read_col(fitsfile *fptr, int datatype, char *column_name, long number_values, void *column_values)
{

    return(C_SPII_read_col(fptr, datatype, column_name, 1, number_values, column_values));
}



/******************************** C_SPII_read_col **************************************/

int C_SPII_read_col(fitsfile *fptr, int datatype, char *column_name, long ispec, long number_values, void *column_values)
{
    int status=0;
    int i, colnum, anynul, typecode;
    long repeat, width;
    int *ivalues;
    long *lvalues;
    float *fvalues;

    /* try to read a keyword of name column_name
       if we succeeded then set the array to the returned value */

    if (!C_SP_read_key(fptr, datatype, column_name, column_values, column_values)) {
	if (datatype == TINT) {
	    headas_chat(5, "%s = %d read from keyword\n", column_name, *(int *)column_values);
	    ivalues = (int *)column_values;
	    for(i=0; i<number_values; i++) ivalues[i] = *(int *)column_values;
	} else if (datatype == TLONG) {
	    headas_chat(5, "%s = %ld read from keyword\n", column_name, *(long *)column_values);
	    lvalues = (long *)column_values;
	    for(i=0; i<number_values; i++) lvalues[i] = *(long *)column_values;
	} else if (datatype == TFLOAT) {
	    headas_chat(5, "%s = %f read from keyword\n", column_name, *(float *)column_values);
	    fvalues = (float *)column_values;
	    for(i=0; i<number_values; i++) fvalues[i] = *(float *)column_values;
	} else {
	    printf("%d is an unsupported datatype in C_SPII_read_col\n", datatype);
	}
	return(0);
    }

    /* get the column number */

    fits_get_colnum(fptr, CASEINSEN, column_name, &colnum, &status);
    if (status) {
	printf("***C_SPII_read_col: Failed to find column or keyword named %s\n", column_name);
	fits_report_error(stderr, status);
	printf("\n");
	return(status);
    }

    /* find out whether this is a scalar or vector column */

    fits_get_coltype(fptr, colnum, &typecode, &repeat, &width, &status);
    if (status) {
	printf("***C_SPII_read_col: Failed to read information about the column named %s\n", column_name);
	fits_report_error(stderr, status);
	printf("\n");
	return(status);
    }

    /* read the column and replicate the values if the column is a scalar */

    fits_read_col(fptr, datatype, colnum, ispec, 1, repeat, NULL, column_values, &anynul, &status);
    if (status) {
	printf("***C_SPII_read_col: Failed to read column or keyword named %s\n", column_name);
	fits_report_error(stderr, status);
	printf("\n");
	return(status);
    }

    if (repeat == 1) {
	if (datatype == TINT) {
	    headas_chat(5, "%s = %d read from scalar column %d\n", column_name, *(int *)column_values, colnum);
	    ivalues = (int *)column_values;
	    for(i=0; i<number_values; i++) ivalues[i] = *(int *)column_values;
	} else if (datatype == TLONG) {
	    headas_chat(5, "%s = %ld read from scalar column %d\n", column_name, *(long *)column_values, colnum);
	    lvalues = (long *)column_values;
	    for(i=0; i<number_values; i++) lvalues[i] = *(long *)column_values;
	} else if (datatype == TFLOAT) {
	    headas_chat(5, "%s = %f read from scalar column %d\n", column_name, *(float *)column_values, colnum);
	    fvalues = (float *)column_values;
	    for(i=0; i<number_values; i++) fvalues[i] = *(float *)column_values;
	} else {
	    printf("%d is an unsupported datatype in C_SPII_read_col\n", datatype);
	}
	return(0);
    }

    headas_chat(5, "%s read from column %d\n", column_name, colnum);

    return(0);

}


/******************************** C_SP_read_key **************************************/

/* reads a keyword */

int C_SP_read_key(fitsfile *fptr, int datatype, char *keyword_name, void *keyword_value, void *keyword_default)
{

    return(C_SPII_read_key(fptr, datatype, keyword_name, 0, keyword_value, keyword_default));

}

/******************************** C_SPII_read_key **************************************/

/* reads a keyword from a type II spectral file - first checks for the keyword then, if
   ispec > 0, for a value in a column */

int C_SPII_read_key(fitsfile *fptr, int datatype, char *keyword_name, long ispec, void *keyword_value, void *keyword_default)
{
    int status=0;
    int anynul=0;
    int colnum;
    char comment[FLEN_COMMENT];

    fits_read_key(fptr, datatype, keyword_name, keyword_value, comment, &status);
    if (status && ispec > 0) {
	status = 0;
	fits_clear_errmsg();
	fits_get_colnum(fptr, CASEINSEN, keyword_name, &colnum, &status);
	fits_read_col(fptr, datatype, colnum, ispec, 1, 1, NULL, keyword_value, &anynul, &status);
    }

    if (!status) {
	if (datatype == TFLOAT)  headas_chat(5, " %s = %f\n", keyword_name, *(float *)keyword_value);
	if (datatype == TSTRING) headas_chat(5, " %s = %s\n", keyword_name , (char *)keyword_value);
	if (datatype == TINT)    headas_chat(5, " %s = %d\n", keyword_name, *(int *)keyword_value);
	if (datatype == TLONG)   headas_chat(5, " %s = %ld\n", keyword_name, *(long *)keyword_value);
    } else if (status) {

	if (datatype == TFLOAT)  *(float *)keyword_value = *(float *)keyword_default;
	if (datatype == TSTRING) strcpy((char *)keyword_value, (char *)keyword_default);
	if (datatype == TINT)    *(int *)keyword_value = *(int *)keyword_default;
	if (datatype == TLONG)   *(long *)keyword_value = *(int *)keyword_default;

	if (datatype == TFLOAT)  headas_chat(1, "***C_SPII_read_key: Cannot find %s keyword - setting it to %f\n", keyword_name, *(float *)keyword_default);
	if (datatype == TSTRING) headas_chat(1, "***C_SPII_read_key: Cannot find %s keyword - setting it to %s\n", keyword_name, (char *)keyword_default);
	if (datatype == TINT)    headas_chat(1, "***C_SPII_read_key: Cannot find %s keyword - setting it to %d\n", keyword_name, *(int *)keyword_default);
	if (datatype == TLONG)   headas_chat(1, "***C_SPII_read_key: Cannot find %s keyword - setting it to %ld\n", keyword_name, *(long *)keyword_default);

    }

    if (datatype != TFLOAT && datatype != TSTRING && datatype != TINT && datatype != TLONG)
	headas_chat(1, "%d is an unsupported datatype in C_SPII_read_key\n", datatype);

    return(status);
}


/*
 ! $Log: legacy_heasp.c,v $
 ! Revision 1.8  2015/02/09 14:53:08  driethmi
 ! Commented out #include xlocale.h.
 !
 ! Revision 1.7  2014/10/02 18:09:24  driethmi
 ! Added CVS logging macro to legacy_heasp.
 !    
*/
