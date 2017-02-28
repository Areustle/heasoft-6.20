#include <math.h>
#include "coordfits.h"
#include "aspectbins.h"
#include "arrays.h"
#include "att_iterator.h"



/*
#define DEBUG
*/

#define new_fillAspectBins fillAspectBins 

/*******************************************************************
* allocate storage space for a set of aspect bins
* and initialize the structure
*********************************************************************/
ASPECTBINS* allocateAspectBins(int nbins) 
{
ASPECTBINS* bin;
int k;

bin=(ASPECTBINS*)malloc(sizeof(ASPECTBINS));

/*******************
* nominal pointing *
*******************/
bin->q0=allocateQuat();

/****************************
* allocate histogram arrays *
****************************/
bin->dur=allocateArray(nbins,nbins);

for(k=0;k<4;++k) {
    bin->q[k]=allocateArray(nbins,nbins);
}


/************
* set nbins *
************/
bin->dimen=nbins;

bin->i0=nbins/2;
bin->j0=nbins/2;

/***************************
* allocate dummy transform *
***************************/
bin->trans=allocateXform2d();
bin->q_offset=allocateQuat();

return(bin);

}

/****************************************************
* free the memory for a set of aspect bins 
*********************************************************/
void destroyAspectBins(ASPECTBINS* bin) 
{
int k;

free(bin->dur);
for(k=0;k<4;++k) {
    free(bin->q[k]);
}

destroyXform2d(bin->trans);
destroyQuat(bin->q0);
destroyQuat(bin->q_offset);

free(bin);

} /* end of destroyAspectBins function */

/**********************************************************************
* change the center pointing and bin size of a set of aspect bins
* and then reset all the bin values to zero.
**********************************************************************/
void resetAspectBins(ASPECTBINS* bin, QUAT* q0, double size) 
{
int k;

/*******************************************
* set the tangent plane coordinate vectors *
*******************************************/
*(bin->q0)=*q0;

/*******************
* set the bin size *
*******************/
bin->size=size;

/***********************
* set all bins to zero *
***********************/
bin->out=0.;
blankArray(bin->dur,bin->dimen,bin->dimen);

for(k=0;k<4;++k) {
    blankArray(bin->q[k],bin->dimen,bin->dimen);
}

} /* end of resetAspectBins function */

 
/**************************************************************************
* add a pointing to the correct aspect bin
***************************************************************************/
void addToAspectBins(ASPECTBINS* bin, QUAT* q, double duration)
{
int i,j;
double xshift,yshift;
double cosrot,sinrot;
double costilt;

       
/**********************************************************
* find the image position corresponding to the quaternion *
**********************************************************/
getQuatOfChange(bin->q_offset, bin->q0, q);

#ifdef DEBUG
printf("addToAspectBins: q_offset: %g %g %g %g\n",
       bin->q_offset->p[0], bin->q_offset->p[1],
       bin->q_offset->p[2], bin->q_offset->p[3] );
#endif

costilt=convertQuatToXform2d(bin->trans, bin->q_offset,
                             0.,0. /*det ref pixel   */,
                             0.,0. /*sky ref pixel   */,
                             1.   /*pixels per radian*/);
if(costilt<=0.) {
    /****************************************************
    * the tilt angle is >=90 degrees, so just call
    * this an out of bounds point
    ****************************************************/
    bin->out+=duration;
    return;
}

/*
 printf("        costilt=%g xshift=%g yshift=%g duration=%g\n",
        costilt, bin->trans->xshift, bin->trans->yshift, duration);
*/

xshift=bin->trans->xshift;
yshift=bin->trans->yshift;

cosrot = bin->trans->rot[0][0] + bin->trans->rot[1][1];
sinrot = bin->trans->rot[0][1] - bin->trans->rot[1][0];

#ifdef DEBUG
printf("xshift=%g yshift=%g cosrot=%g sinrot=%g\n",
            xshift, yshift, cosrot, sinrot);
#endif


/**********************************************
* find the bin containing this image position *
**********************************************/
i=xshift/bin->size + bin->i0;
j=yshift/bin->size + bin->j0;

if(i>=0 && i<bin->dimen && j>=0 && j<bin->dimen) {
    /*******************************
    * point falls within the array *
    *******************************/
    bin->dur[j][i]+=duration;
    bin->q[0][j][i] += xshift*duration;
    bin->q[1][j][i] += yshift*duration;
    bin->q[2][j][i] += sinrot*duration;
    bin->q[3][j][i] += cosrot*duration;
      

} else {
    /*******************************
    * point fell outside the array *
    *******************************/
    bin->out+=duration;
}

} /* end of addToAspectBins function */


/**************************************************************************
* find the peak of a set of aspect bins and calculate the nominal pointing
* returns 1 if the peak is definitely valid
* returns 0 if there may be a higher peak outside the bin structure
***************************************************************************/
int findPeakOfAspectBins(QUAT* q, ASPECTBINS* bin)
{
double duration,max_duration;
int i,j;
int maxi=0,maxj=0;
double sinrot,cosrot;
double norm;

#ifdef DEBUG
printf("findPeakOfAspectBins: start\n");
#endif

/**************************************************
* search the histograms to find the set of four  
* adjacent bins with the longest duration 
**************************************************/
max_duration=-1.;
for(j=1;j<bin->dimen;++j) {
    for(i=1;i<bin->dimen;++i) {
        /******************************************************
        * calculate the total duration in this four bin block *
        ******************************************************/
        duration=boxSum(bin->dur,i,j);

        if(duration>max_duration) {
            max_duration=duration;
            maxi=i;
            maxj=j;
        }

    }
} /* end of loop over all bins */

/**************************************************
* sum the duration on the outer rim of the bins 
* plus the duration outside the bins
**************************************************/
duration=bin->out;
for(i=0;i<bin->dimen;++i) {
    duration += bin->dur[0][i] + bin->dur[bin->dimen-1][i];
}

for(j=1;i<bin->dimen-1;++j) {
    duration += bin->dur[j][0] + bin->dur[j][bin->dimen-1];
}


/**********************************************
* calculate the average 2d transform elements *
**********************************************/
sinrot=boxSum(bin->q[2],maxi,maxj);
cosrot=boxSum(bin->q[3],maxi,maxj);
norm=sqrt(sinrot*sinrot+ cosrot*cosrot);

setXform2dToRotation(bin->trans, sinrot/norm, cosrot/norm,  0.0, 0.0);

applyTranslationToXform2d(bin->trans, 
                          boxSum(bin->q[0],maxi,maxj)/max_duration,
                          boxSum(bin->q[1],maxi,maxj)/max_duration );

/************************************************************
* convert the transform to a relative quaternion and then 
* convert that to a an absolute one
************************************************************/
convertXform2dToQuat(bin->q_offset, bin->trans, 0.0 ,0.0, 0.0, 0.0, 1.);

productOfQuats(q,bin->q0,bin->q_offset);

/**************************************************
* return  true if the solution is definitely good *
**************************************************/
return(duration<=max_duration);

} /* end of findPeakOfAspectBins function */


/*************************************************
* print the non-zero duration bins 
**************************************************/
void printAspectBins(ASPECTBINS* bin)
{
int i,j;

for(j=0;j<bin->dimen;++j) {
    for(i=0;i<bin->dimen;++i) {
        if(bin->dur[j][i]>0.) printf("i=%d j=%d dur=%g\n",i,j,bin->dur[j][i]);
    }
}

printf("out=%g\n",bin->out);

} /* end of printAspectBins function */


/*********************************************************************
* fill a set of aspect bins with the aspect solution in a given file 
* The routine interpolates between adjecent attitude records which 
* are more than max_rotation apart
*********************************************************************/
void orig_fillAspectBins(ASPECTBINS* bin, ATTFILE* file, double max_rotation,
                         GTIS* gti) 
{
long row;

QUAT* q0;       /* quaternions   */
QUAT* q1;       /* used          */
QUAT* delta_q;  /* for           */
QUAT* hat_q;    /* interpolation */

QUAT* q;        /* interpolated quaternion */

double time0,time1;
double duration;

double dist;
double hat;
int step,nsteps;

/***********************
* allocate quaternions *
***********************/
q0     =allocateQuat();
q1     =allocateQuat();
delta_q=allocateQuat();
hat_q  =allocateQuat();

q      =allocateQuat();

/*********************************
* read the first attitude record *
*********************************/
row=1l;
readQuatFromAttFile(file,q1,row);
time1=readTimeFromAttFile(file,row);
++row;

/**********************************
* special treatment for this case *
**********************************/
if(file->nrows == 1) {
    addToAspectBins(bin,q1,1.);
    return;
}
    

/*********************************************
* loop over the rest of the attitude records *
*********************************************/
while(row<=file->nrows) {
    /**********************
    * remember old values *
    **********************/
    time0=time1;
    *q0=*q1;

    /******************
    * read new values *
    ******************/
    time1=readTimeFromAttFile(file,row);
    readQuatFromAttFile(file,q1,row);

    /**********************************************************
    * rotation angle between previous and current quaternions *
    **********************************************************/
    getQuatOfChange(delta_q,q0,q1);
	renormalizeQuat(delta_q);
    dist=2.*getQuatHalfRotation(delta_q);

    /***********************************************************
    * split the full rotation into a number of pieces which are
    * no bigger than the max_rotation param
    ************************************************************/
    nsteps=(int)(dist/max_rotation)+1;
    duration=(time1-time0)/nsteps;

printf("    time %.14g - %.14g duration=%g nsteps=%d\n", time0, time1, duration, nsteps);

    for(step=0;step<nsteps;++step) {    
        /********************************************************
        * calculate the quaternion at the midpoint of this step *
        ********************************************************/
        hat=((double)step+.5)/(double)(nsteps);
        multiplyQuatByScalar(hat_q,delta_q,hat);
        productOfQuats(q,q0,hat_q);


        /************************
        * bin the current point *
        ************************/
        addToAspectBins(bin,q,duration);

    } /* end of loop over interpolation steps */

    
    ++row;

} /* end of loop over attitude file rows */

/********************************
* free the allocated structures *
********************************/
destroyQuat(q0);
destroyQuat(q1);
destroyQuat(delta_q);
destroyQuat(hat_q);

destroyQuat(q);


} /* end of orig_fillAspectBins function */

/********************ATTITERATOR*************************************************
* fill a set of aspect bins with the aspect solution in a given file 
* The routine interpolates between adjecent attitude records which 
* are more than max_rotation apart
*********************************************************************/
void new_fillAspectBins(ASPECTBINS* bin, ATTFILE* file, double max_rotation,
                    GTIS* gti) {

    int i;
    ATTITERATOR* it;
    QUAT* q;
    double duration;

    /*****************************************
    * allocate the iterator and a quaternion *
    *****************************************/
    it = allocateAttIterator(file, max_rotation);
    q = allocateQuat();

    /*********************
    * loop over the GTIs *
    *********************/
    for(i=0; i<gti->n; ++i) {
    
        #ifdef DEBUG
        printf("%d start=%g stop=%g\n", i,gti->start[i], gti->stop[i]);
        #endif

        /*********************************
        * step through the attitude file *
        *********************************/
        for( setAttIteratorInterval(it, gti->start[i], gti->stop[i]);
            moreAttIteratorSteps(   it);
            nextAttIteratorStep(    it) ) {

            #ifdef DEBUG
            printf("i=%d stop=%.14g %.14g %.14g %d\n", 
                   i, gti->stop[i], it->time1, it->time2, it->step);
            #endif

            duration = getAttIteratorValues(it, q);
            addToAspectBins(bin,q,duration);
            #ifdef DEBUG
            printf("added to bins\n");
            #endif

        } /* end of loop through attitude file */

    } /* end of loop over GTIs */

    /**************************
    * free the allocated data *
    **************************/
    destroyQuat(q);
    destroyAttIterator(it);
    
} /* end of fillAspectBins function */



