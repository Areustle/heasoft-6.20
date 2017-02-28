#include "att_iterator.h"

/*
#define DEBUG
*/

/******************************************************************************
* constructor. Note setAttIteratorInterval must be called before the iterator can
* be used.
******************************************************************************/
ATTITERATOR* allocateAttIterator(ATTFILE* att, double max_rotation ) {

    ATTITERATOR* it = (ATTITERATOR*)malloc(sizeof(ATTITERATOR));

    it->att = att;
    it->max_rotation = max_rotation;

    it->q1 = allocateQuat();
    it->q2 = allocateQuat();

    it->delta_q = allocateQuat();
    it->  hat_q = allocateQuat();

    return it;

} /* end of allocateAttIterator function */


/******************************************************************************
* destructor
******************************************************************************/
void destroyAttIterator(ATTITERATOR* it) {

    if(it==NULL) return;

    destroyQuat(it->q1);
    destroyQuat(it->q2);

    destroyQuat(it->delta_q);
    destroyQuat(it->hat_q);

    free(it);
} /* end of destroyAttIterator function */




/******************************************************************************
* This function is for internal use only. it sets the end time for an
* interval provided the start time has already been set.
******************************************************************************/
double findAttIteratorEndTime(ATTITERATOR* it) {

    long row;

    row = findTimeInAttFile(it->att, it->time1);

    if( it->att->nrows ==1 || row == it->att->nrows-1) {
        /***********************************************
        * the stop time trails off the end of the 
        * attitude file, so we extend all the way to
        * the end of the interval
        ***********************************************/
        return it->stop;
    } else  if(row == 1l) {
        /***************************************************
        * check if we are before the first attitude record *
        ***************************************************/
        double time;
        time = readTimeFromAttFile(it->att,row);
        if(time > it->time1 ) {
            /***************************************
            * yes we are before the attitude file
            * so this is the next attitude record 
            ***************************************/
            return time;
        }
    } 
    /*************************************************************
    * if we get here then we are somewhere in the middle 
    * of the attitude file, and the next attitude record is the
    * one after the one we just found
    *************************************************************/
    {
    double time;
    time = readTimeFromAttFile(it->att, row + 1l);
    if(time > it->stop) time = it->stop;
    return time;
    }

} /* end of findEndTime function */



/******************************************************************************
* This function is mostly for internal use. It sets the current interval
* given a start time. This time must be less than the stop time
******************************************************************************/
void setAttIteratorStartTime(ATTITERATOR* it, double time1 ) {

    double dist;
    
#ifdef DEBUG
printf("setAttIteratorStartTime: start\n");
#endif

    /*********************************************************
    * get the time and attitude at the start of the interval *
    *********************************************************/
    it->time1 = time1;
    findQuatInAttFile(it->att, it->q1, it->time1);

#ifdef DEBUG

printf("search_row=%ld reliable=%d\n",
       it->att->search_row, it->att->search_qs_reliable);


printf("search_q0: (%g, %g, %g, %g) \n",
       it->att->search_q0->p[0], it->att->search_q0->p[1], 
       it->att->search_q0->p[2], it->att->search_q0->p[3] );

printf("search_q1: (%g, %g, %g, %g) \n",
       it->att->search_q1->p[0], it->att->search_q1->p[1], 
       it->att->search_q1->p[2], it->att->search_q1->p[3] );

printf("hatq: (%g, %g, %g, %g) \n",
       it->att->hatq->p[0], it->att->hatq->p[1], 
       it->att->hatq->p[2], it->att->hatq->p[3] );

printf("search_time0 = %.14g search_time_1 = %.14g\n",
       it->att->search_time0, it->att->search_time1 );

printf("time1 = %.14g time2 = %.14g\n",
       it->time1, it->time2);

printf("q1: (%g, %g, %g, %g)\n",
       it->q1->p[0], it->q1->p[1], it->q1->p[2], it->q1->p[3]);

#endif


    it->time2 = findAttIteratorEndTime(it);
    findQuatInAttFile(it->att, it->q2, it->time2);

#ifdef DEBUG

printf("------\n");
printf("search_row=%ld reliable=%d\n",
       it->att->search_row, it->att->search_qs_reliable);


printf("search_q0: (%g, %g, %g, %g) \n",
       it->att->search_q0->p[0], it->att->search_q0->p[1], 
       it->att->search_q0->p[2], it->att->search_q0->p[3] );

printf("search_q1: (%g, %g, %g, %g) \n",
       it->att->search_q1->p[0], it->att->search_q1->p[1], 
       it->att->search_q1->p[2], it->att->search_q1->p[3] );

printf("hatq: (%g, %g, %g, %g) \n",
       it->att->hatq->p[0], it->att->hatq->p[1], 
       it->att->hatq->p[2], it->att->hatq->p[3] );

printf("search_time0 = %.14g search_time_1 = %.14g\n",
       it->att->search_time0, it->att->search_time1 );

printf("time1 = %.14g time2 = %.14g diff=%g\n",
       it->time1, it->time2, it->time2 - it->time1);


printf("q1: (%g, %g, %g, %g)\n",
       it->q1->p[0], it->q1->p[1], it->q1->p[2], it->q1->p[3]);

printf("q2: (%g, %g, %g, %g)\n",
       it->q2->p[0], it->q2->p[1], it->q2->p[2], it->q2->p[3]);

#endif


    /*************************************
    * see how many steps we need to take *
    *************************************/
    getQuatOfChange(it->delta_q, it->q1, it->q2);
    dist=2.*getQuatHalfRotation(it->delta_q);

#ifdef DEBUG
    printf("dist=%g nsteps=%d\n\n", dist, it->nsteps);
#endif

    /***********************************************************
    * split the full rotation into a number of pieces which are
    * no bigger than the max_rotation param
    ************************************************************/
    it->nsteps=(int)(dist/it->max_rotation)+1;
    it->duration=(it->time2 - it->time1)/it->nsteps;

    it->step=0;

#ifdef DEBUG
printf("    time %.14g - %.14g duration=%g nsteps=%d\n", 
       it->time1, it->time2, it->duration, it->nsteps);



printf("    nsteps=%d dist=%g max_rotation=%g\n",
        it->nsteps, dist, it->max_rotation);
#endif





} /* end of setAttIteratorInterval function */

/******************************************************************************
* resets the iterator to the beginning of a new GTI. sucessive iterations
* will continue until the given stop time is reached
******************************************************************************/
void setAttIteratorInterval(ATTITERATOR* it, double start, double stop) {

    it->stop=stop;
    setAttIteratorStartTime(it, start);

} /* end of setAttIteratorInterval function */


/******************************************************************************
* increment the iterator one step
******************************************************************************/
void nextAttIteratorStep(ATTITERATOR* it) {

    /*********************
    * increment the step *
    *********************/
    ++(it->step);
    
#ifdef DEBUG
printf("nextAttIteratorStep: step=%d nsteps=%d\n", it->step, it->nsteps);
#endif

    /**********************************
    * see if we have run out of steps *
    **********************************/
    if(it->step>=it->nsteps) {
        /**************************
        * go to the next interval *
        **************************/
        setAttIteratorStartTime(it, it->time2);
    }


} /* end of nextAttIteratorStep function */

/******************************************************************************
* sets the given quaternion to the attitude at the middle of the current
* step. Returns the duration of that step.
******************************************************************************/
double getAttIteratorValues(ATTITERATOR* it, QUAT* q) {

    double hat;

    hat=((double)(it->step)+.5)/(double)(it->nsteps);
#ifdef DEBUG
printf("getAttIteratorValues: hat=%g\n", hat);
#endif

    multiplyQuatByScalar(it->hat_q, it->delta_q, hat);

    productOfQuats(q, it->q1, it->hat_q);

    return it->duration;

} /* end of getAttIteratorValues */

/******************************************************************************
* returns false if the iteration is done
******************************************************************************/
int moreAttIteratorSteps(ATTITERATOR* it) {

#ifdef DEBUG
printf("moreAttIteratorSteps: time1=%g stop=%g diff=%g\n",
       it->time1, it->stop, it->time1 - it->stop);
#endif

    return (it->time1 < it->stop );

} /* end of moreAttIteratorSteps method */

