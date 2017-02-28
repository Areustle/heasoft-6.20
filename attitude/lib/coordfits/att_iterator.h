#ifndef ATT_ITERATOR_INCLUDED

#include "attfile.h"

typedef struct {

    ATTFILE* att;
    double stop;
    double max_rotation;

    double time1;
    double time2;

    QUAT* q1;
    QUAT* q2;

    QUAT* delta_q;
    QUAT* hat_q;

    int nsteps;
    double duration;
    int step;


} ATTITERATOR;

/******************************************************************************
* constructor. Note setAttIteratorInterval must be called before the iterator can
* be used.
******************************************************************************/
ATTITERATOR* allocateAttIterator(ATTFILE* att, double max_rotation );

/******************************************************************************
* destructor
******************************************************************************/
void destroyAttIterator(ATTITERATOR* it);

/******************************************************************************
* This function is for internal use only. it sets the end time for an
* interval provided the start time has already been set.
******************************************************************************/
double findAttIteratorEndTime(ATTITERATOR* it);

/******************************************************************************
* This function is mostly for internal use. It sets the current interval
* given a start time. This time must be less than the stop time
******************************************************************************/
void setAttIteratorStartTime(ATTITERATOR* it, double time1 );

/******************************************************************************
* resets the iterator to the beginning of a new GTI. sucessive iterations
* will continue until the given stop time is reached
******************************************************************************/
void setAttIteratorInterval(ATTITERATOR* it, double start, double stop);

/******************************************************************************
* increment the iterator one step
******************************************************************************/
void nextAttIteratorStep(ATTITERATOR* it);

/******************************************************************************
* sets the given quaternion to the attitude at the middle of the current
* step. Returns the duration of that step.
******************************************************************************/
double getAttIteratorValues(ATTITERATOR* it, QUAT* q);

/******************************************************************************
* returns false if the iteration is done
******************************************************************************/
int moreAttIteratorSteps(ATTITERATOR* it);


#define ATT_ITERATOR_INCLUDED
#endif /* ATT_ITERATOR_INCLUDED */
