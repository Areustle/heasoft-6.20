#ifndef ITERATOR_INCLUDED

#include "coordfits.h"

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


} AttitudeIterator;

/******************************************************************************
* constructor. Note setIteratorInterval must be called before the iterator can
* be used.
******************************************************************************/
AttitudeIterator* allocateIterator(ATTFILE* att, double max_rotation );

/******************************************************************************
* destructor
******************************************************************************/
void destroyIterator(AttitudeIterator* it);

/******************************************************************************
* This function is for internal use only. it sets the end time for an
* interval provided the start time has already been set.
******************************************************************************/
double findIteratorEndTime(AttitudeIterator* it);

/******************************************************************************
* This function is mostly forinterval use. It sets the current interval
* given a start time. This time must be less than the stop time
******************************************************************************/
void setIteratorStartTime(AttitudeIterator* it, double time1 );

/******************************************************************************
* resets the iterator to the beginning of a new GTI. sucessive iterations
* will continue until the given stop time is reached
******************************************************************************/
void setIteratorInterval(AttitudeIterator* it, double start, double stop);

/******************************************************************************
* increment the iterator one step
******************************************************************************/
void nextIteratorStep(AttitudeIterator* it);

/******************************************************************************
* sets the given quaternion to the attitude at the middle of the current
* step. Returns the duration of that step.
******************************************************************************/
double getIteratorValues(AttitudeIterator* it, QUAT* q);

/******************************************************************************
* returns false if the iteration is done
******************************************************************************/
int moreIteratorSteps(AttitudeIterator* it);

#define ITERATOR_INCLUDED
#endif /* ITERATOR_INCLUDED */
