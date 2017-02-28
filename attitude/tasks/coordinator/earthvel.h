#ifndef EARTHVEL_H
#define EARTHVEL_H

/*
 * $Source: /headas/headas/attitude/tasks/coordinator/earthvel.h,v $
 * $Revision: 1.1 $
 * $Date: 2005/02/01 17:02:59 $
 *
 *
 * $Log: earthvel.h,v $
 * Revision 1.1  2005/02/01 17:02:59  rwiegand
 * Updated the earth about sun velocity model.
 *
 */


/*
 * returns negative of earth velocity with respect to sun at mjd
 * in units of C
 */
double compat_earthvel_at_mjd (double vhat[3], double mjd);


#endif
