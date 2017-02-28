/* $Id: aste_hdouble.h,v 1.12 2007/05/07 16:45:31 ishisaki Exp $ */

#ifndef _ASTE_HDOUBLE_H_
#define _ASTE_HDOUBLE_H_

#ifdef __cplusplus
extern "C" {
#endif

double aste_hostdouble(double v);
double aste_netdouble(double v);
void aste_hostdouble_array(double array[], int n);
void aste_netdouble_array(double array[], int n);

float aste_hostfloat(float v);
float aste_netfloat(float v);
void aste_hostfloat_array(float array[], int n);
void aste_netfloat_array(float array[], int n);

#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_HDOUBLE_H_ */
