/* $Id: aste_hdouble.c,v 1.11 2007/05/07 16:45:32 ishisaki Exp $ */
/************************************************************************
  aste_hdouble.c:	convert network byte order (little endian)
					into host byte order (cpu dependent)

  double aste_hostdouble(double v);	// network double -> host double
  double aste_netdouble(double v);	// host double -> network double
  void aste_hostdouble_array(double array[], int n);	// -> host double array
  void aste_netdouble_array(double array[], int n);	// -> network double array

  float aste_hostfloat(float v);	// network float -> host float
  float aste_netfloat(float v);	// network float -> host float
  void aste_hostfloat_array(float array[], int n);	// -> host float array
  void aste_netfloat_array(float array[], int n);	// -> network float array


  2003/12/16	Y.ISHISAKI	version 1.25

  2004/05/10	Y.ISHISAKI	version 1.26
	include sys/types.h for Darwin

************************************************************************/

#include <sys/types.h>
#include <netinet/in.h>
#include "aste_hdouble.h"

double
aste_hostdouble(double v)
{
	static double z = 100.0;
	char buf[8], *p;
	if ( *(char*)&z ) return v;		/* for sun */
	p = (char*)&v;
	buf[0] = p[7];
	buf[1] = p[6];
	buf[2] = p[5];
	buf[3] = p[4];
	buf[4] = p[3];
	buf[5] = p[2];
	buf[6] = p[1];
	buf[7] = p[0];
	return *(double*)buf;
}

double
aste_netdouble(double v)
{
	static double z = 100.0;
	char buf[8], *p;
	if ( *(char*)&z ) return v;		/* for sun */
	p = (char*)&v;
	buf[0] = p[7];
	buf[1] = p[6];
	buf[2] = p[5];
	buf[3] = p[4];
	buf[4] = p[3];
	buf[5] = p[2];
	buf[6] = p[1];
	buf[7] = p[0];
	return *(double*)buf;
}

void
aste_hostdouble_array(double array[], int n)
{
	static double z = 100.0;
	int i;
	char buf[8], *p;
	if ( *(char*)&z ) return;		/* for sun */
	for (i = 0; i < n; i++) {
		p = (char*)&array[i];
		buf[0] = p[7];
		buf[1] = p[6];
		buf[2] = p[5];
		buf[3] = p[4];
		buf[4] = p[3];
		buf[5] = p[2];
		buf[6] = p[1];
		buf[7] = p[0];
		array[i] = *(double*)buf;
	}
}

void
aste_netdouble_array(double array[], int n)
{
	static double z = 100.0;
	int i;
	char buf[8], *p;
	if ( *(char*)&z ) return;		/* for sun */
	for (i = 0; i < n; i++) {
		p = (char*)&array[i];
		buf[0] = p[7];
		buf[1] = p[6];
		buf[2] = p[5];
		buf[3] = p[4];
		buf[4] = p[3];
		buf[5] = p[2];
		buf[6] = p[1];
		buf[7] = p[0];
		array[i] = *(double*)buf;
	}
}

float
aste_hostfloat(float v)
{
	int *p;
	p = (int *)&v;
	*(int *)&v = ntohl(*p);
	return v;
}

float
aste_netfloat(float v)
{
	int *p;
	p = (int *)&v;
	*(int *)&v = htonl(*p);
	return v;
}

void
aste_hostfloat_array(float array[], int n)
{
	int i, *p;
	if ( 1 == ntohl(1) ) {
		return;
	}
	for (i = 0; i < n; i++) {
		p = (int *)(array + i);
		*(int *)(array + i) = ntohl(*p);
	}
}

void
aste_netfloat_array(float array[], int n)
{
	int i, *p;
	if ( 1 == htonl(1) ) {
		return;
	}
	for (i = 0; i < n; i++) {
		p = (int *)(array + i);
		*(int *)(array + i) = htonl(*p);
	}
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
