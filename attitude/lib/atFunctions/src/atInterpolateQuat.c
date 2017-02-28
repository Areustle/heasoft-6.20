/****************************************************************
  atInterpolateQuat
	interpolation between two sets of q-parameters

ñ�� quaternion �� [(x, y, z), w] �Ȥ�����硤�٥��ȥ� (x, y, z) ��
�Ȥ�����ž��ɽ�����뤳�Ȥ��Ǥ��롣ñ�̥٥��ȥ� e �򼴤Ȥ��ơ����� 2��
��ž�������硢quaternion �Ȥ��� [e sin��, cos��] ����ɬ�פ����롣
quaternion q �ϡ�ľ�ܡ��٥��ȥ� v �� quaternion ���Ѥ˻��Ѥ��ƥ٥��ȥ�
v ���ž�����뤳�Ȥ��Ǥ��롣

��=0 �Ȥ������� [0, 0, 0, 1] �ȤʤäƲ�ž�����ñ�̹���ˤʤ롣
[(x, y, z), w] �ε��Ѵ��Ϧ� -> -�ȤȤ��� [(-x, -y, -z), w] �Ǥ��ꡢ
[(x, y, z), w] �� [(-x, -y, -z), -w] ��Ʊ�����Ѵ���ɽ����

����: http://www.nk.rim.or.jp/~jun/3d_info/quaternion.html

	2005/10/09 Y.ISHISAKI	version 2.5

	2008/01/14 Y.ISHISAKI	version 3.0
		bug fix in judging (sin_phi < EPS) as same q-parameter
		use atan2() instead of acos()

****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "atFunctions.h"
#include "atError.h"

int
atInterpolateQuat(
	double t0, AtQuat q0,	/* input: q-parameter q0 at time t0 */
	double t1, AtQuat q1,	/* input: q-parameter q0 at time t0 */
	double t,  AtQuat q		/* input: time t
							   output: interpolated q-parameter q */
)
{
	double phi, sin_phi, phi_t, sin_phi_t;
	AtQuat q0inv, deltaq, deltaq_t;
	AtVect e;

	if ( t0 == t1 ) {		/* same time, no need for interpolation */
		memcpy(q, q0, sizeof(AtQuat));
		return NORMAL_END;
	}

	q0inv[0] = - q0[0];
	q0inv[1] = - q0[1];
	q0inv[2] = - q0[2];
	q0inv[3] =   q0[3];

	atQuatProd(q0inv, q1, deltaq);

	sin_phi =sqrt(deltaq[0]*deltaq[0]+deltaq[1]*deltaq[1]+deltaq[2]*deltaq[2]);

	if ( sin_phi < EPS ) {	/* 0 <= sin_phi, so that fabs() not needed */
		/* same q-parameter, no need for interpolation */
		memcpy(q, q0, sizeof(AtQuat));
		return NORMAL_END;
	}

	e[0] = deltaq[0] / sin_phi;
	e[1] = deltaq[1] / sin_phi;
	e[2] = deltaq[2] / sin_phi;

	phi = atan2(sin_phi, deltaq[3]);	/* [0, PI], because 0 <= sin_phi */
	if ( PI/2 < phi ) {
		phi = phi - PI;		/* (-PI/2, PI/2], choosing smaller |phi| path */
	}
	phi_t = phi * (t - t0) / (t1 - t0);

	sin_phi_t = sin(phi_t);
	deltaq_t[0] = e[0] * sin_phi_t;
	deltaq_t[1] = e[1] * sin_phi_t;
	deltaq_t[2] = e[2] * sin_phi_t;
	deltaq_t[3] = cos(phi_t);

	atQuatProd(q0, deltaq_t, q);

	return NORMAL_END;
}
