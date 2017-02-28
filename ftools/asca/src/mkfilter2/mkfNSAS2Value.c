#include <stdio.h>
#include <math.h>
#include <string.h>
#include "snsrdef.h"
#include "snsrfun.h"
/*
extern void	mv_mul();
extern void	v_sub();
extern double	v_unit();
*/
/************************************************************************/
/*  �ӣӣ������۳Ѥ������������٥��ȥ�����				*/
/*	����   : IN   SSAS_no		SSAS����  0:SSAS-SA, 1:SSAS-SB	*/
/*		 IN   sun_ang		SSAS���۳�			*/
/*		 OUT  sun_ssas[3]	����������SSAS��ɸ�ϡ�          */
/*	����� : �ʤ�						   	*/
/************************************************************************/
/*
void ssas_to_sunvec( SSAS_no, sun_ang, sun_ssas )
	int	SSAS_no;
	double	sun_ang, sun_ssas[3];
{
	double	sun_body[3];

	sun_body[0] =  0.0e0;
	sun_body[1] =  cos( sun_ang );
	sun_body[2] = -sin( sun_ang );

	mv_mul( &bodyn_to_ssas[SSAS_no][0][0], &sun_body[0], &sun_ssas[0], 3, 3 );
}
 */

/*******************************************************************/
/*  �Σӣ������۳Ѥ������������٥��ȥ�����			   */
/*	����   : IN   nsas_pix[2]	NSAS���۳� X,Y		   */
/*		 OUT  sun_nsas[3]	����������NSAS��ɸ�ϡ�     */
/*	����� : INT��	0:����	1:�˥塼�ȥ�ˡ����«���ʤ�         */
/*******************************************************************/
int nsas_to_sunvec( nsas_pix, sun_nsas )
	double	nsas_pix[2], sun_nsas[3];
{
	int	alarm;
	double	NSAS_angle[2], tan_thetax, tan_thetay, fbuf1, fbuf2;

	alarm = NSAS_pixel_to_angle( nsas_pix, NSAS_angle );
	tan_thetax = tan( NSAS_angle[0] );
	tan_thetay = tan( NSAS_angle[1] );
	fbuf1 = 1.0e0 + tan_thetax * tan_thetax	+ tan_thetay * tan_thetay;
	fbuf2 = sqrt( fbuf1 );
	sun_nsas[0] = tan_thetax / fbuf2;
	sun_nsas[1] = tan_thetay / fbuf2;
	sun_nsas[2] = 1.0e0 / fbuf2;

	return( alarm );
}

#define NSAS_MAX_ITERATION	5	/*���Σӣ��ӷ׻������֤��������*/

/*  �˥塼�ȥ�ˡ�ˤ��Σӣ��Ӳ��Ǥ�����٤ؤ��Ѵ�  */
int NSAS_pixel_to_angle( pixel, NSAS_angle )
	double	pixel[2], NSAS_angle[2];
{
	int	i, j;
	double	fbuf1, fbuf2, fbuf3, fbuf4, fbuf5, fbuf6, fbuf7, fbuf8;
	double	NSAS_angle_bef[2], nsas_jacobian[2][2], nsas_residual[2],
		nsas_tana, nsas_tanb, nsas_det;

	/*�����ٽ������*/
	NSAS_angle[0] = 0.0e0;
	NSAS_angle[1] = 0.0e0;
	NSAS_angle_bef[0] = 0.0e0;
	NSAS_angle_bef[1] = 0.0e0;

	/*���˥塼�ȥ�ˡ��*/
	for( i = 0; i < NSAS_MAX_ITERATION; i++ ){
		/*���䥳�ӥ��󡿻ĺ��׻���*/
		nsas_jacobian[0][0] = 0.0e0;
		nsas_jacobian[0][1] = 0.0e0;
		nsas_jacobian[1][0] = 0.0e0;
		nsas_jacobian[1][1] = 0.0e0;
		nsas_residual[0]    = 0.0e0;
		nsas_tana = tan( NSAS_angle[0] );
		nsas_tanb = tan( NSAS_angle[1] );
		fbuf1 = nsas_tana * nsas_tana;
		fbuf2 = nsas_tanb * nsas_tanb;
		fbuf3 = nsas_tana * nsas_tanb;
		fbuf4 = fbuf1 + fbuf2;
		for( j = 0; j < 4; j++ ){
			fbuf5 = nsas_n[j] * nsas_n[j];
			fbuf6 = fbuf5 - 1.0e0;
			fbuf7 = fbuf5 + fbuf6 * fbuf4;
			fbuf8 = sqrt( fbuf7 );
			fbuf7 *= fbuf8;
			nsas_residual[0] += nsas_h[j] / fbuf8;
			nsas_jacobian[0][0] += nsas_h[j] * ( fbuf5 + fbuf6
				* fbuf2 ) / fbuf7;
			nsas_jacobian[0][1] -= nsas_h[j] * fbuf6
				* fbuf3 / fbuf7;
			nsas_jacobian[1][0] -= nsas_h[j] * fbuf6
				* fbuf3 / fbuf7;
			nsas_jacobian[1][1] += nsas_h[j] * ( fbuf5 + fbuf6
				* fbuf1 ) / fbuf7;
		}
		nsas_residual[1] = nsas_residual[0] * nsas_tanb / nsas_l + nsas_yc - pixel[1];
		nsas_residual[0] = nsas_residual[0] * nsas_tana / nsas_l + nsas_xc - pixel[0];
		fbuf1 = ( 1.0e0 + fbuf1 ) / nsas_l;
		fbuf2 = ( 1.0e0 + fbuf2 ) / nsas_l;
		nsas_jacobian[0][0] *= fbuf1;
		nsas_jacobian[0][1] *= fbuf2;
		nsas_jacobian[1][0] *= fbuf1;
		nsas_jacobian[1][1] *= fbuf2;
		/*�����ٹ�����*/
		nsas_det = nsas_jacobian[0][0] * nsas_jacobian[1][1]
			 - nsas_jacobian[0][1] * nsas_jacobian[1][0];
		NSAS_angle[0] -= ( nsas_jacobian[1][1] * nsas_residual[0]
			- nsas_jacobian[0][1] * nsas_residual[1] ) / nsas_det;
		NSAS_angle[1] -= ( - nsas_jacobian[1][0] * nsas_residual[0]
			+ nsas_jacobian[0][0] * nsas_residual[1] ) / nsas_det;
		/*����«Ƚ�ꡡ*/
		if( ( fabs( NSAS_angle[0] - NSAS_angle_bef[0] ) <= nsas_eps )
			&& ( fabs( NSAS_angle[1] - NSAS_angle_bef[1] ) <= nsas_eps ) ){
			break;
		}
		else{
			memcpy( (char *)NSAS_angle_bef, (char *)NSAS_angle, sizeof(NSAS_angle_bef) );
		}
	}
	if( i >= NSAS_MAX_ITERATION ){
		return( 1 );
	}
	else{
		return( 0 );
	}
}

/*******************************************************************/
/*  �ӣԣԲ��ǥǡ��������������٥��ȥ�����			   */
/*	����   : IN   STT_no		STT����  0:STT-A, 1:STT-B  */
/*		 IN   stt_h		STT������ H		   */
/*		 IN   stt_v		STT������ V		   */
/*		 OUT  star_stt[3]	��������STT��ɸ�ϡ�        */
/*	����� : �ʤ�						   */
/*******************************************************************/
void stt_to_starvec( STT_no, stt_h, stt_v, star_stt )
	unsigned int	STT_no;		/*  0:STT-A  1:STT-B  */
	double	stt_h, stt_v, star_stt[3];
{
	double	stt_r, d_h, d_v, gzai, nrm_cnst;

	d_h = stt_h - STT_h0[STT_no];
	d_v = stt_v - STT_v0[STT_no];
	stt_r = sqrt( d_h * d_h + d_v * d_v );
	gzai = STT_delta1[STT_no] * d_h + STT_delta2[STT_no] * d_v
		+ STT_gamma1[STT_no] * stt_r * ( stt_r + STT_gamma2[STT_no] );
	d_h *= 1.0e0 + gzai;
	d_v *= 1.0e0 + gzai;
	nrm_cnst = sqrt( STT_f[STT_no] * STT_f[STT_no] + d_h * d_h + d_v * d_v );
	star_stt[0] = - d_v / nrm_cnst;
	star_stt[1] = d_h / nrm_cnst;
	star_stt[2] = STT_f[STT_no] / nrm_cnst;
}

/*********************************************************************/
/*  �ǣ��ӥǡ��������ϼ��������٥��ȥ�����			     */
/*	����   : IN   gas_data[3]	GAS����		             */
/*		 IN   mtq_bias_data[3]	MTQ�ˤ��Х�������	     */
/*		 OUT  mag_vec[3]	�ϼ���������GAS��ɸ�ϡ�      */
/*	����� : double��	�ϼ����������			     */
/*********************************************************************/
/*
double gas_to_magvec( gas_data, mtq_bias_data, mag_vec )
	double	gas_data[3], mag_vec[3];
	double	mtq_bias_data[3];
{
	double	mag_abs;

	v_sub( gas_data, mtq_bias_data, mag_vec, 3 );
	mag_abs = v_unit( mag_vec, mag_vec, 3 );

	return( mag_abs );
}
 */
