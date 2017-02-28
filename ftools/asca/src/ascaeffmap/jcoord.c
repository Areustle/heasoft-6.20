/************************************************************************/
/*	jcoord	��������
/*
/*	ver 1.0	94/01/01
/*		first coded
/*	ver 1.1	94/01/12
/*		������롼�����̾�Τ��ѹ���
/*	ver 1.3   94/03/28
/*		ŷ���ɸ�ȸ��д��ɸ���Ѵ���SIS�κ�ɸ�Ѵ��ؿ����ɲá�
/*		cfitsio����Ѥ���褦�ˤ�����
/*	ver 1.4   94/04/07  by Ishisaki
/*		�ؿ�̾�������ǽ��jc_��Ĥ�����
/*		jc_init�ǥե����륯�������ɲá�
/*		AtEulerAng��ݥ����Ϥ����ѹ���        
/*	ver 1.5	94/04/10
/*		�������ǧ�ؿ�jc_isinit���ɲá�
/*		������򥻥���˹Ԥʤ���褦��jc_init���ѹ���
/*		jcSetSisVal�ǥ�����ɤ˳�����Ƥ�������礭��������
/*		SIS1���Ѵ����Ǥ��ʤ��Ȥ����Х���fix������
/*		����ѥ������NOATFUNC����������atFunctions����Ѥ�����ʬ��
/*		�Ȥ�ʤ��褦�ˤ�����
/*	ver 1.6   94/04/11
/*		FORTRAN��LUN���֤Ĥ���ʤ��褦�˰����ǥ桼����Ϳ����褦�ˤ�����
/*	ver 1.7   94/04/12
/*		jcSetInitFlag��jcSetSisMat�ǳ�����Ƥ�������礭������ʸ�����
/*		Ĺ��ʬ�����ʤ��Ȥ����Х�����(�к��λ�Ŧ�ˤ��)��
/*	ver 1.8	94/04/21
/*		teldef���ɤ�ʤ��ä����Υ�����������(�к��ˤ��)��
/*		����ޤ�void�����ä��ؿ���int�����ѹ������ơ��������֤��褦�ˤ�����
/*	ver 1.9   94/04/25
/*		jcSetSisMat�ǥ��å�0���ͤ��ɤ����ȴ���Ƥ��ޤ��Ȥ���V1.8�ǿ�����
/*		�����Ƥ��ޤä��Х�������
/*	ver 2.0   94/05/05 (�к�)
/*		float -> double
/*		���顼�����å��ζ���
/*		�ѿ���Ȥäƥ�����ɤ򻲾�
/*		80 ��ˤ�������
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifndef NOATFUNC
#include "atFunctions.h"
#endif /*NOATFUNC*/

#include "jcoord.h"
#include "cfitsio.h"

/* �ޥ��� */
#define I			int
#define D			double	/* 1.9 �ޤ� float */
#define DD			double	/* ��Ȥ�� double */
#define unless(a)	if(!(a))

/* ��� */
#define NSENSOR		4
#define NSIS		2
#define NCCD		4
#define INVERSE		-1.0
#define READONLY	0
#define CHARLEN		73
#define ARCMIN2RAD	0.000290888209
#define DEG2RADIAN	0.017453292519943295769
#define RADIAN2DEG	57.295779513082320877
#define NO	0
#define YES	1

/*#define NOERRMSG	/* write no error messages */

#ifdef NOERRMSG

#define CHKSENSOR(sensor)
#define CHKCOM(sensor)
#define CHKALIGN(sensor)
#define CHKSIS(sensor)
#define CHKCCD(sensor, ccdID)

#else

#define CHKSENSOR(sensor) \
	unless ( (unsigned)sensor < NSENSOR ) { \
		fprintf(stderr, "JCOORD: Invalid Sensor ID (%d)\n", sensor); \
		return JC_INVALID_SENSOR; \
	}

#define CHKCOM(sensor) \
	CHKSENSOR(sensor); \
	if ( NULL == jc_teldef[sensor] ) { \
		fprintf(stderr, "JCOORD: Sensor %d not initialized\n", sensor); \
		return JC_NOT_INITIALIZED; \
	}						 

#define CHKALIGN(sensor) \
	CHKCOM(sensor); \
	if ( ! jc_com[sensor].align.ok ) { \
		fprintf(stderr, "JCOORD: Sensor %d alignment error\n", sensor); \
		return JC_NOT_INITIALIZED; \
	}

#define CHKSIS(sensor) \
	unless ( (unsigned)sensor < NSIS ) { \
		fprintf(stderr, "JCOORD: Sensor %d not SIS\n", sensor); \
		return JC_NOT_SIS; \
	} \
	if ( NULL == jc_teldef[sensor] || ! jc_sis[sensor].ok ) { \
		fprintf(stderr, "JCOORD: SIS %d not initialized\n", sensor); \
		return JC_NOT_INITIALIZED; \
	}

#define CHKCCD(sensor, ccdID) \
	CHKSIS(sensor); \
	unless ( (unsigned)ccdID < NCCD ) { \
		fprintf(stderr, "JCOORD: Invalid CCD ID (%d)\n", ccdID); \
		return JC_INVALID_CCD; \
	}
#endif	/* NOERRMSG */

/* static�ѿ� */
static char *jc_teldef[NSENSOR] = {
	NULL, NULL, NULL, NULL		/* ������˻��Ѥ��줿teldef�ե����� */
};

static struct JC_COM {
	double focallen;			/* ������Υ */
	double mm2arcmin;
	double optaxisx, optaxisy;	/* ���� */
	double det_xcen, det_ycen;	/* �濴�Υԥ����� */
	double det_xscl, det_yscl;	/* �������� [mm/ch] */
#ifndef NOATFUNC
	struct JC_ALIGN {
		int ok;
		AtRotMat rm, rm_inv;	/* ���д異�饤����ȹ��� �� ���εչ��� */
	} align;
#endif
} jc_com[NSENSOR];

static struct JC_SIS {
	int ok;
	struct JC_SIS_CCD {
		double tv[2];			/* SIS translation vector */
		double rm[2][2];		/* SIS rotation matrix */
		double rm_inv[2][2];
	} ccd[NCCD];
} jc_sis[NSIS];

/************************************************************************/
/* �ץ�ȥ��������
/************************************************************************/

/* void fcopen();
void fcgkyd();
void fcclos(); */

/************************************************************************/
/*  jcoord����ǻ��Ѥ����桼�ƥ���ƥ��ؿ���
/************************************************************************/

/************************************************************************/
/*  jcDeterminant
/*    2x2�ι���ι��󼰤�׻�����
/*
/*  �����
/*	���󼰤���
/************************************************************************/
static double
jcDeterminant(mat)
     double (*mat)[2];
{
	return (mat[0][0]*mat[1][1] - mat[1][0]*mat[0][1]);
}

/************************************************************************/
/*  jcSetSisMat
/*    SIS�κ�ɸ�Ѵ�����򥻥åȤ���
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/*	��JC_DIVIDE_BY_ZERO��������� (���󼰤�����)
/************************************************************************/
static int
jcInvMat(mat, mat_inv)
     double (*mat)[2];
     double (*mat_inv)[2];
{
	double delta;
	
	delta = jcDeterminant(mat);
	if ( 0.0 == delta ) return JC_DIVIDE_BY_ZERO;
	
	mat_inv[0][0] =  mat[1][1]/delta;
	mat_inv[0][1] = -mat[0][1]/delta;
	mat_inv[1][0] = -mat[1][0]/delta;
	mat_inv[1][1] =  mat[0][0]/delta;
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jcWhichChip
/*    native��ɸ�ξݸ¤�Ĵ�٤ƤɤΥ��åפ�°���뤫���֤��ؿ�
/*    �ݸ¤������ʤ��Τǡ����åפ��ºݤˤ��뤫�ɤ����ޤǤ�Ĵ�٤ʤ�
/*
/*  �����
/*	CCDID (�ݸ¾���Τ�)
/************************************************************************/
static int
jcWhichChip(nat_x, nat_y)
     double nat_x;
     double nat_y;
{
	double x, y;
	x = nat_x - 640.5;
	y = nat_y - 640.5;
	if ( 0.0 < x ) {
		if ( 0.0 < y ) {
			return 1;		/*�裱�ݸ� = c1*/
		} else {
			return 2;		/*�裴�ݸ� = c2*/
		}
	} else {
		if ( 0.0 < y ) {
    		return 0;		/*�裲�ݸ� = c0*/
		} else {
			return 3;		/*�裳�ݸ� = c3*/
		}
	}
}


/************************************************************************/
/*  �桼�������󶡤����ؿ���
/************************************************************************/

/************************************************************************/
/*  jc_init
/*    jcoord������ؿ�
/*
/*    �����
/*	��JC_NORMAL_END�����ｪλ
/*	��JC_DIVIDE_BY_ZERO��������Υ������
/*	��JC_SISMAT_ERR��SIS�ι���׻�����
/*      ��FITSIO��status��FITSIO���顼 (>0)
/************************************************************************/
int
jc_init(unit, sensor, teldef)
     int unit;
     int sensor;
     char *teldef;
{
	static struct JC_COM com;
	static struct JC_SIS sis;
	static struct {
		char *k;
		double *v;
	} com_table[] = {
		{ "FOCALLEN", &com.focallen },
		{ "OPTAXISX", &com.optaxisx },
		{ "OPTAXISY", &com.optaxisy },
		{ "DET_XCEN", &com.det_xcen },
		{ "DET_YCEN", &com.det_ycen },
		{ "DET_XSCL", &com.det_xscl },
		{ "DET_YSCL", &com.det_yscl },
		{ NULL, NULL }
#ifndef NOATFUNC
	}, align_table[] = {
		{ "ALIGNM11", &com.align.rm[0][0] },
		{ "ALIGNM12", &com.align.rm[0][1] },
		{ "ALIGNM13", &com.align.rm[0][2] },
		{ "ALIGNM21", &com.align.rm[1][0] },
		{ "ALIGNM22", &com.align.rm[1][1] },
		{ "ALIGNM23", &com.align.rm[1][2] },
		{ "ALIGNM31", &com.align.rm[2][0] },
		{ "ALIGNM32", &com.align.rm[2][1] },
		{ "ALIGNM33", &com.align.rm[2][2] },
		{ NULL, NULL }
#endif
	}, sis_table[] = {
		{ "COE_X0_A", &sis.ccd[0].tv[0] },
		{ "COE_Y0_A", &sis.ccd[0].tv[1] },
		{ "COE_X0_B", &sis.ccd[0].rm[0][0] },
		{ "COE_Y0_B", &sis.ccd[0].rm[0][1] },
		{ "COE_X0_C", &sis.ccd[0].rm[1][0] },
		{ "COE_Y0_C", &sis.ccd[0].rm[1][1] },
		{ "COE_X1_A", &sis.ccd[1].tv[0] },
		{ "COE_Y1_A", &sis.ccd[1].tv[1] },
		{ "COE_X1_B", &sis.ccd[1].rm[0][0] },
		{ "COE_Y1_B", &sis.ccd[1].rm[0][1] },
		{ "COE_X1_C", &sis.ccd[1].rm[1][0] },
		{ "COE_Y1_C", &sis.ccd[1].rm[1][1] },
		{ "COE_X2_A", &sis.ccd[2].tv[0] },
		{ "COE_Y2_A", &sis.ccd[2].tv[1] },
		{ "COE_X2_B", &sis.ccd[2].rm[0][0] },
		{ "COE_Y2_B", &sis.ccd[2].rm[0][1] },
		{ "COE_X2_C", &sis.ccd[2].rm[1][0] },
		{ "COE_Y2_C", &sis.ccd[2].rm[1][1] },
		{ "COE_X3_A", &sis.ccd[3].tv[0] },
		{ "COE_Y3_A", &sis.ccd[3].tv[1] },
		{ "COE_X3_B", &sis.ccd[3].rm[0][0] },
		{ "COE_Y3_B", &sis.ccd[3].rm[0][1] },
		{ "COE_X3_C", &sis.ccd[3].rm[1][0] },
		{ "COE_Y3_C", &sis.ccd[3].rm[1][1] },
		{ NULL, NULL }
	};
	int status = 0;
	int i;
	
	int blocksize;
	char comment[CHARLEN];

	CHKSENSOR(sensor);
	
	FCOPEN(unit, teldef, READONLY, &blocksize, &status);
	if ( status ) return status;
	
/* �����󥵤˶��̤Υѥ�᥿���ɤ߽Ф� */
	for (i = 0; NULL != com_table[i].k; i++) {
		FCGKYD(unit, com_table[i].k, com_table[i].v, comment, &status);
		if ( status ) return status;
	}
	
#ifndef NOATFUNC
	com.align.ok = YES;
	for (i = 0; NULL != align_table[i].k; i++) {
		FCGKYD(unit, align_table[i].k, align_table[i].v, comment, &status);
		if ( status ) {
			com.align.ok = NO;
			status = 0;
			break;
		}
	}
	if ( com.align.ok ) {
		status = atInvRotMat(com.align.rm, com.align.rm_inv);
		if ( status ) return JC_MATRIX_ERR;
	}
#endif
	
	if ( 0.0 == com.focallen ) return JC_DIVIDE_BY_ZERO;
	com.mm2arcmin = RADIAN2DEG*60.0 / com.focallen;
	jc_com[sensor] = com;
	
/* SIS��ͭ�Υѥ�᥿���ɤ߽Ф� */
	if ( sensor < NSIS ) {
		sis.ok = YES;
		for (i = 0; NULL != sis_table[i].k; i++) {
			FCGKYD(unit, sis_table[i].k, sis_table[i].v, comment, &status);
			if ( status ) {
				sis.ok = NO;
				status = 0;
				break;
			}
		}
		if ( sis.ok ) {
			for (i = 0; sis.ok && i < NCCD; i++) {
				status = jcInvMat(sis.ccd[i].rm, sis.ccd[i].rm_inv);
				if ( status ) return JC_SISMAT_ERR;
			}
			jc_sis[sensor] = sis;
		}
	}
	
	FCCLOS(unit, &status);
	if ( status ) return status;

/* ������ɤ߽Ф��줿���ˤϽ�����ե饰��Ω�Ƥ� */
	if ( NULL != jc_teldef[sensor] ) free(jc_teldef[sensor]);
	jc_teldef[sensor] = malloc(strlen(teldef)+1);
	if ( NULL == jc_teldef[sensor] ) return JC_ALLOC_ERR;
	strcpy(jc_teldef[sensor], teldef);
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_isinit
/*    jcoord�������ǧ�ؿ�
/*
/*    �����
/*	��������˻��Ѥ��줿teldef�ե�����̾�ؤΥݥ��� (������Ѥߤξ��)
/*	��NULL�ݥ��� (̤������ξ��)
/************************************************************************/
char *
jc_isinit(sensor)
     int sensor;
{
	return ( (unsigned)sensor < NSENSOR ) ? jc_teldef[sensor] : NULL;
}

/************************************************************************/
/*  jc_xrt2det
/*    XRTX/Y[mm]��DETX/Y[mm]���Ѵ�����
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_xrt2det(sensor, xrt_x, xrt_y, det_x, det_y)
     int sensor;
     double xrt_x;
     double xrt_y;
     double *det_x;
     double *det_y;
{
	struct JC_COM *com;
	
	CHKCOM(sensor);
	com = &jc_com[sensor];
	
	*det_x = xrt_x + com->optaxisx;
	*det_y = (xrt_y + com->optaxisy)*INVERSE;
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_det2xrt
/*    DETX/Y[mm]��XRTX/Y[mm]���Ѵ�����
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_det2xrt(sensor, det_x, det_y, xrt_x, xrt_y)
     int sensor;
     double det_x;
     double det_y;
     double *xrt_x;
     double *xrt_y;
{
	struct JC_COM *com;
	
	CHKCOM(sensor);
	com = &jc_com[sensor];
	
	*xrt_x = det_x - com->optaxisx;
	*xrt_y = det_y*INVERSE - com->optaxisy;
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_xrt_rec2pol
/*    XRTX/Y[mm]��XRT��[arcmin]/��[deg]���Ѵ�����
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_xrt_rec2pol(sensor, xrt_x, xrt_y, theta, phi)
     int sensor;
     double xrt_x;
     double xrt_y;
     double *theta;
     double *phi;
{
	CHKCOM(sensor);
	
	*theta = sqrt(xrt_x*xrt_x + xrt_y*xrt_y) * jc_com[sensor].mm2arcmin;
	*phi = atan2(xrt_y, xrt_x)*RADIAN2DEG;
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_xrt_pol2rec
/*    XRT��[arcmin]/��[deg]��XRTX/Y[mm]���Ѵ�����
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_xrt_pol2rec(sensor, theta, phi, xrt_x, xrt_y)
     int sensor;
     double theta;
     double phi;
     double *xrt_x;
     double *xrt_y;
{
	double ftheta;
	
	CHKCOM(sensor);
	ftheta = jc_com[sensor].focallen*(theta*ARCMIN2RAD);
	phi *= DEG2RADIAN;
	*xrt_x = ftheta*cos(phi);
	*xrt_y = ftheta*sin(phi);
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_det_mm2ch
/*    DETX/Y[mm]��DETX/Y[ch]���Ѵ�����
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/*	��JC_DIVIDE_BY_ZERO������ˤ�����(�ԥ����륵�����۾�)
/************************************************************************/
int
jc_det_mm2ch(sensor, det_x, det_y, det_xch, det_ych)
     int sensor;
     double det_x;
     double det_y;
     double *det_xch;
     double *det_ych;
{
	double ch2mm;
	struct JC_COM *com;
	
	CHKCOM(sensor);
	com = &jc_com[sensor];
	
	ch2mm = com->det_xscl;
	if ( 0.0 == ch2mm ) return JC_DIVIDE_BY_ZERO;
	*det_xch = com->det_xcen + det_x/ch2mm;
	
	ch2mm = com->det_yscl;
	if ( 0.0 == ch2mm ) return JC_DIVIDE_BY_ZERO;
	*det_ych = com->det_ycen + det_y/ch2mm;
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_det_ch2mm
/*    DETX/Y[ch]��DETX/Y[mm]���Ѵ�����
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_det_ch2mm(sensor, det_xch, det_ych, det_x, det_y)
     int sensor;
     double det_xch;
     double det_ych;
     double *det_x;
     double *det_y;
{
	struct JC_COM *com;
	
	CHKCOM(sensor);
	com = &jc_com[sensor];
	
	*det_x = (det_xch - com->det_xcen) * com->det_xscl;
	*det_y = (det_ych - com->det_ycen) * com->det_yscl;
	
	return (JC_NORMAL_END);
}

#ifndef NOATFUNC /* NOATFUNC���������Ƥ�����ϻ��Ѥ��ʤ� */
/************************************************************************/
/*  jc_sky2det
/*    (��,��)[deg]��DETX/Y[mm]���Ѵ�����
/*  ���
/*    �����顼�Ѥ�[radian]ñ�̡�ŷ���ɸ��[deg]ñ�̤�������
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_sky2det(ea, sensor, alpha, delta, det_x, det_y)
     AtEulerAng *ea;
     int sensor;
     double alpha;
     double delta;
     double *det_x;
     double *det_y;
{
	AtVect vec_sky;		/* ŷ���ɸ�ϤǤ���ɸŷ�Τΰ��֥٥��ȥ� */
	AtVect vec_sat;		/* ������ɸ�ϤǤ���ɸŷ�Τΰ��֥٥��ȥ� */
	AtVect vec_det;		/* ���д��ɸ�ϤǤ���ɸŷ�Τΰ��֥٥��ȥ� */
	AtRotMat rm_sat;	/* �����λ�����ɽ����ž���� */
	double tmp;
	
	CHKALIGN(sensor);
	
	/* �����顼�Ѥ��ž������Ѵ� */
	atEulerToRM(ea, rm_sat);
	
	/* �ַ��ְޤ�ŷ���ɸ�ϤǤΥ٥��ȥ���Ѵ� */
	atPolDegToVect(1.0, alpha, delta, vec_sky);
	
	/* ŷ���ɸ���������ɸ���Ѵ� */
	atRotVect(rm_sat, vec_sky, vec_sat);
	
	/* ���д�Υ��ե��åȤ����� */
	atRotVect(jc_com[sensor].align.rm, vec_sat, vec_det);
	
	/* ���д��ɸ���Ѵ���look up�ˤ��뤿��Y����ȿž */
	if ( 0.0 == vec_det[2] ) return JC_DIVIDE_BY_ZERO;
	tmp = jc_com[sensor].focallen / vec_det[2];
	*det_x = - tmp * vec_det[0];
	*det_y = - INVERSE * tmp * vec_det[1];
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_det2sky
/*    DETX/Y[mm]��(��,��)[deg]���Ѵ�����
/*  ���
/*    �����顼�Ѥ�[radian]ñ�̡�����ͤ�ŷ���ɸ��[deg]ñ�̤�������
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_det2sky(ea, sensor, det_x, det_y, alpha, delta)
     AtEulerAng *ea;
     int sensor;
     double det_x;
     double det_y;
     double *alpha;
     double *delta;
{
	AtRotMat rm_sat, rm_sat_inv;
	AtVect vec_det;
	AtVect vec_sat;
	AtVect vec_sky;
	double dummy;
	
	CHKALIGN(sensor);
	
	/* �����顼�Ѥ��ž������Ѵ�������Ǥ��εչ������� */
	atEulerToRM(ea, rm_sat);
	atInvRotMat(rm_sat, rm_sat_inv);
	
	/* ���д��ɸ�ϤǤ������٥��ȥ���Ѵ���look down�ˤ��뤿���Y����ȿž */
	vec_det[0] = - det_x;
	vec_det[1] = - INVERSE * det_y;
	vec_det[2] = jc_com[sensor].focallen;
	
	/* ���д�Υ��ե��åȤ��ᤷ�Ʊ�����ɸ�ϤǤ������٥��ȥ�˵��Ѵ� */
	atRotVect(jc_com[sensor].align.rm_inv, vec_det, vec_sat);
	
	/* �����λ�����Ȥä� ŷ���ɸ�ϤǤ������٥��ȥ�˵��Ѵ� */
	atRotVect(rm_sat_inv, vec_sat, vec_sky);
	
	/* ŷ���ɸ���Ѵ� */
	atVectToPolDeg(vec_sky, &dummy, alpha, delta);
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_euler2fov
/*    �����顼�Ѥ��鸡�д��FOV�����ؿ�
/*  ���
/*    �����顼�Ѥ�[radian]ñ�̡�����ͤ�ŷ���ɸ��[deg]ñ�̤�������
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_euler2fov(ea, sensor, alpha, delta, roll)
     AtEulerAng *ea;
     int sensor;
     double *alpha;
     double *delta;
     double *roll;
{
	AtRotMat rm_sat;
	AtRotMat rm_det;
	AtEulerAng ea_det;
	
	CHKALIGN(sensor);
	
	/* �����顼�Ѥ��ž������Ѵ� */
	atEulerToRM(ea, rm_sat);
	
	/* ���д�Υ��饤����ȹ���򤫤��Ƹ��д�β�ž������Ѵ� */
	atRMProd(rm_sat, jc_com[sensor].align.rm, rm_det);
	
	/* ���д�Υ����顼�Ѥ���� */
	atRMToEuler(rm_det, &ea_det);
	
	*alpha = ea_det.phi*RADIAN2DEG;
	*delta = (M_PI_2 - ea_det.theta)*RADIAN2DEG;
	*roll = (M_PI_2 - ea_det.psi)*RADIAN2DEG;
	
	return JC_NORMAL_END;
}
#endif /* NOATFUNC���������Ƥ�����Ϥ����ޤǻ��Ѥ��ʤ� */

/************************************************************************/
/*  jc_sis_raw2det
/*     SIS��RAWX/Y��DETX/Y���Ѵ�����
/*
/*  �Ѵ���ˡ
/*  1. chip2native
/*     teldef�ե�����ˤ������äơ�RAWX/Y��native�ʸ��д��ɸ�Ϥ��Ѵ���
/*     ��������native�ʺ�ɸ�ϤȤϡ���ɸ����SIS�����बQL���եȤʤɤ�
/*     ���Ѥ��Ƥ����Τ�Ʊ�ͤ˼�ꡢ��ɸ���ϰϤ� 1<=x,y<=1280 �������
/*     �濴��(640.5, 640.5)�Ȥ����ΤǤ��롣���ξ�硢���󥵤ˤ�餺
/*     ���åפΰ��֤���ݸ¤�Ʊ���ˤʤ롣
/*     <�Ѵ�����>
/*	  nat_x = AX + BX*RAWX + CX*RAWY
/*	  nat_y = AY + BY*RAWX + CY*RAWY
/*        �Ѵ����� AX,BX,CX,AY,BY,CY��teldef�ե������Ϳ�����롣
/*
/*  2. native2det
/*     native�ʺ�ɸ�Ϥ�S0��-90�١�S1��90�ٲ�ž�����塢Y����ȿž����
/*     DETX/Y���Ѵ����롣
/*     <�Ѵ�����>
/*        S0�ξ��
/*          DETX = 1281 - nat_y
/*	    DETY = 1281 - nat_x
/*        S1�ξ��
/*	    DETX = nat_y
/*	    DETY = nat_x
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_sis_raw2det(sensor, ccdID, raw_x, raw_y, det_x, det_y)
     int sensor;
     int ccdID;
     double raw_x;
     double raw_y;
     double *det_x;
     double *det_y;
{
	int status;
	D det_xch, det_ych;
	
	status = jc_sis_raw2detch(sensor, ccdID, raw_x, raw_y, &det_xch, &det_ych);
	if ( status ) return status;
	
	status = jc_det_ch2mm(sensor, det_xch, det_ych, det_x, det_y);
	return status;
}

int
jc_sis_raw2detch(sensor, ccdID, raw_x, raw_y, det_xch, det_ych)
     int sensor;
     int ccdID;
     double raw_x;
     double raw_y;
     double *det_xch;
     double *det_ych;
{
	int status;
	D nat_x, nat_y;
	
	status = jc_sis_raw2native(sensor, ccdID, raw_x, raw_y, &nat_x, &nat_y);
	if ( status ) return status;
	
	status = jc_sis_native2detch(sensor, nat_x, nat_y, det_xch, det_ych);
	return status;
}

int
jc_sis_raw2native(sensor, ccdID, raw_x, raw_y, nat_x, nat_y)
     int sensor;
     int ccdID;
     double raw_x;
     double raw_y;
     double *nat_x;
     double *nat_y;
{
	struct JC_SIS_CCD *ccdp;
	
	CHKCCD(sensor, ccdID);
	ccdp = &jc_sis[sensor].ccd[ccdID];
	
	*nat_x = ccdp->tv[0] + ccdp->rm[0][0]*raw_x + ccdp->rm[1][0]*raw_y;
	*nat_y = ccdp->tv[1] + ccdp->rm[0][1]*raw_x + ccdp->rm[1][1]*raw_y;
	
	return JC_NORMAL_END;
}

int
jc_sis_native2detch(sensor, nat_x, nat_y, det_xch, det_ych)
     int sensor;
     double nat_x;
     double nat_y;
     double *det_xch;
     double *det_ych;
{
	CHKSENSOR(sensor);
	if (sensor == 0) {
		*det_xch = 1281.0 - nat_y;
		*det_ych = 1281.0 - nat_x;
	} else if (sensor == 1) {
		*det_xch = nat_y;
		*det_ych = nat_x;
	}
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_det2raw
/*     SIS��DETX/Y��RAWX/Y���Ѵ�����
/*
/*  �����
/*     ���åפ��ֹ桢���������åװʳ��ξ��Ǥ�-1
/*
/*  �Ѵ���ˡ
/*  1. det2native
/*     DETX/Y��Y���ˤĤ���ȿž�����塢S0��90�١�S1��-90�ٲ�ž����
/*     native�ʺ�ɸ�Ϥˤ�ɤ���native2det��Ʊ����
/*     <�Ѵ�����>
/*        S0�ξ��
/*          nat_x = 1281 - DETY
/*	    nat_y = 1281 - DETX
/*        S1�ξ��
/*	    nat_x = DETY
/*	    nat_y = DETX
/*
/*  2. native2chip
/*	chip2native�ε��Ѵ�
/*
/*  �����
/*	��JC_NORMAL_END�����ｪλ
/************************************************************************/
int
jc_sis_det2raw(sensor, det_x, det_y, ccdID, raw_x, raw_y)
     int sensor;
     double det_x;
     double det_y;
     int *ccdID;
     double *raw_x;
     double *raw_y;
{
	int status;
	D det_xch, det_ych;
	
 	status = jc_det_mm2ch(sensor, det_x, det_y, &det_xch, &det_ych);
	if ( status ) return status;
	
	status = jc_sis_detch2raw(sensor, det_xch, det_ych, ccdID, raw_x, raw_y);
	return status;
}

int
jc_sis_detch2raw(sensor, det_xch, det_ych, ccdID, raw_x, raw_y)
     int sensor;
     double det_xch;
     double det_ych;
     int *ccdID;
     double *raw_x;
     double *raw_y;
{
	int status;
	D nat_x, nat_y;
	
	status = jc_sis_detch2native(sensor, det_xch, det_ych, &nat_x, &nat_y);
	if ( status ) return status;
	
	status = jc_sis_native2raw(sensor, nat_x, nat_y, ccdID, raw_x, raw_y);
	if ( status ) return status;
	
	if (*raw_x < 5.5 || *raw_x >= 425.5 || *raw_y < 0.5 || *raw_y >= 422.5) {
    	*ccdID = -1;
    }
	
	return JC_NORMAL_END;
}

int
jc_sis_detch2native(sensor, det_xch, det_ych, nat_x, nat_y)
     int sensor;
     double det_xch;
     double det_ych;
     double *nat_x;
     double *nat_y;
{
	CHKSENSOR(sensor);
	if (sensor == 0) {
		*nat_x = 1281.0 - det_ych;
		*nat_y = 1281.0 - det_xch;
	} else if (sensor == 1) {
		*nat_x = det_ych;
		*nat_y = det_xch;
	}
	return JC_NORMAL_END;
}

int
jc_sis_native2raw(sensor, nat_x, nat_y, ccdID, raw_x, raw_y)
     int sensor;
     double nat_x;
     double nat_y;
     int *ccdID;
     double *raw_x;
     double *raw_y;
{
	struct JC_SIS_CCD *ccdp;
	
	CHKSIS(sensor);
	*ccdID = jcWhichChip(nat_x, nat_y);
	ccdp = &jc_sis[sensor].ccd[*ccdID];
	nat_x -= ccdp->tv[0];
	nat_y -= ccdp->tv[1];
	*raw_x = ccdp->rm_inv[0][0]*nat_x + ccdp->rm_inv[1][0]*nat_y;
	*raw_y = ccdp->rm_inv[0][1]*nat_x + ccdp->rm_inv[1][1]*nat_y;
	
	return JC_NORMAL_END;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
