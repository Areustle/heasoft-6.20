/************************************************************************/
/*	jcoord	改定履歴
/*
/*	ver 1.0	94/01/01
/*		first coded
/*	ver 1.1	94/01/12
/*		初期化ルーチンの名称を変更。
/*	ver 1.3   94/03/28
/*		天球座標と検出器座標の変換、SISの座標変換関数を追加。
/*		cfitsioを使用するようにした。
/*	ver 1.4   94/04/07  by Ishisaki
/*		関数名を修正、最初にjc_をつけた。
/*		jc_initでファイルクローズを追加。
/*		AtEulerAngをポインタ渡しに変更。        
/*	ver 1.5	94/04/10
/*		初期化確認関数jc_isinitを追加。
/*		初期化をセンサ毎に行なえるようにjc_initを変更。
/*		jcSetSisValでキーワードに割り当てる配列の大きさを修正、
/*		SIS1の変換ができないというバグをfixした。
/*		コンパイル時にNOATFUNCを定義すればatFunctionsを使用した部分を
/*		使わないようにした。
/*	ver 1.6   94/04/11
/*		FORTRANのLUNがぶつからないように引数でユーザが与えるようにした。
/*	ver 1.7   94/04/12
/*		jcSetInitFlag、jcSetSisMatで割り当てる配列の大きさが、文字列の
/*		長さ分しかないというバグを修正(石崎氏の指摘による)。
/*	ver 1.8	94/04/21
/*		teldefが読めなかった場合のゼロ除算を回避(石崎氏による)。
/*		これまでvoid型だった関数をint型に変更、ステータスを返すようにした。
/*	ver 1.9   94/04/25
/*		jcSetSisMatでチップ0の値を読んだら抜けてしまうというV1.8で新たに
/*		生じてしまったバグを修正。
/*	ver 2.0   94/05/05 (石崎)
/*		float -> double
/*		エラーチェックの強化
/*		変数を使ってキーワードを参照
/*		80 桁におさえる
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

/* マクロ */
#define I			int
#define D			double	/* 1.9 まで float */
#define DD			double	/* もともと double */
#define unless(a)	if(!(a))

/* 定数 */
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

/* static変数 */
static char *jc_teldef[NSENSOR] = {
	NULL, NULL, NULL, NULL		/* 初期化に使用されたteldefファイル */
};

static struct JC_COM {
	double focallen;			/* 焦点距離 */
	double mm2arcmin;
	double optaxisx, optaxisy;	/* 光軸 */
	double det_xcen, det_ycen;	/* 中心のピクセル */
	double det_xscl, det_yscl;	/* スケール [mm/ch] */
#ifndef NOATFUNC
	struct JC_ALIGN {
		int ok;
		AtRotMat rm, rm_inv;	/* 検出器アラインメント行列 と その逆行列 */
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
/* プロトタイプ宣言
/************************************************************************/

/* void fcopen();
void fcgkyd();
void fcclos(); */

/************************************************************************/
/*  jcoordの中で使用されるユーティリティ関数群
/************************************************************************/

/************************************************************************/
/*  jcDeterminant
/*    2x2の行列の行列式を計算する
/*
/*  戻り値
/*	行列式の値
/************************************************************************/
static double
jcDeterminant(mat)
     double (*mat)[2];
{
	return (mat[0][0]*mat[1][1] - mat[1][0]*mat[0][1]);
}

/************************************************************************/
/*  jcSetSisMat
/*    SISの座標変換行列をセットする
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
/*	・JC_DIVIDE_BY_ZERO：ゼロ除算 (行列式がゼロ)
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
/*    native座標の象限を調べてどのチップに属するかを返す関数
/*    象限しか見ないので、チップが実際にあるかどうかまでは調べない
/*
/*  戻り値
/*	CCDID (象限情報のみ)
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
			return 1;		/*第１象限 = c1*/
		} else {
			return 2;		/*第４象限 = c2*/
		}
	} else {
		if ( 0.0 < y ) {
    		return 0;		/*第２象限 = c0*/
		} else {
			return 3;		/*第３象限 = c3*/
		}
	}
}


/************************************************************************/
/*  ユーザーに提供される関数群
/************************************************************************/

/************************************************************************/
/*  jc_init
/*    jcoord初期化関数
/*
/*    戻り値
/*	・JC_NORMAL_END：正常終了
/*	・JC_DIVIDE_BY_ZERO：焦点距離がゼロ
/*	・JC_SISMAT_ERR：SISの行列計算失敗
/*      ・FITSIOのstatus：FITSIOエラー (>0)
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
	
/* 全センサに共通のパラメタの読み出し */
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
	
/* SIS固有のパラメタの読み出し */
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

/* 正常に読み出された場合には初期化フラグを立てる */
	if ( NULL != jc_teldef[sensor] ) free(jc_teldef[sensor]);
	jc_teldef[sensor] = malloc(strlen(teldef)+1);
	if ( NULL == jc_teldef[sensor] ) return JC_ALLOC_ERR;
	strcpy(jc_teldef[sensor], teldef);
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_isinit
/*    jcoord初期化確認関数
/*
/*    戻り値
/*	・初期化に使用されたteldefファイル名へのポインタ (初期化済みの場合)
/*	・NULLポインタ (未初期化の場合)
/************************************************************************/
char *
jc_isinit(sensor)
     int sensor;
{
	return ( (unsigned)sensor < NSENSOR ) ? jc_teldef[sensor] : NULL;
}

/************************************************************************/
/*  jc_xrt2det
/*    XRTX/Y[mm]をDETX/Y[mm]に変換する
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
/*    DETX/Y[mm]をXRTX/Y[mm]に変換する
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
/*    XRTX/Y[mm]をXRTθ[arcmin]/φ[deg]に変換する
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
/*    XRTθ[arcmin]/φ[deg]をXRTX/Y[mm]に変換する
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
/*    DETX/Y[mm]をDETX/Y[ch]に変換する
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
/*	・JC_DIVIDE_BY_ZERO：ゼロによる除算(ピクセルサイズ異常)
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
/*    DETX/Y[ch]をDETX/Y[mm]に変換する
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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

#ifndef NOATFUNC /* NOATFUNCが定義されている時は使用しない */
/************************************************************************/
/*  jc_sky2det
/*    (α,δ)[deg]をDETX/Y[mm]に変換する
/*  注意
/*    オイラー角は[radian]単位、天球座標は[deg]単位で倍精度
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
	AtVect vec_sky;		/* 天球座標系での目標天体の位置ベクトル */
	AtVect vec_sat;		/* 衛星座標系での目標天体の位置ベクトル */
	AtVect vec_det;		/* 検出器座標系での目標天体の位置ベクトル */
	AtRotMat rm_sat;	/* 衛星の姿勢を表す回転行列 */
	double tmp;
	
	CHKALIGN(sensor);
	
	/* オイラー角を回転行列に変換 */
	atEulerToRM(ea, rm_sat);
	
	/* 赤経赤緯を天球座標系でのベクトルに変換 */
	atPolDegToVect(1.0, alpha, delta, vec_sky);
	
	/* 天球座標から衛星座標に変換 */
	atRotVect(rm_sat, vec_sky, vec_sat);
	
	/* 検出器のオフセットを補正 */
	atRotVect(jc_com[sensor].align.rm, vec_sat, vec_det);
	
	/* 検出器座標に変換。look upにするためY軸を反転 */
	if ( 0.0 == vec_det[2] ) return JC_DIVIDE_BY_ZERO;
	tmp = jc_com[sensor].focallen / vec_det[2];
	*det_x = - tmp * vec_det[0];
	*det_y = - INVERSE * tmp * vec_det[1];
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_det2sky
/*    DETX/Y[mm]を(α,δ)[deg]に変換する
/*  注意
/*    オイラー角は[radian]単位、戻り値の天球座標は[deg]単位で倍精度
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
	
	/* オイラー角を回転行列に変換した上でその逆行列を求める */
	atEulerToRM(ea, rm_sat);
	atInvRotMat(rm_sat, rm_sat_inv);
	
	/* 検出器座標系での方向ベクトルに変換。look downにするためにY軸を反転 */
	vec_det[0] = - det_x;
	vec_det[1] = - INVERSE * det_y;
	vec_det[2] = jc_com[sensor].focallen;
	
	/* 検出器のオフセットを戻して衛星座標系での方向ベクトルに逆変換 */
	atRotVect(jc_com[sensor].align.rm_inv, vec_det, vec_sat);
	
	/* 衛星の姿勢を使って 天球座標系での方向ベクトルに逆変換 */
	atRotVect(rm_sat_inv, vec_sat, vec_sky);
	
	/* 天球座標に変換 */
	atVectToPolDeg(vec_sky, &dummy, alpha, delta);
	
	return JC_NORMAL_END;
}

/************************************************************************/
/*  jc_euler2fov
/*    オイラー角から検出器のFOVを求める関数
/*  注意
/*    オイラー角は[radian]単位、戻り値の天球座標は[deg]単位で倍精度
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
	
	/* オイラー角を回転行列に変換 */
	atEulerToRM(ea, rm_sat);
	
	/* 検出器のアラインメント行列をかけて検出器の回転行列に変換 */
	atRMProd(rm_sat, jc_com[sensor].align.rm, rm_det);
	
	/* 検出器のオイラー角を求める */
	atRMToEuler(rm_det, &ea_det);
	
	*alpha = ea_det.phi*RADIAN2DEG;
	*delta = (M_PI_2 - ea_det.theta)*RADIAN2DEG;
	*roll = (M_PI_2 - ea_det.psi)*RADIAN2DEG;
	
	return JC_NORMAL_END;
}
#endif /* NOATFUNCが定義されている時はここまで使用しない */

/************************************************************************/
/*  jc_sis_raw2det
/*     SISのRAWX/YをDETX/Yに変換する
/*
/*  変換方法
/*  1. chip2native
/*     teldefファイルにしたがって、RAWX/Yをnativeな検出器座標系に変換。
/*     ただし、nativeな座標系とは、座標軸をSISチームがQLソフトなどで
/*     使用しているものと同様に取り、座標の範囲を 1<=x,y<=1280 で定義、
/*     中心を(640.5, 640.5)とするものである。この場合、センサによらず
/*     チップの位置する象限は同じになる。
/*     <変換公式>
/*	  nat_x = AX + BX*RAWX + CX*RAWY
/*	  nat_y = AY + BY*RAWX + CY*RAWY
/*        変換係数 AX,BX,CX,AY,BY,CYはteldefファイルで与えられる。
/*
/*  2. native2det
/*     nativeな座標系をS0は-90度、S1は90度回転した後、Y軸を反転して
/*     DETX/Yに変換する。
/*     <変換公式>
/*        S0の場合
/*          DETX = 1281 - nat_y
/*	    DETY = 1281 - nat_x
/*        S1の場合
/*	    DETX = nat_y
/*	    DETY = nat_x
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
/*     SISのDETX/YをRAWX/Yに変換する
/*
/*  戻り値
/*     チップの番号、ただしチップ以外の場所では-1
/*
/*  変換方法
/*  1. det2native
/*     DETX/YをY軸について反転した後、S0は90度、S1は-90度回転して
/*     nativeな座標系にもどす。native2detと同じ。
/*     <変換公式>
/*        S0の場合
/*          nat_x = 1281 - DETY
/*	    nat_y = 1281 - DETX
/*        S1の場合
/*	    nat_x = DETY
/*	    nat_y = DETX
/*
/*  2. native2chip
/*	chip2nativeの逆変換
/*
/*  戻り値
/*	・JC_NORMAL_END：正常終了
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
