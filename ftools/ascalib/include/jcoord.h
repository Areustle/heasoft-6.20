#ifndef _JCOORD_H_
#define _JCOORD_H_

#ifndef NOATFUNC
#ifndef MJD_J2000	/* check atFunctions */
#define NOATFUNC
#endif
#endif

typedef struct {
	double alpha;	/* SKY座標の原点に対応する天球上の位置の赤経(degree) */
	double delta;	/* SKY座標の原点に対応する天球上の位置の赤緯(degree) */
	double roll;	/* SKY座標のY軸と赤道座標の北方向のなす角(degree) */
} JcSkyRef;

#define JC_NORMAL_END		0
#define JC_DIVIDE_BY_ZERO	-1
#define JC_ALLOC_ERR		-2
#define JC_NOT_INITIALIZED	-3
#define JC_INVALID_SENSOR	-4
#define JC_NOT_SIS		-5
#define JC_INVALID_CCD		-6
#define JC_MATRIX_ERR		-10
#define JC_SISMAT_ERR		-11

#define I	int
#define D	double	/* 1.9 まで float */
#define DD	double	/* もともと double */

I jc_init(I unit, I sensor, char *teldef_files);
char *jc_isinit(I sensor);
I jc_xrt2det(I sensor, D xrt_x, D xrt_y, D *det_x, D *det_y);
I jc_det2xrt(I sensor, D det_x, D det_y, D *xrt_x, D *xrt_y);
I jc_xrt_rec2pol(I sensor, D xrt_x, D xrt_y, D *theta, D *phi);
I jc_xrt_pol2rec(I sensor, D theta, D phi, D *xrt_x, D *xrt_y);
I jc_det_mm2ch(I sensor, D det_x, D det_y, D *det_xch, D *det_ych);
I jc_det_ch2mm(I sensor, D det_xch, D det_ych, D *det_x, D *det_y);

#ifndef NOATFUNC
I jc_euler2fov(AtEulerAng *ea, I sensor, DD *alpha, DD *delta, DD *roll);
I jc_fov2euler(DD alpha, DD delta, DD roll, I sensor, AtEulerAng *ea);
I jc_ecs2det(AtEulerAng *ea, I sensor, DD alpha, DD delta, D *det_x, D *det_y);
I jc_det2ecs(AtEulerAng *ea, I sensor, D det_x, D det_y, DD *alpha, DD *delta);
I jc_det2det(I sensor1, D det_x1, D det_y1, I sensor2, D *det_x2, D *det_y2);
I jc_ecs2sky(JcSkyRef *ref, I sensor, DD alpha, DD delta, D *sky_x, D *sky_y);
I jc_sky2ecs(JcSkyRef *ref, I sensor, D sky_x, D sky_y, DD *alpha, DD *delta);
I jc_det2sky(AtEulerAng *ea, JcSkyRef *ref, I sensor,
	     D det_x, D det_y, D *sky_x, D *sky_y);
I jc_sky2det(AtEulerAng *ea, JcSkyRef *ref, I sensor,
	     D sky_x, D sky_y, D *det_x, D *det_y);
I jc_det2ascalinsky(AtEulerAng *ea, JcSkyRef *ref, I sensor,
	     D det_x, D det_y, D *sky_x, D *sky_y);
I jc_ascalinsky2det(AtEulerAng *ea, JcSkyRef *ref, I sensor,
	     D sky_x, D sky_y, D *det_x, D *det_y);
#endif

#define jc_sky_mm2ch	jc_det_mm2ch
#define jc_sky_ch2mm	jc_det_ch2mm
#define jc_ascalinsky_mm2ch	jc_det_mm2ch
#define jc_ascalinsky_ch2mm	jc_det_ch2mm

I jc_sis_raw2det(I sensor, I ccd, D raw_x, D raw_y, D *det_x, D *det_y);
I jc_sis_raw2detch(I sensor, I ccd, D raw_x, D raw_y, D *det_xch, D *det_ych);
I jc_sis_det2raw(I sensor, D det_x, D det_y, I *ccd, D *raw_x, D *raw_y);
I jc_sis_detch2raw(I sensor, D det_xch, D det_ych, I *ccd, D *raw_x, D *raw_y);
I jc_sis_raw2native(I sensor, I ccd, D raw_x, D raw_y, D *nat_x, D *nat_y);
I jc_sis_native2detch(I sensor, D nat_x, D native_y, D *det_xch, D *det_ych);
I jc_sis_detch2native(I sensor, D det_xch, D det_ych, D *nat_x, D *nat_y);
I jc_sis_native2raw(I sensor, D nat_x, D nat_y, I *ccd, D *raw_x, D *raw_y);

#undef DD
#undef D
#undef I

#endif	/* _JCOORD_H_ */
