/* $Id:

	aste_aspect: calculate mean satellite attitude

	2005/06/07 Y.ISHISAKI	version 1.2
		renamed to aste_aspect from com/src/module/AEaspectATTFILE

	2005/06/14 Y.ISHISAKI	version 1.3
		add median

	2005/06/26 Y.ISHISAKI	version 1.4
		add observer, observer_card

	2005/10/24 Y.ISHISAKI	version 1.5
		add obs_mode, obs_id, obs_rem, **_card

	2005/10/27 Y.ISHISAKI	version 1.6
		add *mean_ea, which will point to md_ea or av_ea

	2006/04/26 Y.ISHISAKI	version 1.7
		add *mean, which will point to md or av

	2007/05/07 Y.ISHISAKI	version 1.8
		add nom_pnt, nom_pnt_card
*/

#ifndef _ASTE_ASPECT_H_
#define _ASTE_ASPECT_H_

typedef struct {

/* input parameters */

	int verbose;
	int adopt_median;

	TELDEF *teldef;
	ATTFILE *attitude;

	double t0, t1;
	double sample_sec;

	AtEulerAng ea_base;
	double offset_tolerance;
	double roll_tolerance;

/* output parameters */

	char obs_mode[FLEN_VALUE];
	char obs_id[FLEN_VALUE];
	char obs_rem[FLEN_VALUE];
	char observer[FLEN_VALUE];
	char object[FLEN_VALUE];
	char nom_pnt[FLEN_VALUE];
	double ra_obj;
	double dec_obj;
	char obs_mode_card[FLEN_CARD];
	char obs_id_card[FLEN_CARD];
	char obs_rem_card[FLEN_CARD];
	char observer_card[FLEN_CARD];
	char object_card[FLEN_CARD];
	char nom_pnt_card[FLEN_CARD];
	char ra_obj_card[FLEN_CARD];
	char dec_obj_card[FLEN_CARD];
	int mjdrefi;
	double mjdreff;

	int num_sample;
	int num_accept;
	double dt;
	double tz;

	double av_t;
	double av_mjd;

	AtEulerAng *mean_ea;	/* md_ea or av_ea, depending on adopt_median */
	AtEulerAng md_ea;
	AtEulerAng av_ea;
	AtRotMat rm;
	AtRotMat irm;

	SKYREF av;
	SKYREF md;
	SKYREF sg;
	SKYREF mi;
	SKYREF ma;
	SKYREF *mean;			/* md or av, depending on adopt_median */

	struct {
		double alpha;
		double delta;
		double sun_ang;
	} xaxis,
	  yaxis,
	  zaxis,
	  sun_p,
	  aberr;

	double ma_offset;
	double sg_offset;

} ASTE_ASPECT;

#ifdef __cplusplus
extern "C"
{
#endif

int aste_aspect_init(ASTE_ASPECT *asp);

int aste_aspect_attitude(ASTE_ASPECT *asp);

char **aste_aspect_message(ASTE_ASPECT *asp);

#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_ASPECT_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
