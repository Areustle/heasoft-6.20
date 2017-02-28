/*

  aste_ti2time: TI -> Astro-E time conversion

	2005-02-07	Y.ISHISAKI	version 1.0

	2005-02-15	Y.ISHISAKI	version 1.31

	2005-04-13,25	Y.ISHISAKI	version 1.40

	2005-05-03	Y.ISHISAKI	version 2.5
		move to AEpacketTimeSet/2.5, remove from astetool
		rename recv_time -> s_time

	2005-05-15	Y.ISHISAKI	version 2.7
		aste_ti2time.[ch] moved from com/module/AEpacketTimeSet/VERSION/
		change aste_ti2time_init() arguments

	2005-07-10	Y.ISHISAKI	version 3.0
		add rough, for rough time assignment for repro data

	2005-07-26	Y.ISHISAKI	version 3.1
		add prev_ipos, prev_N, prev_tz, prev_Y to use in get_timc_val()

	2005-08-05	Y.ISHISAKI	version 3.3
		char flag_tc -> signed char flag_tc, for Linux PowerMac
		remove double tc, char flag_tc in struct dp_timc_struct {}, not used

	2006-01-31	Y.ISHISAKI	version 3.8
		warning only for extrapolation error or too large correction
*/

#ifndef _ASTE_TI2TIME_H_
#define _ASTE_TI2TIME_H_

typedef struct {

	int simple;		/* flag to use simplified version without initialization */
	struct simple_ti2time {
		int init_ref;			/* 0:not initialized, 1:initialized */
		unsigned int ref_ti;	/* reference TI (LSB = 1/4096 or 1/32 s) */
		double ref_time;		/* reference Astro-E time */
	} dp, dhu;

	int rough;		/* flag for rough time assignment for repro data */

	char *tim_file;				/* .tim FITS file name */

	int ntpk;					/* number of tpk items */
	struct time_pkt_struct {
		double t0, t1;			/* reference Astro-E time start/stop, s */
		double Y0, Y1;			/* drift time start/stop, 1/4096/4096/100 s */
		double freq;			/* base frequency, close to 4096 Hz */
		unsigned int N0, N1;	/* reference TI start/stop, 1/4096 s */
		int flag_jump;			/* found jump in TI before N0, not b/w N0-N1 */
		int flag_discon;		/* found discontinuity in Y b/w N0-N1 */
	} *tpk;

	int ndpk;					/* number of dpk items */
	struct dp_timc_struct {
		double tz;				/* received time of the DP packet */
		unsigned int N;			/* DP-TI, 1/4096 s */
		int Y;					/* drift time value, 1/4096/4096/100 s */
		unsigned short add_no;	/* satellite time correction sequence number */
	} *dpk;
	int prev_ipos;				/* cache previous values */
	unsigned int prev_N;
	double prev_tz;
	double prev_Y;

	int navg;					/* number of avg items */
	struct dp_dhu_avg_struct {
		double tz;				/* averaged receive time of the DP packet */
		double tc;				/* corrected time of tz */
		double N;				/* averaged DP-TI, 1/4096 s, 32 bit  */
		double H;				/* averaged DHU-TI/8, 1/8s, 24 bit */
		double tz0, tz1;		/* start/stop s_time of averaging */
		unsigned int N0, N1;	/* start/stop DP-TI of averaging */
		int H0, H1;				/* start/stop DHU-TI/8 of averaging */
		char flag_cont;	/* flag if next row is continuous, i.e. no TI jump */
		signed char flag_tc;	/* 0:tc unset, 1:tc set, -1:error */
	} *avg;

	int apid;				/* debug information for last time correction */
	double s_time, calc_time, delt_time;
	double base_t0, base_t1, base_Y0, base_Y1, base_freq, Y, dY;
	unsigned int N, base_N0, base_N1;
	int dN;
	char base_jump, base_discon;

	unsigned int DHU;
	int H;
	double Hf, nv, n0, n1, hv, h0, h1, tz0, tz1;

} TI2TIME;

#ifdef __cplusplus
extern "C"
{
#endif

/************************************************************************
int aste_ti2time_init()	: initialize TI2TIME information

	When tim_file == NULL or "none", use simplified version of aste_ti2time()

Input:
	char *tim_file		: .tim FITS file name

Output:
	TI2TIME **ttpp		: pointer to TI2TIME pointer after initialization

Return_Values:
	0					: success
	-1					: malloc() error
	-2					: extrapolation error in get_timc_val()
	-3					: too large base freq drift, something is wrong
	others				: CFITSIO error
************************************************************************/
int aste_ti2time_init(TI2TIME **ttpp, char *tim_file);


/************************************************************************
int aste_ti2time_free()	: free memory of TI2TIME

Input:
	TI2TIME *ttp		: TI2TIME pointer to free

Return_Values:
	0					: success
	-1					: TI2TIME *ttp is not allocated
************************************************************************/
int aste_ti2time_free(TI2TIME *ttp);


/************************************************************************
int aste_ti2time_dp()	: convert DP-TI -> Astro-E time

Input:
	TI2TIME *ttp		: TI2TIME pointer used for the time correction
	unsigned int N		: DP-TI (time indicator) of the packet, 1/4096 s
	double tz;			: Astro-E time when the packet was received

Output:
	double *aetime		: Astro-E time of packet creation after time correction

Return_Values:
	0					: success
	-1					: no valid time interval in TI2TIME *ttp
************************************************************************/
int aste_ti2time_dp(TI2TIME *ttp, unsigned int N, double tz, double *aetime);


/************************************************************************
int aste_ti2time_dhu()	: convert DHU-TI -> Astro-E time

Input:
	TI2TIME *ttp		: TI2TIME pointer used for the time correction
	unsigned int N		: DHU-TI (time indicator) of the packet, 1/32 s
	double tz;			: Astro-E time when the packet was received

Output:
	double *aetime		: Astro-E time of packet creation after time correction

Return_Values:
	0					: success
	-1					: no valid time interval in TI2TIME *ttp
	-2					: extrapolation error, not +-600 sec
************************************************************************/
int aste_ti2time_dhu(TI2TIME *ttp, unsigned int N, double tz, double *aetime);


/************************************************************************
int aste_ti2time()		: convert TI -> Astro-E time

Input:
	TI2TIME *ttp		: TI2TIME pointer used for the time correction
	int apid			: APID (application process id) of the packet
	unsigned int N		: TI (time indicator) of the packet,
							1/4096 s for DP, 1/32 s for DHU
	double tz;			: Astro-E time when the packet was received

Output:
	double *aetime		: Astro-E time of packet creation after time correction

Return_Values:
	0					: success
************************************************************************/
int aste_ti2time(TI2TIME *ttp, int apid, unsigned int N, double tz, double *aetime);


/************************************************************************
int aste_ti2time_dbl()	: convert TI (double) -> Astro-E time

Input:
	TI2TIME *ttp		: TI2TIME pointer used for the time correction
	int apid			: APID (application process id) of the packet
	double N			: TI (time indicator) of the packet,
							1/4096 s for DP, 1/32 s for DHU
	double tz;			: Astro-E time when the packet was received

Output:
	double *aetime		: Astro-E time of packet creation after time correction

Return_Values:
	0					: success
	others				: error, see aste_ti2time_dp/dhu()
************************************************************************/
int aste_ti2time_dbl(TI2TIME *ttp, int apid, double N, double tz, double *aetime);

int aste_ti2time_dp_dbl(TI2TIME *ttp, double N, double tz, double *aetime);

int aste_ti2time_dhu_dbl(TI2TIME *ttp, double N, double tz, double *aetime);


#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_TI2TIME_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
