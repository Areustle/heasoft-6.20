#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <atFunctions.h>
#include "ascatool.h"
#include "ytlm.h"
#include "ylog.h"

typedef enum { OFF=0, ON=1 } SWITCH;

#ifdef unix
#define near
#endif

static long near
flength(FILE *fp)
{
	long now, len;
	now = ftell(fp);
	fseek(fp, 0L, SEEK_END);
	len = ftell(fp);
	fseek(fp, now, SEEK_SET);
	return len;
}

static void near
sf_to_log(SF *sfp, struct sflogdata *slp)
{
	int i;
	FR *frp;
	struct frlogdata *flp;
	slp->fi = (*sfp)[0][3];
	switch ( (*sfp)[15][32] ) {
	case BITH: slp->rate = 'H'; break;
	case BITM: slp->rate = 'M'; break;
	case BITL: slp->rate = 'L'; break;
	default: slp->rate = 'X';
	}
	slp->stat[0] = (*sfp)[1][32];
	slp->stat[1] = (*sfp)[17][32];
	slp->stat[2] = (*sfp)[33][32];
	frp = &(*sfp)[0];
	flp = slp->fr;
	for (i = 0; i < SFFR; i++) {
		flp->main[0] = *(unsigned*)&(*frp)[16*0+4];
		flp->main[1] = *(unsigned*)&(*frp)[16*1+4];
		flp->main[2] = *(unsigned*)&(*frp)[16*2+4];
		flp->main[3] = *(unsigned*)&(*frp)[16*3+4];
		flp->main[4] = *(unsigned*)&(*frp)[16*4+4];
		flp->main[5] = *(unsigned*)&(*frp)[16*5+4];
		flp->main[6] = *(unsigned*)&(*frp)[16*6+4];
		flp->main[7] = *(unsigned*)&(*frp)[16*7+4];
		flp->mon[0] = *(unsigned*)&(*frp)[48];
		flp->mon[1] = *(unsigned*)&(*frp)[64];
		flp->bos = (*frp)[97];
#ifdef CHKBOX
		flp->hk = (*frp)[127];
#else
		flp->hk = (*frp)[33];
#endif
		frp++;
		flp++;
	}
}

static void near
log_to_fr(struct sflogdata *slp, FR *frp, int frn, ADTIME *adt)
{
	WD bit=BITH;
	long msec=0L;
	struct frlogdata *flp;

	frn %= SFFR;
	(*frp)[0] = 0xfa;
	(*frp)[1] = 0xf3;
	(*frp)[2] = 0x20;
	(*frp)[3] = slp->fi + frn;
	{
		unsigned char *p = (unsigned char*)&slp->date.da_year;
		adt->year = p[0] + p[1]*256;
		/* A potential Y2K problem;  but not used in temp2gain  V4.3 */
		adt->year %= 100;
	}
	adt->month = slp->date.da_mon;
	adt->day = slp->date.da_day;
	adt->hour = slp->time.ti_hour;
	adt->minute = slp->time.ti_min;
	adt->second = slp->time.ti_sec;
	switch ( slp->rate ) {
	case 'H': bit = BITH; msec = frn * 1000L / 32; break;
	case 'M': bit = BITM; msec = frn * 1000L / 4; break;
	case 'L': bit = BITL; msec = frn * 1000L; break;
	}
	until ( msec < 1000 ) {
		msec -= 1000;
		adt->second++;
	}
	adt->msec = (int)msec;
	until ( adt->second < 60 ) {
		adt->second -= 60;
		adt->minute++;
	}
	until ( adt->minute < 60 ) {
		adt->minute -= 60;
		adt->hour++;
	}
	until ( adt->hour < 24 ) {
		adt->hour -= 24;
		adt->day++;
	}
	switch ( frn ) {
	case  1: (*frp)[32] = slp->stat[0]; break;
	case 15: (*frp)[32] = bit; break;
	case 17: (*frp)[32] = slp->stat[1]; break;
	case 33: (*frp)[32] = slp->stat[2]; break;
	}
	flp = slp->fr + frn;
	*(unsigned*)&(*frp)[16*0+4] = flp->main[0];
	*(unsigned*)&(*frp)[16*1+4] = flp->main[1];
	*(unsigned*)&(*frp)[16*2+4] = flp->main[2];
	*(unsigned*)&(*frp)[16*3+4] = flp->main[3];
	*(unsigned*)&(*frp)[16*4+4] = flp->main[4];
	*(unsigned*)&(*frp)[16*5+4] = flp->main[5];
	*(unsigned*)&(*frp)[16*6+4] = flp->main[6];
	*(unsigned*)&(*frp)[16*7+4] = flp->main[7];
	*(unsigned*)&(*frp)[48] = flp->mon[0];
	*(unsigned*)&(*frp)[64] = flp->mon[1];
	(*frp)[97] = flp->bos;
#ifdef QL
	(*frp)[33] = flp->hk;
#else
	(*frp)[127] = flp->hk;
#endif
}

/*#define setlogfn(fn,da,ti)	sprintf(fn,"%02d%02d%02d%02d.log",(da)->da_year,(da)->da_mon, (da)->da_day, (ti)->ti_hour);*/

static void near
setlogfn(char *fn, struct logdate *da, struct logtime *ti)
{
	static struct logdate da_save;
	static struct logtime ti_save;
	char *direc;

	if ( null(da) ) {
		da = &da_save;
		ti = &ti_save;
		ti->ti_hour = ( ti->ti_hour + 1 ) % 24;
		if ( 0 == ti->ti_hour ) da->da_day++;
	}
	direc = getenv(ENV_PHLOG);
	if ( null(direc) ) direc = "";

	/* only used to setup a filename, no Y2K problem; v4.3*/
	sprintf(fn, "%s%02d%02d%02d%02d.log",
			direc, da->da_year%100, da->da_mon, da->da_day, ti->ti_hour);
	da_save = *da;
	ti_save = *ti;
	/*printf("filename = %s\n", fn);*/
}

static char logfn[80];
static int end_time_set = OFF;
static struct logdate end_date;
static struct logtime end_time;
static int logfrn = 0;
static FILE *logfp = NULL;
static struct sflogdata logbuf;

extern int close(int);
extern int dup(int);

void
ascatool_logwrite(SF *sfp, ADTIME *adt)
{
	static int last_saved_minute = -1;
	static struct logdate date;
	static struct logtime time;
	date.da_year = adt->year;
	date.da_mon = adt->month;
	date.da_day = adt->day;
	time.ti_hour = adt->hour;
	time.ti_min = adt->minute;
	time.ti_sec = adt->second;
	if ( null(logfp) || logbuf.time.ti_hour != time.ti_hour ) {
		long offs;
		unless ( null(logfp) ) fclose(logfp);
		setlogfn(logfn, &date, &time);
		logfp = fopen(logfn, "ab");
		if ( null(logfp) ) {
			fprintf(stderr, "\nlog file open error !!!\n");
			exit(1);
		}
		offs = flength(logfp);
		offs /= sizeof(struct sflogdata);
		fseek(logfp, offs*sizeof(struct sflogdata), SEEK_SET);
		logbuf.date = date;
	}
	logbuf.time = time;
	unless ( null(logfp) ) {
		sf_to_log(sfp, &logbuf);
		unless ( 1 == fwrite(&logbuf, sizeof(logbuf), 1, logfp) ) {
			fprintf(stderr, "\nDisk write error !!!\n");
			exit(1);
		}
	}
	unless ( time.ti_min == last_saved_minute ) {
		last_saved_minute = time.ti_min;
		close(dup(fileno(logfp)));		/* file close every 1 minute */
	}
}

static int near
fgettime(long f, struct logtime *t)
{
	int r;
	r = fseek(logfp, f*LOGRECSIZE+sizeof(struct logdate), SEEK_SET);
	if ( r ) return 4;
	unless ( 1 == fread(t, sizeof(*t), 1, logfp) ) return 4;
	/*printf("%ld: %02d:%02d:%02d\n", f, t->ti_hour, t->ti_min, t->ti_sec);*/
	return 0;
}

static int near
cmptime(struct logtime *t1, struct logtime *t2)
{
	if ( t1->ti_hour > t2->ti_hour ) return 1;
	if ( t1->ti_hour < t2->ti_hour ) return -1;
	if ( t1->ti_min > t2->ti_min ) return 1;
	if ( t1->ti_min < t2->ti_min ) return -1;
	if ( t1->ti_sec > t2->ti_sec ) return 1;
	if ( t1->ti_sec < t2->ti_sec ) return -1;
	return 0;
}


int
ascatool_logopen(char *fn)
{
  int ye, mon, day, hour, mi, sec, v;
  struct logdate d;
  struct logtime t, ts, tm, te;
  long fs, fm, fe;

  /* printf("debug: fn = %s\n",fn); */

 sscanf(fn, "%02u%02u%02u%02u%02u%02u", &ye, &mon, &day, &hour, &mi, &sec);
 d.da_year = ye;
 d.da_mon = mon;
 d.da_day = day;
 t.ti_hour = hour;
 t.ti_min = mi;
 t.ti_sec = sec;
 fn += 12;
 end_time_set = OFF;

 switch ( *fn++ ) {
 case '-':
   if ( 6 == sscanf(fn, "%02u%02u%02u%02u%02u%02u",
		    &ye, &mon, &day, &hour, &mi, &sec) ) {
     end_time_set = ON;
   }
   break;
 case '+':
   if ( 1 == sscanf(fn, "%d", &v) ) {
     end_time_set = ON;
     while ( '0' <= *fn && *fn <= '9' ) fn++;
     switch ( *fn ) {
     case 'm':
       mi += v;
       break;
     case 'h':
       hour += v;
       break;
     default:
       sec += v;
     }
     mi += sec / 60;
     sec %= 60;
     hour += mi / 60;
     mi %= 60;
     day += hour / 24;
     hour %= 24;
   }
   break;
   /* default:
   printf("Warning: fn=%s\n",fn); 
   */
 }

 if ( end_time_set ) {
   end_date.da_year = ye;
   end_date.da_mon = mon;
   end_date.da_day = day;
   end_time.ti_hour = hour;
   end_time.ti_min = mi;
   end_time.ti_sec = sec;
 }
 unless ( null(logfp) ) fclose(logfp);
 setlogfn(logfn, &d, &t);

 logfp = fopen(logfn, "rb");
 if ( null(logfp) ) return 1;
 /*    getchar(); */
 fs = 0;
 fe = flength(logfp) / LOGRECSIZE - 1;
 if ( fgettime(fs, &ts) ) return 4;
 if ( fgettime(fe, &te) ) return 4;
 unless ( cmptime(&t, &te) <= 0 ) return 1;
 until ( fs == fe || fs+1 == fe ) {
   if ( 0 == cmptime(&ts, &t) ) break;
   if ( 0 == cmptime(&te, &t) ) {
     fs = fe;
     ts = te;
     break;
   }
   fm = fs + ( fe - fs ) / 2;
   if ( fgettime(fm, &tm) ) return 4;
   unless ( cmptime(&ts, &tm) <= 0 ) return 1;
   unless ( cmptime(&tm, &te) <= 0 ) return 1;
   if ( cmptime(&tm, &t) < 0 ) {
     fs = fm;
     ts = tm;
   } else {
     fe = fm;
     te = tm;
   }
 }
 if ( fseek(logfp, fs*LOGRECSIZE, SEEK_SET) ) return 4;
 logfrn = 0;
 /*printf("%s (%d) : opened\n", logfn, fs);
   getchar();*/
 return 0;
}

static int
read_logbuf(void)
{
	int i;
	unless ( 1 == fread(&logbuf, 13, 1, logfp) ) return 4;
	for (i = 0; i < SFFR; i++) {
		unless ( 1 == fread(&logbuf.fr[i], 42, 1, logfp) ) return 4;
	}
	return 0;
}

int
ascatool_logread(FR *frp, ADTIME *time_ptr)
{
	ADTIME adt;
	AtTime attime;
	
	if ( null(logfp) ) return 1;
	if ( 0 == logfrn ) {
		if ( read_logbuf() ) {
		   setlogfn(logfn, NULL, NULL);

puts("debug:logfn");
puts(logfn);

		   fclose(logfp);
		   logfp = fopen(logfn, "rb");
		   /*printf("%s(%lp): opend\n", logfn, logfp);*/
		   if ( null(logfp) ) return 1;
		   if ( read_logbuf() ) return 4;
		}
	}
	log_to_fr(&logbuf, frp, logfrn, &adt);
	/*_bitrate_ = logbuf.rate;*/
	logfrn = ( logfrn + 1 ) % SFFR;
	
	if ( TIME_IS_ADTIME == _TIME_TYPE_ ) {
		*time_ptr = adt;
	} else {
		attime.yr = adt.year;
		attime.mo = adt.month;
		attime.dy = adt.day;
		attime.hr = adt.hour;
		attime.mn = adt.minute;
		attime.sc = adt.second;
		attime.ms = adt.msec;
		if ( TIME_IS_ATTIME == _TIME_TYPE_ ) {
			*(AtTime*)time_ptr = attime;
		} else {
			if ( TIME_IS_MJD == _TIME_TYPE_ ) {
				atMJulian(&attime, (MJD*)time_ptr);
			} else {
				*(ASCATIME*)time_ptr = attime2asca(attime);
			}
		}
	}
	
	if ( end_time_set ) {
		if ( end_date.da_day < adt.day ) return 1;
		if ( end_date.da_day > adt.day ) return 0;
		if ( end_time.ti_hour < adt.hour ) return 1;
		if ( end_time.ti_hour > adt.hour ) return 0;
		if ( end_time.ti_min < adt.minute ) return 1;
		if ( end_time.ti_min > adt.minute ) return 0;
		if ( end_time.ti_sec < adt.second ) return 1;
		if ( end_time.ti_sec > adt.second ) return 0;
	}
	return 0;
}

int
ascatool_logclose(void)
{
	if ( !null(logfp) && fclose(logfp) ) return 4;
	logfp = NULL;
	return 0;
}

long
ascatool_logsfnum(void)
{
	long ft;
	ft = ftell(logfp);
	return ft / LOGRECSIZE;
}
