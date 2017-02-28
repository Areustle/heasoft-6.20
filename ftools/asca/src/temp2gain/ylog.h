/*
 * log.h
 *
 * log file access routine header		'92/11/13	Y.Ishisaki
 *
 */


/*************** defines and structures for telemetry file ***********/

#define LOGRECSIZE	2701

#ifndef BITH
#define BITH	0x80
#define BITM	0x40
#define BITL	0x20
#endif

struct	logtime	{
	unsigned char	ti_min;		/* Minutes */
	unsigned char	ti_hour;	/* Hours */
	unsigned char	ti_hund;	/* Hundredths of seconds */
	unsigned char	ti_sec;		/* Seconds */
};

struct	logdate	{
	short	da_year;	/* Year */
	char	da_day;		/* Day of the month */
	char	da_mon;		/* Month (1 = Jan) */
};

struct frlogdata {
	unsigned int main[8];
	unsigned int mon[2];
	unsigned char bos;
	unsigned char hk;
};

struct sflogdata {
	struct logdate date;
	struct logtime time;
	unsigned char stat[3];
	unsigned char fi;
	char rate;
	struct frlogdata fr[SFFR];
};

#define ENV_PHLOG		"PHLOG"	/* environ variable for it */

/***************** some proto-typing **********************/

int ascatool_logopen(char *fn);
void ascatool_logwrite(SF *sfp, ADTIME *adt);
int ascatool_logread(FR *frp, ADTIME *adt);
int ascatool_logclose(void);
long ascatool_logsfnum(void);
