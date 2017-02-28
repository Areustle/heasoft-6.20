typedef union {
	Optr   arthop;
	Optr   boolop;
	int    boolean;
	char   string[200];
	char   number[200];
} YYSTYPE;
#define	MISSION	258
#define	INSTRUM	259
#define	DETNAM	260
#define	FILTER	261
#define	DEVICE	262
#define	DIR	263
#define	CFILE	264
#define	CLASS	265
#define	DATATYP	266
#define	CODENAM	267
#define	EXTNO	268
#define	DATE	269
#define	VSD	270
#define	TIME	271
#define	VST	272
#define	REFTIME	273
#define	QUALITY	274
#define	CALDATE	275
#define	DESCRIP	276
#define	CIF	277
#define	PARAM	278
#define	CBD	279
#define	ROW	280
#define	STRNG	281
#define	NUMBER	282
#define	ARTHOP	283
#define	BOOLOP	284
#define	NOT	285


extern YYSTYPE cblval;
