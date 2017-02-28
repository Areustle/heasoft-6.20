/*************************************************

    xisSciUtil.h

    Version 1.0   2007.04.30    Y.ISHISAKI

    Version 1.1   2007.05.03    Y.ISHISAKI
	increase ap4y, ap256y size 16 -> 32
	add window option parameters needed for RAWY -> ACTY conversion
	change declarations of SCI_PARAM, int or char -> short

    Version 1.2   2007.05.05    Y.ISHISAKI
	add hidden_acty

    Version 1.3   2007.05.28    Y.ISHISAKI
	add xisSciWriteKeys()
****************************************************/

#ifndef _xisSciUtil_h_
#define _xisSciUtil_h_

typedef struct {
	short code_id;		/* micro-codel ID */
	short winopt;		/* Window option:0=off 1=1/4 2=1/8 3=1/16 */
	short win_st;		/* Window Start address */
	short win_siz;		/* Window Size */
	short ci;		/* 0:no,1:diag.CI,2:SCI-54rows,3:SCI-108rows */
	short period_rawy;	/* SCI periodicity */
	short start_rawy;	/* SCI start rawy address */
	short ispeed;		/* SCI speed: 1:basic, 4:quad, 8:octuple */
	short nrow;		/* number of SCI rows */
	short ap4n;		/* number of SCI AP4 rows */
	short ap256n;		/* number of SCI AP256 rows */
	short hidden_acty;	/* win_st + start_rawy - n * period */
	short rawy[32];		/* list of SCI rows */
	short ap4y[32];		/* list of SCI AP4 rows */
	short ap256y[32];	/* list of SCI AP256 rows */
	short dacty[1024];	/* ACTY -> dACTY map with RAWY index */
} SCI_PARAM;

int xisSciCalcDeltaACTY(SCI_PARAM *sci);
int xisSciReadKeys(fitsfile *fp, SCI_PARAM *sci);
int xisSciBnkGetKeys(char *bnkname_fitsptr, SCI_PARAM *sci);
int xisSciWriteKeys(fitsfile *fp, SCI_PARAM *sci);

#endif /* _xisSciUtil_h_ */
