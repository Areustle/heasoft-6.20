/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/grading.c,v 3.6 1996/04/16 23:42:27 dunfee Exp $   */
/*                   */

/*
 filename:	grading.c
 purpose:	event grading utilities....
 author:	Geoffrey B. Crew
 date:		June 1993
 */
#include "defs.h"
#include "compat.h"

/*
 *  This file is derived from rv_ev2pcf, with simplifications.
 */
#define Gsngle	0
#define Gsplus	1
#define Gpvert	2
#define Gpleft	3
#define Gprght	4
#define Gpplus	5
#define Gelnsq	6
#define Gother	7

struct look_up { char type; int extr; } table[256];

int		extra[] = { 9,       1,   3,   6,   8 };
unsigned char	emask[] = { 0x00, 0x0a,0x12,0x48,0x50 };

unsigned char	sngle[] = { 0x00 }; 					/* 0 */
unsigned char	splus[] = { 0x01,0x04,0x20,0x80,0x05,0x21,0x81,		/* 1 */
			    0x24,0x84,0xa0,0x25,0x85,0xa4,0xa1,0xa5 };
unsigned char	pvert[] = { 0x02,0x40,0x22,0x82,0x41,0x44,0x45,0xa2 };	/* 2 */
unsigned char	pleft[] = { 0x08,0x0c,0x88,0x8c };			/* 3 */
unsigned char	prght[] = { 0x10,0x30,0x11,0x31 };			/* 4 */
unsigned char	pplus[] = { 0x03,0x06,0x09,0x28,0x60,0xc0,0x90,0x14,	/* 5 */
			    0x83,0x26,0x89,0x2c,0x64,0xc1,0x91,0x34,
			    0x23,0x86,0x0d,0xa8,0x61,0xc4,0xb0,0x15,
			    0xa3,0xa6,0x8d,0xac,0x65,0xc5,0xb1,0x35 };
unsigned char	elnsq[] = { 0x12,0x32,0x50,0x51,0x48,0x4c,0x0a,0x8a,	/* 6 */
			    0x16,0xd0,0x68,0x0b,0x36,0xd1,0x6c,0x8b };

unsigned char	other[256] /* = { all of the rest } */;			/* 7 */

static char	style;
static int	split;
static Real	echoc;

static void	load_table();

/*
 *  Setup the lookup table for event grading.
 */
void
grade_setup()
{
	load_table();
	style = sqwork.sy;
	split = sqwork.st;
	echoc = sqwork.ek[sqwork.hd.sisn];
}

/*
 *  Grade a single event.  Eventually this gets batched.
 */
void
do_grading(ev)
	Event	*ev;
{
	register unsigned char  map;
	register int		j;
	short			phj, sum;

	/*
	 *  Insert the echo correction.
	 */
	if (echoc > 0.0) switch (style) {
		case 6 :	ev->s[7] -= ev->s[6] * echoc;
				ev->s[0] -= ev->s[4] * echoc;
				ev->s[2] -= ev->s[1] * echoc;
		case 3 :	ev->s[8] -= ev->s[7] * echoc;
				ev->s[3] -= ev->s[2] * echoc;
		case 1 :	ev->s[5] -= ev->s[0] * echoc;
		default:	break;	/* no correction */
	}
	/*
	 *  Characterize event & accumulate most of pha
	 */
	for (j = 1, map = 0, sum = ev->s[0]; j < 9; j++) {
		phj = ev->s[j];
		if (phj < split) continue;
		switch (j) {
			case 1: map |= 0x01;             break;
			case 2: map |= 0x02; sum += phj; break;
			case 3: map |= 0x04;             break;
			case 4: map |= 0x08; sum += phj; break;
			case 5: map |= 0x10; sum += phj; break;
			case 6: map |= 0x20;             break;
			case 7: map |= 0x40; sum += phj; break;
			case 8: map |= 0x80;             break;
		}
	}
	/*
	 *  Grade and final pha assignment.
	 */
	ev->g = table[map].type;
	if (ev->g == 6 && (phj = ev->s[table[map].extr]) >= split)
		sum += phj;
	ev->p = sum;
}

/*
 *  Initialize the histogram tables.  Each entry of the table needs
 *  to know which counter to increment, which histogram to increment
 *  and which extra pixels should be included in the summed pha,
 *  which only occurs for the L, Q and Other grades.
 */
static void
load_table()
{
	register int		i;
	register struct look_up	*t;
	unsigned char		b;
	int			j;

	/* load the sngle events into table GRADE 0 */
	for (i = 0; i < sizeof(sngle); i++) {
		t = table + sngle[i];
		t->type = Gsngle;
		t->extr = extra[0];	
	}

	/* load the splus events into table GRADE 1 */
	for (i = 0; i < sizeof(splus); i++) {
		t = table + splus[i];
		t->type = Gsplus;
		t->extr = extra[0];	
	}

	/* load the pvert events into table GRADE 2 */
	for (i = 0; i < sizeof(pvert); i++) {
		t = table + pvert[i];
		t->type = Gpvert;
		t->extr = extra[0];	
	}

	/* load the pleft events into table GRADE 3 */
	for (i = 0; i < sizeof(pleft); i++) {
		t = table + pleft[i];
		t->type = Gpleft;
		t->extr = extra[0];	
	}

	/* load the prght events into table GRADE 4 */
	for (i = 0; i < sizeof(prght); i++) {
		t = table + prght[i];
		t->type = Gprght;
		t->extr = extra[0];	
	}

	/* load the pplus events into table GRADE 5 */
	for (i = 0; i < sizeof(pplus); i++) {
		t = table + pplus[i];
		t->type = Gpplus;
		t->extr = extra[0];	
	}

	/* load the elnsq events into table GRADE 6 */
	for (i = 0; i < sizeof(elnsq); i++) {
		t = table + elnsq[i];
		t->type = Gelnsq;
		t->extr = extra[0];	
		for (b = 0x5a & elnsq[i], j = 0; j < sizeof(emask); j++)
			if (b == emask[j]) {
				t->extr = extra[j];
				break;
			}
	}

	/* load the other events into table GRADE 7 */
	for (i = 0; i < sizeof(other); i++) {
		t = table + i;
		if (t->extr) continue;		/* already loaded */
		t->type = Gother;
		t->extr = extra[0];	
	}
}
