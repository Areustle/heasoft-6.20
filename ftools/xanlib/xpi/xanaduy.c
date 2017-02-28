
/*  A Bison parser, made from xanaduy.y with Bison version GNU Bison version 1.22
  */

#define ZABISON 1  /* Identify Bison output.  */

#define	REAL	258
#define	INTEGER	259
#define	STRING	260
#define	PARAM	261
#define	SLASH	262
#define	EQUALS	263
#define	COMMA	264
#define	SPACE	265
#define	ENDOFFILE	266
#define	SEMICOLON	267
#define	NL	268
#define	PARAMON	269
#define	PARAMOFF	270
#define	IRANGE	271
#define	QUESTION	272

#line 1 "xanaduy.y"

#include <stdio.h>
#include <string.h>

#ifndef HAVE_ALLOCA
#define alloca malloc
#endif

#define ZADEBUG 1

#ifdef VMS
#include "cfortran.h"

#define YCAPCMD(STR) CCALLSFSUB1(YCAPCMD,ycapcmd,STRING,STR)
#define YCAPDELIM(STR) CCALLSFSUB1(YCAPDELIM,ycapdelim,STRING,STR)
#define YCAPPAR(STR) CCALLSFSUB1(YCAPPAR,ycappar,STRING,STR)
#define YCAPVAL(STR) CCALLSFSUB1(YCAPVAL,ycapval,STRING,STR)
#define YNEXTPAR() CCALLSFSUB0(YNEXTPAR,ynextpar)

#else

void YCAPCMD (str)
char *str;
{
  ycapcmd_(str,strlen(str));
}

void YCAPDELIM (str)
char *str;
{
  ycapdelim_(str,strlen(str));
}

void YCAPPAR (str)
char *str;
{
  ycappar_(str,strlen(str));
}

void YCAPVAL (str)
char *str;
{
  ycapval_(str,strlen(str));
}

void YNEXTPAR ()
{
  ynextpar_();
}

void ZANULL ()
{
}

#endif





#line 62 "xanaduy.y"
typedef union {
    char   * value;   
} ZASTYPE;

#ifndef ZALTYPE
typedef
  struct zaltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  zaltype;

#define ZALTYPE zaltype
#endif

#ifndef ZADEBUG
#define ZADEBUG 1
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	ZAFINAL		41
#define	ZAFLAG		-32768
#define	ZANTBASE	18

#define ZATRANSLATE(x) ((unsigned)(x) <= 272 ? zatranslate[x] : 27)

static const char zatranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17
};

#if ZADEBUG != 0
static const short zaprhs[] = {     0,
     0,     1,     4,     6,     8,    10,    13,    16,    20,    24,
    26,    28,    30,    32,    34,    37,    39,    43,    45,    47,
    51,    53,    55,    57,    59,    61,    63,    66,    68,    71,
    73,    76
};

static const short zarhs[] = {    -1,
    18,    19,     0,    11,     0,    20,     0,    20,     0,    21,
    20,     0,    17,    20,     0,    21,    17,    20,     0,    21,
    22,    20,     0,     1,     0,    13,     0,    12,     0,     6,
     0,    23,     0,    23,    22,     0,    24,     0,    26,     8,
    25,     0,    26,     0,    25,     0,    25,     9,    24,     0,
     3,     0,     4,     0,     5,     0,     6,     0,    16,     0,
    14,     0,     7,    14,     0,    15,     0,     7,    15,     0,
     6,     0,     7,     6,     0,     9,     6,     0
};

#endif

#if ZADEBUG != 0
static const short zarline[] = { 0,
    75,    75,    76,    78,    79,    80,    81,    82,    83,    84,
    88,    88,    90,    92,    92,    94,    94,    95,    98,    98,
   100,   100,   101,   102,   103,   106,   106,   107,   108,   109,
   110,   111
};

static const char * const zatname[] = {   "$","error","$illegal.","REAL","INTEGER",
"STRING","PARAM","SLASH","EQUALS","COMMA","SPACE","ENDOFFILE","SEMICOLON","NL",
"PARAMON","PARAMOFF","IRANGE","QUESTION","lines","line","eol","comm","par","parameter",
"values","value","par1",""
};
#endif

static const short zar1[] = {     0,
    18,    18,    18,    19,    19,    19,    19,    19,    19,    19,
    20,    20,    21,    22,    22,    23,    23,    23,    24,    24,
    25,    25,    25,    25,    25,    26,    26,    26,    26,    26,
    26,    26
};

static const short zar2[] = {     0,
     0,     2,     1,     1,     1,     2,     2,     3,     3,     1,
     1,     1,     1,     1,     2,     1,     3,     1,     1,     3,
     1,     1,     1,     1,     1,     1,     2,     1,     2,     1,
     2,     2
};

static const short zadefact[] = {     1,
     3,     0,    10,    13,    12,    11,     0,     2,     4,     0,
     7,    21,    22,    23,    24,     0,     0,    26,    28,    25,
     0,     6,     0,    14,    16,    19,    18,    31,    27,    29,
    32,     8,     9,    15,     0,     0,    24,    20,    17,     0,
     0
};

static const short zadefgoto[] = {     2,
     8,     9,    10,    23,    24,    25,    26,    27
};

static const short zapact[] = {    -4,
-32768,     0,-32768,-32768,-32768,-32768,    -9,-32768,-32768,    16,
-32768,-32768,-32768,-32768,     1,    42,     2,-32768,-32768,-32768,
    -9,-32768,    -9,    31,-32768,     5,     3,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,    38,    38,-32768,-32768,-32768,    10,
-32768
};

static const short zapgoto[] = {-32768,
-32768,    -5,-32768,    15,-32768,   -20,   -12,-32768
};


#define	ZALAST		57


static const short zatable[] = {    40,
     3,    11,     5,     6,    22,     4,     1,    31,   -30,    41,
    36,     5,     6,    35,    38,    32,     7,    33,    12,    13,
    14,    15,    16,    39,    17,     0,     0,     5,     6,    18,
    19,    20,    21,    12,    13,    14,    15,    16,    34,    17,
    12,    13,    14,    37,    18,    19,    20,    28,     0,     0,
     0,     0,     0,    20,     0,    29,    30
};

static const short zacheck[] = {     0,
     1,     7,    12,    13,    10,     6,    11,     6,     8,     0,
     8,    12,    13,     9,    35,    21,    17,    23,     3,     4,
     5,     6,     7,    36,     9,    -1,    -1,    12,    13,    14,
    15,    16,    17,     3,     4,     5,     6,     7,    24,     9,
     3,     4,     5,     6,    14,    15,    16,     6,    -1,    -1,
    -1,    -1,    -1,    16,    -1,    14,    15
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/opt1/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if !defined (MSDOS) && !defined (__TURBOC__) && !defined (__APPLE__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ or __APPLE__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

#ifdef __APPLE__
#include <stdlib.h>
#endif

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define zaerrok		(zaerrstatus = 0)
#define zaclearin	(zachar = ZAEMPTY)
#define ZAEMPTY		-2
#define ZAEOF		0
#define ZAACCEPT	return(0)
#define ZAABORT 	return(1)
#define ZAERROR		goto zaerrlab1
/* Like ZAERROR except do call zaerror.
   This remains here temporarily to ease the
   transition to the new meaning of ZAERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define ZAFAIL		goto zaerrlab
#define ZARECOVERING()  (!!zaerrstatus)
#define ZABACKUP(token, value) \
do								\
  if (zachar == ZAEMPTY && zalen == 1)				\
    { zachar = (token), zalval = (value);			\
      zachar1 = ZATRANSLATE (zachar);				\
      ZAPOPSTACK;						\
      goto zabackup;						\
    }								\
  else								\
    { zaerror ("syntax error: cannot back up"); ZAERROR; }	\
while (0)

#define ZATERROR	1
#define ZAERRCODE	256

#ifndef ZAPURE
#define ZALEX		zalex()
#endif

#ifdef ZAPURE
#ifdef ZALSP_NEEDED
#define ZALEX		zalex(&zalval, &zalloc)
#else
#define ZALEX		zalex(&zalval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef ZAPURE

int	zachar;			/*  the lookahead symbol		*/
ZASTYPE	zalval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef ZALSP_NEEDED
ZALTYPE zalloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int zanerrs;			/*  number of parse errors so far       */
#endif  /* not ZAPURE */

#if ZADEBUG != 0
int zadebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  ZAINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	ZAINITDEPTH
#define ZAINITDEPTH 200
#endif

/*  ZAMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if ZAMAXDEPTH == 0
#undef ZAMAXDEPTH
#endif

#ifndef ZAMAXDEPTH
#define ZAMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int zaparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __za_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__za_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__za_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/opt1/lib/bison.simple"
int
zaparse()
{
  register int zastate;
  register int zan;
  register short *zassp;
  register ZASTYPE *zavsp;
  int zaerrstatus;	/*  number of tokens to shift before error messages enabled */
  int zachar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	zassa[ZAINITDEPTH];	/*  the state stack			*/
  ZASTYPE zavsa[ZAINITDEPTH];	/*  the semantic value stack		*/

  short *zass = zassa;		/*  refer to the stacks thru separate pointers */
  ZASTYPE *zavs = zavsa;	/*  to allow zaoverflow to reallocate them elsewhere */

#ifdef ZALSP_NEEDED
  ZALTYPE zalsa[ZAINITDEPTH];	/*  the location stack			*/
  ZALTYPE *zals = zalsa;
  ZALTYPE *zalsp;

#define ZAPOPSTACK   (zavsp--, zassp--, zalsp--)
#else
#define ZAPOPSTACK   (zavsp--, zassp--)
#endif

  int zastacksize = ZAINITDEPTH;

#ifdef ZAPURE
  int zachar;
  ZASTYPE zalval;
  int zanerrs;
#ifdef ZALSP_NEEDED
  ZALTYPE zalloc;
#endif
#endif

  ZASTYPE zaval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int zalen;

#if ZADEBUG != 0
  if (zadebug)
    fprintf(stderr, "Starting parse\n");
#endif

  zastate = 0;
  zaerrstatus = 0;
  zanerrs = 0;
  zachar = ZAEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  zassp = zass - 1;
  zavsp = zavs;
#ifdef ZALSP_NEEDED
  zalsp = zals;
#endif

/* Push a new state, which is found in  zastate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
zanewstate:

  *++zassp = zastate;

  if (zassp >= zass + zastacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      ZASTYPE *zavs1 = zavs;
      short *zass1 = zass;
#ifdef ZALSP_NEEDED
      ZALTYPE *zals1 = zals;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = zassp - zass + 1;

#ifdef zaoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef ZALSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if zaoverflow is a macro.  */
      zaoverflow("parser stack overflow",
		 &zass1, size * sizeof (*zassp),
		 &zavs1, size * sizeof (*zavsp),
		 &zals1, size * sizeof (*zalsp),
		 &zastacksize);
#else
      zaoverflow("parser stack overflow",
		 &zass1, size * sizeof (*zassp),
		 &zavs1, size * sizeof (*zavsp),
		 &zastacksize);
#endif

      zass = zass1; zavs = zavs1;
#ifdef ZALSP_NEEDED
      zals = zals1;
#endif
#else /* no zaoverflow */
      /* Extend the stack our own way.  */
      if (zastacksize >= ZAMAXDEPTH)
	{
	  zaerror("parser stack overflow");
	  return 2;
	}
      zastacksize *= 2;
      if (zastacksize > ZAMAXDEPTH)
	zastacksize = ZAMAXDEPTH;
      zass = (short *) alloca (zastacksize * sizeof (*zassp));
      __za_bcopy ((char *)zass1, (char *)zass, size * sizeof (*zassp));
      zavs = (ZASTYPE *) alloca (zastacksize * sizeof (*zavsp));
      __za_bcopy ((char *)zavs1, (char *)zavs, size * sizeof (*zavsp));
#ifdef ZALSP_NEEDED
      zals = (ZALTYPE *) alloca (zastacksize * sizeof (*zalsp));
      __za_bcopy ((char *)zals1, (char *)zals, size * sizeof (*zalsp));
#endif
#endif /* no zaoverflow */

      zassp = zass + size - 1;
      zavsp = zavs + size - 1;
#ifdef ZALSP_NEEDED
      zalsp = zals + size - 1;
#endif

#if ZADEBUG != 0
      if (zadebug)
	fprintf(stderr, "Stack size increased to %d\n", zastacksize);
#endif

      if (zassp >= zass + zastacksize - 1)
	ZAABORT;
    }

#if ZADEBUG != 0
  if (zadebug)
    fprintf(stderr, "Entering state %d\n", zastate);
#endif

  goto zabackup;
 zabackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* zaresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  zan = zapact[zastate];
  if (zan == ZAFLAG)
    goto zadefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* zachar is either ZAEMPTY or ZAEOF
     or a valid token in external form.  */

  if (zachar == ZAEMPTY)
    {
#if ZADEBUG != 0
      if (zadebug)
	fprintf(stderr, "Reading a token: ");
#endif
      zachar = ZALEX;
    }

  /* Convert token to internal form (in zachar1) for indexing tables with */

  if (zachar <= 0)		/* This means end of input. */
    {
      zachar1 = 0;
      zachar = ZAEOF;		/* Don't call ZALEX any more */

#if ZADEBUG != 0
      if (zadebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      zachar1 = ZATRANSLATE(zachar);

#if ZADEBUG != 0
      if (zadebug)
	{
	  fprintf (stderr, "Next token is %d (%s", zachar, zatname[zachar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef ZAPRINT
	  ZAPRINT (stderr, zachar, zalval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  zan += zachar1;
  if (zan < 0 || zan > ZALAST || zacheck[zan] != zachar1)
    goto zadefault;

  zan = zatable[zan];

  /* zan is what to do for this token type in this state.
     Negative => reduce, -zan is rule number.
     Positive => shift, zan is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (zan < 0)
    {
      if (zan == ZAFLAG)
	goto zaerrlab;
      zan = -zan;
      goto zareduce;
    }
  else if (zan == 0)
    goto zaerrlab;

  if (zan == ZAFINAL)
    ZAACCEPT;

  /* Shift the lookahead token.  */

#if ZADEBUG != 0
  if (zadebug)
    fprintf(stderr, "Shifting token %d (%s), ", zachar, zatname[zachar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (zachar != ZAEOF)
    zachar = ZAEMPTY;

  *++zavsp = zalval;
#ifdef ZALSP_NEEDED
  *++zalsp = zalloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (zaerrstatus) zaerrstatus--;

  zastate = zan;
  goto zanewstate;

/* Do the default action for the current state.  */
zadefault:

  zan = zadefact[zastate];
  if (zan == 0)
    goto zaerrlab;

/* Do a reduction.  zan is the number of a rule to reduce with.  */
zareduce:
  zalen = zar2[zan];
  if (zalen > 0)
    zaval = zavsp[1-zalen]; /* implement default value of the action */

#if ZADEBUG != 0
  if (zadebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       zan, zarline[zan]);

      /* Print the symbols being reduced, and their result.  */
      for (i = zaprhs[zan]; zarhs[i] > 0; i++)
	fprintf (stderr, "%s ", zatname[zarhs[i]]);
      fprintf (stderr, " -> %s\n", zatname[zar1[zan]]);
    }
#endif


  switch (zan) {

case 3:
#line 76 "xanaduy.y"
{ ZAABORT;;
    break;}
case 4:
#line 78 "xanaduy.y"
{  ZAACCEPT; ;
    break;}
case 5:
#line 79 "xanaduy.y"
{ ZAACCEPT;;
    break;}
case 6:
#line 80 "xanaduy.y"
{ ZAACCEPT; ;
    break;}
case 7:
#line 81 "xanaduy.y"
{ YCAPCMD("?"); ZAACCEPT; ;
    break;}
case 8:
#line 82 "xanaduy.y"
{ YCAPPAR("?"); ZAACCEPT; ;
    break;}
case 9:
#line 83 "xanaduy.y"
{ ZAACCEPT; ;
    break;}
case 10:
#line 85 "xanaduy.y"
{ zaerrok; zaclearin;;
    break;}
case 13:
#line 90 "xanaduy.y"
{ YCAPCMD(zavsp[0].value); ;
    break;}
case 16:
#line 94 "xanaduy.y"
{zaval.value=zavsp[0].value; ;
    break;}
case 17:
#line 95 "xanaduy.y"
{ zaval.value=zavsp[-2].value;;
    break;}
case 18:
#line 96 "xanaduy.y"
{zaval.value=zavsp[0].value;YNEXTPAR();
    break;}
case 20:
#line 98 "xanaduy.y"
{ YCAPDELIM(","); ;
    break;}
case 21:
#line 100 "xanaduy.y"
{/* printf("Got REAL %s  ",$1)*/;YCAPVAL(zavsp[0].value); ;
    break;}
case 22:
#line 101 "xanaduy.y"
{/*printf("Got INTEGER %s  ",$1)*/;YCAPVAL(zavsp[0].value); ;
    break;}
case 23:
#line 102 "xanaduy.y"
{/*printf("Got STRING %s  ",$1)*/;YCAPVAL(zavsp[0].value); ;
    break;}
case 24:
#line 103 "xanaduy.y"
{/*printf("Got PARAM %s  ",$1)*/;YCAPVAL(zavsp[0].value); ;
    break;}
case 25:
#line 104 "xanaduy.y"
{ YCAPVAL(zavsp[0].value); ;
    break;}
case 26:
#line 106 "xanaduy.y"
{ YCAPPAR(zavsp[0].value); ;
    break;}
case 27:
#line 107 "xanaduy.y"
{ YCAPPAR(zavsp[0].value); ;
    break;}
case 28:
#line 108 "xanaduy.y"
{ YCAPPAR(zavsp[0].value); ;
    break;}
case 29:
#line 109 "xanaduy.y"
{ YCAPPAR(zavsp[0].value); ;
    break;}
case 30:
#line 110 "xanaduy.y"
{ YCAPPAR(zavsp[0].value); ;
    break;}
case 31:
#line 111 "xanaduy.y"
{ /*YCAPDELIM("/"); */YCAPPAR(zavsp[0].value); ;
    break;}
case 32:
#line 112 "xanaduy.y"
{ /*YCAPDELIM(","); */YCAPPAR(zavsp[0].value); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/opt1/lib/bison.simple"

  zavsp -= zalen;
  zassp -= zalen;
#ifdef ZALSP_NEEDED
  zalsp -= zalen;
#endif

#if ZADEBUG != 0
  if (zadebug)
    {
      short *ssp1 = zass - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != zassp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++zavsp = zaval;

#ifdef ZALSP_NEEDED
  zalsp++;
  if (zalen == 0)
    {
      zalsp->first_line = zalloc.first_line;
      zalsp->first_column = zalloc.first_column;
      zalsp->last_line = (zalsp-1)->last_line;
      zalsp->last_column = (zalsp-1)->last_column;
      zalsp->text = 0;
    }
  else
    {
      zalsp->last_line = (zalsp+zalen-1)->last_line;
      zalsp->last_column = (zalsp+zalen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  zan = zar1[zan];

  zastate = zapgoto[zan - ZANTBASE] + *zassp;
  if (zastate >= 0 && zastate <= ZALAST && zacheck[zastate] == *zassp)
    zastate = zatable[zastate];
  else
    zastate = zadefgoto[zan - ZANTBASE];

  goto zanewstate;

zaerrlab:   /* here on detecting error */

  if (! zaerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++zanerrs;

#ifdef ZAERROR_VERBOSE
      zan = zapact[zastate];

      if (zan > ZAFLAG && zan < ZALAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -zan if nec to avoid negative indexes in zacheck.  */
	  for (x = (zan < 0 ? -zan : 0);
	       x < (sizeof(zatname) / sizeof(char *)); x++)
	    if (zacheck[x + zan] == x)
	      size += strlen(zatname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (zan < 0 ? -zan : 0);
		       x < (sizeof(zatname) / sizeof(char *)); x++)
		    if (zacheck[x + zan] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, zatname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      zaerror(msg);
	      free(msg);
	    }
	  else
	    zaerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* ZAERROR_VERBOSE */
	zaerror("parse error");
    }

  goto zaerrlab1;
zaerrlab1:   /* here on error raised explicitly by an action */

  if (zaerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (zachar == ZAEOF)
	ZAABORT;

#if ZADEBUG != 0
      if (zadebug)
	fprintf(stderr, "Discarding token %d (%s).\n", zachar, zatname[zachar1]);
#endif

      zachar = ZAEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  zaerrstatus = 3;		/* Each real token shifted decrements this */

  goto zaerrhandle;

zaerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  zan = zadefact[zastate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (zan) goto zadefault;
#endif

zaerrpop:   /* pop the current state because it cannot handle the error token */

  if (zassp == zass) ZAABORT;
  zavsp--;
  zastate = *--zassp;
#ifdef ZALSP_NEEDED
  zalsp--;
#endif

#if ZADEBUG != 0
  if (zadebug)
    {
      short *ssp1 = zass - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != zassp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

zaerrhandle:

  zan = zapact[zastate];
  if (zan == ZAFLAG)
    goto zaerrdefault;

  zan += ZATERROR;
  if (zan < 0 || zan > ZALAST || zacheck[zan] != ZATERROR)
    goto zaerrdefault;

  zan = zatable[zan];
  if (zan < 0)
    {
      if (zan == ZAFLAG)
	goto zaerrpop;
      zan = -zan;
      goto zareduce;
    }
  else if (zan == 0)
    goto zaerrpop;

  if (zan == ZAFINAL)
    ZAACCEPT;

#if ZADEBUG != 0
  if (zadebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++zavsp = zalval;
#ifdef ZALSP_NEEDED
  *++zalsp = zalloc;
#endif

  zastate = zan;
  goto zanewstate;
}
#line 115 "xanaduy.y"


char *progname;
int lineno;

flineno ()
{
    return lineno;
}

/*togdeb()
{
    extern zadebug;

    if (zadebug)
      { 
         zadebug = 0;
      }
    else
      {
         zadebug = 1;
      }
}
*/

zaerror(s) /* print warning message */
char *s;
{
    fprintf(stderr, "%s", s);
    fprintf(stderr, " line %d\n", lineno);
/*    exit(1); */
}
