
/*  A Bison parser, made from brcaldb.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define yyparse cbparse
#define yylex cblex
#define yyerror cberror
#define yylval cblval
#define yychar cbchar
#define yydebug cbdebug
#define yynerrs cbnerrs
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

#line 1 "brcaldb.y"

#include <stdio.h>

#include "ftools.h"

#include "cifdata.h"

/* 
no longer needed since we pass -DHAVE_ALLOCA_H
(and since the current version of bison generates
correct conditionals to include it when appropriate)

#ifdef __sgi
#include <alloca.h>
#endif
*/
 
extern Cifdata *cifdata;
extern int *selrows;

/* These variables define the internal string for YY_INPUT */
extern char expr[];
extern char *exprptr, *exprlim;

extern int read_string;

#define YYDEBUG 1

int  result;
extern int  sren;
char tmpfilbuf[200];
int  errstat=0,srch,i;
extern int  BufLen_3;

#include "optr.h"

extern int parse_error;
extern int exprlen;


#line 42 "brcaldb.y"
typedef union {
	Optr   arthop;
	Optr   boolop;
	int    boolean;
	char   string[200];
	char   number[200];
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		85
#define	YYFLAG		-32768
#define	YYNTBASE	33

#define YYTRANSLATE(x) ((unsigned)(x) <= 285 ? yytranslate[x] : 36)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    31,
    32,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     6,    11,    15,    18,    20,    24,    28,    32,
    36,    40,    44,    48,    52,    56,    60,    64,    68,    72,
    76,    80,    84,    88,    92,    96,   100,   104,   108,   112
};

static const short yyrhs[] = {    34,
     0,    31,    34,    32,     0,    30,    31,    34,    32,     0,
    34,    29,    34,     0,    30,    35,     0,    35,     0,     3,
    28,    26,     0,     4,    28,    26,     0,     5,    28,    26,
     0,     6,    28,    26,     0,     7,    28,    26,     0,     8,
    28,    26,     0,     9,    28,    26,     0,    10,    28,    26,
     0,    11,    28,    26,     0,    12,    28,    26,     0,    13,
    28,    27,     0,    14,    28,    26,     0,    15,    28,    26,
     0,    16,    28,    26,     0,    17,    28,    26,     0,    18,
    28,    27,     0,    19,    28,    27,     0,    20,    28,    26,
     0,    21,    28,    26,     0,    22,    28,    26,     0,    23,
    28,    26,     0,    23,    28,    27,     0,    24,    28,    26,
     0,    25,    28,    27,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    87,    93,    96,   103,   120,   127,   131,   138,   143,   148,
   153,   158,   163,   173,   178,   183,   188,   193,   198,   203,
   208,   213,   218,   223,   228,   233,   238,   259,   264,   281
};

static const char * const yytname[] = {   "$","error","$illegal.","MISSION",
"INSTRUM","DETNAM","FILTER","DEVICE","DIR","CFILE","CLASS","DATATYP","CODENAM",
"EXTNO","DATE","VSD","TIME","VST","REFTIME","QUALITY","CALDATE","DESCRIP","CIF",
"PARAM","CBD","ROW","STRNG","NUMBER","ARTHOP","BOOLOP","NOT","'('","')'","line",
"expr","subexpr",""
};
#endif

static const short yyr1[] = {     0,
    33,    34,    34,    34,    34,    34,    35,    35,    35,    35,
    35,    35,    35,    35,    35,    35,    35,    35,    35,    35,
    35,    35,    35,    35,    35,    35,    35,    35,    35,    35
};

static const short yyr2[] = {     0,
     1,     3,     4,     3,     2,     1,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3
};

static const short yydefact[] = {     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     1,     6,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     5,     0,     0,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    27,    28,    29,    30,     0,     2,
     4,     3,     0,     0,     0
};

static const short yydefgoto[] = {    83,
    26,    27
};

static const short yypact[] = {     0,
   -27,   -26,    33,    34,    35,    36,    37,    38,    39,    40,
    41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
    51,    52,    53,    29,     0,    28,-32768,    56,    57,    58,
    59,    60,    61,    62,    63,    64,    65,    66,    68,    69,
    70,    71,    72,    73,    75,    76,    77,     1,    78,    79,
     0,-32768,    26,     0,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,    27,-32768,
-32768,-32768,    92,    98,-32768
};

static const short yypgoto[] = {-32768,
   -25,    81
};


#define	YYLAST		106


static const short yytable[] = {    53,
    28,    29,     1,     2,     3,     4,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,    23,    79,    75,    76,    81,    24,
    25,     1,     2,     3,     4,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,    23,    54,    54,    54,    80,    82,    51,
    30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
    40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
    50,    55,    56,    57,    58,    59,    60,    61,    62,    63,
    64,    84,    65,    66,    67,    68,    69,    85,    70,    71,
    72,    73,    74,    77,    52,    78
};

static const short yycheck[] = {    25,
    28,    28,     3,     4,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    25,    51,    26,    27,    54,    30,
    31,     3,     4,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,    29,    29,    29,    32,    32,    31,
    28,    28,    28,    28,    28,    28,    28,    28,    28,    28,
    28,    28,    28,    28,    28,    28,    28,    28,    28,    28,
    28,    26,    26,    26,    26,    26,    26,    26,    26,    26,
    26,     0,    27,    26,    26,    26,    26,     0,    27,    27,
    26,    26,    26,    26,    24,    27
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
#if defined (MSDOS) && !defined (__TURBOC__) && !defined (__APPLE__)
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

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
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
__yy_bcopy (char *from, char *to, int count)
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
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 87 "brcaldb.y"
{ yyval.boolean=yyvsp[0].boolean;
				  /*printf("Valid Input.\n");*/
				  result = yyval.boolean;
				;
    break;}
case 2:
#line 93 "brcaldb.y"
{ yyval.boolean = yyvsp[-1].boolean;
				;
    break;}
case 3:
#line 96 "brcaldb.y"
{ /*printf("Negate list.\n");*/
				  if (yyvsp[-1].boolean)
				      yyval.boolean = 0;
				  else
				      yyval.boolean = 1;
				;
    break;}
case 4:
#line 103 "brcaldb.y"
{
				  if (yyvsp[-1].boolop.optyp == OPAND) {
				      /*printf("AND lists.\n");*/
				      if (yyvsp[-2].boolean && yyvsp[0].boolean)
				          yyval.boolean = 1;
				      else
				          yyval.boolean = 0;
				  }
				  else { 
				      /*printf("OR lists.\n");*/
				      if (yyvsp[-2].boolean || yyvsp[0].boolean)
				          yyval.boolean = 1;
				      else
				          yyval.boolean = 0;
				  }
				;
    break;}
case 5:
#line 120 "brcaldb.y"
{ /*printf("Negate list.\n");*/
				  if (yyvsp[0].boolean)
				      yyval.boolean = 0; 
				  else 
				      yyval.boolean = 1;
				;
    break;}
case 6:
#line 127 "brcaldb.y"
{ yyval.boolean = yyvsp[0].boolean; /*printf("subexpr = %d\n",$1);*/
				;
    break;}
case 7:
#line 131 "brcaldb.y"
{
                                  /*printf("Running STRGSEL with %s and %s\n",
                                  cifdata[selrows[sren]].telescop,$3);*/
				  yyval.boolean = strgsel(cifdata[selrows[sren]].telescop,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 8:
#line 138 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].instrume,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 9:
#line 143 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].detnam,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 10:
#line 148 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].filter,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 11:
#line 153 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_dev,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 12:
#line 158 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_dir,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 13:
#line 163 "brcaldb.y"
{
				  strcpy(tmpfilbuf,cifdata[selrows[sren]].cal_file);
				  tmpfilbuf[cifdata[selrows[sren]].filelen]='\0';
                                  BufLen_3 = 199;
				  Cpthnm("CALDB",cifdata[selrows[sren]].cal_dir,
				       tmpfilbuf,&errstat);
				  errstat = 0;
				  yyval.boolean = strgsel(tmpfilbuf,yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 14:
#line 173 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_clas,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 15:
#line 178 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_dtyp,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 16:
#line 183 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_cnam,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 17:
#line 188 "brcaldb.y"
{
				  yyval.boolean = intsel(cifdata[selrows[sren]].cal_xno,
				       yyvsp[-1].arthop.optyp,yyvsp[0].number);
				;
    break;}
case 18:
#line 193 "brcaldb.y"
{
				  yyval.boolean = datesel(cifdata[selrows[sren]].ref_time,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 19:
#line 198 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_vsd,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 20:
#line 203 "brcaldb.y"
{
				  yyval.boolean = timesel(cifdata[selrows[sren]].ref_time,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 21:
#line 208 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_vst,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 22:
#line 213 "brcaldb.y"
{
				  yyval.boolean = dblsel(cifdata[selrows[sren]].ref_time,
				       yyvsp[-1].arthop.optyp,yyvsp[0].number);
				;
    break;}
case 23:
#line 218 "brcaldb.y"
{
				  yyval.boolean = intsel(cifdata[selrows[sren]].cal_qual,
				       yyvsp[-1].arthop.optyp,yyvsp[0].number);
				;
    break;}
case 24:
#line 223 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_date,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 25:
#line 228 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cal_desc,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 26:
#line 233 "brcaldb.y"
{
				  yyval.boolean = strgsel(cifdata[selrows[sren]].cif,
				       yyvsp[-1].arthop.optyp,yyvsp[0].string);
				;
    break;}
case 27:
#line 238 "brcaldb.y"
{
				  srch = prmssel(cifdata[selrows[sren]].cal_cbd,
				       yyvsp[-2].string,yyvsp[0].string);
				  if ((yyvsp[-1].arthop.optyp == OPNE)) {
				       if (srch) {
				            yyval.boolean = 0;
				       }
				       else {
				            yyval.boolean = 1;
				       }
				  }
				  else {
				       if (srch) {
				            yyval.boolean = 1;
				       }
				       else {
				            yyval.boolean = 0;
				       }
				  }
				;
    break;}
case 28:
#line 259 "brcaldb.y"
{
				  yyval.boolean = prmnsel(cifdata[selrows[sren]].cal_cbd,
				       yyvsp[-2].string,yyvsp[-1].arthop.optyp,yyvsp[0].number);
				;
    break;}
case 29:
#line 264 "brcaldb.y"
{
                                  for(i=0;i<9;i++) {
				   srch = strgsel(cifdata[selrows[sren]].cal_cbd[i],
                                       yyvsp[-1].arthop.optyp,yyvsp[0].string);
                                   if (srch) {
                                     yyval.boolean = 1;
                                     if (yyvsp[-1].arthop.optyp == OPEQ) break;
                                   }
                                   else {
                                     yyval.boolean = 0;
                                     if (yyvsp[-1].arthop.optyp == OPNE) break;
                                   }
				   /*$$ = strgsel(cifdata[selrows[sren]].cal_cbd,
                                       $2.optyp,$3);*/
                                  }
				;
    break;}
case 30:
#line 281 "brcaldb.y"
{
				  yyval.boolean = intsel((sren+1),yyvsp[-1].arthop.optyp,yyvsp[0].number);
				;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/opt1/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 286 "brcaldb.y"


/* All the functions in the bryfuncs.c file were contained here.  They were
 * removed on June 12, 1995 to accomodate a compiler bug found on the DEC
 * ULTRIX macine. The bug is not understood, but is somehow related to the 
 * ftoosltuct.h file.  When DEC ULTRIX no longer needs to be supported, 
 * the bryfuncs.c functions should be reintegrated. -- RSZ */

cberror(errmsg)
char *errmsg;
{
    extern char *cbtext;
    char contxt[200], tmpstr[200];
    int i;

	sprintf(contxt,"%s\n",errmsg);
        Fcerr(contxt);
        parse_error = 1;
        sprintf(contxt,"Could not parse the token \"%s\"\n",cbtext);
        Fcerr(contxt);

        for (i=0;i<exprlen-1;i++)
            tmpstr[i] = '-';

        tmpstr[exprlen-1] = '^';
        tmpstr[exprlen] = '\0';

        sprintf(contxt,"%s\n",expr);
        Fcerr(contxt);
        Fcerr(tmpstr);
        YYABORT;
}
