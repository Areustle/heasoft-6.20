%{
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
int  sren;
char tmpfilbuf[200];
int  errstat=0,srch,i;
int  BufLen_3;

#include "optr.h"

extern int parse_error;
extern int exprlen;

%}

%union {
	Optr   arthop;
	Optr   boolop;
	int    boolean;
	char   string[200];
	char   number[200];
}

%token	<string>	MISSION
%token	<string>	INSTRUM
%token	<string>	DETNAM
%token	<string>	FILTER
%token	<string>	DEVICE
%token	<string>	DIR
%token	<string>	CFILE
%token	<string>	CLASS
%token	<string>	DATATYP
%token	<string>	CODENAM
%token	<string>	EXTNO
%token	<string>	DATE
%token	<string>	VSD
%token	<string>	TIME
%token	<string>	VST
%token	<string>	REFTIME
%token	<string>	QUALITY
%token	<string>	CALDATE
%token	<string>	DESCRIP
%token	<string>	CIF
%token	<string>	PARAM
%token	<string>	CBD
%token	<string>	ROW
%token	<string>	STRNG
%token	<number>	NUMBER
%token	<arthop>	ARTHOP
%token	<boolop>	BOOLOP
%token	<string>	NOT

%type <boolean>	line expr subexpr

%left   BOOLOP
%right	NOT
%left   ARTHOP

%%

line:	expr			{ $$=$1;
				  /*printf("Valid Input.\n");*/
				  result = $$;
				}
	;

expr:	  '(' expr ')'		{ $$ = $2;
				}

	| NOT '(' expr ')'	{ /*printf("Negate list.\n");*/
				  if ($3)
				      $$ = 0;
				  else
				      $$ = 1;
				}

	| expr BOOLOP expr  	{
				  if ($2.optyp == OPAND) {
				      /*printf("AND lists.\n");*/
				      if ($1 && $3)
				          $$ = 1;
				      else
				          $$ = 0;
				  }
				  else { 
				      /*printf("OR lists.\n");*/
				      if ($1 || $3)
				          $$ = 1;
				      else
				          $$ = 0;
				  }
				}

	| NOT subexpr		{ /*printf("Negate list.\n");*/
				  if ($2)
				      $$ = 0; 
				  else 
				      $$ = 1;
				}

	| subexpr		{ $$ = $1; /*printf("subexpr = %d\n",$1);*/
				}
	;

subexpr:  MISSION ARTHOP STRNG	{
                                  /*printf("Running STRGSEL with %s and %s\n",
                                  cifdata[selrows[sren]].telescop,$3);*/
				  $$ = strgsel(cifdata[selrows[sren]].telescop,
				       $2.optyp,$3);
				}

	| INSTRUM ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].instrume,
				       $2.optyp,$3);
				}

	| DETNAM ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].detnam,
				       $2.optyp,$3);
				}

	| FILTER ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].filter,
				       $2.optyp,$3);
				}

	| DEVICE ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_dev,
				       $2.optyp,$3);
				}

	| DIR ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_dir,
				       $2.optyp,$3);
				}

	| CFILE	ARTHOP STRNG	{
				  strcpy(tmpfilbuf,cifdata[selrows[sren]].cal_file);
				  tmpfilbuf[cifdata[selrows[sren]].filelen]='\0';
                                  BufLen_3 = 199;
				  Cpthnm("CALDB",cifdata[selrows[sren]].cal_dir,
				       tmpfilbuf,&errstat);
				  errstat = 0;
				  $$ = strgsel(tmpfilbuf,$2.optyp,$3);
				}

	| CLASS	ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_clas,
				       $2.optyp,$3);
				}

	| DATATYP ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_dtyp,
				       $2.optyp,$3);
				}

	| CODENAM ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_cnam,
				       $2.optyp,$3);
				}

	| EXTNO	ARTHOP NUMBER	{
				  $$ = intsel(cifdata[selrows[sren]].cal_xno,
				       $2.optyp,$3);
				}

	| DATE ARTHOP STRNG	{
				  $$ = datesel(cifdata[selrows[sren]].ref_time,
				       $2.optyp,$3);
				}

	| VSD ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_vsd,
				       $2.optyp,$3);
				}

	| TIME ARTHOP STRNG	{
				  $$ = timesel(cifdata[selrows[sren]].ref_time,
				       $2.optyp,$3);
				}

	| VST ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_vst,
				       $2.optyp,$3);
				}

	| REFTIME ARTHOP NUMBER	{
				  $$ = dblsel(cifdata[selrows[sren]].ref_time,
				       $2.optyp,$3);
				}

	| QUALITY ARTHOP NUMBER	{
				  $$ = intsel(cifdata[selrows[sren]].cal_qual,
				       $2.optyp,$3);
				}

	| CALDATE ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_date,
				       $2.optyp,$3);
				}

	| DESCRIP ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cal_desc,
				       $2.optyp,$3);
				}

	| CIF ARTHOP STRNG	{
				  $$ = strgsel(cifdata[selrows[sren]].cif,
				       $2.optyp,$3);
				}

	| PARAM	ARTHOP STRNG	{
				  srch = prmssel(cifdata[selrows[sren]].cal_cbd,
				       $1,$3);
				  if (($2.optyp == OPNE)) {
				       if (srch) {
				            $$ = 0;
				       }
				       else {
				            $$ = 1;
				       }
				  }
				  else {
				       if (srch) {
				            $$ = 1;
				       }
				       else {
				            $$ = 0;
				       }
				  }
				}

	| PARAM	ARTHOP NUMBER	{
				  $$ = prmnsel(cifdata[selrows[sren]].cal_cbd,
				       $1,$2.optyp,$3);
				}

	| CBD ARTHOP STRNG	{
                                  for(i=0;i<9;i++) {
				   srch = strgsel(cifdata[selrows[sren]].cal_cbd[i],
                                       $2.optyp,$3);
                                   if (srch) {
                                     $$ = 1;
                                     if ($2.optyp == OPEQ) break;
                                   }
                                   else {
                                     $$ = 0;
                                     if ($2.optyp == OPNE) break;
                                   }
				   /*$$ = strgsel(cifdata[selrows[sren]].cal_cbd,
                                       $2.optyp,$3);*/
                                  }
				}

	| ROW ARTHOP NUMBER	{
				  $$ = intsel((sren+1),$2.optyp,$3);
				}
	;

%%

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
