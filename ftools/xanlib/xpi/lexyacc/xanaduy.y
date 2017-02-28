%{
#include <stdio.h>
#include <string.h>

#ifndef HAVE_ALLOCA
#define alloca malloc
#endif

#define YYDEBUG 1

#ifdef VMS
#include "cfortran.h"

#define YCAPCMD(STR) CCALLSFSUB1(YCAPCMD,ycapcmd,STRING,STR)
#define YCAPDELIM(STR) CCALLSFSUB1(YCAPDELIM,ycapdelim,STRING,STR)
#define YCAPPAR(STR) CCALLSFSUB1(YCAPPAR,ycappar,STRING,STR)
#define YCAPVAL(STR) CCALLSFSUB1(YCAPVAL,ycapval,STRING,STR)
#define YNEXTPAR() CCALLSFSUB0(YNEXTPAR,ynextpar)

#else

YCAPCMD (str)
char *str;
{
  ycapcmd_(str,strlen(str));
}

YCAPDELIM (str)
char *str;
{
  ycapdelim_(str,strlen(str));
}

YCAPPAR (str)
char *str;
{
  ycappar_(str,strlen(str));
}

YCAPVAL (str)
char *str;
{
  ycapval_(str,strlen(str));
}

YNEXTPAR ()
{
  ynextpar_();
}

YYNULL ()
{
}

#endif




%}

%union {
    char   * value;   
}

%token <value> REAL INTEGER STRING PARAM SLASH EQUALS COMMA SPACE
%token <value> ENDOFFILE SEMICOLON NL PARAMON PARAMOFF IRANGE QUESTION
%type <value> par1 parameter par value values comm eol


%start lines

%%

lines:	| lines line
        | ENDOFFILE { YYABORT;} ;

line:  eol {  YYACCEPT; }
       | eol { YYACCEPT;}
       | comm eol { YYACCEPT; }
       | QUESTION eol { YCAPCMD("?"); YYACCEPT; }
       | comm QUESTION eol { YCAPPAR("?"); YYACCEPT; }
       | comm par eol { YYACCEPT; }
       | error 
                { yyerrok; yyclearin;}
       ;

eol:  NL | SEMICOLON ;

comm: PARAM { YCAPCMD($1); }; 

par: parameter | parameter par ;

parameter: values {$$=$1; } | 
           par1 EQUALS value { $$=$1;} |
           par1 {$$=$1;YNEXTPAR()} ; 

values: value | value COMMA values { YCAPDELIM(","); } ;

value:     REAL {/* printf("Got REAL %s  ",$1)*/;YCAPVAL($1); } | 
           INTEGER {/*printf("Got INTEGER %s  ",$1)*/;YCAPVAL($1); } | 
           STRING {/*printf("Got STRING %s  ",$1)*/;YCAPVAL($1); } |
           PARAM {/*printf("Got PARAM %s  ",$1)*/;YCAPVAL($1); } |
           IRANGE { YCAPVAL($1); } ;

par1:   PARAMON { YCAPPAR($1); } |
        SLASH PARAMON { YCAPPAR($2); } |
        PARAMOFF { YCAPPAR($1); } |
        SLASH PARAMOFF { YCAPPAR($2); } |
        PARAM { YCAPPAR($1); } |
	SLASH PARAM { /*YCAPDELIM("/"); */YCAPPAR($2); } |
	COMMA PARAM { /*YCAPDELIM(","); */YCAPPAR($2); } ;


%%

char *progname;
int lineno;

flineno ()
{
    return lineno;
}

/*togdeb()
{
    extern yydebug;

    if (yydebug)
      { 
         yydebug = 0;
      }
    else
      {
         yydebug = 1;
      }
}
*/

yyerror(s) /* print warning message */
char *s;
{
    fprintf(stderr, "%s", s);
    fprintf(stderr, " line %d\n", lineno);
/*    exit(1); */
}
