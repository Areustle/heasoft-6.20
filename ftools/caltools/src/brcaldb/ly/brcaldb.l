%{
#include <string.h>
#include <math.h>

#include "optr.h"
#include "y_tab.h"

/* Redefine YY_INPUT so that lex input will come from internal memory */
#undef YY_INPUT
#define YY_INPUT(b,r,ms) (r=rdmemstr(b,ms))

#define min(X, Y)  ((X) < (Y) ? (X) : (Y))
#ifdef vms
#define isatty(fd) 0
#endif

int len;
int exprlen;

%}

not             ("!"|".not."|".NOT."|not|NOT)
mission		(mission|telescop|MISSION|TELESCOP)
instrum		(ins|inst|instrume|instrument|INS|INST|INSTRUME|INSTRUMENT)
detnam		(det|detnam|DET|DETNAM)
filter		(filt|filter|FILT|FILTER)
device		(dev|cal_dev|DEV|CAL_DEV)
dir		(dir|cal_dir|DIR|CAL_DIR)
cfile		(file|FILE|cal_file|CAL_FILE|files|FILES)
class		(class|CLASS|cal_clas|CAL_CLAS)
datatyp		(type|TYPE|cal_dtyp|CAL_DTYP)
codenam		(dataset|DATASET|codename|CODENAME|cal_cnam|CAL_CNAM|codenam|CODENAM)
extno		(ext|EXT|extno|EXTNO|cal_xno|CAL_XNO)
date		(date|DATE)
vsd		(vsd|VSD|cal_vsd|CAL_VSD)
time		(time|TIME)
vst		(vst|VST|cal_vst|CAL_VST)
reftime		(reftime|REFTIME|ref_time|REF_TIME)
quality		(qual|QUAL|quality|QUALITY|cal_qual|CAL_QUAL)
caldate		(caldate|CALDATE|cal_date|CAL_DATE)
descrip		(desc|DESC|cal_desc|CAL_DESC)
cif		(cif|CIF)
row		(row|"#"row|ROW|"#"ROW)
cbd		(cbd|CBD|cal_cbd|CAL_CBD|bounds|BOUNDS|boundary|BOUNDARY)
param		([a-zA-Z_0-9]*[a-zA-Z_][a-zA-Z0-9]*)
or              ("||"|".or."|".OR."|or|OR)
and             ("&&"|".and."|".AND."|and|AND)
equal           ("=="|".eq."|".EQ."|eq|EQ|=)
not_equal       ("!="|".ne."|".NE."|ne|NE)
greater         (">"|".gt."|".GT."|gt|GT)
lesser          ("<"|".lt."|".LT."|lt|LT)
greater_eq      (">="|"=>"|".ge."|".GE."|ge|GE)
lesser_eq       ("<="|"=<"|".le."|".LE."|le|LE)
string		([a-zA-Z_\-\./:0-9]*[a-zA-Z_\-/:][a-zA-Z_\-\./:0-9]*)
sqstring	(\'[^\']*\')
dqstring	(\"[^\"]*\")
real            ([0-9]*"."[0-9]+)|([0-9]*"."*[0-9]+[eEdD][+-]?[0-9]+)
integer		([0-9]+)

%s BOPMODE
%s AOPMODE
%s VALMODE
%%

([\ \t]+)		{exprlen += 1;}

{not}		{
		  strcpy(cblval.string,cbtext);
		  exprlen += cbleng;
		  return NOT;
		}

<INITIAL>{mission}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return MISSION;
		}

<INITIAL>{instrum}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return INSTRUM;
		}

<INITIAL>{filter}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return FILTER;
		}

<INITIAL>{device}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return DEVICE;
		}

<INITIAL>{dir}		{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return DIR;
		}

<INITIAL>{cfile}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return CFILE;
		}

<INITIAL>{class}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return CLASS;
		}

<INITIAL>{datatyp}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return DATATYP;
		}

<INITIAL>{codenam}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return CODENAM;
		}

<INITIAL>{extno}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return EXTNO;
		}

<INITIAL>{date}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return DATE;
		}

<INITIAL>{vsd}		{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return VSD;
		}

<INITIAL>{time}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return TIME;
		}

<INITIAL>{vst}		{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return VST;
		}

<INITIAL>{reftime}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return REFTIME;
		}

<INITIAL>{quality}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return QUALITY;
		}

<INITIAL>{caldate}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return CALDATE;
		}

<INITIAL>{descrip}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return DESCRIP;
		}

<INITIAL>{cif}		{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return CIF;
			}

<INITIAL>{row}		{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return ROW;
		}

<INITIAL>{cbd}		{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return CBD;
		}

<INITIAL>{param}	{
		  strcpy(cblval.string,cbtext);
		  BEGIN AOPMODE;
		  exprlen += cbleng;
		  return PARAM;
		}

<BOPMODE>{or}	{
		  strcpy(cblval.boolop.opstr,cbtext);
		  cblval.boolop.optyp = OPOR;
		  BEGIN INITIAL;
		  exprlen += cbleng;
		  return BOOLOP;
		}

<BOPMODE>{and}	{
		  strcpy(cblval.boolop.opstr,cbtext);
		  cblval.boolop.optyp = OPAND;
		  BEGIN INITIAL;
		  exprlen += cbleng;
		  return BOOLOP;
		}

<AOPMODE>{equal}		{
		  strcpy(cblval.arthop.opstr,cbtext);
		  cblval.arthop.optyp = OPEQ;
		  BEGIN VALMODE;
		  exprlen += cbleng;
		  return ARTHOP;
		}

<AOPMODE>{not_equal}	{
		  strcpy(cblval.arthop.opstr,cbtext);
		  cblval.arthop.optyp = OPNE;
		  BEGIN VALMODE;
		  exprlen += cbleng;
		  return ARTHOP;
		}

<AOPMODE>{greater}	{
		  strcpy(cblval.arthop.opstr,cbtext);
		  cblval.arthop.optyp = OPGT;
		  BEGIN VALMODE;
		  exprlen += cbleng;
		  return ARTHOP;
		}

<AOPMODE>{lesser}	{
		  strcpy(cblval.arthop.opstr,cbtext);
		  cblval.arthop.optyp = OPLT;
		  BEGIN VALMODE;
		  exprlen += cbleng;
		  return ARTHOP;
		}

<AOPMODE>{greater_eq}	{
		  strcpy(cblval.arthop.opstr,cbtext);
		  cblval.arthop.optyp = OPGE;
		  BEGIN VALMODE;
		  exprlen += cbleng;
		  return ARTHOP;
		}

<AOPMODE>{lesser_eq}	{
		  strcpy(cblval.arthop.opstr,cbtext);
		  cblval.arthop.optyp = OPLE;
		  BEGIN VALMODE;
		  exprlen += cbleng;
		  return ARTHOP;
		}

<VALMODE>"*"?{string}"*"?			{
		  strcpy(cblval.string,cbtext);
		  BEGIN BOPMODE;
		  exprlen += cbleng;
		  return STRNG;
		}

<VALMODE>"*"?{sqstring}"*"?	{
		  stripq();
		  BEGIN BOPMODE;
		  exprlen += cbleng;
		  return STRNG;
		}

<VALMODE>"*"?{dqstring}"*"?	{
		  stripq();
		  BEGIN BOPMODE;
		  exprlen += cbleng;
		  return STRNG;
		}

<VALMODE>"*"(({real})|({integer}))"*"	{
                  strcpy(cblval.string,cbtext);
		  BEGIN BOPMODE;
		  exprlen += cbleng;
		  return STRNG;
		}

<VALMODE>{real}	{
		  len = 0; 
                  while (cbtext[len] != '\0')
                       {
                        if (cbtext[len] == 'd') cbtext[len]='e';
                        if (cbtext[len] == 'D') cbtext[len]='e';
                        if (cbtext[len] == 'E') cbtext[len]='e';
                        len++;
                       }
		  strcpy(cblval.number,cbtext);
		  BEGIN BOPMODE;
		  exprlen += cbleng;
                  return NUMBER;
                }

<VALMODE>{integer}	{
		  strcpy(cblval.number,cbtext);
		  BEGIN BOPMODE;
		  exprlen += cbleng;
		  return NUMBER;
		}

(.|\n)		{
		  exprlen += 1;
		  return cbtext[0];
		}

('\0')		{
		  return (0);
		}

%%

extern char expr[];
extern char *exprptr;
extern char *exprlim;

extern char *colmptr;
extern char *colmlim;

int rdmemstr(buf, max_size)
 char *buf;
 int max_size;
 {
     char *strbufptr=0, *strbuflim=0;
     extern int read_string;
     int n;

     switch (read_string) {
          case 1: strbufptr = exprptr; strbuflim = exprlim; break;
          case 2: strbufptr = colmptr; strbuflim = colmlim; break;
     }

     n = min(max_size,strbuflim - strbufptr);

     if(n>0) {
          memcpy(buf,strbufptr,n);
          strbufptr += n;
     }
     return n;
 }

int stripq()
{
   int n = 0, wrdlen = 0; 
   char c=0;
   char *q1=0, *q2;
 
   if (cbtext[0] == '\'') {
       c = '\'';   
       q1 = &cbtext[0];
   } 
   else if (cbtext[0] == '\"') {
       c = '\"';   
       q1 = &cbtext[0];
   } 
   else if (cbtext[1] == '\'') {
       c = '\'';   
       q1 = &cbtext[1];
   } 
   else if (cbtext[1] == '\"') {
       c = '\"';   
       q1 = &cbtext[1];
   } 
 
   q2 = strrchr(cbtext,c); 
   wrdlen = q2-q1-1; 

   if (cbtext[0] == '*') {
       cblval.string[n] = '*';
       n++;
   }

   strncpy(&cblval.string[n],&q1[1],wrdlen);
   if (q2[1] == '*') {
      cblval.string[n+wrdlen] = '*';
      cblval.string[n+wrdlen+1] = '\0';
   }
   else {
      cblval.string[n+wrdlen] = '\0';
   }

}

begin_initial()
{
    BEGIN INITIAL;
}

int cbwrap()
        {   
        return 1;
        }
