%{

/* 
   08Nov96 MJT
   Brought in soc_read from old flex.skel (which is
   incompatible with present versions of flex)
*/

#include "cfortran.h"

PROTOCCALLSFFUN2 (INT,GTBUFC,gtbufc,PSTRING,INT)
#define GTBUFC(buffer,nbytes) CCALLSFFUN2(GTBUFC,gtbufc,PSTRING,INT,buffer,nbytes);
#define PROMPTC(buffer) CCALLSFSUB1(PROMPTC,promptc,PSTRING,buffer);
#define YCAPLINE(buffer) CCALLSFSUB1(YCAPLINE,ycapline,PSTRING,buffer);

static sock_chan = 0;
soc_read (fd, buf, nbyte)
     int fd;
     char *buf;
     int nbyte;
{
  int n;
  int n1;

/* See if we have stacked commands */

  if (0 != (n = stackcmd(buf,nbyte)))
    {
      YCAPLINE(buf);
      return (n);
    }
  
#ifdef vms
  if (!sock_chan) sock_chan = sockchan();
#else
  sock_chan = 0;
#endif

  /* Do we have a saved string to read? */
  if (0 != (n = xpistr(buf)))
    {
      /* We got a string and are done */
    } 
  else
    {
      if (sock_chan)
	{
#ifdef vms
	  PROMPTC("This is ignored");
#ifdef MULTINET
	  n = socket_read(sock_chan,buf,nbyte);
#else
	  puts ("You built this without multinet support");
	  puts ("Please rebuild with multinet support");
	  puts ("Something like cc/define=multinet xanadul");
	  puts ("should work");
#endif
#else
	  puts("Opps, this shouldn't happen!  It think's it's VMS");
#endif
	} 
      else 
	{
	  n = GTBUFC(buf,nbyte);
	  /*		n = read(fd,buf,nbyte);
	   */
	}
    }

  stackcmdcap(buf);
  n1 = stackcmd(buf,nbyte);
  
  YCAPLINE(buf);
  if (n != -1) n = n1;
  
  return (n);
}

/* gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#include "y_tab.h"
#define YY_INPUT(buf,result,max_size) \
        if ( (result = soc_read( fileno(yyin), (char *) buf, max_size )) < 0 ) \
            YY_FATAL_ERROR( " "/*"read() in flex scanner failed" */);
#define YY_NULL 0

%}
xantok    [A-Za-z0-9\?\$\/\.\[\-\+\@\%]+[A-Za-z0-9.\\\[\]\_\-\+\*\?\:\;\$\/\%]*
xantokon  ([A-Za-z0-9]+"+")
xantokoff ([A-Za-z0-9]+"-")
integer   [+-]?[0-9]+
irange    [+-]?[0-9]+"-"[+-]?[0-9]+
dreal     ([+-]?[0-9]*"."[0-9]*)
ereal     ([+-]?[0-9]*"."[0-9]*[eE][+-]?[0-9]+)
real      {dreal}|{ereal}
string    \"[^\"\n]*[\"\n]
/* cont      "-\n" */
slash     "/"
space     " "
comma     ","
nl        \n
semicolon ;
comment   ![^\n]*\n
%%
{xantokon}                         {
                                     /* printf("Lex xantokon %s. ",yytext);*/
                                     yylval.value = yytext;
                                     return (PARAMON);
                                   }
{xantokoff}                        {
                                     /* printf("Lex xantokoff %s. ",yytext);*/
                                     yylval.value = yytext;
                                     return (PARAMOFF);
                                   }
{xantok}                           {
                                     /* printf("Lex ParPart %s. ",yytext); */
                                     yylval.value = yytext;
                                     return (PARAM);
                                   }
=                                  {
                                     /* printf("Lex Equals %s. ",yytext); */
                                     yylval.value = yytext;
                                     return (EQUALS);
                                   }
"?"                               {
                                     /* printf("Lex question %s. ",yytext);  */
                                     yylval.value = yytext;
                                     return (QUESTION);
                                   }
{integer}                          {
                                     /* printf("Lex Integer %s. ",yytext); */
                                     yylval.value = yytext;
                                     return INTEGER;
                                   }
{irange}                           {
                                     /* printf("Lex Integer %s. ",yytext); */
                                     yylval.value = yytext;
                                     return IRANGE; 
                                   }
{real}                             {
                                     /* printf("Lex Real %s. ",yytext); */
                                     yylval.value = yytext;
                                     return REAL;
                                   }
{string}                           {
                                     /* printf("Lex String %s. ",yytext); */
                                     yylval.value = yytext;
                                     return STRING;
                                   }
{nl}                               {
                                     extern int lineno;
                                     lineno++; 
				     yylval.value = "\n";
				     return NL;
				   }
{comment}                          {
                                     extern int lineno;
                                     lineno++; 
				     yylval.value = "\n";
				     return NL;
				   }
{semicolon}                        {
                                     yylval.value = ";";
                                     return SEMICOLON;
                                   }
{slash}				   {
                                     yylval.value = "/";
                                     return SLASH;
                                   }
{space}                            {
/*                                     yylval.value = " ";
                                     return SPACE;
*/
                                   }
{comma}                            {
                                     yylval.value = ",";
                                     return COMMA;
                                   }
<<EOF>>                            {
                                     printf ("Lex EOF");
                                     yylval.value = "\004";
				     return ENDOFFILE;
				   }
%%

int yywrap()
{
  /* MJT -- 08 Nov 1996
     Supplied for compatibility with
     pre-2.5.1 versions of flex which
     do not recognize %option noyywrap 
  */
  return(1);
}
