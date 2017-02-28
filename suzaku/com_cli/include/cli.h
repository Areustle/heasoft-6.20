#ifdef __CFORTRAN_LOADED

/*------------------------------------------------------------------
fortran filename   : version.f
------------------------------------------------------------------*/

#define CLVERS() CCALLSFSUB0(CLVERS,clvers)

/*------------------------------------------------------------------
fortran filename   : clalir.f
------------------------------------------------------------------*/

#define CLALIR(A1,A2,A3)  CCALLSFSUB3(CLALIR,clalir,STRING,PSTRING,PINT,A1,A2,A3)

#define CLALIL(A1,A2,A3)  CCALLSFSUB3(CLALIL,clalil,STRING,PSTRING,PINT,A1,A2,A3)

#define CLALII(A1,A2)  CCALLSFSUB2(CLALII,clalii,STRING,STRING,A1,A2)

#define CLALIP() CCALLSFSUB0(CLALIP,clalip)

/*------------------------------------------------------------------
fortran filename   : clatof.f
------------------------------------------------------------------*/

#define CLATOF(A1,A2)  CCALLSFSUB2(CLATOF,clatof,STRING,PFLOAT,A1,A2)

#define CLATOD(A1,A2)  CCALLSFSUB2(CLATOD,clatod,STRING,PDOUBLE,A1,A2)

#define CLATOI(A1,A2)  CCALLSFSUB2(CLATOI,clatoi,STRING,PINT,A1,A2)

#define CLITOA(A1,A2)  CCALLSFSUB2(CLITOA,clitoa,INT,PSTRING,A1,A2)

#define CLFTOA(A1,A2)  CCALLSFSUB2(CLFTOA,clftoa,FLOAT,PSTRING,A1,A2)

#define CLDTOA(A1,A2)  CCALLSFSUB2(CLDTOA,cldtoa,DOUBLE,PSTRING,A1,A2)

#define CLXTOA(A1,A2,A3)  CCALLSFSUB3(CLXTOA,clxtoa,DOUBLE,INT,PSTRING,A1,A2,A3)

 PROTOCCALLSFFUN5(INT,CLFRMT,clfrmt,STRING,INT,INT,DOUBLE,PSTRING)
#define CLFRMT(A2,A3,A4,A5,A6)  CCALLSFFUN5(CLFRMT,clfrmt,STRING,INT,INT,DOUBLE,PSTRING,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : clgetl.f
------------------------------------------------------------------*/

#define CLIERR(A1,A2)  CCALLSFSUB2(CLIERR,clierr,INT,STRING,A1,A2)

#define CLSTOP() CCALLSFSUB0(CLSTOP,clstop)

#define CLSUSP() CCALLSFSUB0(CLSUSP,clsusp)

#define CLRESM() CCALLSFSUB0(CLRESM,clresm)

#define CLCURL(A1,A2,A3)  CCALLSFSUB3(CLCURL,clcurl,PINT,PINT,PSTRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : clmacr.f
------------------------------------------------------------------*/

#define CLMACS(A1,A2)  CCALLSFSUB2(CLMACS,clmacs,PSTRING,PINT,A1,A2)

#define CLMACL(A1,A2,A3)  CCALLSFSUB3(CLMACL,clmacl,STRING,PSTRING,PINT,A1,A2,A3)

#define CLGETV(A1,A2,A3)  CCALLSFSUB3(CLGETV,clgetv,STRING,PSTRING,PINT,A1,A2,A3)

#define CLSETV(A1,A2)  CCALLSFSUB2(CLSETV,clsetv,STRING,STRING,A1,A2)

#define CLSETI(A1,A2)  CCALLSFSUB2(CLSETI,clseti,STRING,INT,A1,A2)

#define CLSETF(A1,A2)  CCALLSFSUB2(CLSETF,clsetf,STRING,FLOAT,A1,A2)

#define CLSETD(A1,A2)  CCALLSFSUB2(CLSETD,clsetd,STRING,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : clopen.f
------------------------------------------------------------------*/

#define CLOPEN(A1,A2,A3,A4,A5)  CCALLSFSUB5(CLOPEN,clopen,STRING,INT,STRING,PINT,STRING,A1,A2,A3,A4,A5)

#define CLCLOS(A1,A2)  CCALLSFSUB2(CLCLOS,clclos,INT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : clword.f
------------------------------------------------------------------*/

#define CLWORD(A1,A2,A3,A4)  CCALLSFSUB4(CLWORD,clword,STRING,STRING,PINT,PSTRINGV,A1,A2,A3,A4)

#define CLPART(A1,A2,A3)  CCALLSFSUB3(CLPART,clpart,STRING,INT,PSTRING,A1,A2,A3)

#define CLPART2(A1,A2,A3,A4)  CCALLSFSUB4(CLPART2,clpart2,STRING,INT,PSTRING,PINT,A1,A2,A3,A4)

#define CLSUBP(A1,A2,A3)  CCALLSFSUB3(CLSUBP,clsubp,STRING,INT,PINT,A1,A2,A3)

#define CLSUBP2(A1,A2,A3,A4)  CCALLSFSUB4(CLSUBP2,clsubp2,STRING,INT,PINT,PINT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : flgrd.f
------------------------------------------------------------------*/

#define CLEROK() CCALLSFSUB0(CLEROK,clerok)

 PROTOCCALLSFFUN1(INT,CLILVL,clilvl,INT)
#define CLILVL(A2)  CCALLSFFUN1(CLILVL,clilvl,INT,A2)

#define CLILUN(A1)  CCALLSFSUB1(CLILUN,clilun,PINT,A1)

#define LINRD(A1,A2)  CCALLSFSUB2(LINRD,linrd,INT,INT,A1,A2)

#define FLGRD(A1,A2)  CCALLSFSUB2(FLGRD,flgrd,INT,INT,A1,A2)

#define FLARD(A1,A2)  CCALLSFSUB2(FLARD,flard,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : intrdl.f
------------------------------------------------------------------*/

/*
#define INTRD(A1,A2)  CCALLSFSUB2(INTRD,intrd,BYTE,INT,A1,A2)
*/
/* 23-DEC-1993   change by hand (BYTE->STRING)  M.Hirayama */
/* 23-DEC-1993   change by hand (INT->PINT)  M.Hirayama */
#define INTRD(A1,A2)  CCALLSFSUB2(INTRD,intrd,STRING,PINT,A1,A2)

/* 3-MAY-1994   write with hand K.Matsuzaki */
#define INTRDL(A1,A2,A3,A4)  CCALLSFSUB4(INTRDL,intrdl,STRING,PINT,INT,INT,A1,A2,A3,A4)

/*
#define FLTRD(A1,A2)  CCALLSFSUB2(FLTRD,fltrd,BYTE,FLOAT,A1,A2)
*/
/* 23-DEC-1993   change by hand (BYTE->STRING)  M.Hirayama */
/* 23-DEC-1993   change by hand (FLOAT->PFLOAT)  M.Hirayama */
#define FLTRD(A1,A2)  CCALLSFSUB2(FLTRD,fltrd,STRING,PFLOAT,A1,A2)

/*
#define FDPRD(A1,A2)  CCALLSFSUB2(FDPRD,fdprd,BYTE,FLOAT,A1,A2)
*/
/* 23-DEC-1993   change by hand (BYTE->STRING)  M.Hirayama */
/* 23-DEC-1993   change by hand (FLOAT->PDOUBLE)  M.Hirayama */
#define FDPRD(A1,A2)  CCALLSFSUB2(FDPRD,fdprd,STRING,PDOUBLE,A1,A2)
/*
#define HEXRD(A1,A2)  CCALLSFSUB2(HEXRD,hexrd,BYTE,INT,A1,A2)
*/
/* 23-DEC-1993   change by hand (BYTE->STRING)  M.Hirayama */
/* 23-DEC-1993   change by hand (INT->PINT)  M.Hirayama */
#define HEXRD(A1,A2)  CCALLSFSUB2(HEXRD,hexrd,STRING,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : keyrd.f
------------------------------------------------------------------*/

/*
#define KEYRD(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(KEYRD,keyrd,INT,STRING,PSTRING,STRINGV,INT,INT,A1,A2,A3,A4,A5,A6)
*/
/* 18-Mar-1994   change by hand (INT->PINT)  H.Kubo */
#define KEYRD(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(KEYRD,keyrd,INT,STRING,PSTRING,STRINGV,INT,PINT,A1,A2,A3,A4,A5,A6)

#define AMBRD(A1,A2,A3,A4)  CCALLSFSUB4(AMBRD,ambrd,STRINGV,INT,PSTRING,PINT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : lenrd.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(INT,LENRD,lenrd,STRING)
#define LENRD(A2)  CCALLSFFUN1(LENRD,lenrd,STRING,A2)

 PROTOCCALLSFFUN2(INT,LKBRD,lkbrd,PSTRING,INT)
#define LKBRD(A2,A3)  CCALLSFFUN2(LKBRD,lkbrd,PSTRING,INT,A2,A3)

 PROTOCCALLSFFUN2(INT,CLRCHR,clrchr,STRING,BYTE)
#define CLRCHR(A2,A3)  CCALLSFFUN2(CLRCHR,clrchr,STRING,BYTE,A2,A3)

/*------------------------------------------------------------------
fortran filename   : logrd.f
------------------------------------------------------------------*/
/*
#define LOGRD(A1,A2)  CCALLSFSUB2(LOGRD,logrd,BYTE,PLOGICAL,A1,A2)
*/
/* 23-DEC-1993   change by hand (BYTE->STRING)  M.Hirayama */
#define LOGRD(A1,A2)  CCALLSFSUB2(LOGRD,logrd,STRING,PLOGICAL,A1,A2)

#define AFFIRM(A1,A2)  CCALLSFSUB2(AFFIRM,affirm,STRING,PLOGICAL,A1,A2)

/*------------------------------------------------------------------
fortran filename   : miscunix.f
------------------------------------------------------------------*/

  PROTOCCALLSFFUN0(LOGICAL,CLITTY,clitty)
#define CLITTY() CCALLSFFUN0(CLITTY,clitty)

#define CLOWRT(A1)  CCALLSFSUB1(CLOWRT,clowrt,STRING,A1)

#define CLSYSO(A1,A2,A3,A4)  CCALLSFSUB4(CLSYSO,clsyso,INT,STRING,STRING,PINT,A1,A2,A3,A4)

#define CLEXEC(A1,A2,A3)  CCALLSFSUB3(CLEXEC,clexec,STRING,STRING,STRING,A1,A2,A3)

#define CLFDEL(A1)  CCALLSFSUB1(CLFDEL,clfdel,STRING,A1)

/*
#define STRUPC(A1,A2)  CCALLSFSUB2(STRUPC,strupc,INT,STRING,A1,A2)
*/
/* 23-DEC-1993   change by hand (STRING->PSTRING)  M.Hirayama */
#define STRUPC(A1,A2)  CCALLSFSUB2(STRUPC,strupc,INT,PSTRING,A1,A2)

#define CLSTRUPC(A1,A2)  CCALLSFSUB2(CLSTRUPC,clstrupc,INT,PSTRING,A1,A2)

#define CLSTRDWC(A1,A2)  CCALLSFSUB2(CLSTRDWC,clstrdwc,INT,PSTRING,A1,A2)

  PROTOCCALLSFFUN2(INT,CLSTRICMP,clstricmp,STRING,STRING)
#define CLSTRICMP(A2,A3)  CCALLSFFUN2(CLSTRICMP,clstricmp,STRING,STRING,A2,A3)

#define CLSIGI(A1)  CCALLSFSUB1(CLSIGI,clsigi,INT,A1)

 PROTOCCALLSFFUN1(LOGICAL,CLSIGC,clsigc,INT)
#define CLSIGC(A2)  CCALLSFFUN1(CLSIGC,clsigc,INT,A2)

#define CLSIGR(A1)  CCALLSFSUB1(CLSIGR,clsigr,INT,A1)

#define CLSIGX(A1)  CCALLSFSUB1(CLSIGX,clsigx,INT,A1)

#define CLSIGH(A1)  CCALLSFSUB1(CLSIGH,clsigh,INT,A1)

#define CLSIGF(A1,A2)  CCALLSFSUB2(CLSIGF,clsigf,INT,FLOAT,A1,A2)


/*------------------------------------------------------------------
fortran filename   : numrd.f
------------------------------------------------------------------*/

#define NUMRD(A1,A2,A3)  CCALLSFSUB3(NUMRD,numrd,DOUBLE,INT,BYTE,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : printf.f
------------------------------------------------------------------*/

#define PRINTF(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)  CCALLSFSUB11(PRINTF,printf,INT,STRING,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)

/*------------------------------------------------------------------
fortran filename   : qopen.f
------------------------------------------------------------------*/

/* 11-SEP-1994   change by hand (STRING->PSTRING)  Y.Ishisaki */
/*
 PROTOCCALLSFFUN6(LOGICAL,QOPEN,qopen,STRING,INT,STRING,STRING,STRING,STRING)
*/
 PROTOCCALLSFFUN6(LOGICAL,QOPEN,qopen,STRING,INT,PSTRING,STRING,STRING,STRING)
#define QOPEN(A2,A3,A4,A5,A6,A7)  CCALLSFFUN6(QOPEN,qopen,STRING,INT,PSTRING,STRING,STRING,STRING,A2,A3,A4,A5,A6,A7)

 PROTOCCALLSFFUN5(LOGICAL,XOPEN,xopen,INT,PSTRING,STRING,STRING,STRING)
#define XOPEN(A2,A3,A4,A5,A6)  CCALLSFFUN5(XOPEN,xopen,INT,PSTRING,STRING,STRING,STRING,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : tvdumb.f
------------------------------------------------------------------*/

#define TVCLR() CCALLSFSUB0(TVCLR,tvclr)

#define TVEDIT() CCALLSFSUB0(TVEDIT,tvedit)

#define TVINPT() CCALLSFSUB0(TVINPT,tvinpt)

#define TVPROM() CCALLSFSUB0(TVPROM,tvprom)

#define TVLOC1() CCALLSFSUB0(TVLOC1,tvloc1)

#define TVMENU() CCALLSFSUB0(TVMENU,tvmenu)

#define TVPUTL() CCALLSFSUB0(TVPUTL,tvputl)

#define TVSCRL() CCALLSFSUB0(TVSCRL,tvscrl)

/*------------------------------------------------------------------
fortran filename   : txtrd.f
------------------------------------------------------------------*/

#define TXTRD(A1,A2)  CCALLSFSUB2(TXTRD,txtrd,STRING,PSTRING,A1,A2)

#define TITRD(A1,A2)  CCALLSFSUB2(TITRD,titrd,STRING,PSTRING,A1,A2)

#define INTRDX(A1,A2,A3)  CCALLSFSUB3(INTRDX,intrdx,STRING,PINT,PSTRING,A1,A2,A3)

#define FLTRDX(A1,A2,A3)  CCALLSFSUB3(FLTRDX,fltrdx,STRING,PFLOAT,PSTRING,A1,A2,A3)

#define FDPRDX(A1,A2,A3)  CCALLSFSUB3(FDPRDX,fdprdx,STRING,PDOUBLE,PSTRING,A1,A2,A3)

#define HEXRDX(A1,A2,A3)  CCALLSFSUB3(HEXRDX,hexrdx,STRING,PINT,PSTRING,A1,A2,A3)

#define OPTRD(A1,A2)  CCALLSFSUB2(OPTRD,optrd,STRING,PSTRING,A1,A2)

#define UGETRD(A1,A2)  CCALLSFSUB2(UGETRD,ugetrd,STRING,INT,A1,A2)

#define CLLAST(A1)  CCALLSFSUB1(CLLAST,cllast,PINT,A1)

/*------------------------------------------------------------------
fortran filename   : opnrd.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(INT,OPNRD,opnrd,STRING)
#define OPNRD(A2)  CCALLSFFUN1(OPNRD,opnrd,STRING,A2)

 PROTOCCALLSFFUN1(INT,LUNRD,lunrd,INT)
#define LUNRD(A2)  CCALLSFFUN1(LUNRD,lunrd,INT,A2)

/*------------------------------------------------------------------
fortran filename   : clecho.f
------------------------------------------------------------------*/

#define CLECHO(A1)  CCALLSFSUB1(CLECHO,clecho,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : sysunix.f
------------------------------------------------------------------*/

#define CLPROM(A1)  CCALLSFSUB1(CLPROM,clprom,STRING,A1)

#define CLPUTL(A1,A2)  CCALLSFSUB2(CLPUTL,clputl,INT,STRING,A1,A2)

#endif	/* __CFORTRAN_LOADED */

#ifdef __cplusplus
extern "C"
{
#endif

/* cwrapper.c */
void CLvers(void);
void CLintrd(char *prompt, int *value);
void CLintrdL(char *prompt, int *value, int lower, int upper);
void CLintrdX(char *prompt, int *value, char *text, int text_size);
void CLhexrd(char *prompt, int *value);
void CLhexrdL(char *prompt, int *value, int lower, int upper);
void CLhexrdX(char *prompt, int *value, char *text, int text_size);
void CLfltrd(char *prompt, float *value);
void CLfltrdL(char *prompt, float *value, double lo, double hi);
void CLfltrdX(char *prompt, float *value, char *text, int text_size);
void CLfdprd(char *prompt, double *value);
void CLfdprdL(char *prompt, double *value, double lower, double upper);
void CLfdprdX(char *prompt, double *value, char *text, int text_size);
void CLtxtrd(char *prompt, char *text, int text_size);
void CLtitrd(char *prompt, char *text, int text_size);
void CLlogrd(char *prompt, int *value);
void CLkeyrd(int mode, char *prompt, char *word, char **table, int ntable, int *choice, int word_size);
int CLopnrd(char *file);
int CLlunrd(int lun);
void CLopen(char *fn, int lun, char *mode, int *error, char *ext);
int CLiopen(int lun, char *fn, char *ext, char *mode);
int CLqopen(char *prompt, int lun, char *file, char *mode, char *form, char *stat, int file_size);
int CLxopen(int lun, char *file, char *mode, char *form, char *stat, int file_size);
void CLaffirm(char *prompt, int *value);
void CLatof(char *string, float *value);
void CLatod(char *string, double *value);
void CLatoi(char *string, int *value);
void CLftoa(double value, char *string, int string_size);
void CLdtoa(double value, char *string, int string_size);
void CLitoa(int value, char *string, int string_size);
void CLstrupc(char *string);
void CLstrdwc(char *string);
int CLstricmp(char *s1, char *s2);
int CLstrnicmp(char *s1, char *s2, int len);
void CLword(char *string, char *delim, int *nword, char *words, int word_size);
void CLpart(char *string, int np, char *word, int word_size);
void CLpart2(char *string, int np, char *word, int *length, int word_size);
void CLsubp(char *string, int np, int *index);
void CLsubp2(char *string, int np, int *index, int *index2);
void CLugetrd(char *string, int length);
void CLalii(char *alias, char *command);
void CLalip(void);
void CLgetv(char *input, char *output, int output_size, int *return_length);
void CLsetv(char *var, char *value);
void CLseti(char *var, int value);
void CLsetf(char *var, double value);
void CLsetd(char *var, double value);
void CLexec(char *command, char *in, char *out);
void CLfdel(char *file);
void CLstar(int icom);
void CLiopt(char *chopt, int *ival);
void CLecho(char *string);
void CLprom(char *prompt);
void CLputl(int lun, char *string);
int CLgets(int lun, char *string, int string_size, int *length);
void CLclos(int lun, int *ierr);
void CLsusp(void);
void CLstop(void);
void CLresm(void);
void CLcurl(int *lunp, int *linumb, char *linbuf, int linbuf_size);
void CLerok(void);
void CLlast(int *length);
int CLilvl(void);
void CLilun(int *lun);
void CLflas(int IFLQ, int IFCR);
void CLflag(int *IFLQ, int *IFCR);
void CLierr(int level, char *message);
int CLilog(int sw);
void CLsleep(double sec);
void CLgetlun(int *lun);
void CLfreelun(int lun);
void CLmarklun(int lun);
void CLrhis(char *file, int *error);
void CLwhis(char *file, int lines, int *error);
void CLphis(char *file);
int CLexitcode(void);
void CLsetpath(char *path);
void CLfindpath(char *file, char *ext, char *path, int *lpath);
void CLsyso(int lun, char *file, char *mode, int *error);
int CLfgetc(int lun, char *c);
int CLfputc(int lun, int c);
void CLfseek(int lun, int offset, int whence);
int CLftell(int lun);
int CLfnum(int lun);
void CLflush(int lun);
void CLchdir(char *path);
void CLgetwd(char *path, int path_size);
void CLtempnam(char *directory, char *prefix, char *output, int output_size);

/* unixc.c */

#ifdef __cplusplus
}
#endif
