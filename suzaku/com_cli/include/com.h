#ifdef __CFORTRAN_LOADED

/*------------------------------------------------------------------
fortran filename   : chval.f
------------------------------------------------------------------*/

/*
#define CHVAL(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(CHVAL,chval,STRING,INT,STRINGV,STRINGV,STRING,PINT,A1,A2,A3,A4,A5,A6)
*/
/* 23-DEC-1993   change by hand (PINT->PVOID)  M.Hirayama */
#define CHVAL(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(CHVAL,chval,STRING,INT,STRINGV,STRINGV,STRING,PVOID,A1,A2,A3,A4,A5,A6)

#define SHVAL(A1,A2,A3,A4,A5)  CCALLSFSUB5(SHVAL,shval,STRING,STRING,INT,LOGICAL,INT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : comhelp.f
------------------------------------------------------------------*/

#define COMHLP(A1,A2,A3)  CCALLSFSUB3(COMHLP,comhlp,INT,STRINGV,STRINGV,A1,A2,A3)

#define HLPLIB(A1)  CCALLSFSUB1(HLPLIB,hlplib,STRING,A1)

#define CGETHL(A1)  CCALLSFSUB1(CGETHL,cgethl,STRING,A1)


/*------------------------------------------------------------------
fortran filename   : comutl.f
------------------------------------------------------------------*/

#define COMKEY(A1,A2)  CCALLSFSUB2(COMKEY,comkey,STRING,STRING,A1,A2)

#define GETTXT(A1,A2)  CCALLSFSUB2(GETTXT,gettxt,STRING,STRING,A1,A2)

#define COMTCH(A1,A2,A3,A4)  CCALLSFSUB4(COMTCH,comtch,STRINGV,INT,STRING,INT,A1,A2,A3,A4)

#define INIT_COM_TALK(A1)  CCALLSFSUB1(INIT_COM_TALK,init_com_talk,STRING,A1)

#define INICOM(A1)  CCALLSFSUB1(INICOM,inicom,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : crange.f
------------------------------------------------------------------*/

#define CRANGE(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(CRANGE,crange,STRING,INT,STRINGV,STRINGV,STRING,PINT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : inquire.f
------------------------------------------------------------------*/

#define INQUIRE(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(INQUIRE,inquire,STRING,INT,STRINGV,STRINGV,INT,INTV,A1,A2,A3,A4,A5,A6)

#define INQUIR(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(INQUIR,inquir,STRING,INT,STRINGV,STRINGV,INT,PINT,A1,A2,A3,A4,A5,A6)

#define INQUR2(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(INQUR2,inqur2,STRING,INT,STRINGV,STRINGV,INT,PINT,A1,A2,A3,A4,A5,A6)

#define CHOSIT(A1,A2,A3,A4,A5)  CCALLSFSUB5(CHOSIT,chosit,STRING,INT,STRINGV,STRINGV,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : modbits.f
------------------------------------------------------------------*/

#define MODBIT(A1,A2,A3,A4,A5)  CCALLSFSUB5(MODBIT,modbit,STRING,INT,STRINGV,STRINGV,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : modval.f
------------------------------------------------------------------*/

/*
#define MODVAL(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(MODVAL,modval,STRING,INT,STRINGV,STRINGV,STRING,PINT,A1,A2,A3,A4,A5,A6)
*/
/* 23-DEC-1993   change by hand (PINT->PVOID)  M.Hirayama */
#define MODVAL(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(MODVAL,modval,STRING,INT,STRINGV,STRINGV,STRING,PVOID,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : procmode.f
------------------------------------------------------------------*/

#define PRCMOD(A1)  CCALLSFSUB1(PRCMOD,prcmod,PLOGICAL,A1)

/*------------------------------------------------------------------
fortran filename   : showit.f
------------------------------------------------------------------*/

#define SHOWIT(A1,A2,A3,A4)  CCALLSFSUB4(SHOWIT,showit,STRING,INT,STRINGV,STRINGV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : switch.f
------------------------------------------------------------------*/

#define SWITCH(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(SWITCH,switch,STRING,INT,STRINGV,STRINGV,STRING,PLOGICAL,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : telbit.f
------------------------------------------------------------------*/

#define TELBIT(A1,A2,A3,A4)  CCALLSFSUB4(TELBIT,telbit,STRING,INT,STRINGV,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : tellit.f
------------------------------------------------------------------*/

#define TELLIT(A1)  CCALLSFSUB1(TELLIT,tellit,STRING,A1)

#define TELALL(A1,A2)  CCALLSFSUB2(TELALL,telall,INT,STRINGV,A1,A2)

#endif	/* __CFORTRAN_LOADED */

#ifdef __cplusplus
extern "C"
{
#endif

/* cwrapper. */
void CMchval(char *quest, int nval, char *names[], char *help[], char *vartyp, void *array);
void CMinicom(char *prompt);
void CMcrange(char *quest, int nval, char *names[], char *help[], char *vartyp, void *array);
void CMinquir(char *quest, int nval, char *names[], char *help[], int nreply, int lord[]);
void CMmodbit(char *quest, int nbits, char *names[], char *help[], int *bits);
void CMmodval(char *quest, int nval, char *names[], char *help[], char *vartyp, void *array);
void CMshowit(char *quest, int nval, char *names[], char *help[]);
void CMswitch(char *quest, int nval, char *names[], char *help[], char *vartyp, int array[]);

#ifdef __cplusplus
}
#endif
