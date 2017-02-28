/* $Id: evs.h,v 1.7 2005/11/29 19:58:07 ishisaki Exp $ */

#ifndef _EVS_H_
#define _EVS_H_

/*------------------------------------------------------------------
fortran filename   : evs.f
------------------------------------------------------------------*/

#ifdef __CFORTRAN_LOADED

#define EVSIZ() CCALLSFSUB0(EVSIZ,evsiz)

#define EVSCLR_ALL() CCALLSFSUB0(EVSCLR_ALL,evsclr_all)

#define EVSACM() CCALLSFSUB0(EVSACM,evsacm)

#define EVSDEF(A1)  CCALLSFSUB1(EVSDEF,evsdef,STRING,A1)

#define EVSVAL(A1,A2)  CCALLSFSUB2(EVSVAL,evsval,STRING,LOGICAL,A1,A2)

#define EVSSET(A1)  CCALLSFSUB1(EVSSET,evsset,STRING,A1)

#define EVSCLR(A1)  CCALLSFSUB1(EVSCLR,evsclr,STRING,A1)

 PROTOCCALLSFFUN2(LOGICAL,EVSKEY,evskey,STRING,PINT)
#define EVSKEY(A2,A3)  CCALLSFFUN2(EVSKEY,evskey,STRING,PINT,A2,A3)

 PROTOCCALLSFFUN1(LOGICAL,EVSISDEF,evsisdef,STRING)
#define EVSISDEF(A2)  CCALLSFFUN1(EVSISDEF,evsisdef,STRING,A2)

 PROTOCCALLSFFUN1(LOGICAL,EVS,evs,STRING)
#define EVS(A2)  CCALLSFFUN1(EVS,evs,STRING,A2)

#define EVSNUM(A1,A2)  CCALLSFSUB2(EVSNUM,evsnum,STRING,PINT,A1,A2)

 PROTOCCALLSFFUN0(INT,EVSNDF,evsndf)
#define EVSNDF() CCALLSFFUN0(EVSNDF,evsndf)

#define EVSOUT(A1)  CCALLSFSUB1(EVSOUT,evsout,INT,A1)

/* 08-Oct-1994   added by Y.Ishisaki */
#define EVSF(A1,A2)  CCALLSFFUN2(EVS,evs,STRING,PINT,A1,A2)
#define EVSFSET(A1,A2)  CCALLSFSUB2(EVSFSET,evsset,STRING,PINT,A1,A2)

#endif	/* __CFORTRAN_LOADED */

#ifdef __cplusplus
extern "C"
{
#endif

/* shmevs.c */
int EvsShmCreate(char *shm_file);
int EvsShmOpen(char *shm_file);
int EvsShmClose(void);
int EvsIz(void);
int EvsEnd(void);
void EvsClrAll(void);
void EvsRstAll(void);
int EvsRst(char *key);
void EvsAcm(void);
int EvsDef(char *key);
int EvsKey(char *key, int *index);
int EvsIsDef(char *key);
int EvsNdf(void);
void EvsOut(void);
int EvsVal(char *key, int logic);
int EvsSet(char *key);
int EvsClr(char *key);
int Evs(char *key);
int EvsPut(char *key, double val);
int EvsAdd(char *key, double val);
int EvsGet(char *key, double *val);
int EvsNum(char *key, double *num);
int EvsfVal(char *key, int *index_ptr, int logic);
int EvsfSet(char *key, int *index_ptr);
int EvsfClr(char *key, int *index_ptr);
int Evsf(char *key, int *index_ptr);
int EvsfPut(char *key, int *index_ptr, double val);
int EvsfAdd(char *key, int *index_ptr, double val);
int EvsfGet(char *key, int *index_ptr, double *val);
int EvsfNum(char *key, int *index_ptr, double *num);

#ifdef __cplusplus
}
#endif

#define EvsfValM(key, logic)	{\
	static int index = 0;\
	EvsfVal(key, &index, logic);\
}

#define EvsfSetM(key)	{\
	static int index = 0;\
	EvsfSet(key, &index);\
}

#define EvsfClrM(key)	{\
	static int index = 0;\
	EvsfClr(key, &index);\
}

#define EvsfPutM(key, val)	{\
	static int index = 0;\
	EvsfPut(key, &index, val);\
}

#define EvsfAddM(key, val)	{\
	static int index = 0;\
	EvsfAdd(key, &index, val);\
}

#define EvsfGetM(key, val)	{\
	static int index = 0;\
	EvsfGet(key, &index, val);\
}

#define EvsfNumM(key, num)	{\
	static int index = 0;\
	EvsfNum(key, &index, num);\
}

#endif	/* _EVS_H_ */
