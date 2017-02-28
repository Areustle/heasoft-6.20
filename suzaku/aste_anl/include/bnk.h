/* $Id: bnk.h,v 1.9 2005/11/29 19:58:07 ishisaki Exp $ */

#ifndef _BNK_H_
#define _BNK_H_

/*------------------------------------------------------------------
fortran filename   : bnk.f
------------------------------------------------------------------*/

#ifdef __CFORTRAN_LOADED

#define BNKINI(A1)  CCALLSFSUB1(BNKINI,bnkini,INT,A1)

#define BNKDEF(A1,A2)  CCALLSFSUB2(BNKDEF,bnkdef,STRING,INT,A1,A2)

#define BNKEQV(A1,A2,A3,A4)  CCALLSFSUB4(BNKEQV,bnkeqv,STRING,INT,STRING,INT,A1,A2,A3,A4)

#define BNKLST() CCALLSFSUB0(BNKLST,bnklst)

 PROTOCCALLSFFUN2(LOGICAL,BNKKEY,bnkkey,STRING,PINT)
#define BNKKEY(A2,A3)  CCALLSFFUN2(BNKKEY,bnkkey,STRING,PINT,A2,A3)

 PROTOCCALLSFFUN1(LOGICAL,BNKISDEF,bnkisdef,STRING)
#define BNKISDEF(A2)  CCALLSFFUN1(BNKISDEF,bnkisdef,STRING,A2)

 PROTOCCALLSFFUN0(INT,BNKNDF,bnkndf)
#define BNKNDF() CCALLSFFUN0(BNKNDF,bnkndf)

#define BNKPUT(A1,A2,A3)  CCALLSFSUB3(BNKPUT,bnkput,STRING,INT,PVOID,A1,A2,A3)

#define BNKGET(A1,A2,A3,A4)  CCALLSFSUB4(BNKGET,bnkget,STRING,INT,PINT,PVOID,A1,A2,A3,A4)

#define BNKFPUT(A1,A2,A3,A4)  CCALLSFSUB4(BNKFPUT,bnkfput,STRING,PINT,INT,PVOID,A1,A2,A3,A4)

#define BNKFGET(A1,A2,A3,A4,A5)  CCALLSFSUB5(BNKFGET,bnkfget,STRING,PINT,INT,PINT,PVOID,A1,A2,A3,A4,A5)

#endif	/* __CFORTRAN_LOADED */

#ifdef __cplusplus
extern "C"
{
#endif

/* shmbnk.c */
int BnkShmCreate(int buffer_size, char *shm_file);
int BnkShmOpen(char *shm_file);
int BnkShmClose(void);
int BnkIni(int buffer_size);
int BnkEnd(void);
int BnkDef(char *key, int size);
int BnkKey(char *key, int *index);
int BnkIsDef(char *key);
int BnkNdf(void);
int BnkEqv(char *new_key, int size, char *old_key, int start);
int BnkPut(char *key, int size, void *ptr);
int BnkGet(char *key, int size, int *used, void *ptr);
int BnkfPut(char *key, int *index_ptr, int size, void *ptr);
int BnkfGet(char *key, int *index_ptr, int size, int *used, void *ptr);
void BnkLst(void);
int BnkConnect(char *server);
int BnkExport(char *key);
int BnkExportAll(void);
int BnkServer(int port);

#ifdef __cplusplus
}
#endif

#define BnkfPutM(key,size,value)	{\
	static int index = 0;\
	BnkfPut(key, &index, size, value);\
}

#define BnkfGetM(key,size,used,value)	{\
	static int index = 0;\
	BnkfGet(key, &index, size, used, value);\
}

#endif	/* _BNK_H_ */
