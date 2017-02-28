/**************************************
c evdump.f
c     sis/gis data dump routines
c
c     H.Kubo     11/10 1993
c     H.Kubo     11/18 1993 EVS DUMP:BEGIN,ENTRY,OK
c     H.Kubo     system flags are changed
c     M.Hirayama 12/06 1993 subprogram name dump -> evdump
c     M.Hirayama 12/06 1993 add ANL_put_version
c     M.Hirayama 12/06 1993 include '/utk/lib/asca_anl_v0.3/include/asca_anl.inc'
c     M.Hirayama 12/10 1993 change var. name (recid -> eventid)
c     M.Hirayama 12/12 1993 change include file ('/utk/lib/...' -> 'Includes.inc')
c     Y.Ishisaki 04/10 1994 GIS: -> ANL:, use ANL:SENSOR
**************************************/
#include <stdio.h>
#include <cfortran.h>
#include <hbook.h>
#include <cli.h>
#include <com.h>
#include <bnk.h>
#include <evs.h>
#include <asca_anl.h>
#include <anl_misc.h>

void evdump_init(int *);
void evdump_com(int *);
void evdump_his(int *);
void evdump_bgnrun(int *);
void evdump_ana(int, int, int *);
void evdump_endrun(int *);
void evdump_exit(int *);

void
evdump_init(int *status)
{
	EVSDEF("EVDUMP:BEGIN");
	EVSDEF("EVDUMP:ENTRY");
	EVSDEF("EVDUMP:OK");
	ANL_PUT_VERSION("EVDUMP", "version 2.0");
/*	BnkConnect("localhost");
	BnkExportAll();*/
	
	*status = ASCA_ANL_OK;
}

void
evdump_com(int *status)
{
	*status = ASCA_ANL_OK;
}

void
evdump_his(int *status)
{
	*status = ASCA_ANL_OK;
}

void
evdump_bgnrun(int *status)

{
	EVSSET("EVDUMP:BEGIN");
	
	*status = ASCA_ANL_OK;
}

void
evdump_ana_sis(int sensor, double ascatime)
{
	int x, y;
	int pha, phas[9];
	int rawx, rawy;
	int detx, dety;
	int ccdid, grade;
	int size;
    int i;
	
	printf("\nSIS%d\n", sensor);
	
	/* get event data */
	BNKGET("ANL:X",sizeof(x),size,&x);
	BNKGET("ANL:Y",sizeof(y),size,&y);
	BNKGET("ANL:RAWX",sizeof(rawx),size,&rawx);
	BNKGET("ANL:RAWY",sizeof(rawy),size,&rawy);
	BNKGET("ANL:DETX",sizeof(detx),size,&detx);
	BNKGET("ANL:DETY",sizeof(dety),size,&dety);
	BNKGET("SIS:CCDID",sizeof(ccdid),size,&ccdid);
	printf("ANL:X %d\n",x);
	printf("ANL:Y %d\n",y);
	printf("ANL:RAWX %d\n",rawx);
	printf("ANL:RAWY %d\n",rawy);
	printf("ANL:DETX %d\n",detx);
	printf("ANL:DETY %d\n",dety);
	printf("SIS:CCDID %d\n",ccdid);
	printf("ANL:TIME %lf\n",ascatime);
    if (EVS("SIS:BRIGHT mode")||EVS("SIS:BRIGHT2 mode")||EVS("SIS:FAST mode")){
		BNKGET("ANL:PHA",4,size,&pha);
	    BNKGET("SIS:GRADE",4,size,&grade);
	    printf("SIS:PHA %d\n",pha);
	    printf("SIS:GRADE %d\n",grade);
    } else if (EVS("SIS:FAINT mode")) {
	    BNKGET("SIS:PHAS",sizeof(phas),size,phas);
		for (i=0;i<9;i++) {
			printf("SIS:PHAS %d\n",phas[i]);
	    }
    }
	return;
}

void
evdump_ana_gis(int sensor, double ascatime)
{
	int i;
	int x, y;
	int pha, pi;
	int phas[256];
	int rawx, rawy;
	int detx, dety;
	int rt, rti;
	int size;
	
	printf("\nGIS%d\n", sensor);
	
	/* get event data */
	BNKGET("ANL:X",sizeof(x),size,&x);
	BNKGET("ANL:Y",sizeof(y),size,&y);
	printf("ANL:X %d\n",x);
	printf("ANL:Y %d\n",y);
	printf("ANL:TIME %lf\n",ascatime);
    if (EVS("GIS:PH mode")) {
		BNKGET("ANL:PHA",sizeof(pha),size,&pha);
		BNKGET("ANL:PI",sizeof(pi),size,&pi);
		BNKGET("ANL:RAWX",sizeof(rawx),size,&rawx);
		BNKGET("ANL:RAWY",sizeof(rawy),size,&rawy);
		BNKGET("ANL:DETX",sizeof(detx),size,&detx);
		BNKGET("ANL:DETY",sizeof(dety),size,&dety);
		BNKGET("GIS:RISE_TIME",sizeof(rt),size,&rt);
		BNKGET("GIS:RTI",sizeof(rti),size,&rti);
		printf("ANL:PHA %d\n",pha);
		printf("ANL:PI %d\n",pi);
		printf("ANL:RAWX %d\n",rawx);
		printf("ANL:RAWY %d\n",rawy);
		printf("ANL:DETX %d\n",detx);
		printf("ANL:DETY %d\n",dety);
		printf("GIS:RISE_TIME %d\n",rt);
		printf("GIS:RTI %d\n",rti);
	} else if (EVS("GIS:MPC mode")) {
		BNKGET("GIS:PHAS",sizeof(phas), size, phas);
		printf("GIS:PHAS(%d)", size/4);
		for (i = 0; i < size/sizeof(*phas); i++) {
			printf(" %d", phas[i]);
		}
		printf("\n");
	}
	return;
}

void
evdump_ana(int nevent, int eventid, int *status)
{
	int size, sensor;
	double ascatime;
	
	EVSSET("EVDUMP:ENTRY");
	
	BNKGET("ANL:SENSOR", sizeof(sensor), size, &sensor);
	BNKGET("ANL:TIME", sizeof(ascatime), size, &ascatime);
	
	switch (sensor) {
	case SENSOR_PSEUDO:
		printf("\n PSEUDO \n ANL:TIME %lf\n", ascatime);
		break;
	case 0: case 1:
		evdump_ana_sis(sensor, ascatime);
		break;
	case 2: case 3:
		evdump_ana_gis(sensor, ascatime);
		break;
	default:
		;
	}
	
	EVSSET("EVDUMP:OK");
	
	*status = ASCA_ANL_OK;
}

void
evdump_endrun(int *status)
{
	*status = ASCA_ANL_OK;
}

void
evdump_exit(int *status)
{
	*status = ASCA_ANL_OK;
}

/*-------------------
   C wrapper for evdump
-------------------*/

FCALLSCSUB1(evdump_init,EVDUMP_INIT,evdump_init,PINT)
FCALLSCSUB1(evdump_com,EVDUMP_COM,evdump_com,PINT)
FCALLSCSUB1(evdump_his,EVDUMP_HIS,evdump_his,PINT)
FCALLSCSUB1(evdump_bgnrun,EVDUMP_BGNRUN,evdump_bgnrun,PINT)
FCALLSCSUB3(evdump_ana,EVDUMP_ANA,evdump_ana,INT,INT,PINT)
FCALLSCSUB1(evdump_endrun,EVDUMP_ENDRUN,evdump_endrun,PINT)
FCALLSCSUB1(evdump_exit,EVDUMP_EXIT,evdump_exit,PINT)
