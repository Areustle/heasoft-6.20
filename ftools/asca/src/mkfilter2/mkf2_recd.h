/*  type definition of struct MkfRECD
 *  ver 1.0  Mar. 18, 1994  by T.Takeshima
 *  ver 1.1 Feb 10, 1998 by Jeff Guerber, RSTX/GSFC.  Added GtiList struct.
 *  ver 1.2 1998-07-15 by Jeff Guerber.  Incr. MkfRECD.start from 16 to 24
 *     to accomodate new date/time format.
 */

#include    "atFunctions.h"

typedef struct{
    double           ascatime;            /* Ascatime at the middle of bin */
    char             start[24];           /* Start time of record */
    float            width;               /* binwidth in second */
    int              ETI;                 /* Extended TI counter */
    unsigned char    Bit_Rate;            /* DP Bit Rate H/M/L F15 W32 B0-2*/
    unsigned char    ACS;                 /* ACS Status  0:fine mode, */
					  /* 1:corse mode F32n+4 W19 B0 */
    float            NSAS;                /* NSAS cone angle (deg) */

    float            Euler[3];            /* R.A and Dec. of Z-axis dirction */
					  /* third Euler angle (deg) */
    AtPolarVect      Sat_Pos;             /* Satellite position */

    float            elv;                 /* target elevation from */
					  /* Earth regde at ascatime */
    float            COR;                 /* Cut Off Rigidity at ascatime */
    unsigned char    FOV;                 /* Field of Veiw: 0:Sky/ */
					  /* 1:Dark Earth, 2:Bright Earth */
    float            Bright_Earth;        /* Angle from Bright Earth (deg) */
    unsigned char    SAA;                 /* Passage of South Atlantic */
                                          /* Anormaly  1:yes 0:no */
    float            T_SAA;               /* Time after SAA passage (sec) */
    unsigned char    SUNSHINE;            /* Satellite day or night */
    float            T_DY_NT;             /* Time after day/night tran (sec) */

    unsigned char    SIS_obs_mode[2];     /* SIS observation mode */
					  /* 0:Faint, 1:Bright, 2:fast */
					  /* 3:Frame, 4:DarkFrame */
                                          /*  5:Histgram, 6:Integration */
    int              SIS_ID[2];           /* SIS CCD ID LIST */
    unsigned char    SIS_dscr[2];         /* SIS discriminator */
					  /* Address Discri Ena +2, */
					  /* Level   Discri Ena +1 */
    unsigned char    SIS_add_dscr;        /* 0:in, 1:out */
    unsigned char    SIS_Grade[2];        /* SIS grade discri value */
					  /* for Bright/Faint Mode */
    int              SIS_Event_thr[8];    /* SIS event threshold for */
					  /* each chip */
    int              SIS_Split_thr[8];    /* SIS event threshold for */
					  /* each chip */
    unsigned char    SIS_AE[2];           /* SIS AE information for S0 */
					  /* and S1.  0:Observe/ */
					  /* 1:A-off/ 2:Power off */
    float            SIS_temp[2];         /* SIS CCD temperature */
    float            SIS_event_No[8];     /* SIS HK: Event number (chip) */
    float            SIS_pixel_No[8];     /* SIS HK: Pixel number over */
					  /* the threshold (chip) */
    float            SIS_tlm_event[8];    /* SIS TLM event number (chip) */
    unsigned char    SIS_satf[8];         /* SIS TLM saturation flag (chip) */
                                          /* 0: unsaturated; 1: saturated */

    unsigned char    GIS_obs_mode;        /* GIS observation mode, */
					  /* 0:PH/ 1:MPC/ 2:PCAL */
    unsigned char    GIS_HVL[2];          /* GIS_HV HVL2, HVL3  status */
					  /* 16:off/ 8:reduction/ */
					  /* 0-7:level */
    unsigned char    GIS_HVH[2];          /* GIS_HV HVH2, HVH3  status */
					  /* 16:off/ 8:reduction/ 0-7:level */
    float            GIS_LDHIT[2];        /* GIS Lower Discri Hit */
					  /* counting rate */
    float            GIS_H0[2];           /* GIS HK monitor H0 */
    float            GIS_H1[2];           /* GIS HK monitor H1 */
    float            GIS_H2[2];           /* GIS HK monitor H2 */
    float            GIS_L0[2];           /* GIS HK monitor L0 */
    float            GIS_L1[2];           /* GIS HK monitor L1 */
    float            GIS_L2[2];           /* GIS HK monitor L2 */
    float            GIS_CPU_in[2];       /* GIS HK CPU_in count */
    float            GIS_CPU_out[2];      /* GIS HK CPU_out count */
    float            GIS_tlm_event[2];    /* GIS telemetry event count */
    unsigned char    GIS_CPU_status[2];   /* GIS CPU status,  run +4, */
					  /* stop +2, error +1 */
    unsigned char    GIS_Ham_Err;         /* GIS Hamming Error */
					  /* 0:off, 1:on */
    float            GIS_temp[3];         /* GIS S2, S3, RBM temperature */

    float            GIS_HVH_monitor[2];  /* GIS S2, S3 HV-H monitor */
    float            GIS_HVL_monitor[2];  /* GIS S2, S3 HV-L monitor */
    float            GIS_HVHCM[2];        /* GIS S2, S3 HV-H current monitor */

    unsigned char    RBM_flg[2];          /* RBM flag status  former */
					  /* GIS, latter SIS 0:off  1:on */
    float            RBM_count;           /* RBM counting rate */
    float            ANG_DIST;            /* Angular Distance of FOV */
} MkfRECD;

double dp10DyeElv( );

typedef struct {     /* struct for GTI lists: */
  int     ngti;      /*   number of elements in list */
  double *start;     /*   malloced array of start times */
  double *stop;      /*   malloced array of stop times */
} GtiList;
