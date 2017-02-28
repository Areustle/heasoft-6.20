/* readinput.c */

#include "like.h"


void readinput(float *ranal, int *clobber, int *status)
{
   int    BufLen_2=FILESZ+1;
   int    i;

   char   phase[20];
   char   mapfile[80]; 
   char   msg[80];
   char   field[30];


   typedef struct
   {
      int    lu[100];
      int    year, month, day;
      int    cal_curve_index, cal_tasc_coinc;
      float  pi180;
      char   signal;
      char   sigmsg[FILESZ];
      char   g_device[10];
      char   mapfile[80], ctlfile[80], cmapfile[80];
      char   emapfile[80], gmapfile[80], bmapfile[80];
      char   mapfile_e[80], mapfile_c[80], lmapfile[80];
      char   gbiasfile[80], newmapfile[80];
      char   psdfile[80], edpfile[80], sarfile[80];
      char   cal_bin_dir[80], cmapfile_f[80];
      char   prg[6], ver[8];
      char   tmploc[20];
      char   coord_sys;
      char   spr_chr[2];
      char   fullmap, publish, initial, verbose, report, calc_uncert;
      char   debug, gmap_e_dif, gmap_null, gmap_conv, report2, spectral;
      char   spare1, spare2, spare3, top1;
      int    clobber, jout_p_type, jae_sr_type, jae_2nd_calib;
   }  CNFREP_DEF;
#define CNFREP COMMON_BLOCK(CNFREP,cnfrep)
COMMON_BLOCK_DEF(CNFREP_DEF,CNFREP);

   typedef struct
   {
      char   pwd1[80];
      char   misc_dir[80];
      char   egret_doc[100];
      char   data_dir[80];
      char   calib_dir[80];
      char   browser[50];
      char   info_cmd[150];
      char   browser_flg, browser_on, browser_back;
   }  ENVIRN_DEF;
#define ENVIRN COMMON_BLOCK(ENVIRN,envirn)
COMMON_BLOCK_DEF(ENVIRN_DEF,ENVIRN);


   typedef struct
   {
      char  keepfitsdir[150];
      char  phase[10][150];
   }  VARJ14_DEF;
#define VARJ14 COMMON_BLOCK(VARJ14,varj14)
COMMON_BLOCK_DEF(VARJ14_DEF,VARJ14);


   strcpy(field, "tempfildir");
   Uclgst(field, CNFREP.cal_bin_dir, status);
   if (*status != 0)
      strcpy(msg, "Error while reading tempfildir.");

   if (*status == 0)
   {
      strcpy(field, "datadir");
      Uclgst(field, ENVIRN.data_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading datadir.");
   }

   if (*status == 0)
   {
      strcpy(field, "calibdir");
      Uclgst(field, ENVIRN.calib_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading calibdir.");
   }

   if (*status == 0)
   {
      strcpy(field, "miscdir");
      Uclgst(field, ENVIRN.misc_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading miscdir.");
   }

   if (*status == 0)
   {
      strcpy(field, "cmapfile");
      Uclgst(field, CNFREP.cmapfile, status);
      if (*status != 0)
      {
         strcpy(msg, "Error while reading cmapfile.");
      }

      else
      {
	 printf("data_dir: %s \n", ENVIRN.data_dir);
         strcat(strcpy(CNFREP.cmapfile_f, ENVIRN.data_dir), CNFREP.cmapfile);
	 printf("mapfile: %s \n", CNFREP.cmapfile_f);
	 strcpy(strchr(CNFREP.cmapfile_f, '\0'), ".fits");
	 /* strcpy(strstr(CNFREP.cmapfile_f, '\0'), ".fits"); */
	 printf("mapfile: %s \n", CNFREP.cmapfile_f);
      }
   }

   if (*status == 0)
   {
      strcpy(field, "emapfile");
      Uclgst(field, mapfile, status);
      if (*status != 0)
      {
         strcpy(msg, "Error while reading emapfile.");
      }

      else
      {
         strcat(strcpy(CNFREP.emapfile, ENVIRN.data_dir), mapfile);
	 /* strcpy(strstr(CNFREP.emapfile, "\0"), ".fits"); */
	 strcpy(strchr(CNFREP.emapfile, '\0'), ".fits");
      }
   }

   if (*status == 0)
   {
      /*strcpy(field, "gmapfile");
      Uclgst(field, mapfile, status);
      if (*status != 0)
         strcpy(msg, "Error while reading gmapfile.");

	 else */
      strcpy(mapfile, "standard");
         strcat(strcpy(CNFREP.gmapfile, ENVIRN.data_dir), mapfile);
      /* strcpy(index(CNFREP.gmapfile, '\0'), ".fits"); */
   }

   if (*status == 0)
   {
      strcpy(field, "lmapfile");
      Uclgst(field, CNFREP.lmapfile, status);
      if (*status != 0)
         strcpy(msg, "Error while reading lmapfile.");
   }

   if (*status == 0)
   {
      strcpy(field, "gbiasfile");
      Uclgst(field, CNFREP.gbiasfile, status);
      if (*status != 0)
         strcpy(msg, "Error while reading gbiasfile.");
   }

   if (*status == 0)
   {
      strcpy(field, "egretdoc");
      Uclgst(field, ENVIRN.egret_doc, status);
      if (*status != 0)
         strcpy(msg, "Error while reading egretdoc.");
   }

   if (*status == 0)
   {
      for (i=0; i<NPHASE; ++i)
      {
	 sprintf(phase, "phase%d", i);
         Uclgst(phase, VARJ14.phase[i], status);
         if (*status != 0)
            strcpy(msg, "Error while reading phase keyword.");
      }
   }

   if (*status == 0)
   {
      strcpy(field, "ranal");
      Uclgsr(field, ranal, status);
      if (*status != 0)
         strcpy(msg, "Error while reading ranal.");
   }

   if (*status == 0)
   {
      strcpy(field, "clobber");
      Uclgsi(field, clobber, status);
      if (*status != 0)
         strcpy(msg, "Error while reading clobber flag.");
   }


   if (*status != 0)
      Fcerr(msg);
}



FCALLSCSUB3(readinput, READINPUT, read_input, PFLOAT, PINT, PINT)

