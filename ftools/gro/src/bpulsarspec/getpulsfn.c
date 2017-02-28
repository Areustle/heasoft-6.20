/* Copyright (C) 1999 Lucent Technologies */
/* Excerpted from 'The Practice of Programming' */
/* by Brian W. Kernighan and Rob Pike */

#include <bpulsarspec.h>

 void getpulsfn(char *infname, char *data_dir, char *bpulsfn)
{
	int ii, BufLen_2 = 50;
        char bpulsfil[51];
        int status = 0;
        char string1[100],msg[50]="";

     /* get name of director */
	strcpy(msg,"data_dir");
        Uclgst(msg, data_dir, &status);
        if (status != 0)
          {
           printf ("Problem getting name of data directory!\n");
           exit (1);
          }
  
      /* read in file name*/
        status = 0;
        strcpy(msg,"bpulsfil");
        Uclgst(msg, bpulsfil, &status);     
         if (status != 0)
        {
          printf ("Problem getting name of BATSE occultation data file!\n");
          exit (1);
          }
     
      for (ii = 0; ii<100;ii++)string1[ii]='\0'; 
      strcat(strcat(string1,data_dir),"/");
      strcat(string1,bpulsfil);
      strcpy(infname,string1);
      strcpy(bpulsfn,bpulsfil);
      return;

}


