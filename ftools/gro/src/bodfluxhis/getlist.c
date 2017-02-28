/* Copyright (C) 1999 Lucent Technologies */
/* Excerpted from 'The Practice of Programming' */
/* by Brian W. Kernighan and Rob Pike */

#include <stdio.h>
#include <ctype.h>
#include <bodfluxhis.h>
enum { MINLEN = 6 };

/* strings main: find printable strings in files */
 void getlist(char *listfn[], int *nfile, char *data_dir)
{
	int ii,i,c,j=0, BufLen_2 = 50, len=0;
	FILE *fin;
        char buf[50],*fname,*string3;
        char bodfil[51];
        int status = 0;
        char string[]="mkdir ./";
        char string1[100],string2[100],string4[100];
        char *tmpfn, msg[50]="";

     /* get name of director */
     strcpy(msg,"data_dir");
     Uclgst(msg, data_dir, &status);
     if (status != 0)
        {
          printf ("Problem getting name of data director!");
          exit (1);
          }

     /* make new directory to hold data file */
      strcat(string,data_dir);
      /*system(string);*/
  
      /* read in file name*/
        status = 0;
        strcpy(msg,"bodfil");
        Uclgst(msg, bodfil, &status);     
         if (status != 0)
        {
          printf ("Problem getting name of BATSE occultation data file!");
          exit (1);
          }

   if (strncmp(bodfil,"@",1) != 0)
    {
      listfn[0] = malloc(200);
      for (ii = 0; ii<100;ii++)string1[ii]='\0'; 
      strcat(strcat(string1,data_dir),"/");
      strcat(string1,bodfil);
      strcpy(listfn[0],string1);
      *nfile = 1;
      return;
    }
     else
     {
       len = strlen(bodfil);
       string3 = (char*)malloc(len);
       for (ii = 0; ii<len-1;ii++)string3[ii]='\0';
       for (ii = 0; ii<len-1;ii++)
       string3[ii]=bodfil[ii+1];
       string3[len-1] = '\0';
       for (ii = 0; ii<100;ii++)string2[ii]='\0'; 
       strcat(strcat(string2,data_dir),"/");
       strcat(string2,string3); 
       
	if ((fin = fopen(string2, "rb")) == NULL)
	  {
	   printf("can't open %s:", fname);
           exit(1);
	  }
	else {
           
          for (ii = 0; ii<100;ii++)string4[ii]='\0';
          for (ii = 0; ii<49;ii++)buf[ii]='\0';    
          strcat(strcat(string4,data_dir),"/");
	  do {	/* once for each string */
	       for (i = 0; (c = getc(fin)) != EOF; ) 
                 {
		  if (!isprint(c))
		    break;
		  buf[i++] = c;
		  if (i >= 50)
		    break;
		 }
		 if (i >= MINLEN) /* print if long enough */
		   {
                        tmpfn = (char*) malloc(51);
                        for (ii = 0; ii<51;ii++)tmpfn[ii]='\0';  
                        strcat(strcpy(tmpfn,string4),buf);
                        listfn[j] = (char*) malloc(200);
                        strcpy(listfn[j],tmpfn); 
                         j++;
		   }
	     }while (c != EOF);
           
				fclose(fin);
				*nfile = j;
	}
		
     }
     if (*nfile > 10) 
       {
         printf("file number should less than 10!");
         exit(1);
       }

}


