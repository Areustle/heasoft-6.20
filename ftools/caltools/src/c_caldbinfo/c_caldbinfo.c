# include <string.h>
# include <stdlib.h>
# include <stdio.h>
# include <ctype.h>
# include <time.h>
# include <stddef.h>


/*  Description : This subroutine checks that the CALDB is accessible to the user. *

    Passed parameters : See the FORTRAN version $FTOOLS/hip/callib/src/gen/caldbinfo.f. 
                        All parameters are the same as the Fortran version.                        

    Origin : original C-version

    Authors/modification History:
      Lorraine Breedon (1.0.0:98 Jan 05) Original Version */
    
# include "fitsio.h"
# include "xpi.h"
# include "ftools.h"
# include "ftoolstruct.h"
# include "cfitsio.h"
# include "cfortran.h"
# include "pfile.h"
# include "ctype.h"


#define MAX_FNAME        160             /* max length file name */

#define BufLen_2 ((unsigned)(MAX_FNAME-1))    /* Cf. Uclgst macro */

void c_gcaldbinfo2(int chatter2, char infmode2[80], char missn2[80], char instr2[80], char *ciff, int *status);
void c_rdcnfg2(int chatter2, char missn2[80], char instr2[80], char instr3[80], char cnfgpth[160], char *ciff, int *status);

int c_calo()
{   
   int errstat=0, status=0;
   char *sptr="C_CALDBINFO";
   char *vptr="1.0.0";
   char taskname[40];
   char infmode[10], missn[10],  instr[10];
   char output[160], *ciff;
   char infmode2[80], missn2[80],instr2[80];
   int chatter, chatter2;

/* Give user info */
   puts(" ");
   strcpy(output,""); 
   strcat(output,"** Using ");
   strcat(output,sptr);
   strcat(output," version ");
   strcat(output,vptr);
   strcat(output," **");
   printf("%s\n",output);
   puts(" ");

       
   /*cfortran call to initialize the TASK common block used by Fcerr*/
    C2FCBSTR("c_caldbinfo",TASK.taskname,0);

   
   /*Open the par file and read any command line arguments
    OpenDefaultPF(argc, argv); */

/* get chattiness flag */

        Uclgsi("chatter",&chatter,&errstat);
        if(errstat) {
           Fcerr("Problem getting CHATTER parameter");
           return(errstat);
        }  
          
        chatter2=chatter;
/* get infomode parameter */
        Uclgst("infomode",infmode,&errstat);
        if(errstat) {
           errstat=0;
           puts("Problem getting INFOMODE parameter"); 
           puts("setting MODE = BASIC");
           strcpy(infmode,"BASIC");
           strcpy(infmode2,infmode);

        } else {
/* convert infomode parameter to upper case  */
           (void)ffupch(infmode);
           strcpy(infmode2,infmode);

        } 

/* get mission and instrument parameters if requested */
        if (strcmp(infmode2,"INST") == 0) {
            Uclgst("mission",missn,&errstat);
            if(errstat) {
               Fcerr("Problem getting MISSION parameter");
               return(errstat);
            }
            (void)ffupch(missn);
             strcpy(missn2,missn);
        
            Uclgst("instrument",instr,&errstat);
            if(errstat) {
               Fcerr("Problem getting INSTRUMENT parameter");
               return(errstat); 
            }
            (void)ffupch(instr);
            strcpy(instr2,instr);

         } else {


            
            if (strcmp(infmode2,"BASIC") != 0) {
               puts("..Invalid INFMODE entered !!....please try again");
               errstat=1;
               goto END;
            }
        }

    /* CloseDefaultPF(); */


/* get all CALDB environment stuff & read the caldb.config file to obtain
location of appropriate caldb.indx file */
      
       errstat=0;

       c_gcaldbinfo2(chatter2,infmode2,missn2,instr2,&ciff,&errstat);
       if (!errstat) {
            printf("\n ");
            printf("\n..LOCAL CALDB APPEARS TO BE SET UP AND ACCESSIBLE"); 
            puts(" ");      
       } 
       END: printf(" \n");
       strcpy(output,""); 
       strcat(output,"** ");
       strcat(output,sptr);
       strcat(output," version ");
       strcat(output,vptr);
       if (errstat) {
           strcat(output," FAILED\n");
       } else {
           strcat(output," finished **\n");
       }
       printf(output);
       exit(0);
}




void c_gcaldbinfo2(chatter2,infmode2,missn2,instr2,ciff,status)
int *status,chatter2;
char missn2[80],instr2[80],*ciff, infmode2[80];
{
   char *configptr="CALDBCONFIG";
   char *caldbptr="CALDB";
   char *aliasptr="CALDBALIAS";
   int caldblen=0, cnflen=0, aliaslen=0, test=0, errstat=0;
   char cnfgpth[120]=" ", caldbpth[120]=" ", aliaspth[160]=" ";
   char instr3[80];
   int alias_flag;
   char version[]="1.0.0";



/* ENV variable stuff for CALDB...*/
        strcpy(caldbpth,getenv(caldbptr)); 
        caldblen=strlen(caldbpth);
        if (caldblen == 0) {
            puts(" ");
            printf("ERROR - c_gcaldbinfo2 version %s\n", version);
            puts("CALDB environment variable not set");
            puts("...the environ-var/logical CALDB must be set");
            puts("...to point to the top of the CALDB directory structure.");
            puts("...See the Caldb Installation Guide (CAL/GEN/94-004) for");
            puts("...details.");
            *status=1;
            return;
            
         } else {
            if (chatter2 >= 20) {
                puts(" "); 
                printf("c_gcaldbinfo2 version %s\n", version);
                puts("...environ-var/logical CALDB defined");
                printf("...CALDB-path=%s\n",caldbpth);
            }
         }

/* ENV variable stuff for CALDBCONFIG... */
         strcpy(cnfgpth,getenv(configptr)); 
         cnflen=strlen(cnfgpth); 

        if (cnflen == 0) {
            puts(" ");
            printf("ERROR - c_gcaldbinfo2 version %s\n", version);
            puts("CALDBCONFIG environment variable not set");
            puts("...the environ-var/logical CALDBCONFIG must be set");
            puts("...to point to your local Caldb configuration file.");
            puts("...See the Caldb Installation Guide (CAL/GEN/94-004) for");
            puts("...details.");
            *status=1;
            return;

         
        } else { 
           if (chatter2 >= 20) { 
               puts(" ");
               printf("c_gcaldbinfo2 version %s\n", version);
               puts("...environ-var/logical CALDBCONFIG defined");
               printf("...CALDBCONFIG file=%s\n",cnfgpth); 
           }
        } 

/*  ENV variable stuff for ALIASVAR.. */
         strcpy(aliaspth,getenv(aliasptr)); 
         aliaslen=strlen(aliaspth); 

        if (aliaslen == 0) {
            puts(" ");
            printf("ERROR - c_gcaldbinfo2 version %s\n", version);
            puts("CALDBALIAS environment variable not set");
            puts("...the environ-var/logical CALDBALIAS must be set");
            puts("...to point to your local Caldb alias file.");
            puts("...See the Caldb Installation Guide (CAL/GEN/94-004) for");
            puts("...details.");
            *status=1;
            return;

        } else { 
            if (chatter2 >= 20) {
               puts(" ");
               printf("c_gcaldbinfo2 version %s\n", version);
               puts("...environ-var/logical CALDBALIAS defined");
               printf("...CALDBALIAS file=%s\n",aliaspth); 
            }
        } 


/* Now perform mode-specific checks ..*/
       alias_flag=0;   
       errstat=0; 
       if (strcmp(infmode2,"INST")==0) {
/* sort out any instrument aliases */
          if (strcmp(instr2,"XRT") == 0) {
             strcpy(instr3,"XRT1");
             alias_flag=1;
          }
          if (strcmp(instr2,"SIS") == 0) {
             strcpy(instr3,"SIS0");
             alias_flag=1;
          }
          if (strcmp(instr2,"GIS") == 0) {
             strcpy(instr3,"GIS2");
             alias_flag=1;
          }
          if (strcmp(instr2,"PSPC") == 0) {
             strcpy(instr3,"PSPCB");
             alias_flag=1;
          } 
          if (alias_flag==0) {
             strcpy(instr3,instr2);
          }
          c_rdcnfg2(chatter2,missn2,instr2,instr3,cnfgpth,&ciff,&errstat); 
          if (errstat != 0 ) {
              if (errstat > 1) {
                 printf(" \n");
                 printf("ERROR - c_gcaldbinfo2 version %s", version);
                 printf("\n...CALDB NOT correctly configured for the instrument %s,"
                     "\n...onboard the mission %s",instr2,missn2);
                 puts(" ");
              }
              *status=2;
              return;
          }
 
             
        }
}


void c_rdcnfg2(chatter2,missn2,instr2,instr3,cnfgpth,ciff,status)
int *status, chatter2;
char missn2[80],instr2[80],instr3[80],cnfgpth[160], *ciff;
{
    FILE *fptr;   /* file pointer */
    int i=0;
    char letter[160], line[160], word[160], misval[160], instval[160];
    char cifdev[160], cif[160], cifdir[160], calpath[160], output[160];
    int index=0;
    int token;
    int location_flag=0;
    char version[]="1.0.0";
   
   
/* open the caldb.config file */
    if ((fptr=fopen(cnfgpth,"r")) ==NULL ) {
       puts(" ");
       printf("ERROR - c_rdcnfg2 version %s", version);
       printf("\n .....Problem opening CALDBCONFIG file %s",cnfgpth);
       *status=1;
       return;
    } else {
       if (chatter2 >= 20) {
          puts(" ");
          printf("c_rdcnfg2 version %s", version);
          printf("\n .....Opened the CALDBCONFIG file %s\n",cnfgpth);
       }
    }

/* read the caldb.config file */
    
    for (i=0; i<=200; i++) {
        fgets(line,160,fptr);
        
        if (!(feof(fptr))) {
            sscanf(line,"%s",letter);
            
            if ((strcmp(letter,"#") != 0 ) && (strcmp(letter,"GEN") != 0 )) {
                                        token=0; 
                    index=0;
/* obtain the 1st 5 tokens in each line and assign each token to specific
   mission CALDB variables */
                    while (index < strlen(line)) { 
                           
                           sscanf(&line[index],"%s",word);
                           index+=strlen(word) + 1;
                           while (line[index] == ' ') {
                                 index++;
                                 
                           } 
                           token++;

                           if (token == 1) {
                              strcpy(misval,word);
                           }
                           if(token == 2) {
                                
                              strcpy(instval,word);
                           }
                           if(token == 3) {
                              strcpy(cifdev,word);
                               if (strcmp(cifdev,"CALDB") == 0) {
                                   strcpy(calpath,getenv(cifdev));
                               }     

			   }
                           if(token == 4) {
                              strcpy(cifdir,word);
                           }
                           if(token == 5) {
                              strcpy(cif,word);
                              goto LABEL;
                           }
   
                          
                  }  
                  LABEL: if ((strcmp(missn2,misval) == 0) &&       
                                  (strcmp(instr3,instval) == 0 )) {
 /* construct cif directory & then construct cif pathname in total */
                                 strcpy(output,"");
                                 strcat(output,calpath);
                                 strcat(output,"/");
                                 strcat(output,cifdir);
                                 strcat(output,"/");
                                 strcat(output,cif);
                                 strcpy(ciff,output);
                                 location_flag=1;
                                 if (chatter2 >= 20) {
                                    puts(" ");
                                    printf("c_rdcnfg2 version %s", version);
                                    printf("\n .....CAL data directory: %s",output);
                                    printf("\n .....CAL Index File: %s",output);
                                    puts(" ");
                                 }
                         } 
                  
             
            }
        }
    }
    /* if get to here and location_flag=0 then....*/
    if (location_flag==0) {
       puts(" ");
       printf("ERROR - c_rdcnfg2 version %s", version);
       printf("\n .....Unable to find a valid entry for mission %s,"
              "\n .....and instrument %s,"
              "\n .....in the Caldb configuration file %s",missn2,instr2,cnfgpth);
       printf("\n ");
       *status=2;
       fclose(fptr);
       return;
    } else {
      fclose(fptr);
    }
}





