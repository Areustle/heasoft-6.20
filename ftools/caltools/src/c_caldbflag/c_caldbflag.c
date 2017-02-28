# include <string.h>
# include <stdlib.h>
# include <stdio.h>
# include <ctype.h>
# include <time.h>
# include <stddef.h>


/*  Description : For each cal file (in an input ASCII list), switches all
                  required flags in the caldb.indx to a requested value
                  (provided file is listed in the caldb.indx file ofcourse).

    Passed parameters : 

    Origin : original C-version

    Authors/modification History:
      Lorraine Breedon (1.0.0:98 Feb 11) Original Version */
    
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
#define ARRSZ            500 

void c_gcaldbinfo(int chatter2, char infmode2[80], char missn2[80], char instr2[80], char ciff[160], int *status);
void c_rdcnfg(int chatter2, char missn2[80], char instr2[80], char instr3[80], char cnfgpth[160], char ciff[160], int *status);

int c_calg()
{   
   FILE *afptr;   /* ASCII file pointer */
   fitsfile *ffptr;   /* pointer to the cal FITS file */

   int errstat=0, status=0, i=0, nn=0, n=0;
   char vers[5];
   char *sptr="C_CALDBFLAG";
   char *vptr="1.0.0";
   char taskname[40];
   char infmode2[]="INST";
   char calexpr[160], missn[80],  instr[80], inpfile[160];
   char output[160], ciff[160];
   char calexpr2[80], missn2[80],instr2[80], inpfile2[160];
   char line[160], letter[160], compfnam[ARRSZ][80], calfile[80];
   int chatter, chatter2;
   int detflag,filtflag,clasflag,dtypflag,cnamflag,qualflag,descflag;
   int detflag2,filtflag2,clasflag2,dtypflag2,cnamflag2,qualflag2,
       descflag2;
   char detswit[20], filtswit[80], classwit[80], dtypswit[80],
        cnamswit[80], descswit[80];
   char detswit2[20],filtswit2[80], classwit2[80], dtypswit2[80],
        cnamswit2[80], descswit2[80];
   int nfiles, calen,br1pos=0,br2pos=0,lslash;
   int qualswit;
   long qualswit2=0L;
   char path[ARRSZ][80],filename[ARRSZ][80],fileandext[ARRSZ][80];
   char compfile[ARRSZ][160];


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

   strcpy(vers,"");
   strcat(vers,vptr);
       
   /*cfortran call to initialize the TASK common block used by Fcerr*/
    C2FCBSTR("c_caldbflag",TASK.taskname,0);

   
   /*Open the par file and read any command line arguments
    OpenDefaultPF(argc, argv); */

/* get chattiness flag */

        Uclgsi("chatter",&chatter,&errstat);
        if(errstat) {
           Fcerr("Problem getting CHATTER parameter");
           return(errstat);
        }  
          
        chatter2=chatter; 

/* get calexpr parameter */
        Uclgst("calfexp",calexpr,&errstat);
        if(errstat) {
           Fcerr("Problem getting CALFEXP parameter");
           return(errstat);
        }
        (void)ffupch(calexpr);
        strcpy(calexpr2,calexpr); 

 /* get mission and instrument parameters if CALDB requested */
        if ((strcmp(calexpr,"CALDB") == 0 ) ||
                (strcmp(calexpr2,"caldb") == 0)) {
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

         }


/* name of input ASCII file */
        inpfile[0]='\0';
        Uclgst("inpfile",inpfile,&errstat);
        if(errstat) {
           Fcerr("Problem getting INPFILE parameter");
           return(errstat);
        }
       
        strcpy(inpfile2,inpfile);


/* get the flag and switch parameters */

/* DETNAM parameter */
        Uclgsi("det_flag",&detflag,&errstat);
        if(errstat) {
           Fcerr("Problem getting DET_FLAG parameter");
           return(errstat);
        }  
        detflag2=detflag;
        if (detflag2 == 1) {
            Uclgst("det_swit",detswit,&errstat);
            if(errstat) {
               Fcerr("Problem getting DET_SWIT parameter");
               return(errstat); 
            }
            (void)ffupch(detswit);
            strcpy(detswit2,detswit);          
        }
/* FILTER parameter */

        Uclgsi("filt_flag",&filtflag,&errstat);
        if(errstat) {
           Fcerr("Problem getting FILT_FLAG parameter");
           return(errstat);
        }  
        filtflag2=filtflag;
        if (filtflag2 == 1) {
            Uclgst("filt_swit",filtswit,&errstat);
            if(errstat) {
               Fcerr("Problem getting FILT_SWIT parameter");
               return(errstat); 
            }
            (void)ffupch(filtswit);
            strcpy(filtswit2,filtswit);

        }
/* CAL_CLAS parameter */

       Uclgsi("clas_flag",&clasflag,&errstat);
        if(errstat) {
           Fcerr("Problem getting CCLAS_FLAG parameter");
           return(errstat);
        }  
        clasflag2=clasflag;
        if (clasflag2 == 1) {
            Uclgst("clas_swit",classwit,&errstat);
            if(errstat) {
               Fcerr("Problem getting CLAS_SWIT parameter");
               return(errstat); 
            }
            (void)ffupch(classwit);
            strcpy(classwit2,classwit);

        }
/* CAL_DTYP parameter */

       Uclgsi("dtyp_flag",&dtypflag,&errstat);
        if(errstat) {
           Fcerr("Problem getting DTYP_FLAG parameter");
           return(errstat);
        }  
        dtypflag2=dtypflag;
        if (dtypflag2 == 1) {
            Uclgst("dtyp_swit",dtypswit,&errstat);
            if(errstat) {
               Fcerr("Problem getting DTYP_SWIT parameter");
               return(errstat); 
            }
            (void)ffupch(dtypswit);
            strcpy(dtypswit2,dtypswit);

        }
/* CAL_CNAM parameter */

       Uclgsi("cnam_flag",&cnamflag,&errstat);
        if(errstat) {
           Fcerr("Problem getting CNAM_FLAG parameter");
           return(errstat);
        }  
        cnamflag2=cnamflag;
        if (cnamflag2 == 1) {
            Uclgst("cnam_swit",cnamswit,&errstat);
            if(errstat) {
               Fcerr("Problem getting CNAM_SWIT parameter");
               return(errstat); 
            }
            (void)ffupch(cnamswit);
            strcpy(cnamswit2,cnamswit);

        }
/* CAL_QUAL parameter */

       Uclgsi("qual_flag",&qualflag,&errstat);
        if(errstat) {
           Fcerr("Problem getting QUAL_FLAG parameter");
           return(errstat);
        }  
        qualflag2=qualflag;
        if (qualflag2 == 1) {
            Uclgsi("qual_swit",&qualswit,&errstat);
            if(errstat) {
               Fcerr("Problem getting QUAL_SWIT parameter");
               return(errstat); 
            }
            if ((qualswit != 0) && (qualswit != 5)) {
               errstat=1;
               Fcerr("ERROR: QUAL_FLAG parameter may only be 0 or 5");
               return(errstat);
            } else {
               qualswit2=(long)qualswit;
            }
        }
/* CAL_DESC parameter */

       Uclgsi("desc_flag",&descflag,&errstat);
        if(errstat) {
           Fcerr("Problem getting DESC_FLAG parameter");
           return(errstat);
        }
        descflag2=descflag;  
        if (descflag2 == 1) {
            Uclgst("desc_swit",descswit,&errstat);
            if(errstat) {
               Fcerr("Problem getting DESC_SWIT parameter");
               return(errstat); 
            }
            (void)ffupch(descswit);
            strcpy(descswit2,descswit);

        }

    /* CloseDefaultPF(); */
/* have to re-allocate vers variable after Uclgst calls ....an XPI bug!! */
            strcpy(vers,"");
            strcat(vers,vptr);



/* get all CALDB environment stuff & read the caldb.config file to obtain
location of appropriate caldb.indx file */
      
    errstat=0;
    c_gcaldbinfo(chatter2,infmode2,missn2,instr2,ciff,&errstat);
      if (errstat) {
           printf(" \n");
           printf("ERROR - C_CALDBFLAG version %s", vers);
           printf("\n Problem with your CALDB set-up ");
           goto END;
    }

/* open input ASCII file containing filenames whose CIF values need
to be switched */

    if ((afptr=fopen(inpfile2,"r")) ==NULL ) {
       puts(" ");
       Fcerr("Problem opening input ASCII file");
       return(errstat);
    }   

/* get cal files from ASCII file */
    nfiles=0;
    for (i=1; i<=ARRSZ; i++) {
        fgets(line,160,afptr);
        if (!(feof(afptr))) {
            sscanf(line,"%s",letter);
            if ((strcmp(letter,"#") != 0 ) && (strcmp(letter," ") != 0 )) {
                strcpy(compfnam[i],line);
                nfiles++;
            }
        }
     }
     fclose(afptr);

    if (nfiles==0) {
        Fcerr("input ASCII file empty");
        return(errstat);
    }
 

/* get cal file info from ASCII file */
    nn=0;
    
    for (i=1; i<=nfiles; i++) {
       strcpy(calfile,compfnam[i]);
       calen=strlen(calfile);
       for (n=0; n<=calen; n++) {
           if (calfile[n]=='[') {
               br1pos=n;
           }
           if (calfile[n]==']') {
 
               br2pos=n;
           }
       }
       lslash=0;
       for (n=1; n<br1pos; n++) {
           nn=br1pos-n;
                 if ((calfile[nn]=='/') && (lslash==0)) {
              
               lslash=nn;
           }
       }
  
/* determine path to file */
       for (n=0; n<lslash; n++) {
           path[i][n]=calfile[n];
       }

/* determine filename */
       nn=0;
       for (n=lslash+1; n<br1pos; n++) {
            nn=n-(lslash+1);
            filename[i][nn]=calfile[n];
       }
   
/* determine filename[#ext] */
       nn=0;
       for (n=lslash+1; n<=br2pos; n++) {
           nn=n-(lslash+1);
           fileandext[i][nn]=calfile[n];
       }
    }

/* check to see if files exist */
   for (i=1; i<=nfiles; i++) {
       strcat(compfile[i]," ");
       strcat(compfile[i],path[i]);
       strcat(compfile[i],"/");
       strcat(compfile[i],filename[i]);
    
       fits_open_file(&ffptr,compfile[i],READONLY,&status);
       if (status) {
          puts(" ");
          printf("WARNING: can't find file : %s\n", compfile[i]);
       }  
       fits_close_file(ffptr,&status);
   }
 
/* now do the business */
   c_chflags(chatter2,ciff,compfnam,fileandext,nfiles,
              detflag2,detswit2,filtflag2,filtswit2,clasflag2,classwit2,
             dtypflag2,dtypswit2,cnamflag2,cnamswit2,qualflag2,qualswit2,
             descflag2,descswit2,&errstat);
   if (errstat) {
           printf(" \n");
           printf("\n Problem switching the requested flags ...aborting ");
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

c_chflags(chatter2,ciff,compfnam,fileandext,nfiles,
              detflag2,detswit2,filtflag2,filtswit2,clasflag2,classwit2,
             dtypflag2,dtypswit2,cnamflag2,cnamswit2,qualflag2,qualswit2,
             descflag2,descswit2,errstat)
int chatter2,*errstat, nfiles, detflag2, filtflag2, clasflag2, 
     dtypflag2, cnamflag2, qualflag2, descflag2;
long qualswit2;
char detswit2[20],compfnam[ARRSZ][80],  
     fileandext[ARRSZ][80], filtswit2[80], ciff[160],
     classwit2[80], dtypswit2[80], cnamswit2[80], descswit2[80];
{
   

   fitsfile *fptr;   /* pointer to the FITS file (cif) */
   int i, j, nn, n;
   int status=0, start=0;
   char version[]="1.0.0";
   int hdutype, lslash, callen,nfound;
   long felem,longnull,nelem,qulval,extval,naxes[2];
   int dircol,filcol,extcol,detcol,filtcol,clascol,qualcol,anynull;
   char *filval[ARRSZ],*detnam[ARRSZ],*cal_clas[ARRSZ],*cal_dtyp[ARRSZ],
         *cal_cnam[ARRSZ],*cal_desc[ARRSZ],output[160],*filter[ARRSZ];
   char strnull[80]; 
   int exist[ARRSZ], desccol, dtypcol,cnamcol,len;
   char chext[80];
   double dblnull;
   char calif[80],calincif[80];
   
  
/* initialise arrays */
   for (i=0; i<ARRSZ; i++) {
        exist[i]=0;
        filval[i]=(char*)malloc(160*sizeof(char*));
        detnam[i]=(char*)malloc(20*sizeof(char*));
        filter[i]=(char*)malloc(80*sizeof(char*));
        cal_dtyp[i]=(char*)malloc(80*sizeof(char*));
        cal_cnam[i]=(char*)malloc(80*sizeof(char*));
        cal_clas[i]=(char*)malloc(80*sizeof(char*));
        cal_desc[i]=(char*)malloc(160*sizeof(char*));
   }
         
/* open the caldb.indx file */
   fits_open_file(&fptr,ciff,READWRITE,&status);
    if (status) {
       puts(" ");
       printf("ERROR - c_chflags version %s", version);
       printf("\n .....Unable to open file %s",ciff);
       status=2;
       return(status);
    } else {
       if (chatter2 >= 20) {
          puts(" ");
          printf("c_chflags version %s", version);
          printf("\n ..... OK ! opened the file %s\n",ciff);
          
       }
    }


/* move to the first extension == 2nd HDU */
    fits_movabs_hdu(fptr,2,&hdutype,&status);
    if (status) {
       puts(" ");
       printf("ERROR - c_chflags version %s", version);
       printf("\n .....Problem moving to 1st extn of file %s",ciff);
       return(status);
    }


/* find out how many rows the CIF contains */
    fits_read_keys_lng(fptr,"NAXIS",1,2,naxes,&nfound,&status);
    if (status) {
       puts(" ");
       printf("ERROR - c_chflags version %s", version);
       printf("\n .....Problem reading NAXIS2 of file %s",ciff);
       return(status);
    }

    if (naxes[1]==0) {
       puts(" ");
       printf("ERROR - c_chflags version %s", version);
       printf("\n ..... The following file appears to be empty ! %s",ciff);
       status=1;
       return(status);
    }

        
/* Get the column numbers of all the column numbers to be read */

   fits_get_colnum(fptr,CASESEN,"CAL_DIR",&dircol,&status);
    if (status) {
       puts(" ");
       printf("ERROR - c_chflags version %s", version);
       printf("\n .....Problem finding CAL_DIR column of file %s",ciff);
       return(status);
    }
    
    fits_get_colnum(fptr,CASESEN,"CAL_FILE",&filcol,&status);
    if (status) {
       puts(" ");
       printf("ERROR - c_chflags version %s", version);
       printf("\n .....Problem finding CAL_FILE column of file %s",ciff);
       return(status);
    }

    fits_get_colnum(fptr,CASESEN,"CAL_XNO",&extcol,&status);
    if (status) {
       puts(" ");
       printf("ERROR - c_chflags version %s", version);
       printf("\n .....Problem finding CAL_XNO column of file %s",ciff);
       return(status);
    }


    if (detflag2 == 1) {
       fits_get_colnum(fptr,CASESEN,"DETNAM",&detcol,&status);
       if (status) {
          puts(" ");
          printf("ERROR - c_chflags version %s", version);
          printf("\n .....Problem finding DETNAM column of file %s",ciff);
          return(status);
       }
    }
    
    if (filtflag2 == 1) {
       fits_get_colnum(fptr,CASESEN,"FILTER",&filtcol,&status);
       if (status) {
         puts(" ");
         printf("ERROR - c_chflags version %s", version);
         printf("\n .....Problem finding FILTER column of file %s",ciff);
         return(status);
       }
    }

    if (clasflag2 == 1) {
       fits_get_colnum(fptr,CASESEN,"CAL_CLAS",&clascol,&status);
       if (status) {
          puts(" ");
          printf("ERROR - c_chflags version %s", version);
          printf("\n .....Problem finding CAL_CLAS column of file %s",ciff);
          return(status);
       }
    }

    if (dtypflag2 == 1) {
       fits_get_colnum(fptr,CASESEN,"CAL_DTYP",&dtypcol,&status);
       if (status) {
          puts(" ");
          printf("ERROR - c_chflags version %s", version);
          printf("\n .....Problem finding CAL_DTYP column of file %s",ciff);
          return(status);
       }
    }

    
    if (cnamflag2 == 1) {
       fits_get_colnum(fptr,CASESEN,"CAL_CNAM",&cnamcol,&status);
       if (status) {
          puts(" ");
          printf("ERROR - c_chflags version %s", version);
          printf("\n .....Problem finding CAL_CNAM column of file %s",ciff);
          return(status);
       }
    }

    if (qualflag2 == 1) {
       fits_get_colnum(fptr,CASESEN,"CAL_QUAL",&qualcol,&status);
       if (status) {
          puts(" ");
          printf("ERROR - c_chflags version %s", version);
          printf("\n .....Problem finding CAL_QUAL column of file %s",ciff);
          return(status);
       }
    }

     
    if (descflag2 == 1) {
        fits_get_colnum(fptr,CASESEN,"CAL_DESC",&desccol,&status);
        if (status) {
           puts(" ");
           printf("ERROR - c_chflags version %s", version);
           printf("\n .....Problem finding CAL_DESC column of file %s",ciff);
           return(status);
        }
    }    
       
/* read each row and and assemble full pathname and ext number */

    nelem=1;
    felem=1;
    strcpy(strnull," ");
    dblnull=0.0;
    longnull=0;
    
    for (i=1; i<=(int)naxes[1]; i++) {

         fits_read_col_str(fptr,filcol,i,felem,nelem,
                           strnull,&filval[i],&anynull,&status); 

         if (status) {
            puts(" ");
            printf("ERROR - c_chflags version %s", version);
            printf("\n .....Problem getting CAL_FILE value in CIF %s",ciff);
            printf("\n .....Problematic row %d", i);
            return(status);
         }
           

         fits_read_col(fptr,TLONG,extcol,i,felem,nelem,
                           &longnull,&extval,&anynull,&status);
         if (status) {
            puts(" ");
            printf("ERROR - c_chflags version %s", version);
            printf("\n .....Problem getting CAL_XNO value in CIF %s",ciff);
            printf("\n .....Problematic row %d", i);
            return(status);
         } 

/* get calfilename in the CIF independent of path */
         lslash=0;
         len=strlen(filval[i]);
         for (n=0; n<=len; n++) {
             nn=len-n;
             if ((filval[i][nn]=='/') && (lslash==0)) {
                lslash=nn;
             }
         }
         nn=0;
         if (lslash ==0 ) {
             start=0;
         } else {
             start=lslash+1;
         }
         for (n=start; n<=len; n++) {
             nn=n-(start);
             calif[nn]=filval[i][n];
         }


/* concatenate [#ext] onto calfilename */
         sprintf(chext,"%ld",extval);
         strcpy(calincif,calif);
         strcat(calincif,"[");
         strcat(calincif,chext);    
         strcat(calincif,"]");       


            

/* Compare the cal filename in the CIF with those in the input ASCII file.
If filenames match, switch the appropriate flg to the required value */

         for (j=1; j<=nfiles; j++) {

              if (strcmp(calincif,fileandext[j]) == 0 ) {
                  exist[j]=1;

/* filename match, cal file in ASCII list obviously exists in CIF !! */
                 if (detflag2==1) {
                     
/* write new DETNAM value to CIF */

                     strcpy(detnam[i],detswit2);

                     fits_write_col(fptr,TSTRING,detcol,i,felem,nelem,
                           &detnam[i],&status);
                     if (status) {
                        puts(" ");
                        printf("ERROR - c_chflags version %s", version);
                        printf("\n .....Problem writing new DETNAM value in CIF %s",ciff);
                       printf("\n .....Problematic row %d", j);
                       return(status); 
                     } 
                 } 
                 if (filtflag2==1) {

/* write new FILTER value to CIF */

                     strcpy(filter[i],filtswit2); 
                     fits_write_col(fptr,TSTRING,filtcol,i,felem,nelem,
                           &filter[i],&status);
                     if (status) {
                        puts(" ");
                        printf("ERROR - c_chflags version %s", version);
                        printf("\n .....Problem writing new FILTER value in CIF %s",ciff);
                       printf("\n .....Problematic row %d", j);
                       return(status);
                     } 
                 }



                 if (clasflag2==1) {

/* write new CAL_CLAS value to CIF */
                     strcpy(cal_clas[i],classwit2);
                     fits_write_col(fptr,TSTRING,clascol,i,felem,nelem,
                           &cal_clas[i],&status);
                     if (status) {
                        puts(" ");
                        printf("ERROR - c_chflags version %s", version);
                        printf("\n .....Problem writing new CAL_CLAS value in CIF %s",ciff);
                       printf("\n .....Problematic row %d", j);
                       return(status);
                     }
                 }


                if (dtypflag2==1) {

/* write new CAL_DTYP value to CIF */
                     strcpy(cal_dtyp[i],dtypswit2);
                     fits_write_col(fptr,TSTRING,dtypcol,i,felem,nelem,
                           &cal_dtyp[i],&status);
                     if (status) {
                        puts(" ");
                        printf("ERROR - c_chflags version %s", version);
                        printf("\n .....Problem writing new CAL_DTYPE value in CIF %s",ciff);
                       printf("\n .....Problematic row %d", j);
                       return(status);
                     } 
                }

                if (cnamflag2==1) {

/* write new CNAM value to CIF */
                     strcpy(cal_cnam[i],cnamswit2);   
                     fits_write_col(fptr,TSTRING,cnamcol,i,felem,nelem,
                           &cal_cnam[i],&status);
                     if (status) {
                        puts(" ");
                        printf("ERROR - c_chflags version %s", version);
                        printf("\n .....Problem writing new CAL_CNAM value in CIF %s",ciff);
                       printf("\n .....Problematic row %d", j);
                       return(status);
                     } 
                }


                if (qualflag2==1) {

/*  write new CAL_QUAL value to CIF */
                     qulval=qualswit2;
                     fits_write_col(fptr,TLONG,qualcol,i,felem,nelem,
                           &qulval,&status);
                     if (status) {
                        puts(" ");
                        printf("ERROR - c_chflags version %s", version);
                        printf("\n .....Problem writing new CAL_QUAL value in CIF %s",ciff);
                       printf("\n .....Problematic row %d", j);
                       return(status);
                     } 
                }

                if (descflag2==1) {

/* write new CAL_DESC value to CIF */
                     strcpy(cal_desc[i],descswit2);
                     fits_write_col(fptr,TSTRING,desccol,i,felem,nelem,
                           &cal_desc[i],&status);
                     if (status) {
                        puts(" ");
                        printf("ERROR - c_chflags version %s", version);
                        printf("\n .....Problem writing new CAL_DESC value in CIF %s",ciff);
                       printf("\n .....Problematic row %d", j);
                       return(status);
                     } 
                 } 
              } 
         } 
    }
  

/* warn if cal filename in ASCII list does not exist in the CIF */

   for (i=1; i<=nfiles; i++) {
       if (exist[i] == 0) {
           puts(" ");
           printf("WARNING - c_chflags version %s", version);
           printf("\n ......Cal file in ASCII list: %s",compfnam[i]);
           printf("\n ......does not exist in the CIF !");
           puts(" ");
       }   
   }          

/* close the index file */

   fits_close_file(fptr,&status);
   if (status) {
            puts(" ");
            printf("ERROR - c_chflags version %s", version);
            Fcerr("Problem closing the CIF");
            return(status);
   }


}

void c_gcaldbinfo(chatter2,infmode2,missn2,instr2,ciff,status)
int *status,chatter2;
char missn2[80],instr2[80],ciff[160], infmode2[80];
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
            printf("ERROR - c_gcaldbinfo version %s\n", version);
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
                printf("c_gcaldbinfo version %s\n", version);
                puts("...environ-var/logical CALDB defined");
                printf("...CALDB-path=%s\n",caldbpth);
            }
         }

/* ENV variable stuff for CALDBCONFIG... */
         strcpy(cnfgpth,getenv(configptr)); 
         cnflen=strlen(cnfgpth); 

        if (cnflen == 0) {
            puts(" ");
            printf("ERROR - c_gcaldbinfo version %s\n", version);
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
               printf("c_gcaldbinfo version %s\n", version);
               puts("...environ-var/logical CALDBCONFIG defined");
               printf("...CALDBCONFIG file=%s\n",cnfgpth); 
           }
        } 

/*  ENV variable stuff for ALIASVAR.. */
         strcpy(aliaspth,getenv(aliasptr)); 
         aliaslen=strlen(aliaspth); 

        if (aliaslen == 0) {
            puts(" ");
            printf("ERROR - c_gcaldbinfo version %s\n", version);
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
               printf("c_gcaldbinfo version %s\n", version);
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
          c_rdcnfg(chatter2,missn2,instr2,instr3,cnfgpth,ciff,&errstat); 
          if (errstat != 0 ) {
              if (errstat > 1) {
                 printf(" \n");
                 printf("ERROR - c_gcaldbinfo version %s", version);
                 printf("\n...CALDB NOT correctly configured for the instrument %s,"
                     "\n...onboard the mission %s",instr2,missn2);
                 puts(" ");
              }
              *status=2;
              return;
          }
 
             
        }
}


void c_rdcnfg(chatter2,missn2,instr2,instr3,cnfgpth,ciff,status)
int *status, chatter2;
char missn2[80],instr2[80],instr3[80],cnfgpth[160], ciff[160];
{
    FILE *fptr;   /* file pointer */
    int i=0;
    char letter[160], line[160], word[160], misval[160], instval[160];
    char cifdev[160],  cifdir[160], calpath[160], output[160],cif[20];
    int index=0;
    int token;
    int location_flag=0;
    char version[]="1.0.0";
   
   
/* open the caldb.config file */
    if ((fptr=fopen(cnfgpth,"r")) ==NULL ) {
       puts(" ");
       printf("ERROR - c_rdcnfg version %s", version);
       printf("\n .....Problem opening CALDBCONFIG file %s",cnfgpth);
       *status=1;
       return;
    } else {
       if (chatter2 >= 20) {
          puts(" ");
          printf("c_rdcnfg version %s", version);
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
                                    printf("c_rdcnfg version %s", version);
                                    printf("\n .....CAL data directory: %s",ciff);
                                    printf("\n .....CAL Index File: %s",ciff);
                                    puts(" ");
                                 }
                         } 
                  
             
            }
        }
    }
    /* if get to here and location_flag=0 then....*/
    if (location_flag==0) {
       puts(" ");
       printf("ERROR - c_rdcnfg version %s", version);
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

 





                     

                         
                                       





