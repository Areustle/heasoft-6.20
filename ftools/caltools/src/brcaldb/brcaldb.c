#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

/* int MAIN_; /* work around SunOS 4.1.3 bug

stuff moved to hbrcaldb.c (MJT 16Jul97)

#include <stdio.h> */
#include <string.h>

/*These include files define internal structures for program*/
#include "optr.h"       /*needed by y.tab.h*/
#include "y_tab.h"      /*contains col ID's*/
#include "cifdata.h"    /*cif data structure*/
#include "colm.h"       /*structure of column nodes*/
#include "cnfgdata.h"   /*structure of config file data*/
#include "cifnode.h"    /*structure of ciflist*/
#include "alscfgnode.h" /*structure of alias_config file data*/
#include "selrowsnode.h" /*structure of selrows lists*/

#include "xpi.h"
#include "ftools.h"
#include "ftoolstruct.h"
#include "cfitsio.h"

/*Declare pointers to cifdata and selected rows list*/
Cifdata *cifdata;
int *selrows;
int sren;        /*selected row list index variable*/

/*These variables are used by the lexer and parser code*/
#define EXPRSIZE 1000
char expr[EXPRSIZE];
char *exprptr, *exprlim;
char *colmptr, *colmlim;
int read_string;
int parse_error;

/*flags for cl params*/
int parse_disp_cols = 1;
int cols_on_cl = 0;
int notgotwrap = 0;

int BufLen_1, BufLen_2, BufLen_3, BufLen_4, BufLen_5;
int BufLen_6, BufLen_7, BufLen_8, BufLen_9, BufLen_10;

void mkciflist(char *milist, Cnfgnode *cnfghead, Alscfgnode *alscfghead, Cifnode **cifhead, int *status);
void mkcnfglist(Cnfgnode **cnfghead, int *status);
void cifselector(int nrows, int newrows[], int *nnewrows, int *status);
void show_cmd(Colmnode *colhead, int nranges, int rowrange1[15], int rowrange2[15], int wrap);

int brcalb()
{
 
    int  nrows=0, i;
    int  errstat=0, status=0;
 
    char milist[1000];
    char contxt[160];

    char rows[80];
    int  nrange,numranges,rowrange1[15],rowrange2[15];
    int wrap,banner,getmilist,putmilist=0,reprompt;
    
    char collist[200];
    Colmnode *colhead = (Colmnode *) NULL; /*Colmnode defined in colm.h*/
    int ckcolid();

    Cnfgnode *cnfghead;
    Cifnode *cifhead, *cifnode, *nxtcif, *badcifhead, *badcifnode;
    Alscfgnode *alscfghead;
    Selrowsnode *selrowshead, *selrowsnode, *nxtselrows;

    /*XPI interface variables*/
    char string[2];
    int  parse=0;
    char prompt[9], command[255];
    int comnum;
    char dsk[160],dir[160];
    char nam[160],ver[160];
    int idone = 0,ncmds;

    char targetdir[200];

    /*extern int cbdebug;
    cbdebug = 1;*/

    /*Initialize variables used by GTCOM2*/
#include "setxpisysvar.h"
    strcpy(string," \0");
    strcpy(prompt,"brcaldb>\0");
    strcpy(nam,"brcaldb\0");
    strcpy(ver," \0");

    badcifhead = (Cifnode *)NULL;

    /*read config file into memory*/
    mkcnfglist(&cnfghead,&errstat);

    /*read alias file into memory*/
    mkalslist(&alscfghead,&errstat);

    /*Open the par file and read any command line arguments
    OpenDefaultPF(argc, argv); */
        Uclgsb("banner",&banner,&errstat);
        if(errstat) {
            Fcerr("Error getting banner parameter");
            banner = 1;
            errstat = 0;
        }

        if(banner) {
            /*display cnfglist*/
            strcpy(contxt,"The following missions and instruments are available:");
            Fcecho(contxt);
            Fcecho(" ");
            disp_banner(cnfghead);
        }

        milist[0]='\0';
        BufLen_2 = 999;
        Uclgst("milist",milist,&errstat);
        if(errstat) {
            Fcerr("Error getting milist parameter");
            return(RETURN);
        }

        Uclgsb("wrap",&wrap,&errstat);
        if(errstat) {
            Fcerr("Error getting wrap parameter");
            errstat = 0;
            wrap = 1;
        }

        Uclgsb("reprompt",&reprompt,&errstat);
        if(errstat) {
            Fcerr("Error getting reprompt parameter");
            errstat = 0;
            reprompt = 0;
        }
    /* CloseDefaultPF(); */

    /*cfortran call to initialize the TASK common block used by Fcerr*/
    C2FCBSTR("brcaldb",TASK.taskname,0);

    /*Tell XPI not to create a log file*/
    Gtcom2_nolog();

reset:

    expr[0]='\0';
    cifhead = (Cifnode *)NULL;
    nrows=0;

    /*Initialize parser error flag*/
    parse_error = 0;

    /*Construct a list of cifs to read based on user's missions and insts*/
    getmilist = 1;
    while(getmilist) {
      mkciflist(milist,cnfghead,alscfghead,&cifhead,&errstat);
      if (errstat != 0) {
        Fcerr("Unable to parse mission/instrument list");
        Fcerr("Unable to create Index File list");
        Fcerr("Check mission and instrument values");
        if(!reprompt) {
          return(RETURN);
        }
        errstat = 0;

        for(cifnode=cifhead;cifnode!=(Cifnode *)NULL;
        cifnode=nxtcif) {
          nxtcif = cifnode -> next;
          free((char *)cifnode);
        }
        cifhead = (Cifnode *) NULL;

        milist[0]='\0';
        BufLen_2 = 999;
        Uclgst("milist",milist,&errstat);
        if (errstat) {
            Fcerr("Error getting milist parameter");
            return(RETURN);
        }
        putmilist = 1;
      }
      else {
        getmilist = 0;
      }
    }

    /*
    for(cifnode=cifhead;cifnode!=(Cifnode *)NULL;cifnode=cifnode->next) {
        printf("%s\n",cifnode->cif);
    }
    */

    /*Determine the number of rows that will be read from all the cifs*/
    prevwcifs(cifhead,&nrows,&errstat);

    /*Allocate memory for the cifdata and the selected rows list*/
    cifdata = (Cifdata *)malloc(nrows*sizeof(Cifdata));
    selrows = (int *)malloc(nrows*sizeof(int));

    selrowshead = (Selrowsnode *) malloc(sizeof(Selrowsnode));
    (selrowshead->next) = (Selrowsnode *)NULL;
    (selrowshead -> rows) = selrows;
    (selrowshead -> nrows) = nrows;

    /*Before reading the data into memory tell the user to be patient*/
    Fcecho("Loading Index files into memory... please wait");

    /*Read the data into memory*/
    rdcifs(cifhead,&badcifhead,&errstat);
    if(errstat) {
        errstat = 0;
        Fcerr("Error reading Index files");
        Fcerr("Some entries from the following CIFs may not be available");
        for(badcifnode=badcifhead;badcifnode!=(Cifnode *)NULL;
        badcifnode=badcifnode->next) {
            Fcerr(badcifnode->cif);
        }
    }

    /*A filtering should be preformed here based on the user's missions
      and instruments.  This will weed out unwanted instruments that are
      stored in the same cif.*/
    if(strlen(expr)!=0) {
        filter(&nrows,&selrowshead,&errstat);
    }
    selrowshead -> nrows = nrows;

    /*Tell the user how many entries are available*/
    sprintf(contxt,"Number of entries available: %d\n",nrows);
    Fcecho(contxt);

    /*Get commands until user types quit*/
    while (1) {

        /*XPI command line interface; most arguments are not needed*/
        BufLen_1 = 1;
        BufLen_4 = 254;
        Gtcom2(string,&parse,prompt,command,&comnum,dsk,dir,nam,ver,&idone);
        errstat = 0;

        if(putmilist) {
            milist[0]='\0';
            Uclpst("milist",milist,&errstat);
            errstat = 0;
        }

        /*get the number of command line arguments; only needed for more
          complicated commands*/
        Uclgno(&ncmds);

        /*If idone=-1 an error occurred; =1 means XPI specific command,
          e.g. "?"; =0 command was found on command line*/
        if (idone == -1) return(RETURN);
        else if (idone == 1) continue;

        /*save parameters before quitting*/
        if (strcmp(command,"QUIT") == 0) {
            Xpisavepar(&errstat);
            return(RETURN);
        }

        /*filter the data using a selection expression*/
        else if (strcmp(command,"FILTER") == 0) {
            expr[0]='\0';
            BufLen_2 = 999;
            Uclgst("expr",expr,&errstat);
            if(errstat) {
                Fcerr("Error getting expr parameter");
                Fcerr("Unable to execute filter command");
                errstat = 0;
                continue;
            }

            /*printf("filtering with expr: %s\n",expr);*/

            /*filter the data and find out how many rows are left*/
            filter(&nrows,&selrowshead,&errstat);
            if (!errstat) {
                sprintf(contxt,"Number of entries selected: %d",nrows);
                Fcecho(contxt);
            }
            else {
                errstat = 0;
            }

            if((selrowshead->nrows) == 0) {
                Fcecho("No rows selected! Undoing filter action...");
                selrowsnode = selrowshead->next;
                free((char *)selrowshead->rows);
                free((char *)selrowshead);
                selrowshead = selrowsnode;
                selrows = selrowsnode -> rows;
                nrows = selrowsnode -> nrows;
            }
        }

        /*Display the selected data*/
        else if (strcmp(command,"SHOW") == 0) {

            /*Get rows to be displayed and parse into ranges*/
            rows[0]='\0';
            BufLen_2 = 79;
            Uclgst("rows",rows,&errstat);
            if (errstat) {
                errstat = 0;
                Fcerr("Error getting rows parameter");
                Fcerr("Unable to execute show command");
                continue;
            }

            Fcgrgs(rows,nrows,&numranges,rowrange1,rowrange2);

            /*Get the columns to be displayed*/
            collist[0]='\0';
            BufLen_2 = 199;
            Uclgst("cols",collist,&errstat);
            if (errstat) {
                errstat = 0;
                Fcerr("Error getting cols parameter");
                Fcerr("Unable to execute show command");
                continue;
            }

            /*If cols were specified on the cl set flag to reparse collist*/
            Uclgot("cols",&errstat);
            if (errstat) {
                errstat = 0;
            }
            else {
                cols_on_cl = 1;
            }

            if (cols_on_cl || parse_disp_cols) {
                parse_disp_cols = 0;
                cols_on_cl = 0;

                /*parse column list and create linked list*/
                gtcollist(&colhead,collist);
            }

            /*See if wrap param was on cl.  If so reset wrap param*/
            Uclgot("wrap",&notgotwrap);
            if(notgotwrap) {
                notgotwrap = 0;
            }
            else {
                Uclgsb("wrap",&wrap,&errstat);
                if (errstat) {
                    errstat = 0;
                    Fcerr("Error getting wrap parameter");
                    Fcerr("Unable to execute show command");
                    continue;
                }
            }

            /*Reset the display line number counter*/
            PGFCOUNT.count = 0;

            /*Invoke the displayer*/
            if (colhead != (Colmnode *)NULL) {
                show_cmd(colhead,numranges,rowrange1,rowrange2,wrap);
            }

        }

        else if (strcmp(command,"COPY") == 0) {
            rows[0]='\0';
            BufLen_2 = 79;
            Uclgst("copyrows",rows,&errstat);
            if(errstat) {
                Fcerr("Error getting copyrows parameter");
                Fcerr("Unable to execute copy command");
                errstat = 0;
                continue;
            }

            Fcgrgs(rows,nrows,&numranges,rowrange1,rowrange2);

            targetdir[0]='\0';
            BufLen_2 = 199;
            Uclgst("targetdir",targetdir,&errstat);
            if(errstat) {
                Fcerr("Error getting targetdir parameter");
                Fcerr("Unable to execute copy command");
                errstat = 0;
                continue;
            }

            copy_cmd(numranges,rowrange1,rowrange2,targetdir);
        }

        else if (strcmp(command,"RESTART") == 0) {
            Uclgsb("banner",&banner,&errstat);
            if(errstat) {
                Fcerr("Error getting banner parameter");
                Fcerr("Unable to execute restart command");
                errstat = 0;
                continue;
            }

            if(banner) {
                /*display cnfglist*/
                strcpy(contxt,"The following missions and instruments are available.");
                Fcecho(contxt);
                Fcecho(" ");
                disp_banner(cnfghead);
            }

            milist[0]='\0';
            BufLen_2 = 199;
            Uclgst("milist",milist,&errstat);
            if(errstat) {
                Fcerr("Error getting milist parameter");
                Fcerr("Unable to execute restart command");
                errstat = 0;
                continue;
            }

            free((char *)cifdata);
            for(cifnode=cifhead;cifnode!=(Cifnode *)NULL;
            cifnode=nxtcif) {
              nxtcif = cifnode -> next;
              free((char *)cifnode);
            }
            for(selrowsnode=selrowshead;selrowsnode!=(Selrowsnode *)NULL;
            selrowsnode=nxtselrows) {
              nxtselrows = selrowsnode -> next;
              free((char *)selrowsnode->rows);
              free((char *)selrowsnode);
            }
            goto reset;
        }

        else if (strcmp(command,"BACKUP") == 0) {
            if ((selrowshead->next) != (Selrowsnode *) NULL) {
              selrowsnode = selrowshead->next;
              free((char *)selrowshead->rows);
              free((char *)selrowshead);
              selrows = selrowsnode -> rows;
              nrows = selrowsnode -> nrows;
            }
        }

        else if (strcmp(command,"RESET") == 0) {
            if ((selrowshead->next) != (Selrowsnode *) NULL) {

              for(selrowsnode=selrowshead;
              (selrowsnode->next)!=(Selrowsnode *)NULL;
              selrowsnode=nxtselrows) {
                nxtselrows = selrowsnode -> next;
                free((char *)selrowsnode->rows);
                free((char *)selrowsnode);
              }

              selrowshead = selrowsnode;
              selrows = selrowshead->rows;
              nrows = selrowshead->nrows;

            }
        }
    }
}


prevwcifs(cifhead,nrows,status)
Cifnode *cifhead;
int *nrows, *status;
{
    Cifnode *cifnode;
    int i,unit,blcksz,hdutyp,ncifrows,errstat;
    char comment[81];

    /*for (i=0;i<ncifs;i++) {*/
    for(cifnode=cifhead;cifnode!=(Cifnode *)NULL;cifnode=cifnode->next) {

         comment[0]='\0';
         errstat = 0;
         ncifrows = 0;

         FCGIOU(&unit,&errstat);
         FCOPEN(unit,cifnode->cif,0,&blcksz,&errstat);
         if(errstat != 0) {
             errstat = 0;
             FCCLOS(unit,&errstat);
             FCFIOU(unit,&errstat);
             continue;
         }

         FCMAHD(unit,2,&hdutyp,&errstat);
         FCGKYJ(unit,"NAXIS2",&ncifrows,comment,&errstat);
         FCCLOS(unit,&errstat);
         FCFIOU(unit,&errstat);

         *nrows += ncifrows;
    }
}

rdcifs(cifhead,badcifhead,status)
Cifnode *cifhead,**badcifhead;
int *status;
{
    Cifnode *cifnode,*badcifnode,*nxtbadcif;
    char *oldcif=0;
    int a,i,j,unit,blcksz,hdutyp,anyf,ncifrows,row,errstat=0,readerror=0;
    char comment[81];
    int FitsStrBufLen;

    typedef struct { int telescop;
                     int instrume;
                     int detnam;
                     int filter;
                     int cal_dev;
                     int cal_dir;
                     int cal_file;
                     int cal_clas;
                     int cal_dtyp;
                     int cal_cnam;
                     int cal_cbd;
                     int cal_xno;
                     int cal_vsd;
                     int cal_vst;
                     int ref_time;
                     int cal_qual;
                     int cal_date;
                     int cal_desc;
                   } Colnum;
 
    Colnum colnum;
 

    if (*badcifhead != (Cifnode *) NULL) {
        for(badcifnode= *badcifhead;badcifnode!=(Cifnode *)NULL;
        badcifnode=nxtbadcif) {
            nxtbadcif = (badcifnode -> next);
            free((char *) badcifnode);
        }
        *badcifhead = (Cifnode *) NULL;
    }


    j = 0;
    for(cifnode=cifhead;cifnode!=(Cifnode *)NULL;cifnode=cifnode->next) {

        comment[0]='\0';
        errstat = 0;

        if (readerror != 0) {
            readerror = 0;
            *status = 1;
            badcifnode = (Cifnode *)malloc(sizeof(Cifnode));
            (badcifnode -> next) = *badcifhead;
            *badcifhead = badcifnode;
            (badcifnode -> cif) = oldcif;
        }

        FCGIOU(&unit,&errstat);
        FCOPEN(unit,cifnode->cif,0,&blcksz,&errstat);
         if(errstat != 0) {
             readerror = 1;
             oldcif = cifnode->cif;
             errstat = 0;
             FCCLOS(unit,&errstat);
             FCFIOU(unit,&errstat);
             continue;
         }

        FCMAHD(unit,2,&hdutyp,&errstat);
        FCGKYJ(unit,"NAXIS2",&ncifrows,comment,&errstat);

        FCGCNO(unit,1,"TELESCOP",&colnum.telescop,&errstat);
        FCGCNO(unit,1,"INSTRUME",&colnum.instrume,&errstat);
        FCGCNO(unit,1,"DETNAM",&colnum.detnam,&errstat);
        FCGCNO(unit,1,"FILTER",&colnum.filter,&errstat);
        FCGCNO(unit,1,"CAL_DEV",&colnum.cal_dev,&errstat);
        FCGCNO(unit,1,"CAL_DIR",&colnum.cal_dir,&errstat);
        FCGCNO(unit,1,"CAL_FILE",&colnum.cal_file,&errstat);
        FCGCNO(unit,1,"CAL_CLAS",&colnum.cal_clas,&errstat);
        FCGCNO(unit,1,"CAL_DTYP",&colnum.cal_dtyp,&errstat);
        FCGCNO(unit,1,"CAL_CNAM",&colnum.cal_cnam,&errstat);
        FCGCNO(unit,1,"CAL_CBD",&colnum.cal_cbd,&errstat);
        FCGCNO(unit,1,"CAL_XNO",&colnum.cal_xno,&errstat);
        FCGCNO(unit,1,"CAL_VSD",&colnum.cal_vsd,&errstat);
        FCGCNO(unit,1,"CAL_VST",&colnum.cal_vst,&errstat);
        FCGCNO(unit,1,"REF_TIME",&colnum.ref_time,&errstat);
        FCGCNO(unit,1,"CAL_QUAL",&colnum.cal_qual,&errstat);
        FCGCNO(unit,1,"CAL_DATE",&colnum.cal_date,&errstat);
        FCGCNO(unit,1,"CAL_DESC",&colnum.cal_desc,&errstat);
         if(errstat != 0) {
             readerror = 1;
             oldcif = cifnode->cif;
             errstat = 0;
             FCCLOS(unit,&errstat);
             FCFIOU(unit,&errstat);
             continue;
         }


        for(row=1;row<=ncifrows;row++) {
           FitsStrBufLen=11;
           FCGCVS(unit,colnum.telescop,row,1,1," ",cifdata[j].telescop,
                  &anyf,&errstat);
           cifdata[j].telelen = strlen(cifdata[j].telescop);

           FitsStrBufLen=11;
           FCGCVS(unit,colnum.instrume,row,1,1," ",cifdata[j].instrume, 
                  &anyf,&errstat);
           cifdata[j].instlen = strlen(cifdata[j].instrume);

           FitsStrBufLen=21;
           FCGCVS(unit,colnum.detnam,row,1,1," ",cifdata[j].detnam,
                  &anyf,&errstat);
           cifdata[j].detnlen = strlen(cifdata[j].detnam);

           FitsStrBufLen=11;
           FCGCVS(unit,colnum.filter,row,1,1," ",cifdata[j].filter,
                  &anyf,&errstat);
           cifdata[j].filtlen = strlen(cifdata[j].filter);

           FitsStrBufLen=21;
           FCGCVS(unit,colnum.cal_dev,row,1,1," ",cifdata[j].cal_dev,
                  &anyf,&errstat);
           cifdata[j].devlen = strlen(cifdata[j].cal_dev);

           FitsStrBufLen=71;
           FCGCVS(unit,colnum.cal_dir,row,1,1," ",cifdata[j].cal_dir,
                  &anyf,&errstat);
           cifdata[j].dirlen = strlen(cifdata[j].cal_dir);

           FitsStrBufLen=41;
           FCGCVS(unit,colnum.cal_file,row,1,1," ",cifdata[j].cal_file,
                  &anyf,&errstat);
           cifdata[j].filelen = strlen(cifdata[j].cal_file);

           FitsStrBufLen=4;
           FCGCVS(unit,colnum.cal_clas,row,1,1," ",cifdata[j].cal_clas,
                  &anyf,&errstat);
           cifdata[j].claslen = strlen(cifdata[j].cal_clas);

           FitsStrBufLen=5;
           FCGCVS(unit,colnum.cal_dtyp,row,1,1," ",cifdata[j].cal_dtyp,
                  &anyf,&errstat);
           cifdata[j].dtyplen = strlen(cifdata[j].cal_dtyp);

           FitsStrBufLen=21;
           FCGCVS(unit,colnum.cal_cnam,row,1,1," ",cifdata[j].cal_cnam,
                  &anyf,&errstat);
           cifdata[j].cnamlen = strlen(cifdata[j].cal_cnam);

           FitsStrBufLen=71;
           for (a=1;a<=9;a++) {
               FCGCVS(unit,colnum.cal_cbd,row,a,1," ",
                      &cifdata[j].cal_cbd[a-1][0],&anyf,&errstat);
               cifdata[j].cbdlen[a-1] = strlen(cifdata[j].cal_cbd[a-1]);
           }

           FCGCVJ(unit,colnum.cal_xno,row,1,1,0,&cifdata[j].cal_xno,
                  &anyf,&errstat);

           FitsStrBufLen=9;
           FCGCVS(unit,colnum.cal_vsd,row,1,1," ",cifdata[j].cal_vsd,
                  &anyf,&errstat);

           FitsStrBufLen=9;
           FCGCVS(unit,colnum.cal_vst,row,1,1," ",cifdata[j].cal_vst,
                  &anyf,&errstat);

           FCGCVD(unit,colnum.ref_time,row,1,1,(double)0,
                  &cifdata[j].ref_time,&anyf,&errstat);

           FCGCVJ(unit,colnum.cal_qual,row,1,1,0,&cifdata[j].cal_qual,
                  &anyf,&errstat);

           FitsStrBufLen=9;
           FCGCVS(unit,colnum.cal_date,row,1,1," ",cifdata[j].cal_date,
                  &anyf,&errstat);

           FitsStrBufLen=71;
           FCGCVS(unit,colnum.cal_desc,row,1,1," ",cifdata[j].cal_desc,
                  &anyf,&errstat);
           cifdata[j].desclen = strlen(cifdata[j].cal_desc);
    
           cifdata[j].cif = cifnode->cif;
           cifdata[j].ciflen = strlen(cifnode->cif);

           selrows[j] = j;
           j++;

           if (errstat != 0) {
               errstat = 0;
               readerror = 1;
           }
        
        }

        FCCLOS(unit,&errstat);
        FCFIOU(unit,&errstat);

        oldcif = cifnode->cif;
    }

}

void mkciflist(milist,cnfghead,alscfghead,cifhead,status)
  char *milist;
  Cnfgnode *cnfghead;
  Alscfgnode *alscfghead;
  Cifnode **cifhead;
  int *status;
{
     Cifnode *cifnode;
     Cnfgnode *cnfgnode;
     Instnode *instnode=0;
     Alscfgnode *alscfgnode;
     Alsnode *alsnode;
     Insnode *insnode=0;
     char *sep, itemp[10], mtemp[10];
     int state = 1;
     int i, nodup;
     int errstat = 0;
     char instdir[160];
     int notfirst=0;
     int foundcif,foundalias,nins=0,n;

     for(cifnode= *cifhead;cifnode!=(Cifnode *)NULL;cifnode=cifnode->next) {
         free((char *)cifnode);
     }
     *cifhead = (Cifnode *)NULL;

     for(i=0;i<strlen(milist);i++) {
       milist[i]=toupper(milist[i]);
     }

     sep = milist;

     while(1) {
       if (state) {                                 /*get mission token*/
            while (*sep == ' ')                     /*move to next token*/
                 sep++;

            milist = sep;                           /*reset string head*/

            if(strcmp(milist,"ALL")==0) {
              for(cnfgnode=cnfghead;cnfgnode!=(Cnfgnode *)NULL;
              cnfgnode=cnfgnode->next) {
                for(instnode=cnfgnode->instnode;instnode!=(Instnode *)NULL;
                instnode=instnode->next) {

                  nodup = 1;
                  for(cifnode= *cifhead;cifnode!=(Cifnode *)NULL;
                  cifnode=cifnode->next) {
                    if(strcmp(cifnode->cif,instnode->cif) == 0) {
                        nodup = 0;
                    }
                  }

                  if(nodup) {
                    cifnode = (Cifnode *) malloc(sizeof(Cifnode));
                    cifnode -> next = *cifhead;
                    *cifhead = cifnode;
                    cifnode -> cif = instnode->cif;
                  }
                }
              }
              return;
            }

            while (*sep != ' ') {                   /*move to end of token*/
                 if ((*sep == ',') || (*sep == '\0')) {
                      *status = 1;
                      return;
                 }
                 sep++;
            }

            strncpy(mtemp,milist,sep-milist);       /*copy token to mtemp*/
            mtemp[sep-milist] = '\0';

            state = 0;                              /*look for inst*/
       }
       else {                                       /*get instr token*/
            while (*sep == ' ')                     /*move to next token*/
                 sep++;

            milist = sep;                           /*reset string head*/

            while ((*sep != ' ')                     /*move to end of token*/
                && (*sep != ',')
                && (*sep != '\0'))
                 sep++;

            strncpy(itemp,milist,sep-milist);       /*copy token to itemp*/
            itemp[sep-milist] = '\0';


            /*printf("Mission: %s Inst: %s\n",mtemp,itemp);*/

            foundalias = 0;
            for(alscfgnode=alscfghead;alscfgnode!=(Alscfgnode *)NULL;
            alscfgnode=alscfgnode->next) {
              if(strcmp(mtemp,alscfgnode->mission) == 0) {
                for(alsnode=alscfgnode->alsnode;alsnode!=(Alsnode *)NULL;
                alsnode=alsnode->next) {
                  if(strcmp(itemp,alsnode->alias) == 0) {
                    foundalias = 1;
                    nins = alsnode->nins;
                    insnode = alsnode->insnode;
                    break;
                  }
                }
              }
              if(foundalias) break;
            }

            if(!foundalias) nins = 1;

            for(n=0;n<nins;n++) {
              if(foundalias) {
                strcpy(itemp,insnode->ins);
                insnode=insnode->next;
              }

              /*get cif value*/

              foundcif = 0;
              for(cnfgnode=cnfghead;cnfgnode!=(Cnfgnode *)NULL;
              cnfgnode=cnfgnode->next) {
                if(strcmp(cnfgnode->mission,mtemp) == 0) {
                  for(instnode=cnfgnode->instnode;instnode!=(Instnode *)NULL;
                  instnode=instnode->next) {
                    if(strcmp(instnode->inst,itemp) == 0) {
                      foundcif = 1;
                      break;
                    }
                  }
                  if (foundcif) break;
                }
              }
  
              if(!foundcif) {
                  *status = 1;
                  return;
              }

              nodup = 1;                         /*check for duplicate cifs*/
              for(cifnode= *cifhead;cifnode!=(Cifnode *)NULL;
              cifnode=cifnode->next) {
                  if(strcmp(cifnode->cif,instnode->cif) == 0) {
                      nodup = 0;
                  }
              }

              if(nodup) {
                  cifnode = (Cifnode *) malloc(sizeof(Cifnode));
                  cifnode -> next = *cifhead;
                  *cifhead = cifnode;
                  cifnode -> cif = instnode->cif;
              }


              if (notfirst) {
                  strcat(expr," || ");
              }
              else {
                  expr[0] = '\0';
                  notfirst = 1;
              }
              strcat(expr,"(mission == ");
              strcat(expr,mtemp);
              strcat(expr," && inst == ");
              strcat(expr,itemp);
              strcat(expr,")");

            }

            if (*sep == ',') {                      /*get mission token next*/
                 state = 1;
                 sep++;
            }
            else if (*sep == '\0') {                /*at end of string*/
                 return;
            }
            else if (*sep == ' ') {                 /*don't know what's up*/
                 while (*sep == ' ')                /*move to next token*/
                      sep++;
                 if (*sep == ',') {                 /*get mission token next*/
                      state = 1;
                      sep++;
                 }  
                 else if (*sep == '\0') {           /*at end of string*/
                      return;
                 }
            }                                /*otherwise get instr token next*/
       }
     }
}

filter(nrows,selrowshead,status)
int *nrows;
Selrowsnode **selrowshead;
int *status;
{
    int *newrows,nnewrows,errstat=0;
    Selrowsnode *selrowsnode;

            /*allocate space for new list of selected rows*/
            /*for simplicity make it the size of the old row list*/
            newrows = (int *)malloc(*nrows * sizeof(int));

            /*Call the filtering routine (source is in brcaldb.y file)*/
            cifselector(*nrows,newrows,&nnewrows,&errstat);

            /*Overwrite old rows with new ones.  The old rows should be
              saved when the backup command is installed*/
            if (errstat != 0) {
                free((char *)newrows);
            }
            else {
                selrowsnode = (Selrowsnode *) malloc(sizeof(Selrowsnode));
                selrowsnode->next = *selrowshead;
                *selrowshead = selrowsnode;

                selrowsnode -> rows = newrows;
                selrowsnode -> nrows = nnewrows;

                selrows = newrows;
                *nrows = nnewrows;
            }
}

gtcollist(colhead,collist)
char *collist;
Colmnode **colhead;
{
    int i;
    Colmnode *oldcolnode, *colnode, *nextnode; /*Colmnode defined in colm.h*/
    char contxt[200];

    /*Uppercase user's column list*/
    for (i=0;i<strlen(collist);i++)
        collist[i] = toupper(collist[i]);

    /*Erase the old column information*/
    for(colnode= *colhead;colnode!= (Colmnode *) NULL;colnode=nextnode) {
        nextnode = colnode -> next;
        free ((char *)colnode);
    }

    /*Create a linked list of columns to display*/
    if (strlen(collist) != 0) {
        mkcollist(collist,colhead);
    }
    else {
        *colhead = (Colmnode *) NULL;
    }

    /*Check each column to make sure that user isn't trying to display
      something unrealistic*/

    oldcolnode = (Colmnode *) NULL;
    colnode = *colhead;
    while (1) {
        if (colnode == (Colmnode *) NULL) break;
        if (!ckcolid(colnode -> col)) {
            sprintf(contxt,"Unkown display column: %s\n",colnode -> colstr);
            Fcecho(contxt);
            sprintf(contxt,"Display column ignored...\n");
            Fcecho(contxt);
            if (colnode == *colhead) {
                *colhead = colnode -> next;
                free((char *)colnode);
                colnode = *colhead;
            }
            else {
                oldcolnode -> next = colnode -> next;
                free((char *)colnode);
                colnode = oldcolnode -> next;
            }
        }
        else {
            oldcolnode = colnode;
            colnode = colnode -> next;
        }
    }    
}

mkcollist(collist,colhead)
char *collist;
Colmnode **colhead;
{
    Colmnode *colnode=0, *newcolnode;
    int begin_initial();
    int first = 1;
    int tmpcol;
    extern int cbleng;
    extern FILE *cbin;

    begin_initial();
    cbrestart(cbin);
    read_string = 2;
    colmptr = collist;
    colmlim = collist + strlen(colmptr) + 1;

    /*Invoke lexer to determine dispaly column, then create new node*/
    while ((tmpcol = cblex()) != 0) {
        newcolnode = (Colmnode *) malloc(sizeof(Colmnode));
        if (first) {
            *colhead = newcolnode;
            first = 0;
        }
        else {
            colnode -> next = newcolnode;
        }

        colnode = newcolnode;

        /*Write column ID to colmnode*/
        colnode -> col = tmpcol;

        /*Write user's column string to col node*/
        colnode -> colstr = (char *) malloc((cbleng+1) * sizeof(char));
        strcpy(colnode -> colstr,cblval.string);
        colnode -> colstr[cbleng] = '\0';
 
        /*reset lexer*/
        begin_initial();
    }
 
    colnode -> next = (Colmnode *) NULL;
}

int ckcolid(col)
int col;
{
    /*Col ID values are set in y.tab.h*/
    /*Only the following col ID's are valid*/

    switch (col) {
        case MISSION : return (1);
        case INSTRUM : return (1);
        case DETNAM  : return (1);
        case FILTER  : return (1);
        case DEVICE  : return (1);
        case DIR     : return (1);
        case CFILE   : return (1);
        case CLASS   : return (1);
        case DATATYP : return (1);
        case CODENAM : return (1);
        case EXTNO   : return (1);
        case DATE    : return (1);
        case VSD     : return (1);
        case TIME    : return (1);
        case VST     : return (1);
        case REFTIME : return (1);
        case QUALITY : return (1);
        case CALDATE : return (1);
        case DESCRIP : return (1);
	case CIF     : return (1);
        case CBD     : return (1);
        default:       return (0);
    }
}

void mkcnfglist(cnfghead,status)
Cnfgnode **cnfghead;
int *status;
{
    char cnfgfile[200];
    char misval[11],insval[11],cifdev[21],cifdir[71],cif[200];
    char insdev[11],insdir[71];
    int unit,lnum,errstat;
    int found;
    Cnfgnode *cnfgnode, *lastcnfgnode=0;
    Instnode *insnode, *lastinsnode=0;

    *cnfghead = (Cnfgnode *) NULL;

    Cgetlun(&unit);
    BufLen_3 = 199;
    Ocnfg(unit,1,cnfgfile,&errstat);

    while (1) {

      misval[0]='\0';
      insval[0]='\0';
      cifdev[0]='\0';
      cifdir[0]='\0';
      cif[0]='\0';
      insdev[0]='\0';
      insdir[0]='\0';

      BufLen_4 = 10;
      BufLen_5 = 10;
      BufLen_6 = 20;
      BufLen_7 = 70;
      BufLen_8 = 199;
      BufLen_9 = 10;
      BufLen_10 = 70;
      Rcnfgl(unit,1,&lnum,misval,insval,cifdev,cifdir,cif,insdev,insdir,&errstat);
      if(errstat == -1) break;
      if(errstat >  0) {
        *status = errstat;
        return;
      }

      BufLen_3 = 199;
      Cpthnm(cifdev,cifdir,cif,&errstat);

      found = 0;

      for(cnfgnode= *cnfghead;cnfgnode != (Cnfgnode *)NULL;cnfgnode=(cnfgnode->next)){
        if(strcmp(cnfgnode->mission,misval) == 0) {
          found = 1;
          break;
        }
        lastcnfgnode=cnfgnode;
      }

      if (!found) {
        cnfgnode = (Cnfgnode *) malloc(sizeof(Cnfgnode));
        cnfgnode -> next = (Cnfgnode *) NULL;
        cnfgnode -> instnode = (Instnode *) NULL;
        strcpy(cnfgnode -> mission,misval);
        if (*cnfghead == (Cnfgnode *)NULL) {
          *cnfghead = cnfgnode;
        }
        else {
          lastcnfgnode -> next = cnfgnode;
        }
      }

      for(insnode=cnfgnode->instnode;insnode!=(Instnode *)NULL;insnode=insnode->next){
        lastinsnode = insnode;
      }

      insnode = (Instnode *) malloc(sizeof(Instnode));
      insnode -> next = (Instnode *) NULL;

      if(cnfgnode -> instnode == (Instnode *) NULL) {
        cnfgnode -> instnode = insnode;
      }
      else {
        lastinsnode -> next = insnode;
      }

      strcpy(insnode -> inst,insval);
      strcpy(insnode -> cif,cif);
    }
    Ccnfg(unit);
}

disp_banner(cnfghead)
Cnfgnode *cnfghead;
{

    Cnfgnode *cnfgnode;
    Instnode *instnode;
    char display[128];

    for(cnfgnode=cnfghead;cnfgnode != (Cnfgnode *) NULL;
    cnfgnode = cnfgnode -> next) {

        display[0] = '\0';
        strcat(display,cnfgnode->mission);
        padsp(display,(10-strlen(cnfgnode->mission)));
        strcat(display," ");

        for(instnode=cnfgnode->instnode;instnode != (Instnode *) NULL;
        instnode = instnode -> next) {

            strcat(display,instnode->inst);
            strcat(display," ");

        }

        Fcecho(display);
        /*Fcecho(" ");*/
    }
    Fcecho(" ");
}

mkalslist(alscfghead,status)
Alscfgnode **alscfghead;
int *status;
{
    Alscfgnode *alscfgnode;
    Alsnode *alsnode;
    Insnode *insnode;

    int unit,errstat=0,blcksz,hdutyp,extno=1,anyf;
    int aliascol,aliasnocol,valuescol;
    int row,nrows,nalias,n;
    int FitsStrBufLen;
    char alsfile[200];
    char comment[81];
    char dmymission[71];

    *alscfghead = (Alscfgnode *) NULL;

    strcpy(alsfile,"alias_config.fits");
    BufLen_3 = 199;
    Cpthnm("CALDB","software/tools",alsfile,&errstat);

    FCGIOU(&unit,&errstat);
    FCOPEN(unit,alsfile,0,&blcksz,&errstat);

    while(1) {
      extno++;
      FCMAHD(unit,extno,&hdutyp,&errstat);
      if(errstat != 0) {
          break;
      }

      FCGCNO(unit,1,"ALIAS",&aliascol,&errstat);
      FCGCNO(unit,1,"ALIAS_NO",&aliasnocol,&errstat);
      FCGCNO(unit,1,"VALUES",&valuescol,&errstat);

      alscfgnode = (Alscfgnode *) malloc(sizeof(Alscfgnode));
      alscfgnode -> next = *alscfghead;
      *alscfghead = alscfgnode;
      alscfgnode -> alsnode = (Alsnode *) NULL;

      comment[0]='\0';
      FCGKYS(unit,"EXTNAME",dmymission,comment,&errstat);
      strncpy(alscfgnode->mission,dmymission,10);

      comment[0]='\0';
      FCGKYJ(unit,"NAXIS2",&nrows,comment,&errstat);

      for(row=1;row<=nrows;row++) {
        alsnode = (Alsnode *) malloc(sizeof(Alsnode));
        alsnode -> next = alscfgnode -> alsnode;
        alscfgnode -> alsnode = alsnode;
        alsnode -> insnode = (Insnode *) NULL;

        FitsStrBufLen=10;
        FCGCVS(unit,aliascol,row,1,1," ",alsnode->alias,&anyf,&errstat);

        FCGCVJ(unit,aliasnocol,row,1,1,0,&(alsnode->nins),&anyf,&errstat);
        for(n=1;n<=alsnode->nins;n++) {
          insnode = (Insnode *) malloc(sizeof(Insnode));
          insnode -> next = alsnode -> insnode;
          alsnode -> insnode = insnode;

          FitsStrBufLen=10;
          FCGCVS(unit,valuescol,row,n,1," ",insnode->ins,&anyf,&errstat);
        }
      }
    }

    FCCLOS(unit,&errstat);
    FCFIOU(unit,&errstat);
}
