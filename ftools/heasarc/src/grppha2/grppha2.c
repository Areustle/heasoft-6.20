

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <limits.h>

#include "ftools.h"        /* for FORTRAN interface */
#include "cftools.h"       /* standard C library constants */
#include "fitsio.h"        /* cfitsio defined constants */
#include "xpi.h"           /* parameter file reading functions*/
#include "ftoolstruct.h"   /* C-Fortran common blocks */
#include "grppha2.h"     /* task specific definitions */

void grppha2()
{
    static char version[] = "Ver 3.0";
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    char respfile[FLEN_BUFFER];
    char ancrfile[FLEN_BUFFER];
    char root_respfile[FLEN_BUFFER];
    char root_ancrfile[FLEN_BUFFER];
    char commands[MAX_COMMAND][FLEN_BUFFER];
    char subinfo[100];

    int numranges=0;
    int firstrow[MAX_RANGE];
    int lastrow[MAX_RANGE];
    int clobber=0;
    int chatter=9;
    int status=0;
    int ncom = 0;
    int i;
    int new_respfile=0, new_ancrfile=0;
    int root_resp=0, root_ancr=0;

     
    /* ----- initialisation  -------------*/
    strcpy(infile, "");
    strcpy(outfile, "");
    strcpy(respfile, "");
    strcpy(ancrfile, "");
    strcpy(root_respfile, "");
    strcpy(root_ancrfile, "");


    for (i=0; i<MAX_COMMAND; ++i)
        strcpy(commands[i], " ");

    /* ----- get parameters -------------*/

    status = 0;
    grppha2_par(infile, outfile, &ncom, commands, &numranges,
                  firstrow, lastrow, respfile, ancrfile, 
                  &clobber, &chatter, &status);

    strcpy(subinfo,"..... Error retrieving parameters for the task");
    StatusChk(status, subinfo);
 
    if(strcmp(respfile, "")) new_respfile = 1;
    if(strcmp(ancrfile, "")) new_ancrfile = 1;

    if (new_respfile) {
        if (respfile[0] !='@' ) {
            root_resp = 1;
            strcpy (root_respfile, respfile);
            new_respfile = 0;
        }
        else 
            strcpy(respfile, respfile+1);
    }

    if (new_ancrfile) {
        if (ancrfile[0] !='@') {
            root_ancr = 1;
            strcpy (root_ancrfile, ancrfile);
            new_ancrfile = 0;
        }
        else 
            strcpy(ancrfile, ancrfile+1);
    }
  
    grppha2_do(infile, outfile, version, ncom, commands, numranges,
                 firstrow, lastrow, new_respfile, respfile, new_ancrfile, 
                 ancrfile, root_resp, root_respfile, root_ancr, root_ancrfile,
                 chatter, &status);
    status = 0;
    /*strcpy(subinfo,"..... Error in grppha2_do ");*/
    /*StatusChk(status, subinfo);*/
    return;
}

/***************************************************************************
function:
    grppha2_par

description:
      gets the parameters for the task

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void grppha2_par(char *infile, char *outfile, int *ncom,
                   char commands[][FLEN_BUFFER], int *numranges,
                   int *firstrow, int *lastrow, char *respfile, char *ancrfile,
                   int *clobber, int *chatter, int *status)

*****************************************************************************/
void grppha2_par(char *infile, char *outfile, int *ncom, 
                   char commands[][FLEN_BUFFER], int *numranges, 
                   int *firstrow, int *lastrow, char *respfile, char *ancrfile,
                   int *clobber, int *chatter, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    char subinfo[100];
    char command[FLEN_BUFFER] ;
    char spectra_num[FLEN_BUFFER] ;
    char msg[50]="";
    int i;
   
    strcpy(command, " ");
    strcpy(spectra_num, " ");

    *status = 0;
    strcpy(msg,"chatter");
    Uclgsi(msg, chatter, status);
    if (*status){
       strcpy(subinfo,"..... Error retrieving chatter parameter \n");
       StatusChk(*status, subinfo);
    }
    strcpy(msg,"clobber");
    Uclgsb(msg, clobber, status);
    if (*status){
       strcpy(subinfo,"..... Error retrieving clobber parameter\n");
       StatusChk(*status, subinfo);
    }
    strcpy(msg,"infile");
    Uclgst(msg, infile, status);
    if (infile[0] =='\0') {
       strcpy(subinfo,"..... Error: filename is required\n");
       *status = 1;
       StatusChk(*status, subinfo);
    }
    if (*status) {
       strcpy(subinfo,"..... Error retrieving INFILE parameter \n");
       StatusChk(*status, subinfo);
    }
    CheckInFile(infile);
    strcpy(msg,"outfile");
    Uclgst(msg, outfile, status);
    if (*status){
       strcpy(subinfo,"..... Error retrieving OUTFILE parameter \n");
       StatusChk(*status, subinfo);
    }
    if (outfile[0] =='\0') {
       strcpy(subinfo,"..... Error: filename is required\n");
       *status = 1;
       StatusChk(*status, subinfo);
    }
    CheckFile(outfile, chatter, clobber);

    /** get the spectra number from user **/
    /** provided help on this prompt **/
    for (;;) {
        strcpy(msg,"spectra_num");
        Uclgst(msg, spectra_num, status);
        if (*status){
           strcpy(subinfo,"..... Error reading SPECTRA parameter \n");
           StatusChk(*status, subinfo);
        }

        if (spectra_num[0] =='\0') {
           strcpy(subinfo,"..... Error: spectra number is required\n");
           *status = 1;
           StatusChk(*status, subinfo);
        }

        if ( (! strncasecmp(spectra_num, "HELP", 4)) ||
             (! strncasecmp(spectra_num, "\\?", 2)) )
            help_spectra_num();
        else if (! strncasecmp(spectra_num, "QUIT",4)) exit(1);
        else if (! strncasecmp(spectra_num, "Q",1)) exit(1);
        else if (! strncasecmp(spectra_num, "EXIT",4)) exit(1);
        else if (! strncasecmp(spectra_num, "X",1)) exit(1);
        else {
            /** parsing the range of spectra into meaningful values *****/
            Fcgrgs(spectra_num, INT_MAX, numranges, firstrow, lastrow);
            if (*numranges > MAX_RANGE) { 
                printf("Error: Exceeded maximum range of %d \n", (int)MAX_RANGE);
                *status = 1;
                return;
            } else  
                break;
        }
    }

    /** get the command  from user **/
    for (;;) {
        strcpy(msg,"command");
        Uclgst(msg, command, status);
        if (*status){
           strcpy(subinfo,"..... Error retrieving COMMAND parameter \n");
           StatusChk(*status, subinfo);
        }
        
        /*----------------------------------------------------
        if command has '@' as first character, then
           >>1. read list file to load the commands
           >>2. parse the commands
        else
           parse the command into commands

           COMMAND is what user input &
           COMMANDS are meaningful commands
           NCOM is the number of commands parsed
        -----------------------------------------------------*/ 
        if (command[0] == '@') {
            strcpy(command, command+1); 
            read_cmdfile(command, ncom, commands, status);
            break;
        } else {
            if (! strncasecmp(command, "HELP GROUP", 10)) help_group();
            else if (! strncasecmp(command, "HELP GOOD", 9)) help_good(); 
            else if (! strncasecmp(command, "HELP BAD", 8)) help_bad(); 
            else if (! strncasecmp(command, "HELP SYS", 8)) help_systematics(); 
            else if (! strncasecmp(command, "HELP", 4)) help_help(); 
            else if (! strncasecmp(command, "\\?", 2)) help_help();
            else if (! strncasecmp(command, "QUIT",4)) exit(1);
            else if (! strncasecmp(command, "Q",1)) exit(1);
            else if (! strncasecmp(command, "EXIT",4)) exit(1);
            else if (! strncasecmp(command, "X",1)) exit(1);
            else { 
                parse_command(command, ncom, commands, status);
                break;
            }
        }  
    }
    strcpy(msg,"respfile");
    Uclgst (msg, respfile, status);
    if (*status){
       strcpy(subinfo,"..... Error retrieving RESPFILE parameter \n");
       StatusChk(*status, subinfo);
    }
    strcpy(msg,"ancrfile");
    Uclgst (msg, ancrfile, status);
    if (*status){
       strcpy(subinfo,"..... Error retrieving ANCRFILE parameter \n");
       StatusChk(*status, subinfo);
    }


    return;
}

/***************************************************************************
function:
    read_cmdfile

description:
      gets the commands from the command file
      One command in a line

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void read_cmdfile(char *command, int *ncom, char *commands[][FLEN_BUFFER], 
                  int *status)

*****************************************************************************/
void read_cmdfile(char *command, int *ncom, char commands[][FLEN_BUFFER], 
                  int *status)
{
    char subinfo[100];
    char line[LINE_LENGTH];

    int len;
    int i;

    FILE *fp;

    if ( !(fp = fopen(command, "r"))) {
        sprintf(subinfo,"ERROR: opening command file %s \n", command);
        *status = 1;
        DispMsg(0,0,subinfo);
        return;
    }

    i = 0;
    while (fgets(line, LINE_LENGTH-1, fp)) {
        strcpy(commands[i], line);
        i++;
        if (i > MAX_COMMAND) {
            sprintf(subinfo,"\n Error: Exceeds maximum allowed commands  \n"); 
            DispMsg(0,0, subinfo);
            *status = 1;
            return;
        }
    } 

    *ncom = i;
    for (i=0; i<*ncom; ++i) {
        printf("%d %s\n", i, commands[i]);
        for (;;) {
            if (commands[i][0] == ' ') 
                strcpy(commands[i], commands[i]+1);
            else
                break;
        }
    }
    *status = 0;
    return;
} 

/***************************************************************************
function:
    parse_command

description:
      parses the commands separated by commas and retrieves the commands 

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void parse_command(char *command, int *ncom, char commands[][FLEN_BUFFER],
                   int *status)

*****************************************************************************/
void parse_command(char *command, int *ncom, char commands[][FLEN_BUFFER],
                   int *status)
{
    char *p = NULL;
    char subinfo[100];

    int i;
   
    i = 0;
    p = strtok(command,","); 
    strcpy(commands[i], p);    
    i++;

    while (p = strtok ('\0', ",")) {
        strcpy(commands[i], p);
        i++;
        if (i == MAX_COMMAND) {
            sprintf(subinfo,"\n Error: Exceeds maximum allowed commands  \n");
            DispMsg(0,0, subinfo);
            *status = 1;
            return;
        }
    }

    *ncom = i;
    for (i=0; i<*ncom; ++i) {
        for (;;) {
            if (commands[i][0] == ' ') 
                strcpy(commands[i], commands[i]+1);
            else
                break;
        }
    }
    *status = 0;
    return;
}


/***************************************************************************
function:
      grppha2_do

description:
      perform the commands requested by user 
      writes to output file

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void  grppha2_do(char *infile, char *outfile, char *version, int ncom, 
                   char commands[][FLEN_BUFFER], int numranges,
                   int *firstrow, int *lastrow, int new_respfile, 
                   char *respfile, int new_ancrfile, char *ancrfile, 
                   int root_resp, char *root_respfile, int root_ancr, 
                   char *root_ancrfile, int chatter, int *status);
*****************************************************************************/
void  grppha2_do(char *infile, char *outfile, char *version, int ncom, 
                   char commands[][FLEN_BUFFER], int numranges,
                   int *firstrow, int *lastrow, int new_respfile, 
                   char *respfile, int new_ancrfile, char *ancrfile,
                   int root_resp, char *root_respfile, int root_ancr, 
                   char *root_ancrfile, int chatter, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;       /* required by cfortran.h*/
    fitsfile *infp1=NULL;
    fitsfile *infp2=NULL;

    char function[] = {"grppha2_do in grppha2"};
    char comment[FLEN_BUFFER];
    char keyname[FLEN_KEYWORD];
    char value[FLEN_VALUE];
    char telescop[FLEN_VALUE] = {""};
    char instrume[FLEN_VALUE] = {""};
    char detnam[FLEN_VALUE] = {""};
    char filter[FLEN_VALUE] = {""};
    char phaversn[FLEN_VALUE] = {""};
    char hduclass[FLEN_VALUE] = {""};
    char hduclas1[FLEN_VALUE] = {""};
    char hduclas2[FLEN_VALUE] = {""};
    char hduclas3[FLEN_VALUE] = {""};
    char hduclas4[FLEN_VALUE] = {""};
    char hduvers1[FLEN_VALUE] = {""};
    char dmode[FLEN_VALUE] = {""};
    char chantyp[FLEN_VALUE] = {""};
    char stnullval[FLEN_VALUE] = {""};
    char tmp[FLEN_BUFFER] = {""};

    char *history[NUMHIS];
    char *com[NUMHIS];
    char *rowid[1];

    char **backfil = NULL;
    char **corrfil = NULL;
    char **respfil = NULL;
    char **ancrfil = NULL;
    char **rowidstr = NULL;
    short *channel=NULL;
    short *quality=NULL;
    short *grping=NULL;
    short snullval=0;
    short *conv2=NULL; 

    int extno=0;
    int morekeys=10;
    int hdutype=0;
    int nbackfil;
    int ncorrfil;
    int nrespfil;
    int nancrfil;
    int nXflt;
    int dtype;
    int qerror=0, nqerror=0;
    int qsys=0, nqsys=0;
    int qqual=0,nqqual =0;
    int qgroup=0, nqgroup=0;
    int pois;
    int i, j, k, n;
    int nkeys, keynum;
    int colnum=0;
    int rowid_colnum=0;
    int pha_colnum=0;
    int err_colnum=0;
    int sys_colnum=0;
    int qual_colnum=0;
    int grp_colnum=0;
    int back_colnum=0, corr_colnum=0, resp_colnum=0, ancr_colnum=0;
    int anynul=0;
    int nhis =0;
    int nbackscal=0, ncorrscal =0;
    int firsttime=1;
    int qgroup_new=0, qqual_new=0, qsys_new=0, qerror_new=0;
    int loopnumber = 0;
    int respfil_len=0, ancrfil_len=0;
    int temp_length=0;
    int hdunum=0;
    int found_ext = 0;

    long fchan=0, rows=0;
    long detchans=0;
    long nspec=0;
    long *ipha;
    long lnullval=0L;
    long lvar =0L;
    long long_j=0L;
    long  row1=1, elem1=1;
   
    double areascal=0.0;
    double backscal=0.0;
    double corrscal=0.0;
    double *serr;
    double *syserr;
    double enullval = 0.0;
    double rval=0.0;
    double *pha;
    double texpos=0.0;
    double *rawcounts;

    FILE *file1, *file2;
    char name[80];
    int no_respfile=0, no_ancrfile=0;

/*------------------------------------------------------------------------
     open infile for reading  & outfile for writing
------------------------------------------------------------------------*/
   *status=0;
   remove(outfile);
   if (ffopen(&infp1, infile, READONLY, status)) *status = 0;
   if (ffinit(&infp2, outfile, status)) *status = 0;
   if (ffcopy(infp1, infp2, morekeys, status)) *status = 0;
   if (ffcrhd(infp2, status)) *status = 0; 

/*------------------------------------------------------------------------
    get total hdunumber from input file
    needed later to copy other extensions from input to output
------------------------------------------------------------------------*/
   if (ffthdu(infp1, &hdunum, status)) Printerror(*status);
 /*------------------------------------------------------------------------
    look for SPECTRUN extension
    keep on copying the extensions before SPECTRUM extensions
    from input file to output file
------------------------------------------------------------------------*/
 extno = 1;
   while(*status != 107)
   {
      extno = extno +1;
      ffmahd(infp1, extno, &hdutype, status);

      if (ffgkys(infp1, "EXTNAME", keyname, comment, status)) *status = 0;
      else {
          if ((strcmp(keyname,"SPECTRUM")) == 0) found_ext = 1;
      }
    
      if ( ! found_ext) {
          if (ffgkys(infp1, "HDUCLAS1", keyname, comment, status)) *status = 0;
          else {
              if ((strcmp(keyname,"SPECTRUM")) == 0) found_ext = 1;
          }
      }

 if ((strcmp(keyname,"SPECTRUM")) == 0) {
          if (chatter >= 10) printf("Found SPECTRUM extension \n");

       *status = readphaIIkeywd(infp1,  chatter, telescop, instrume,
                                detnam, filter, phaversn, hduclass,
                                hduclas1, hduclas2, hduclas3, hduclas4,
                                hduvers1, &fchan, &texpos, &areascal,
                                &nbackscal, &ncorrscal, &nbackfil,
                                &ncorrfil, &nrespfil, &nancrfil, dmode,
                                &detchans, chantyp, &dtype, &qerror, &qsys,
                                &qqual, &qgroup, &pois, &nspec) ;
       *status = 0;
       if ( *status) Printerror(status);
      break;
      }
      else {
          if (ffcopy(infp1, infp2, 0, status)) Printerror(*status);
          if (ffcrhd(infp2, status)) Printerror(*status);
      }

    }
     
    /*------------------------------------------------------------------------
     if the file containing the names of respfile and ancrfile is given by 
     user, then open them and read the no. of files of resp and ancr files
     given by user. 
    ------------------------------------------------------------------------*/
    *status=0;
    if (new_respfile) {
        if (file1 = fopen(respfile, "r")) {
           while ( (fscanf(file1, "%s", name)) != EOF) no_respfile++; 
           fclose(file1);
           nrespfil = no_respfile;
           if (no_respfile != nspec) {
               printf("Error: Mismatch in no. of spectra %d VS no. of response " 
                              "file %d\n", nspec, no_respfile);
               *status = 1;
               return;
           }
        }
        else {
            printf("Error: Could not open the file %s \n", respfile);
            *status = 1;
            return;
        }
    }

    if(new_ancrfile) {
        if (file2 = fopen(ancrfile, "r")) {
            while ( (fscanf(file2, "%s", name)) != EOF) no_ancrfile++;
            fclose(file2);
            nancrfil = no_ancrfile;
            if (no_ancrfile != nspec) { 
                printf("Error: Mismatch in no. of spectra %d VS no. of ancillary " 
                               "file %d\n", nspec, no_ancrfile);
                *status = 1;
                return;
            } 
        }
        else {
            printf("Error: Could not open the file %s \n", ancrfile);
            *status = 1;
            return;
        }
    }
  
    backfil  = (char **) malloc(nbackfil * sizeof(char *));
    corrfil  = (char **) malloc(ncorrfil * sizeof(char *));
    respfil  = (char **) malloc(nrespfil * sizeof(char *));
    ancrfil  = (char **) malloc(nancrfil * sizeof(char *));
    rowidstr = (char **) malloc(1000 * sizeof(char *));
     for (i=0; i<nbackfil; ++i) {
        backfil[i] = malloc(FLEN_VALUE * sizeof(char));
        strcpy (backfil[i], "       ");
    }

    for (i=0; i<ncorrfil; ++i) {
        corrfil[i] = malloc(FLEN_VALUE * sizeof(char));
        strcpy (corrfil[i], "       ");
    }   

    for (i=0; i<nrespfil; ++i) {
        respfil[i] = malloc(FLEN_VALUE * sizeof(char));
        strcpy (respfil[i], "       ");
    }

    for (i=0; i<nancrfil; ++i) {
        ancrfil[i] = malloc(FLEN_VALUE * sizeof(char));
        strcpy (ancrfil[i], "       ");
    }
   *status=0;
    *status = fits_get_num_rows(infp1, &rows, status);
    if (*status != 0 )
        {
         printf ("Problem getting number of rows!\n");
         exit (1);
         }
    rowidstr = (char **) malloc(rows * sizeof(char *));
    for (i=0; i<rows; ++i) {
        rowidstr[i] = malloc(FLEN_VALUE * sizeof(char));
        strcpy (rowidstr[i], "       ");
    }
    /**************************************************8*/
    
    if (fits_get_colnum(infp1, 0, "ROWID", &rowid_colnum, status)) {
	    PrintWarning(function, version, " ... ROWID column not found ", 
			 chatter, 10, status);
    }  else {
     if (fits_read_col_str(infp1, rowid_colnum, row1, elem1, rows,
                                      stnullval, rowidstr, &anynul, status))
                                         Printerror(status);
    }

/***************************************************/

   *status=0;
    if (nbackfil > 1) {
        if (fits_get_colnum(infp1, 0, "BACKFILE", &back_colnum,
                            status)) Printerror(status);
        if (fits_read_col_str(infp1, back_colnum, row1, elem1,  nbackfil,
                              stnullval, backfil, &anynul, status))
                                               Printerror(status);
    }  else {
       if (fits_read_key_str(infp1, "BACKFILE", backfil[0], comment, status))
                                          Printerror(status);
    }

    if (ncorrfil > 1) {
        if (fits_get_colnum(infp1, 0, "CORRFILE", &corr_colnum,
                            status)) Printerror(status);
        if (fits_read_col_str(infp1, corr_colnum, row1, elem1,  ncorrfil,
                              stnullval, corrfil, &anynul, status))
                                     Printerror(status);
    }  else {
       if (fits_read_key_str(infp1, "CORRFILE", corrfil[0], comment, status))
                                     Printerror(status);
    }
   
    if (new_respfile) {
        no_respfile = 0;
        printf("new_respfile\n");
        if (file1 = fopen(respfile, "r")) {
           while ( (fscanf(file1, "%s", name)) != EOF) {
               strcpy(respfil[no_respfile], name);
               no_respfile++; 
           
           }
            
           fclose(file1);
       }
   
    }   else { 
        if (nrespfil > 1) { 
            if (! root_resp) {
                if (fits_get_colnum(infp1, 0, "RESPFILE", &resp_colnum,
                                    status)) Printerror(status);
                if (fits_read_col_str(infp1, resp_colnum, row1, elem1,  nrespfil,
                                      stnullval, respfil, &anynul, status))
                                         Printerror(status);
            } else {
                if (fits_get_colnum(infp1, 0, "ROWID", &rowid_colnum,
                                    status)) Printerror(status);
                if (fits_read_col_str(infp1, rowid_colnum, row1, elem1,  nrespfil,
                                      stnullval, respfil, &anynul, status))
                                         Printerror(status);
                for (n = 0; n < nrespfil; n++) {
                    strcpy(tmp, root_respfile);
                    strcat(tmp, "_");
                    strcat(tmp, respfil[n]);
                    strcat(tmp, ".rmf");
                    strcpy(respfil[n], tmp);
                }  
            }
        }  else {
        *status = 0;
         respfil[0] = malloc(100);
	 if (fits_read_key_str(infp1, "RESPFILE", respfil[0], comment, status))
          printf(" ...RESPFILE keyword not found!\n");
	  *status = 0; 
        }
    }
    
     
    if (nrespfil > 1) {  /* find the longest filename */
        for (n = 0; n < nrespfil; n++) {
            temp_length = strlen(respfil[n]) ;
            respfil_len = temp_length > respfil_len?temp_length:respfil_len ; 
        }
    }

    if (new_ancrfile) {
        no_ancrfile = 0;
        if (file2 = fopen(ancrfile, "r")) {
           while ( (fscanf(file2, "%s", name)) != EOF) {
               strcpy(ancrfil[no_ancrfile], name);
               no_ancrfile++; 
           }
           fclose(file2);
        }
    } else {
      if (nancrfil > 1){
	*status = 0;
            if (! root_ancr) {
                if (fits_get_colnum(infp1, 0, "ANCRFILE", &ancr_colnum,
                                    status)) *status = 0;
                if (fits_read_col_str(infp1, ancr_colnum, row1, elem1,  nancrfil,
                                      stnullval, ancrfil, &anynul, status))
                                               *status=0;
                    
            } else {
                if (fits_get_colnum(infp1, 0, "ROWID", &rowid_colnum,
                                    status)) Printerror(status);
                if (fits_read_col_str(infp1, rowid_colnum, row1, elem1, nancrfil,
                                      stnullval, ancrfil, &anynul, status))
                                         Printerror(status);
             
                for (n = 0; n < nancrfil; n++) {                             
                    strcpy(tmp, root_ancrfile);
                    strcat(tmp, "_");
                    strcat(tmp, ancrfil[n]);
                    strcat(tmp, ".arf");
                    strcpy(ancrfil[n], tmp);
                }
            }
        } else {
           ancrfil[0] = malloc(100);
           if (fits_read_key_str(infp1, "ANCRFILE", ancrfil[0], comment, status))
           printf(" ...ANCRFILE keyword not found!\n");
           *status = 0;
        }
    }
   
    if (nancrfil > 1) {  /* find the longest filename */
        for (n = 0; n < nancrfil; n++) {
            temp_length = strlen(ancrfil[n]) ;
            ancrfil_len = temp_length > ancrfil_len?temp_length:ancrfil_len; 
        }
    }

    if (chatter >= 10) printf("Returned from reading SPECTRUM extension\n");
 /*------------------------------------------------------------------
    check if user has entered '-' in the spectra number,
    last row should be the last spectrum number
--------------------------------------------------------------------*/
    if (lastrow[numranges-1] == INT_MAX) lastrow[numranges-1] = nspec;
    for (i=0; i<numranges; ++i){
        if (firstrow[i]  > nspec) {
            numranges = i;
            printf("** Warning: Input row > total rows available %d\n", nspec);
            if (i==0) {
                printf("...... starting input row no. > "
                                "total available rows %d\n", nspec);
                *status = 1;
                return;
            }
            else
                printf("...... will do only upto  %d  rows\n", lastrow[i-1]);

            break;
        }
        else if (lastrow[i] > nspec) {
            lastrow[i] = nspec;
            numranges = i+1;
            printf("** Warning: Input row > total rows available %d\n", nspec);
            printf("...... will do only upto total available rows %d\n", nspec);
            break;
        }
    }

    for (i=0; i< NUMHIS; ++i){
        history[i] = (char *) malloc (FLEN_BUFFER*sizeof(char));
        com[i] = (char *) malloc (FLEN_BUFFER*sizeof(char));
        strcpy(history[i], "  ");
        strcpy(com[i], "  ");
    }
 
    nhis = 0;
    sprintf(history[nhis], "The output file is written by \"GRPPHA2\"");
    nhis++;
    sprintf(history[nhis], "The input file is \"%s\"",infile);
    nhis++;
    strcpy(history[nhis], "This file is the result of grouping of "
                          "channels as follows:");
    nhis++;
    for (i=0; i<ncom; ++i, nhis++) {
        ffupch(commands[i]);
        strcpy(history[nhis],commands[i]);
    }

    nhis++;
    strcpy(history[nhis], "Grouping is performed on the following spectra "
                           "(rows) of the");
    nhis++;
    strcpy(history[nhis]," input file  "); 
    strcat(history[nhis], infile);

    nhis++;
    for (i=0; i<numranges; ++i, nhis++)
            sprintf(history[nhis], "       %d  - %d", firstrow[i], lastrow[i]);

    nhis++;
 
/*-------------------- READ DATA ---------------------*/
                  /* CHANNELS */
    channel = (short *) malloc(detchans*sizeof(short));

    assert (channel != NULL);

    for (i=0; i< detchans; ++i) channel[i] = 0;

    if (fits_get_colnum(infp1, 0, "CHANNEL", &colnum,
                        status)) Printerror(status);

    if (fits_read_col_sht(infp1, colnum, 1L, 1L, detchans,
                          snullval, channel, &anynul,
                          status)) Printerror(status);

    if (qerror) {  /** STAT_ERR **/
        if (fits_get_colnum(infp1, 0, "STAT_ERR", &err_colnum,
                            status)) Printerror(status);
    }
    serr = (double*) malloc(detchans*sizeof(double));
    assert (serr != NULL);

    if (qsys) { /** SYS_ERR **/
        if (fits_get_colnum(infp1, 0, "SYS_ERR", &sys_colnum,
                            status)) Printerror(status);
    }
    syserr = (double*) malloc(detchans*sizeof(double));
    assert (syserr != NULL);

    if (qqual) { /** QUALITY **/
        if (fits_get_colnum(infp1, 0, "QUALITY", &qual_colnum, status))
               Printerror(status);
    }
    quality = (short *) malloc(detchans*sizeof(short));
    assert (quality != NULL);

    if (qgroup) {
        if (fits_get_colnum(infp1, 0, "GROUPING", &grp_colnum,
                            status)) Printerror(status);
    }
    grping = (short *) malloc(detchans*sizeof(short));
    assert (grping != NULL);

    for (i=0; i< detchans; ++i) {
        serr[i] = 0.0;
        syserr[i] = 0.0;
        quality[i] = 0;
        grping[i] = 0;
    }
     /******************** COUNTS or RATE ********************/
    if (dtype == 1) { 
        ipha = (long *) malloc(detchans*sizeof(long));
        for (i=0; i<detchans; ++i) ipha[i] = 0L;
    } else {
        pha = (double *) malloc(detchans*sizeof(double));
        for (i=0; i<detchans; ++i) pha[i] = 0.0; 
    }

    rawcounts = (double *) malloc(detchans*sizeof(double));
    for (i=0; i<detchans; ++i) rawcounts[i] = 0.0;

    if (!qerror) {
        if (! pois) {
            if (fits_read_key_dbl(infp1, "STAT_ERR", &rval, comment, status))
                *status = 0;
            else {
                qerror_new = 1;
                nqerror = 1;
                for (i=0; i<detchans; ++i) serr[i] = rval;
            }
        }
    }

    if (dtype == 1) { 
        if (fits_get_colnum(infp1, 0, "COUNTS", &pha_colnum, status)) 
                                       Printerror(status);
    } else {      /** dtype == 2 **/
        if (fits_get_colnum(infp1, 0, "RATE", &pha_colnum, status)) 
                                       Printerror(status);
    }

    rowid[0] = (char *) malloc(FLEN_BUFFER*sizeof(char));
    conv2 = (short *) malloc(detchans*sizeof(short));
    assert (conv2 != NULL);
  
    for (i=0; i<numranges; ++i){
        /** read one row at a time **/
        for (j=firstrow[i]; j<=lastrow[i]; ++j){ 
            long_j = (long)j;
            if (dtype == 1) { 
                if (fits_read_col_lng(infp1, pha_colnum, long_j, 1L, detchans,
                                      lnullval, ipha, &anynul, status)) 
                                      Printerror(status);
                for (k=0; k<detchans; ++k) rawcounts[k] = (double) ipha[k];
            } else { 
                if (fits_read_col_dbl(infp1, pha_colnum, long_j, 1L, detchans,
                                      enullval, pha, &anynul, status))
                                      Printerror(status);
                for (k=0; k<detchans; ++k) rawcounts[k] = pha[k] * texpos;
            }

            if (qerror) {
                qerror_new=1;
                if (fits_read_col_dbl(infp1, err_colnum, long_j, 1L, detchans,
                  enullval, serr, &anynul, status)) Printerror(status);
            }

            if(qsys) {
                if (fits_read_col_dbl(infp1, sys_colnum, long_j, 1L, detchans,
                  enullval, syserr, &anynul, status)) Printerror(status);
            } 
         
            if(qqual) {
                if (fits_read_col_sht(infp1, qual_colnum, long_j, 1L, detchans,
                  enullval, quality, &anynul, status)) Printerror(status);
            } 
         
            if(qgroup) {
                if (fits_read_col_sht(infp1, grp_colnum, long_j, 1L, detchans,
                  enullval, grping, &anynul, status)) Printerror(status);
            }
 
            for (k=0; k<ncom; ++k) {
                ffupch(commands[k]);

                if (! strncmp(commands[k], "GROUP MIN ", 9 )) 
                    group_min(commands[k], detchans, channel, rawcounts, 
                              &qgroup_new, grping, &qqual_new, quality, 
                              loopnumber, chatter, status);

                else if (! strncmp(commands[k], "GROUP ", 6 )) 
                    group_chan(commands[k], detchans, channel, &qgroup_new, 
                               grping, loopnumber, chatter, status);

                else if (! strncmp(commands[k], "GOOD", 4 )) 
                    group_good(commands[k], detchans, channel, &qqual_new, 
                               quality, loopnumber, chatter, status);

                else if (! strncmp(commands[k], "BAD", 3 )) 
                    group_bad(commands[k], detchans, channel, &qqual_new, 
                              quality, loopnumber, chatter, status);

                else if (! strncmp(commands[k], "SYSTEMATICS", 10 )) 
                    group_systematics(commands[k], detchans, channel, 
                             &qsys_new, syserr, loopnumber, chatter, status);
       
                else {
                    printf(" Unknown command  \"%s\"\n", commands[i]);
                    *status=1;
                }
                if (*status) {
                    printf("Error in command \"%s\"\n", commands[i]);
                    return;
                }
            }


            /* ---------------------------------------------------------
            first check if this is first time, then write all the keywords 
            and the write the data.  Otherwise write the data
            -----------------------------------------------------------*/
      
            if (firsttime) {
                *status = writephaIIkeywd(infp2, chatter, nhis, history, ncom, 
                              com, keyname, telescop, instrume, detnam, filter, 
                              phaversn, hduclas2, fchan, texpos, areascal,
                              nbackscal, backscal, ncorrscal, corrscal,
                              nbackfil, backfil[0], ncorrfil, corrfil[0],
                              nancrfil, ancrfil[0], ancrfil_len, nrespfil, 
                              respfil[0], respfil_len, dmode, detchans, 
                              chantyp, dtype, qerror_new, qsys_new, qqual_new, 
                              qgroup_new, detchans, nspec) ;          
                firsttime = 0;
            }
            /*---------------------------------------------------
                    write the data 
            -----------------------------------------------------*/
         
            colnum= 1;
            conv2[0] = (short)(j-1);
            *status=0;
         
            if (fits_write_col(infp2, TSHORT, colnum, row1, elem1, 1L,
                            conv2, status)) Printerror(status);
 
            /* Write column 2: ROWID (1-element array) */
            colnum++;
            /*sprintf(rowid[0], "SPEC_%04d", j);*/
    
            strcpy(rowid[0],rowidstr[j-1]);
            
            if (fits_write_col(infp2, TSTRING, colnum, row1, elem1, 1L, 
                                rowid, status)) Printerror(status);;
       
            /* Write column 3: CHANNEL (detchans-element array) */
            colnum++;
            for (k=0; k<detchans; k++)
                if (fits_write_col(infp2, TSHORT, colnum, row1, elem1, 
                               detchans, channel, status)) Printerror(status);
       
            /* Write column 4: COUNTS/RATE (detchans-element array) */
            colnum++;
            if (dtype == 1)
            {   
                if (fits_write_col(infp2, TLONG, colnum, row1, elem1, 
                           detchans, ipha, status))
                *status=0;
            }
            else
            {
                if (fits_write_col(infp2, TDOUBLE, colnum, row1, elem1, 
                           detchans, pha, status)) 
               *status=0;
            }

            /* Write column 5: STAT_ERROR (detchans-element array) */
            if (qerror_new) {
                colnum++;
                if (fits_write_col(infp2, TDOUBLE, colnum, row1, elem1,
                               detchans, serr, status))
                *status=0;
            }
    
            /* Write column 6: SYS_ERROR (detchans-element array) */
            if (qsys_new) {
                colnum++;
                if (fits_write_col(infp2, TDOUBLE, colnum, row1, elem1,
                           detchans, syserr, status)) Printerror(status);
            }
            /* Write column 7: QUALITY (detchans-element array) */
            if (qqual_new) {
                colnum++;
                if (fits_write_col(infp2, TSHORT, colnum, row1, elem1,
                             detchans, quality, status)) Printerror(status);
            }

            /* Write column 8: GROUPING (detchans-element array) */
            if (qgroup_new) {
                colnum++;
                if (fits_write_col(infp2, TSHORT, colnum, row1, elem1,
                              detchans, grping, status)) Printerror(status);
            }

            /* Write column 9: BACKFIL (1-element array) */
            if (nbackfil > 1) {
                colnum++;
                if (fits_write_col_str(infp2,  colnum, row1, elem1,
                               1L, &backfil[j-1], status)) Printerror(status);
            }

            /* Write column 10: CORRFIL (1-element array) */
            if (ncorrfil > 1) {
                colnum++;
                if (fits_write_col_str(infp2,  colnum, row1, elem1,
                               1L, &corrfil[j-1], status)) Printerror(status);
            }

            /* Write column 11: RESPFIL (1-element array) */
            if (nrespfil > 1) {
                colnum++;
                if (fits_write_col_str(infp2,  colnum, row1, elem1,
                               1L, &respfil[j-1], status)) Printerror(status);

            }

            /* Write column 12: ANCRFIL (1-element array) */

            if (nancrfil > 1) {
                colnum++;
                if (fits_write_col_str(infp2,  colnum, row1, elem1,
                               1L, &ancrfil[j-1], status)) Printerror(status);
            }

            strcpy(keyname, "NAXIS2");
            if (fits_modify_key_lng(infp2, keyname, row1, "&", status))
               Printerror(status);
              
            row1++;
            loopnumber++;
           
        }  /** end of j loop for row numbers **/ 
    }   /** end of i loop for numranges **/
    
     /* if (ffclos(infp2, status)) Printerror(status);*/

     /*---------------------------------------------------------
      Now to delete the COMMENTS from primary extension
      COMMENTS containing from input file info that the file
      was written by ISDC DAL version etc etc.
     ----------------------------------------------------------*/
   
   /*  if (outfile[0] == '!')
       {
         ffopen(&infp2, outfile+1, READWRITE, status);
  
       }
     else
       {
         ffopen(&infp2, outfile, READWRITE, status);
	 }*/
    
     if (fits_get_hdrpos(infp2, &nkeys, &keynum, status)) Printerror(*status);
    
     for(i=0; i<nkeys; ++i){
         if (ffgkyn(infp2, i, keyname, value, comment, status)) 
            Printerror(*status);
         if (! strcmp(keyname, "COMMENT")) {
             if (strstr(comment, "ISDC"))
                 break;
         }
     }

     if (i < nkeys) {
         for (k=i; k<=nkeys; ++k) {  
             if (ffdrec(infp2, k, status)) 
	       *status=0;
         }
     }
     
     /*----------------------------------------------------------
      Add comments
     ------------------------------------------------------------*/
     ncom = 0;
     sprintf(com[ncom],  "This file has been created by GRPPHA2 ");
     strcat(com[ncom], version);

     if (ffpcom(infp2, com[ncom], status)) *status=0;
     /*----------------------------------------------------------
      Free the memory
     ------------------------------------------------------------*/
     free (channel); channel = NULL;
     if (dtype == 1) { free (ipha); ipha = NULL; }
     else {free (pha); pha = NULL; }
     free (rawcounts); rawcounts = NULL;

     free (serr); serr = NULL;
     free (syserr); syserr = NULL;
     free (quality); quality = NULL;
     free (grping); grping = NULL;
     free (conv2); conv2 = NULL;
     free(rowid[0]); rowid[0] = NULL;

     for (i=0; i< NUMHIS; ++i) {
         free(history[i]);  history[i] = NULL;
         free(com[i]);  com[i] = NULL;
     }
     /*----------------------------------------------------------
      Copy other extensions from input file (if any)
     ------------------------------------------------------------*/
     for (extno=extno+1; extno<=hdunum; extno++){
         if (ffmahd(infp1, extno, &hdutype, status))*status=0 ; 
         if (ffcrhd(infp2, status)) *status=0 ;
         if (ffcopy(infp1, infp2, 0, status)) *status=0 ;
     }
     
     if (ffclos(infp1, status)) *status = 0;
     if (ffclos(infp2, status)) *status = 0; 
     return;
} 

/***************************************************************************
function:
      group_min

description:
      perform the command 'GROUP MIN mincount'  

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void group_min(char *command, int nchan, double *rawcounts, 
               int *qgroup, short *grping, int *qqual_new, short *quality,
               int loopnumber, int chatter, int *status)

*****************************************************************************/
void group_min(char *command, int nchan, short *channel, double *rawcounts, 
               int *qgroup_new, short *grping, int *qqual_new, short *quality,
               int loopnumber, int chatter, int *status)
{
    char subinfo[80];

    int i, j;
    int mincount = 0;
    int rebin;
    int startbin;

    double sum_raw_cts = 0.; 

    if (sscanf(command, "%*s %*s %d", &mincount)) {
        if (mincount == 0) {
            sprintf(subinfo, "No Minimum counts value provided !\n");
            DispMsg(0,0, subinfo);
            *status = 1;
            return;
        }
    } else {
        *status = 1;
        sprintf(subinfo, "invalid integer on command line !\n");
        DispMsg(0,0, subinfo);
        return;
    }
    if (*status) {
        sprintf(subinfo, "SYNTAX : GROUP MIN MINCTS\n");
        DispMsg(0,0, subinfo);
        sprintf(subinfo, "FOR EXAMPLE : GROUP MIN 60\n");
        DispMsg(0,0, subinfo);
        return;
    }

    i = 0;
    while (i<nchan){
        rebin = 0;
        sum_raw_cts = 0.0;
        if ( quality[i] ) grping[i] = 0;  /* quality is not 0, 
                                             that is, bad channel  */ 
        else {
            sum_raw_cts += rawcounts[i];
            if (sum_raw_cts >= mincount) grping[i] = 1;
            else {
                rebin = 1;
                startbin = i;
                i++;
                /* go binning until threshold count (mncount) is reached */
                while ( (rebin) && (i<nchan) ) {
                    if (! quality[i]) sum_raw_cts += rawcounts[i];
                    if (sum_raw_cts >= mincount ) rebin = 0;
                    else  i++;
                }
                /* ------------------------------------------------------------
                now check if rebin is still true. If so, there is spare channel 
                because the grouping did not complete 
                ---------------------------------------------------------------*/
                if (rebin) {
                    if (! loopnumber) {
                        printf("Spare channels are %d : %d\n" ,
                              channel[startbin], channel[nchan-1]);
                    }

                    for (j=startbin; j<i; ++j) {
                        grping[j] = 0;
                        quality[i] = 2;
                    }
                    *qqual_new = 1;
                } else {
                    grping[startbin] = 1;
                    for (j=startbin+1; j<i; ++j) grping[j] = -1;
                }
            }        /* end of sum_raw_cts >= mincount clause  */
        }            /* end of good quality */
        i++;
    } 
    *qgroup_new = 1;
    *status = 0; 

    return;
} 


/***************************************************************************
function:
      group_chan

description:
      perform the command 'GROUP MINCHAN MAXCHAN NUMCHAN'

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void group_chan(char *command, int nchan, short *channel, int *qgroup, 
                short *grping, int loopnumber, int chatter, int *status)

*****************************************************************************/
void group_chan(char *command, int nchan, short *channel, int *qgroup_new, 
                short *grping, int loopnumber, int chatter, int *status)
{
    char subinfo[80];
    char errmsg[] ={ "Error in parsing the command GROUP "};
    char syntax[] ={" SYNTAX:  GROUP minchan maxchan numchan \n"
                    " For example:  GROUP  10  20  3 \n"
                    " will group 10-12, 13-15, 16-18, and 19 and 20 are \n"
                    " spare channel \n"
                    " Please try again !\n"
                   };

    int startchan=0, lastchan=0;
    int minchan=0, maxchan=0, numchan=0;
    int rebin = 0;
    int nmiss = 0;
    int missingchan[MAXMISS];
    int i=0;
    int index1=0, index2=0;
    int found_first_chan=0, found_last_chan=0;
    int curchan=0;
    int prebin=0;

    if (sscanf(command, "%*s %d %d %d", &minchan, &maxchan, &numchan)) {
        if (numchan == 0) {
            sprintf(subinfo, "How many channels to group is not specified!\n");
            DispMsg(0,0, subinfo);
            *status = 1;
            return;
        }
    } else {
        *status = 1;
        sprintf(subinfo, "invalid integer on command line !\n");
        DispMsg(0,0, subinfo);
    }
    if (*status) {
        DispMsg(0,0, errmsg);
        DispMsg(0,0, syntax);
        return;
    }

    if (maxchan > channel[nchan-1]) {
        printf(" Maxchan exceeds that of available channels = %d \n",
                                   channel[nchan-1]);
        *status = 1;
        return;
    }
   

    startchan = minchan;
    do {
        lastchan = startchan + numchan -1;
        if (lastchan > maxchan) {
            if (! loopnumber) {
                printf("\nSetting channels  %d  to  %d  as spare channels\n\n", 
                                    startchan, maxchan);
            }
            break;
        }
        /* find out the first bin and last bin */
        for (i=0; i<nchan; ++i) {
            if (channel[i] == startchan) {
                 index1 = i;
                 found_first_chan = 1;
            }
 
            if (channel[i] == lastchan) {
                 found_last_chan = 1;
                 index2 = i;
                 break;
            }
        }
        /* check if either of the startchan and lastchan are not found  */
        if (! found_first_chan) {
            if (! found_last_chan) {
                if (! loopnumber) {
                    printf(" Channels  %d  to  %d not present \n", 
                                      startchan, lastchan);
                }
            } else {
                if (! loopnumber) {
                    printf(" Channel  %d  not present \n",startchan);
                    printf("\n Setting channels  %d  to  %d  as spare "
                               "channels\n\n", startchan,lastchan);
                }
            }
        }
        else if (! found_last_chan) {
            if (! loopnumber) {
                printf(" Channel  %d  not present \n", lastchan);
                printf("\nSetting channels  %d  to  %d  as spare channels\n\n", 
                                         startchan,lastchan);
            }
        }
        else if ((found_first_chan) && (found_last_chan)) {
        /*----------------------------------------------------
        here we have found the  start and last bin
         1.  check if there is missing channels in between 
         2.  check to ensure that previous binning is not overwritten
        --------------------------------------------------------------*/
            /* 1. check for missing channels  */
            curchan = startchan +1;
            for (i=index1+1; i<=index2; ++i) {
                 if (channel[i] != curchan) {
                     missingchan[nmiss] = curchan;
                     nmiss++;
                 }
                 curchan++;
             }
             if (nmiss) {
                 if (! loopnumber) {
                     printf(" Missing channel in GROUP  %d  %d  %d\n",
                                                minchan, maxchan, numchan);
                     printf(" Missing channels are :\n");
                     for (i=0; i<nmiss; ++i)  printf(" %d ",missingchan[i]);
                     printf(" Grouping  %d  to %d  is still performed\n",  
                                           minchan, maxchan);
                 }
            }
            /* 2. chack for previous binning */
            prebin = 0;
            for (i=index1; i<= index2; ++i) {
                if (i == index2) {  /** this if is added as it gives array 
                                        bound from purify  **/
                     if (grping[i] == -1) prebin = 1;
                } else {
                    if (((grping[i] == 1) && (grping[i+1] == -1)) || 
                                    (grping[i] == -1)) {
                        prebin = 1;
                        break;
                    }
               }
            }
            if (prebin) {
                if (! loopnumber) {
                    printf(" Overlap with pre-existing grouping for "
                           "channels  %d  to  %d \n", startchan, lastchan);
                }
            } else {
                grping[index1] = 1;
                for (i=index1+1; i<= index2; ++i)  grping[i] = -1;
            }
            *qgroup_new = 1;             /* grouping is true for this loop */
            *status = 0;
        }
        startchan = lastchan + 1;
        if (startchan > maxchan) break;
        rebin = 1;
    } while (rebin);

    return;
}

/***************************************************************************
function:
      group_good

description:
      perform the command 'GOOD MINCHAN MAXCHAN '

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void group_good(char *command, int nchan, short *channel, int *qqual_new, 
                short *quality, int loopnumber, int chatter, int *status)

*****************************************************************************/
void group_good(char *command, int nchan, short *channel, int *qqual_new, 
                short *quality, int loopnumber, int chatter, int *status)
{
    char subinfo[80];
    char errmsg[] ={ "Error in parsing the command GOOD "};
    char syntax[] ={" SYNTAX:  GOOD minchan maxchan \n"
                    " For example: GOOD  10  20  \n"
                    " channels between minachan and maxchan (inclusive)\n"
                    " are set good  (quality=0) \n"
                    " Please try again !\n"
                    };

    int qualflg = 0;
    int minchan=0, maxchan=0;
    int found_first_chan=0, found_last_chan=0;
    int i;

    *status = 0;
    if (sscanf(command, "%*s %d %d ", &minchan, &maxchan)) {
        if (chatter >= 10) 
            printf("Found minchan= %d  and maxchan=%d\n", minchan, maxchan);
    } else {
        *status = 1;
        sprintf(subinfo, "invalid integer on command line !\n");
        DispMsg(0,0, subinfo);
    }
    if (*status) {
        DispMsg(0,0, errmsg);
        DispMsg(0,0, syntax);
        return;
    }

    find_channel(minchan, maxchan, nchan, channel, &found_first_chan,
                 &found_last_chan, loopnumber);
    set_qual_flag(minchan, maxchan, qualflg, quality, found_first_chan,
                  found_last_chan, channel, nchan, qqual_new);
 
    return;
}

/**********************************************************************
function:
      set_qual_flag

description:
      sets the quality flag of the channels to desired flag value = qualflg

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void set_qual_flag(int startchan, int lastchan, int qualflg, short *quality,
                   int found_first_chan, int found_last_chan, short *channel,
                   int nchan, int *qqual)

**********************************************************************/
void set_qual_flag(int startchan, int lastchan, int qualflg, short *quality, 
                   int found_first_chan, int found_last_chan, short *channel, 
                   int nchan, int *qqual_new)
{
    int setqual = 1;
    int i=0;

    *qqual_new = 0;
    if ((found_first_chan) && (found_last_chan)) {
        while (setqual) {
            if ((channel[i] >= startchan) && (channel[i] <= lastchan)) {
                quality[i] = qualflg;
                if (channel[i] == lastchan) setqual = 0;
            }
            ++i;
        }
    }

    for(i=0; i<nchan;++i){ 
        if (quality[i]) { *qqual_new = 1; break; }
    }
    return;
}

/************************************************************************
function:
      find_channel

description:
      finds if the channels required are available in the input channels
      
author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void find_channel(int startchan, int lastchan, int nchan, short *channel,
                  int *found_first_chan, int *found_last_chan, int loopnumber)

*************************************************************************/
void find_channel(int startchan, int lastchan, int nchan, short *channel, 
                  int *found_first_chan, int *found_last_chan, int loopnumber)
{
    int i=0;

    /* find out the firstchan and lastchan */
    for (i=0; i<nchan; ++i) {  
        if (channel[i] == startchan) {
             *found_first_chan = 1;
        } 
        else if  (channel[i] == lastchan) {
             *found_last_chan = 1;
             break;
        }
    }

    /* check if either of the startbin and lastbin are not found  */
    if (! *found_first_chan) {
        if (! *found_last_chan) {
            if (! loopnumber) {
                printf(" Channels  %d  to  %d not present \n", 
                                        startchan, lastchan);
            }
        } else {
            if (! loopnumber) {
                printf(" Channel  %d  not present \n",startchan);
                printf("\nSetting channels  %d  to  %d  as spare channels\n\n", 
                       startchan,lastchan);
            }
        }
    }
    else if (! *found_last_chan) {
           if (! loopnumber) {
               printf(" Channel  %d  not present \n", lastchan);
               printf("\nSetting channels  %d  to  %d  as spare channels\n\n", 
                      startchan,lastchan);
           }
    }

    return;
}

/*********************************************************************
function:
      group_bad

description:
      groups the bad channels defined by user 
      that is, sets the channels to bad quality = 5 

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void group_bad(char *command, int nchan, short *channel, int *qqual_new,
                short *quality, int loopnumber, int chatter, int *status)

*************************************************************************/
void group_bad(char *command, int nchan, short *channel, int *qqual_new, 
                short *quality, int loopnumber, int chatter, int *status)
{
    char subinfo[80];
    char *p=NULL;
    char errmsg[] ={ "Error in parsing the command BAD "};
    char syntax[] ={" SYNTAX:  BAD minchan maxchan \n"
                    " For example: BAD  10    20  \n"
                    " channels between minachan and maxchan (inclusive)\n"
                    " are set bad (quality=5) \n"
                    " Please try again !\n"
                   };

    int minchan=0, maxchan=0;
    int found_first_chan=0, found_last_chan=0;
    int qualflg = 5;
    int i=0;

    *status = 0;
    if (sscanf(command, "%*s %d %d ", &minchan, &maxchan)) {
        if (chatter >= 10) 
            printf("Found minchan= %d  and maxchan=%d\n", minchan, maxchan);
    } else {
        *status = 1;
        sprintf(subinfo, "invalid integer on command line !\n");
        DispMsg(0,0, subinfo);
    }
    if (*status) {
        DispMsg(0,0, errmsg);
        DispMsg(0,0, syntax);
        return;
    }

    find_channel(minchan, maxchan, nchan, channel, &found_first_chan,
                 &found_last_chan, loopnumber);

    set_qual_flag(minchan, maxchan, qualflg, quality, found_first_chan,
                  found_last_chan, channel, nchan, qqual_new);

    *status=0;
    return;
}

/********************************************************************
function:
      group_systematics

description:
      sets the systematic error column of the channels to user defind
      error.  If user defines err=0.03, that is the systematic error
      of 3% should ve added to the error calculation of the channels.

author:
      Banashree M Seifert (March, 1999)

modification history:

usage:
void group_systematics(char *command, int nchan, short *channel,            
                       int *qsys_new, double *syserr, int loopnumber, 
                       int chatter, int *status)

*********************************************************************/
void group_systematics(char *command, int nchan, short *channel,            
                       int *qsys_new, double *syserr, int loopnumber, 
                       int chatter, int *status)
{
    char subinfo[80];
    char errmsg[] ={ "Error in parsing the command SYSTEMATICS "};
    char syntax[] ={" SYNTAX:  SYSTEMATICS minchan maxchan err \n"
                    " For example:  SYSTEMATICS  10  20  0.03\n"
                    " The channels between 10 and 20 (inclusive) will have \n"
                    " a fractional systematiocs error defined by err.\n"
                    " An err = 0.03 corresponds to a systematic error of 3% \n"
                    " of the observed  PHA  count rate for that channel.\n"
                    " Please try again !\n"
                   };

    int found_first_chan=0;
    int found_last_chan=0;
    int minchan=0, maxchan=0;
    int setsys = 1;
    int i = 0;

    double err = 0.0;

    *status = 0;
    if (sscanf(command, "%*s %d %d %lf",  &minchan, &maxchan, &err)) {
        if (chatter >= 10) 
            printf("Found minchan= %d  and maxchan=%d & error=%f\n", 
                            minchan, maxchan, err);
    } else {
        *status = 1;
        sprintf(subinfo, "invalid integer on command line !\n");
        DispMsg(0,0, subinfo);
    }
    if (*status) {
        DispMsg(0,0, errmsg);
        DispMsg(0,0, syntax);
        return;
    }

    find_channel(minchan, maxchan, nchan, channel, &found_first_chan,
                 &found_last_chan, loopnumber);

    if ((found_first_chan) && (found_last_chan)) {
        while (setsys) {
            if ((channel[i] >= minchan) && (channel[i] <= maxchan)) {
                syserr[i] = err;
                if (channel[i] == maxchan) setsys = 0;
            }
            ++i;
        }
    }
    for(i=0; i<nchan;++i){
        if (syserr[i]) { *qsys_new = 1;   break; }
    }

    *status=0;
    return;
}

/*--------------------------------------------------------------------
                   help_help
---------------------------------------------------------------------*/
void help_help()
{
    char hlp_strng[] = 
      {"\n ***************************************************************\n"
       " The following \"families\" of command strings are currently "
       " available:\n"
       " group     - to group (or rebin) channels \n"
       " bad       - to set channels to bad quality (ignored by XSPEC etc) \n"
       " good      - to (re)set channels to good quality \n"
       " systematics  - to set the fractional systematic  error of the data \n"
       " help      - this listing \n"
       " ?         - this listing \n"
       " help group - help on grouping commands\n"
       " help bad - help on bad commands\n"
       " help good - help on good commands\n"
       " help sys[tematics] - help on systematics commands\n"
       "  \n"
       " Several commands can be specified on the command  line by seperating \n"
       " them with a comma \",\". \n"
       " *************************************************************** \n"
      };
    printf ("%s \n",hlp_strng);
}
/*------------------------------------------------------------------------
                help_group
--------------------------------------------------------------------------*/
void help_group()
{
    char hlp_strng[] = 
      {"\n" 
       "*********************************************************************\n"
       " GROUP \n"
       " ----- \n"
       " \n"
       " There are 3 methods by which the grouping  can be set:\n"
       " \n"
       " GROUP minchan maxchan nchan\n"
       " The data is grouped from minchan to maxchan (inclusive) with nchan\n"
       " bins in each group.  Any spare channels will be left ungrouped and\n"
       " the user informed.  Any grouping request which partially overlaps\n"
       " a pre-existing grouping will be ignored and the user informed.\n"
       " A maximum of 50 sets of groupings is allowed on the command line.\n"
       " \n"
       " If a file name is mentioned preceeded by \"@\" sign the grouping\n"
       " information is read (free-format) from the data file.  This file is in\n"
       " ASCII format and can consist of up to 50 lines (sets of groupings,\n" 
       " one per line) with the syntax minchan maxchan nchan.\n"
       " \n"
       " GROUP min rcnts\n"
       " The grouping is set such that each new grouping contains a minimum\n"
       " of rcnts counts in each bin. Channels that are defined as  BAD are\n"
       " not included.  Any spare channel will be defined as BAD (QUALITY=2).\n"
       "*********************************************************************\n"
       " \n"
      };
    printf("%s \n", hlp_strng);
}

/*------------------------------------------------------------------------
                help_good
--------------------------------------------------------------------------*/
void help_good()
{
    char hlp_strng[] = 
      {"\n"
       "*********************************************************************\n"
       " GOOD \n"
       " ----- \n"
       "\n"
       " This command family sets the quality flags such that the specified\n"
       " channels can be ignored by certain subsequent commands and downstream\n"
       " software (e.g., XSPEC). The quality flags of unspecified channels are\n"
       " unchanged.\n"
       " \n"
       " There are 2 methods whereby channels can be set bad:\n"
       " \n"
       " GOOD minchan maxchan\n"
       " Channels between minchan and maxchan (inclusive) are set bad \n"
       " (Quality = 0).\n"
       " \n"
       " If a file name is mentioned preceeded by \"@\" sign the quality\n"
       " information is read (free-format) from the data file.  This file is in\n"
       " ASCII format and can consist of up to 50 lines (one set per line) with\n"
       " the syntax minchan maxchan.  Any spare channels and overlapping will be\n"
       " left ungrouped and user will be informed.\n"
       "*********************************************************************\n"
       "\n"
      };
    printf("%s \n", hlp_strng);
}

/*------------------------------------------------------------------------
                help_bad
--------------------------------------------------------------------------*/
void help_bad()
{
    char hlp_strng[] = 
      {"\n"
       "*********************************************************************\n"
       " BAD \n"
       " ---- \n"
       "\n"
       " This command family sets the quality flags such that the specified\n"
       " channels can be ignored by certain subsequent commands and downstream\n"
       " software (e.g., XSPEC).  The quality flags of unspecified  channels\n"
       " are unchanged. \n"
       "  \n"
       " There are 2 methods whereby channels can be set bad: \n"
       "  \n"
       " BAD minchan maxchan \n"
       " Channels between minchan and maxchan (inclusive) are set bad \n"
       " (Quality = 5). \n"
       "  \n"
       " If a file name is mentioned preceeded by \"@\" sign the quality \n"
       " information is read (free-format) from the data file.  This file is in \n"
       " ASCII format and can consist of up to 50 lines (one set per line) with\n"
       " the syntax minchan maxchan.  Any spare channels and overlapping will be\n"
       " left ungrouped and user will be informed.\n"
       "*********************************************************************\n"
       "\n"
       };
    printf("%s \n", hlp_strng);
}

/*------------------------------------------------------------------------
                help_systematics
--------------------------------------------------------------------------*/
void help_systematics()
{
    char hlp_strng[] = 
      {"\n"
       "*******************************************************************\n"
       " SYSTEMATICS \n"
       " ------------ \n"
       "\n"
       " This command family sets the fractional systematic error for each \n"
       " PHA channel which should be combined with the corresponding \n"
       " statistical error on the data to define the true (total) error\n"
       " on the data.  It is stressed that this command obviously does \n"
       " NOT change the observed (statistical) error associated with the \n"
       " PHA data.  Rather SYS_ERR column is filled with the appropriate \n"
       " values, and the command is therefore reversible.\n"
       " \n"
       " There are 2 methods whereby the systematic errors can be set:\n"
       " \n"
       " SYSTEMATICS minchan maxchan err\n"
       " Channels between minchan and maxchan (inclusive) will have a \n"
       " fractional systematic error of value err.  Err of 0.03 corresponds to a\n"
       " systematic error of 3% of the observed PHA count rate for that channel.\n"
       " \n"
       " If a file name is mentioned preceeded by \"@\" sign the error \n"
       " information is read (free-format) from the data file.  This file is in\n"
       " ASCII format and can consist of up to 50 lines (one set per line) with \n"
       " the syntax minchan maxchan err.  Any spare channels and overlapping will\n"
       " be left ungrouped and user will be informed.\n"
       "*********************************************************************\n"
       "\n"
      };
    printf("%s \n", hlp_strng);
}

/*------------------------------------------------------------------------
                help_spectra_num
--------------------------------------------------------------------------*/
void help_spectra_num()
{
    char hlp_strng[] =
      {"\n"
       "********************* HELP SPECTRA ***********************************\n"
       " SPECTRA \n"
       " -------- \n"
       "\n"
       " The range of spectra (rows) to be input.  The default of  \"-\" means\n"
       " all rows.  The first ten rows could be specified as \"1-10\" or just\n"
       " \"-10\".  To input the first ten rows and all rows from 100 through the \n"
       " last (inclusive), use \"1-10,100-\".  An input of \"1,3,7,23\" will do\n"
       " only those four rows.\n"
       "********************* END OF HELP SPECTRA ***************************\n"
       "\n"
      };
    printf("%s \n", hlp_strng);
}



 
