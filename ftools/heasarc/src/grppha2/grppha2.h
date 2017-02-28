/* grppha2 constants */

#define FLEN_BUFFER   257
#define MAX_COMMAND   50
#define MAX_RANGE     50
#define LINE_LENGTH   80
#define MAXMISS   20
#define NUMHIS 60

void grppha2();

void grppha2_par(char *infile, char *outfile, int *ncom,
                   char commands[][FLEN_BUFFER], int *numranges,
                   int *firstrow, int *lastrow, char *respfile,
                   char *ancrfile, int *clobber, int *chatter, int *status);

void read_cmdfile(char *command, int *ncom, char commands[][FLEN_BUFFER],
                  int *status);

void parse_command(char *command, int *ncom, char commands[][FLEN_BUFFER],
                   int *status);

void  grppha2_do(char *infile, char *outfile, char *version, int ncom, 
                   char commands[][FLEN_BUFFER], int numranges,
                   int *firstrow, int *lastrow, int new_respfile,
                   char *respfile, int new_ancrfile, char *ancrfile,
                   int root_resp, char *root_respfile, int root_ancr,
                   char *root_ancrfile, int chatter, int *status);
  
void group_min(char *command, int nchan, short *channel, double *rawcounts, 
               int *new_qgroup, short *grping, int *new_qqual, short *quality, 
               int loopnumber, int chatter, int *status);

void group_chan(char *command, int nchan, short *channel, int *new_qgroup, 
                short *grping, int loopnumber, int chatter, int *status);     

void group_good(char *command, int nchan, short *channel, int *new_qqual,
                short *quality, int loopnumber, int chatter, int *status);

void group_bad(char *command, int nchan, short *channel, int *new_qqual,
               short *quality, int loopnumber, int chatter, int *status);

void set_qual_flag(int startchan, int lastchan, int qualflg, short *quality,
                   int found_first_chan, int found_last_chan, short *channel,
                   int nchan, int *new_qqual);                                                    
void find_channel(int startchan, int lastchan, int nchan, short *channel,
                  int *found_first_chan, int *found_last_chan, int loopnumber);

void group_systematics(char *command, int nchan, short *channel,
                       int *new_qsys, double *syserr, int loopnumber, 
                       int chatter, int *status);

void help_spectra_num();
void help_help();
void help_group();
void help_good();
void help_bad();
void help_systematics();



int readphaIIkeywd(fitsfile *fptr, int chatter, char *telescop, char *instrume,
          char *detnam, char *filter, char *phaversn, char *hduclass,
          char *hduclas1, char *hduclas2, char *hduclas3,
          char *hduclas4, char *hduvers1, long *fchan, double *texpos,
          double *areascal, int *nbackscal, int *ncorrscal, int *nbackfil,
          int *ncorrfil, int *nrespfil,  int *nancrfil,
          char *dmode, long *detchans, char *chantyp,
          int *dtype, int *qerror,  int *qsys,
          int *qqual, int *qgroup,
          int *pois, long *nspec);


int writephaIIkeywd(fitsfile *fptr, int chatter, int nk_history,
             char *history[], int nk_comm, char *com[], char *keyname, 
             char *telescop, char *instrume, char *detnam, char *filter, 
             char *phaversn, char *hduclas2, long fchan, double texpos, 
             double areascal, int nbackscal, double backscal, int ncorrscal, 
             double corrscal, int nbackfil, char *backfil, int ncorrfil, 
             char *corrfil, int nancrfil, char *ancrfil, int ancrfil_len, 
             int nrespfil, char *respfil, int respfil_len, char *dmode, 
             long detchans, char *chantyp, int dtype, int qerror, int qsys, 
             int qqual, int qgroup, long nchan, long nspec);



