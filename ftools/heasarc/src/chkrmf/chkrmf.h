
/* fdelhdu constants */
#define FLEN_BUFFER   257

void chkrmf();
void chkrmfgp(char *infile, char *outfile, int *chatter, int *clobber,
              int *status);
void chkrmfdo(char *infile, char *outfile, int chatter, int clobber,
	      int *status);

