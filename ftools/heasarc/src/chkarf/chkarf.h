
/* fdelhdu constants */
#define FLEN_BUFFER   257

void chkarf();
void chkarfgp(char *infile, char *outfile, int *chatter, int *clobber,
              int *status);
void chkarfdo(char *infile, char *outfile, int chatter, int clobber,
	      int *status);

