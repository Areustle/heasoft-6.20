
#define COMMAND_PARAM_LENGTH 1024
#define FILENAME_LENGTH 128

typedef struct {

char command[COMMAND_PARAM_LENGTH];

char outfile[FILENAME_LENGTH];

int history;


} PARAM;


/*******************************************************************************
*
*******************************************************************************/
PARAM* readParam();

int parstamp_path (const char * path);

