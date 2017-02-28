/*
 *	xrt_termio.c: --- terminal input/output module ---
 *
 *	DESCRIPTION:
 *		A module comprising functions for general terminal I/O, e.g. issuing
 *		messages to <stderr>, user-friendly reading of values from <stdin>,
 *		and stuff like that ...
 *
 *	CHANGE HISTORY:
 *
 *      1.1.4 - BS 18/05/2004 - Modified "Error" routine:   
 *                              Used 'taskname' and 'get_toolnamev' instead of global.PrgName
 *                              Used 'headas_chat' instead of 'Infolines'
 *      1.1.5 - BS 04/03/2004 - Modified SplitLines routine to use length == sizeof(card->Comment[]) 
 *      1.1.6 - BS 31/08/2005 - Added CopyFile routine
 *	AUTHOR	:
 *		UL, Thu May 5 16:10:55 METDST 1994
 */

#define XRT_TERMIO_C
#define XRT_TERMIO_VERSION	1.1.6


						/********************************/
						/*        header files          */
						/********************************/

#include "xrt_termio.h"

#define MAX_LINES		20		/* maximum # of continuation lines	*/
#define MAX_LINE_LEN	100		/* maximum allowed line length		*/

#define LINE_LEN		FLEN_ERRMSG

						/********************************/
						/*           globals            */
						/********************************/

static struct {
	Verbosity_t		VerboseLevel;	/* the verbosity level	*/
	char			PrgName[32];	/* program name			*/
} global = { NORMAL, "" };




						/********************************/
						/*       internal functions     */
						/********************************/

						/********************************/
						/*       external functions     */
						/********************************/

/*
 *	SYNOPSIS:
 *		void Error(const char *info, const char *fmt, ...)
 *
 *	DEPENDENCIES:
 *
 *	<none>
 */
void Error(const char *info, const char *fmt, ...)
{
    va_list		args;			/* variable argument list	*/
    char		buff1[1024],	/* message to print			*/
                        buff2[1024];

    char                taskname[FLEN_FILENAME];
    
    get_toolnamev(taskname);

    va_start(args, fmt);
	if (taskname[0])
		if (info)
			sprintf(buff2, "%s: Error: %s: ", taskname, info);
		else
			sprintf(buff2, "%s: Error: ", taskname);
	else
		if (info)
			sprintf(buff2, "%s: Error: ", info);

    vsprintf(buff1, fmt, args);
	strcat(strncat(buff2, buff1, sizeof(buff2)-strlen(buff2)-2), "\n");
    va_end(args);

	headas_chat(MUTE,"%s", buff2);
} /* Error */



/*
 *	SYNOPSIS:
 *
 *	DESCRIPTION:
 *
 *		see xrt_termio.h
 *
 *	DEPENDENCIES:
 *
 */
void SysError(const char *info, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    fprintf(stderr, "%s: ", info);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ": ");
    perror((char *)NULL);
    va_end(args);
} /* SysError */



/*
 *	SYNOPSIS:
 *
 *	DESCRIPTION:
 *
 *		see xrt_termio.h
 *
 *	DEPENDENCIES:
 *
 */
void Infolines(const Verbosity_t VerboseLevel, const char *fmt, ...)
{
	char	tmp[1024];
	va_list args;

	if (global.VerboseLevel >= VerboseLevel) {
		char		*(*StrPtr)[];
		unsigned	nLines, n;

		va_start(args, fmt);
    	vsprintf(tmp, fmt, args);
		nLines = SplitLines(tmp, LINE_LEN, &StrPtr);
		for (n=0; n<nLines-1; ++n) {
			fputs((*StrPtr)[n], stdout);
			fputc('\n', stdout);
		}
		if (nLines)
			fputs((*StrPtr)[nLines-1], stdout);
    	va_end(args);
	}
} /* Infolines */



void Info(const Verbosity_t VerboseLevel, const char *fmt, ...)
{
	va_list args;

	if (global.VerboseLevel >= VerboseLevel) {
		va_start(args, fmt);
    	vfprintf(stdout, fmt, args);
		fflush(stdout);
    	va_end(args);
	}
} /* Info */



/*
 *	SYNOPSIS:
 *
 *	DESCRIPTION:
 *
 *		see xrt_termio.h
 *
 *	DEPENDENCIES:
 *
 */
void Message(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	vfprintf(stdout, fmt, args);
	fflush(stdout);
	va_end(args);
} /* Message */

unsigned SplitLines(const char *Line, unsigned length, char *(**StrVec)[])
{
	static char		*Lines[MAX_LINES];
	static BOOL 	FirstCall = TRUE;
	const char		*ActPtr;
	char			*tmp;
	unsigned		LenLeft, nLines;

	length--; /* BS 04/03/2005 -- The max Lines[n] dimension has to be equal to card->Comment[] 
		     dimension */


	if (FirstCall) {
		unsigned	n;

		FirstCall = FALSE;
		for (n=0; n<MAX_LINES; ++n)
			if (!(Lines[n] = (char *)malloc(MAX_LINE_LEN+1))) {
				fprintf(stderr, "SplitLines: Cannot allocate space for "
						"Lines array\n");
				exit(1);
			}
	}
	if (length > MAX_LINE_LEN) {
		fprintf(stderr, "SplitLines: %u exceeds MAX_LINE_LEN.\n", length);
		return 0;
	}

	*StrVec 	= &Lines;
	nLines		= 0;
	ActPtr		= Line;
	LenLeft		= strlen(Line);
	while (LenLeft > 0) {
		strncpy(Lines[nLines], ActPtr, (length));
		Lines[nLines][length] = '\0';
		if (LenLeft > (length)) {
			if ((tmp = strrchr(Lines[nLines], ' '))) {
				unsigned	len;
				*tmp = '\0';
				len = strlen(Lines[nLines]) + 1;
				ActPtr += len;
				LenLeft -= len;
			} else {
				ActPtr += (length);
				LenLeft -= (length);
			}
			if (++nLines >= MAX_LINES) {
				fprintf(stderr, "SplitLines: MAX_LINES exceeded - no reason to"
						" worry\n");
				return nLines;
			}
		} else {
		  break;
		}
	}

	return ++nLines;
} /* SplitLines */

/*
 *	SYNOPSIS:
 *
 *	DESCRIPTION:
 *
 *		see xrt_termio.h
 *
 *	DEPENDENCIES:
 *
 */
void SetVerboseLevel(const Verbosity_t VerboseLevel)
{
	if (VerboseLevel>=MUTE && VerboseLevel<=CHATTY) {
		global.VerboseLevel = VerboseLevel;
	} else {
		Error("SetVerboseLevel", "Illegal verbosity level, "
			  "resetting to NORMAL ...");
		global.VerboseLevel = NORMAL;
	}
} /* SetVerboseLevel */

/*
 *	SYNOPSIS:
 *
 *          void ReadInFileNamesFile(char *InFileNames)
 *
 *	DESCRIPTION:
 *
 *
 *	DEPENDENCIES:
 *
 */

void ReadInFileNamesFile(char *InFileNames)
{
	char	FileName[MAXFNAME_LEN];
	FILE	*fp;

	if (!(fp = fopen(InFileNames+1, "r"))) {
		Error("ReadInFileNamesFile", "Could not open file `%s' with list "
			  "event list file names.", InFileNames+1);
		PrgExit(FOPEN_ERR);
	}

	InFileNames[0] = '\0';
	while (!feof(fp))
		if (fscanf(fp, "%s\n", FileName))
			strcat(strcat(InFileNames, FileName), " ");

	if (InFileNames[0])
		/*
		 *	chop-off trailing blank
		 */
		InFileNames[strlen(InFileNames)-1] = '\0';

	fclose(fp);
} /* ReadInFileNamesFile */
/*
 *	SYNOPSIS:
 *		void FitsioError(int status)
 *
 *	DEPENDENCIES:
 *      fits_report_error(stderr,status);
 *	<none>
 */
void FitsioError(int status)
{
  Message("\n======== Fitsio Error Report:\n");
  fits_report_error(stderr,status);
  Message("=============================\n\n");

} /* FitsioError */

void PrgExit(const Error_t error)
{
	static char *ErrorStr[] = {	"no error",
								"`fopen' failure",
								"general file systen error",
								"memory allocation routine failure",
								"library error",
								"matching error",
								"illegal condition",
								"limit exceeded",
								"input error",
								"external signal received",
								"miscellaneous"
							  };

	if (error)
		fprintf(stderr, "exiting program: reason: "
				"%s\n",
				error < NUM_OF(ErrorStr) ? ErrorStr[error] : "unknown");

	exit(error);
} /* PrgExit */

int CopyFile(char *source, char *destination)
{

      FILE *fsource, *fdestination;

      unsigned char *buffer;

      int count;

 

      if((fsource = fopen(source, "rb")) == NULL)
	{
	  
	  Error("CopyFile", "Could not open file `%s' file ",source);
	  return NOT_OK;
	}

 

      if((fdestination = fopen(destination, "wb")) == NULL)

      {

	Error("CopyFile", "Could not open file `%s' file ",destination);
	fclose(fsource);
	return NOT_OK;

      }

 

      buffer = (unsigned char *)malloc(BUF_SIZE + 1);

 

      if(buffer == NULL) return NOT_OK;

 

      while((count = fread(buffer, sizeof(char),

            BUF_SIZE, fsource)) != 0)

            fwrite(buffer, sizeof(char), count, fdestination);

 

      free(buffer);

 

      fclose(fsource);

      fclose(fdestination);

 

      return OK;

}
