/**************************************************************************************

	I N T E G R A L   S C I E N C E   D A T A   C E N T E R

	P A R A M E T E R   I N T E R F A C E   L I B R A R Y

Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
File:		ISDCcopy.c
Description:	Parameter Interface Library - sample C program
Authors:	Jerzy.Borkowski@obs.unige.ch (JB)
History:	17-Aug-98 (JB) : version 1.0 release
History:	16-Feb-00 (JB) : bugfix in CommonInt, version 1.4.2

**************************************************************************************/
/*
 * this program demonstrates how to call PIL library from C code
 * Should you wish to see other ISDC compliant
 * program look for it in Common library directory.
 *
 * This program copies infile to outfile. Default values are taken from
 * parameter file. New value are stored in parameter file.
 *
*/


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <isdc.h>


#define	BUFSIZE	(8192)

char		infname[PIL_LINESIZE], outfname[PIL_LINESIZE],  buf[BUFSIZE];


int	isdccopy_logger(char *s)
 {
   printf("\n%s\n", s ? s : "NULLstring");
   return(0);
 }


int	main(int argc, char **argv)
{ 
  int		r, r2, i, fsize, delta;
  FILE		*inf, *outf;
  struct stat	fst;
  char		*es;


/* initialize stuff, check for errors  */

r = CommonInit("ISDCcopy", "1.0", argc, argv);

/*
if (PIL_OK != r)
  { es = PIL_err_handler(r);
    printf("CommonInit failed : %s\n", (es ? es : "unknown error"));
    return(10);
  } 
*/

r = PILSetLoggerFunction(isdccopy_logger);

/* please note, that we are ignoring server mode flag (if any) returned by
   CommonInit. We always run in ISDC_SINGLE_MODE */

if (PIL_BOGUS_CMDLINE == PILVerifyCmdLine())
  { printf("Extra parameters found in command line !\n");
    return(11);
  }

	/* get our parameters, and try to open 2 files */

r = PILGetFname("InFile", infname);
r2 = PILGetFname("OutFile", outfname);



inf = fopen(infname, "r");
if (NULL != inf)  { outf = fopen(outfname, "w"); } 
else  { outf = NULL; }

	/* check for errors */

if ((NULL != inf) && (NULL != outf) && (PIL_OK == r) && (PIL_OK == r2))
  {
  		/* get file size in bytes */
    fstat(fileno(inf), &fst);

		/* copy up to BUFSIZE bytes in one iteration */
    for (i=0; i<fst.st_size; i+=delta)
     { delta = BUFSIZE;
       if ((i+delta) > fst.st_size) delta = fst.st_size - i;

       if (delta != fread(buf, 1, delta, inf)) 
         { r = PIL_BAD_ARG;		/* read error */
           break;
         }
       if (delta != fwrite(buf, 1, delta, outf)) 
         { r = PIL_BAD_ARG;		/* write error, disk full, or protection ? */
           break;
         }
     }
  }
else
  { r = PIL_BAD_ARG;	/* signal couldnt get params/files */
  }

if (NULL != outf)  fclose(outf);
if (NULL != inf)  fclose(inf);

CommonExit(r);		/* shut down PIL stuff */

return(r);		/* return exit code to parent process, if it cares */

}
