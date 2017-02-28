/*
 * pil_lint.c
 *
 * this program checks PIL parameter file for correctness.
 *
*/


#include <stdio.h>
/* SCREW 1145: Use string.h instead of strings.h. */
#include <string.h>
#include <pil.h>


int	main(int argc, char **argv)
{ char		ctmp;
  int		r, i, parcnt, minmaxflag;
  PIL_VALUE	vmin, vmax;
  PIL_PARAM	pp;

if (2 != argc)
  { printf("usage:\n\tpil_lint parameter_filename\n\n");
    return(10);
  }

PILSpecialMode |= PIL_NO_POSITIONAL | PIL_BYPASS_NAMING | PIL_OPEN_RDONLY;

strncpy(PILModuleName, argv[1], PIL_LINESIZE - 1);
PILModuleName[PIL_LINESIZE - 1] = 0;

if (PIL_OK != (r = PILInit(argc, argv)))
  { printf("PILInit failed : %s\n", PIL_err_handler(r));
    return(10);
  } 

if (PIL_OK != (r = PILGetNumParameters(&parcnt)))
  { printf("PILGetNumParameters failed : %s\n", PIL_err_handler(r));
    return(11);
  } 

for (i=0; i<parcnt; i++)
 { if (PIL_OK != (r = PILGetParameter(i, &pp, &minmaxflag, &vmin, &vmax)))
     { printf("PILGetParameter failed : %s\n", PIL_err_handler(r));
       return(12);
     }
   if (PIL_FORMAT_ERR == pp.format)
     { if (sscanf(pp.strline, " %c", &ctmp) > 0)	/* ignore TAB/SPACE only lines */
         printf("pil_lint: FILE %s: INVALID LINE %d: %s\n", argv[1], i+1, pp.strline);
     }
 }

PILClose(PIL_OK);
return(PIL_OK);
}
