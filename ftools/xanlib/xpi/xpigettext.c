#include <stdlib.h>
#include <string.h>
#include "ape/ape_util.h"
#include "ape/ape_trad.h"
#include "ape/ape_error.h"
#include "cfortran.h"

#define XPIHELP(par_name) CCALLSFSUB1(XPIHELP,xpihelp,STRING,par_name)
#define XCREAD_FROM_C(prompt,text,len,status) CCALLSFSUB4(XCREAD_FROM_C,xcread_from_c,STRING,PSTRING,PINT,PINT,prompt,text,len,status)

/* Custom text-getter which is registered with APE by OpenDefaultPF.
   When registered, APE calls back here to get input text.  This way
   we can handle input "?" (call XPIHELP) and allow XCREAD to correctly
   perform the necessary voodoo for e.g. xselect (all previously handled
   by XPIQUERYPAR).
*/

int xpi_get_text(const char * prompt, const char * par_name, char ** text) {

	int status = 0;

	while (eOK == status) {

		/*
		status = ape_util_get_text(prompt, text);
		*/
	        char textbuffer[2048] = "";
		int len_textbuffer = 0;

		/* Call XCREAD entry point XCREAD_FROM_C which also returns
		   the length of textbuffer.  We want to preserve spaces ' '
		   entered for parameter values (e.g. to "zero-out" a string
		   value) but CFortran drops them, so here we pad out the 
		   length of textbuffer with spaces to get around this.
		*/
		XCREAD_FROM_C((char *) prompt, textbuffer, len_textbuffer, status);
		while (strlen(textbuffer) < len_textbuffer) strcat(textbuffer," ");

		if (eOK == status) {

			static int no_xpi_help = -1;
			if (-1 == no_xpi_help) {
				no_xpi_help = getenv("XPINOHELP") ? 1:0;
			}

			/* TODO: check for leading/trailing spaces */
			if (0 == strcmp(textbuffer,"?") && !no_xpi_help) {
				XPIHELP((char *) par_name);
			} else {
	        		*text = malloc((1+strlen(textbuffer))*sizeof(char));
	        		strcpy(*text, textbuffer);
				break;
			}
		}

	}
	return status;
}
