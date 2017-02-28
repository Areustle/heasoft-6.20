#include <string.h>
#include "cfortran.h"
#include "ape/ape_list.h"
#include "ape/ape_par.h"
#include "ape/ape_trad.h"
#include "ape/ape_error.h"

void apeparsepar (char * name, char * type, char * mode, char * value, char * min, char * max, char * prompt, int * status) {
	ApeParFile * par_file = 0;
	ApeList * par_list = 0;
	ApePar * par = 0;
	char * par_result = 0;
	static ApeListIterator itor = 0;
	*status = 0;

/* get current par file (assumes ape_init has been called): */

	if (eOK == *status) {
		*status = ape_trad_get_current(&par_file);
	}

/* get list of pars: */

	if (eOK == *status) {
		*status = ape_io_get_par_cont(par_file, &par_list);
	}

/* loop over list, getting name/type/etc. for each: */

	if (itor == 0) itor = ape_list_begin(par_list);

	if (itor != ape_list_end(par_list)) {

		do {

			par = ape_list_get(itor);

			if (eOK == *status) {
				*status = ape_par_get_field(par, eName, &par_result);
			}

			if (0 == par_result || 0 == *par_result) {
				itor = ape_list_next(itor);
			} else {
				break;
			}

                } while (itor != ape_list_end(par_list));

		if (eOK == *status && 0 != par_result && 0 != *par_result) {

			/* In principle this (strcpy) is dangerous and should
			 * be replaced by strncpy to account for the hardcoded
			 * sizes of the Fortran variables */
			strcpy(name, par_result);
			free(par_result);
			*status = ape_par_get_field(par, eType, &par_result);

			if (eOK == *status) {
			   strcpy(type, par_result);
			   free(par_result);
			   *status = ape_par_get_field(par, eMode, &par_result);
			}
			if (eOK == *status) {
			   strcpy(mode, par_result);
			   free(par_result);
			   *status = ape_par_get_field(par, eValue, &par_result);
			}
			if (eOK == *status) {
			   strcpy(value, par_result);
			   free(par_result);
			   *status = ape_par_get_field(par, eMin, &par_result);
			}
			if (eOK == *status) {
			   strcpy(min, par_result);
			   free(par_result);
			   *status = ape_par_get_field(par, eMax, &par_result);
			}
			if (eOK == *status) {
			   strcpy(max, par_result);
			   free(par_result);
			   *status = ape_par_get_field(par, ePrompt, &par_result);
			}
			if (eOK == *status) {
			   strcpy(prompt, par_result);
			   free(par_result);
			}

		}

		if (itor != ape_list_end(par_list)) itor = ape_list_next(itor);

	} else {

		itor = 0;
		*status = 1;
	}

	return;

}
FCALLSCSUB8(apeparsepar, APEPARSEPAR, apeparsepar, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PINT)
