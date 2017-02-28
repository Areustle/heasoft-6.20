#include "ape/ape_trad.h"
#include "ape/ape_par.h"
#include "cfortran.h"

int xpi_get_text(const char * prompt, const char * name, char ** text);

int xselect_ape_init(void) {
	int argc = 1;
	const char* argv[] = {"xselect"};
	int status = ape_trad_init(argc,(char**)argv);

	/* Override any attempt to turn off prompting, e.g. using
	 * HEADASNOQUERY, as xselect is not wired for that option. */
	ape_util_override_query_mode(APE_UTIL_QUERY_DEFAULT);

	/* Register custom XPI text-getter with APE, such that APE
	 * then calls back here to get input text.  For more info
	 * see xanlib/xpi/xpigettext.c. */
	ape_par_register_get_text(xpi_get_text);

	return status;
}
FCALLSCFUN0(INT, xselect_ape_init, XSELECT_APE_INIT, xselect_ape_init)
