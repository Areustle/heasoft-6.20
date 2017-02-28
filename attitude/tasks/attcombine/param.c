#include <stdlib.h>
#include <string.h>

#include "param_wrappers.h"
#include "param.h"
#include "attfile.h"
#include "align.h"
#include "caldbquery.h"
#include "headas.h"

/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void) {

    PARAM* param;
    char interpolation[FILENAME_DIMEN];
    char alignfile[FILENAME_DIMEN];

    param = (PARAM*) calloc(1, sizeof(PARAM));

    read_string_param("first"   , param->first    , FILENAME_DIMEN);
    read_string_param("second"  , param->second   , FILENAME_DIMEN);
    read_string_param("combined", param->combined , FILENAME_DIMEN);

    read_string_param("alignfile", alignfile, FILENAME_DIMEN);
    if (strcasecmp(alignfile, "NONE")) {
        if (!strncasecmp(alignfile, "CALDB", 5)) {
            CALDBQuery query = { 0 };
            strcpy(query.codename, "ALIGNMENT");
            strcpy(query.instrument, "SC");
            query.infile = param->first;
            set_caldb_query_qualifiers(&query, alignfile);
            if (simple_caldb_query(&query, 0, alignfile)) {
                headas_chat(0, "unable to resolve alignfile=%s\n", alignfile);
                free(param);
                return 0;
            }
            HDpar_note("alignfile", alignfile);
        }

        param->align = readAlign(alignfile);
        if (!param->align)
            return 0;
    }

    read_string_param("interpolation", interpolation, FILENAME_DIMEN);
    if (!strcasecmp(interpolation, "CONSTANT")) {
        param->interpolation = ATTFILE_CONSTANT;
        headas_chat(2, "using CONSTANT interpolation for corrections\n");
    }
    else {
        param->interpolation = ATTFILE_LINEAR;
        headas_chat(2, "using LINEAR interpolation for corrections\n");
    }

    param->history = read_boolean_param("history");

    return(param);
}
