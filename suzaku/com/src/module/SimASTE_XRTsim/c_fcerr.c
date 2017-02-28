#include <stdio.h>
#include <string.h>
#include "headas.h"

void
c_fcerr(char *errmsg)
{
    char context[1024];

    if ( NULL == errmsg ) {
		return;
	}
    get_toolname(context);
    strncat(context, " : ", sizeof(context)-1);
    strncat(context, errmsg, sizeof(context)-1);
    context[sizeof(context)-1] = 0;
    fprintf(stderr, "%s\n", context);

    return;
}
