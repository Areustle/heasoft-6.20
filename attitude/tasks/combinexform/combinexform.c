#include <stdlib.h>
#include <string.h>
#include <math.h>

#define TOOLSUB combinexform
/* fatal.h / headas_main() require that TOOLSUB be defined first */

#include "commands.h"

#include "headas.h"
#include "coordfits.h"
#include "param.h"
#include "att_fatal.h"

#include "headas_main.c"

/****************************************************************************
*****************************************************************************
* transform to a FITS image from one coordinate system to another.
****************************************************************************/
void TOOLSUBAUX(void) {


PARAM* param;

COMMANDS* commands;
COMBOXFORM* combo;


set_toolversion("0.0.0");

/**********************
* read the parameters *
**********************/
param=readParam();

if(param==NULL) {
    fprintf(stderr, "Could not read parameters\n");
    att_fatal(1);
}

commands = readCommands(param->command);

combo = executeCommands(commands);

writeComboXform(combo, param->outfile);


/*******************************************
 * write the parameter list (to primary+0) *
 ******************************************/
parstamp_path(param->outfile);

} /* end of main function */

