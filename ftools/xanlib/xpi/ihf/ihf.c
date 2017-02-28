
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef unix

#include "readline/readline.h"
#include "readline/history.h"

rl_hook_func_t *old_rl_completion_entry_function;

#endif

#include "textnode.h"
#include "topicnode.h"

int ihf_error=0;
Topicnode *rootnode;

/* The following creates a fortran wrapper for the ihf subroutine called
 * f_ihf.  Fortran routines using the IHF package should call f_ihf to
 * access the ihf function below. */
#include "cfortran.h"
void ihf();
FCALLSCSUB2(ihf,F_IHF,f_ihf,STRING,STRING)

/*
 * The top level function in the ihf package
 */

void ihf(filename, topics) 
char *filename;
char *topics;
{
    int histoffset;
    void ihf_init(), ihf_cleanup(), ihf_rd_help_file(), ihf_be_helpful();

    /* Preform preliminary assignations, etc. */
    ihf_init(&histoffset);

    /* Read the help file */
    ihf_rd_help_file(filename);
    if(ihf_error) {
         return;
     }

    /* Talk to the user and provide them with help */
    if(topics[0] == '\0') {
        ihf_be_helpful((char *)NULL);
    }
    else {
        ihf_be_helpful(topics);
    }

    ihf_cleanup(histoffset);

}

/*
 * Initialize housekeeping and readline variables. On vms systems, the
 * readline stuff is ignored.
 */

void ihf_init(histoffset)
int *histoffset;
{

    char *ihf_topic_generator();

#ifdef unix
    /*Get the history offset number for the current command line*/
    /*All history entries made during this ihf session will be removed later*/

    *histoffset = where_history();

    /*The ihf uses it's own completion function.  If one is already in use*/
    /*replace it with ours, but save the old one for restoration later*/

    old_rl_completion_entry_function = (rl_hook_func_t *) rl_completion_entry_function;
    rl_completion_entry_function = (rl_hook_func_t *) ihf_topic_generator;

#endif

}

/*
 * Free memory which doesn't need to stick around and reset readline variables.
 * On vms systems, the readline stuff is ignored.
 */

void ihf_cleanup(histoffset)
int histoffset;
{
#ifdef unix
    rl_completion_entry_function = old_rl_completion_entry_function;

    /**** The release of the history memory has been commented out because
     **** a newer version of readline is required to fix a bug that occurs
     **** with the history_get function.  Too much work has been done trying
     **** to get the IHF package installed in XPI, and it's easier to simply
     **** forget about this feature. =( */

    /* I use histoffset+1 here because history_get returns the history
     * entry from histoffset-1. I think this is a bug in the history facility */

    /*
    while(history_get(histoffset+1) != (HIST_ENTRY *) NULL) {
        HIST_ENTRY *entry = remove_history(histoffset);
        free((char *) entry->line);
        free((char *) entry);
    }
    */

#endif
}

