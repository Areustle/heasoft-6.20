/*
 *  Define commands and their parameters through parameter files
 */ 

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/xmtcl.h"
#include "../include/cmddef.h"
#include "cfortran.h"

#define LINELEN 80

static cmddef *cmdlist[MAX_CMDNUM]; /* List of commands [Built as called] */
static int numcmds = 0;

/*----------------------------------------------------------------*
 *  DOCUMENTATION
 *----------------------------------------------------------------*
 *
 *  Array of cmddef's, cmdlist, contains commands.
 *  Each cmddef, contains a name, location in array
 *     and a linked list of pardef's.
 *  pardef's contain parameter specs and values.
 *  A linked list of argdefs contains the values which were
 *     not identified as parameters.
 */
/*----------------------------------------------------------------*
 *  Exported routines
 *----------------------------------------------------------------*/

/*
 *  Data cleanup
 */
void argfree(argdef *argptr);  /* Free all arguments starting with given */
void parfree(pardef *parptr);  /* Free all parameters starting with given */
void cmdfree(cmddef *cmdptr);  /* Free command entry */

/*
 *  Loading command definitions
 */
pardef *parload (char *parfile); /* Create parlist from file */
cmddef *cmdload (char *cmdname); /* Add command to cmdlist */

/* 
 *  Set parameters based on arguments
 */
int parassign(pardef *parlist, argdef **rootargptr);

/*
 *  Parameter lookup (Used by cgpar[] routines)
 */
pardef *parfind(pardef *parlist, char *parname); 

/*
 *  Retrieve parameter values (from C)
 */
void cgparl(pardef *parlist, char *parname, int *parval, int *status);
void cgpari(pardef *parlist, char *parname, int *parval, int *status);
void cgpard(pardef *parlist, char *parname, double *parval, int *status);
void cgparr(pardef *parlist, char *parname, float *parval, int *status);
void cgpars(pardef *parlist, char *parname, char *parval, int maxparlen, 
            int *status);
void cgparlr(pardef *parlist, char *parname, float *parval, int *parnum,
             int maxnum, int *status);
void cgparld(pardef *parlist, char *parname, double *parval, int *parnum,
             int maxnum, int *status);
/*
 *  (from FORTRAN)
 *  CALL GPAR[LIDRS](cmdid, parname, parval, status)
 */

/*
 *  Set parameter values (from C)
 */
void csparl(pardef *parlist, char *parname, int parval, int *status);
void cspari(pardef *parlist, char *parname, int parval, int *status);
void cspard(pardef *parlist, char *parname, double parval, int *status);
void csparr(pardef *parlist, char *parname, float parval, int *status);
void cspars(pardef *parlist, char *parname, char *parval, int *status);
/*
 *  (from FORTRAN)
 *  CALL SPAR[LIDRS](cmdid, parname, parval, status)
 */

/*----------------------------------------------------------------*
 *  Internal use only
 *----------------------------------------------------------------*/

/*
 *  Allocate values (for pardef) given type and value string
 */ 
void *allocval (char type, char *valstr);

/*
 *  Parse data creation
 */
/*argdef *arginit();*/             /* Create and init argument entry */
pardef *parinit();             /* Create and init parameter entry */
cmddef *cmdinit();             /* Create and init command entry */

/*
 *  Argument manipulation (Only used by parassign)
 */
argdef *argremove(argdef *lastarg, argdef *curarg, argdef **rootargptr);
                                 /* Remove argument and return it */
argdef *argmove(argdef *lastarg, argdef *curarg, 
                argdef **rootargptr, argdef **setargptr);

/*----------------------------------------------------------------*
 *  IMPLEMENTATION
 *----------------------------------------------------------------*/
    
argdef *arginit() {
/*
 *  Allocate new arg entry and initialize it
 */
    argdef *newarg;

    newarg = (argdef *) malloc(sizeof(argdef));
    if ( !newarg ) {
       cxwrite(" Could not allocate argument entry", 5);
       return(newarg);
    }
    newarg->name = NULL;
    newarg->value = NULL;
    newarg->next = NULL;
    return(newarg);
}

pardef *parinit() {
/*
 *  Allocate new parameter entry and initialize it
 */
    pardef *newpar;

    newpar = (pardef *) malloc(sizeof(pardef));
    if ( !newpar ) {
       cxwrite(" Could not allocate parameter entry", 5);
       return(newpar);
    }

    newpar->name = NULL;
    newpar->type = '\0';
    newpar->mode = NULL;
    newpar->deflt = NULL;
    newpar->min = NULL;
    newpar->max = NULL;
    newpar->prompt = NULL;
    newpar->value = NULL;
    newpar->next = NULL;
    return(newpar);
}

cmddef *cmdinit() {
/*
 *  Allocate new command entry and initialize it
 */
    cmddef *newcmd;

    newcmd = (cmddef *) malloc(sizeof(cmddef));
    if ( !newcmd ) {
       cxwrite(" Could not allocate command entry", 5);
       return(newcmd);
    }

    newcmd->id = -1;
    newcmd->name = NULL;
    newcmd->parlist = NULL;
    newcmd->cmdargc = 0;
    newcmd->cmdargs = NULL;
    newcmd->curarg = NULL;
    return(newcmd);
}

void argfree(argdef *argptr) {
/*
 *  Free argument entries
 */
    argdef *curptr, *nextptr;
    
    curptr = argptr;
    while ( curptr ) {
       nextptr = curptr->next;
       if ( curptr->name ) free(curptr->name);
       if ( curptr->value ) free(curptr->value);
       free(curptr);
       curptr = nextptr;
    }
}

void parfree (pardef *parptr) {
/*
 *  Free parameter entries
 */
    pardef *curptr, *nextptr;
    
    curptr = parptr;
    while ( curptr ) {
       nextptr = curptr->next;
       if ( curptr->name ) free(curptr->name);
       if ( curptr->mode ) free(curptr->mode);
       if ( curptr->deflt ) free(curptr->deflt);
       if ( curptr->min ) free(curptr->min);
       if ( curptr->max ) free(curptr->max);
       if ( curptr->prompt ) free(curptr->prompt);
       if ( curptr->value ) free(curptr->value);
       free(curptr);
       curptr = nextptr;
    }
}


void cmdfree (cmddef *cmdptr) {
/*
 *  Free a command entry
 */
    if ( cmdptr->name ) free(cmdptr->name);
    parfree(cmdptr->parlist);
    argfree(cmdptr->cmdargs);

    cmdptr->id = -1;
    cmdptr->name = NULL;
    cmdptr->parlist = NULL;
    cmdptr->cmdargc = 0;
    cmdptr->cmdargs = NULL;
    cmdptr->curarg = NULL;

}

void *allocval (char type, char *valstr) {
/*
 *  Allocate space for types (b,i,r)
 */
   void *vptr;
   int i, opbr, clbr, vlen;

   if ( !valstr ) return NULL;
   switch (type) {
      case 'b': 
          vptr = malloc(sizeof(int));
          if ( vptr ) {
             if ( *valstr == 'y' ) {
                * (int *) vptr = 1;
             } else {
                * (int *) vptr = 0;
             }
          }
          break;
      case 'i': 
          vptr = malloc(sizeof(int));
          if ( vptr ) * (int *) vptr = atoi(valstr);
          break;
      case 'r': 
          vptr = malloc(sizeof(double));
          if ( vptr ) * (double *) vptr = atof(valstr);
          break;
      case 's': 

          /* Strip curly braces (only if one set start and end) */

          vlen = strlen(valstr);
          opbr = 0;
          clbr = 0;
          for ( i = 0; i < vlen; i++ ) {
             if ( valstr[i] == '{' ) {
                if ( i == 0 || valstr[i-1] != '\\' ) opbr++;
             } else if ( valstr[i] == '}' ) {
                if ( i == 0 || valstr[i-1] != '\\' ) clbr++;
             }
          }
         
          if ( valstr[0] == '{' && valstr[vlen-1] == '}' &&
               opbr == 1        && clbr == 1 ) {

             vptr = malloc( (vlen+1)*sizeof(char));
             for ( i = 1; i < vlen-1; i++ ) {
               *((char *) vptr+i-1) = valstr[i];
             }
             *((char *) vptr+vlen-2) = '\0';

          /* Strip \" (backslash-quotes) */

          } else if ( valstr[0] == '\\' && valstr[1] == '"' &&
                      vlen >= 4 && valstr[vlen-2] == '\\' && 
                               valstr[vlen-1] == '"' ) {
             vptr = malloc( (vlen-3)*sizeof(char));
             for ( i = 2; i < vlen-2; i++ ) {
                *((char *) vptr+i-2) = valstr[i];
             }
             *((char *) vptr+vlen-3) = '\0';
          } else {
             vptr = (void *) stralloc(valstr);
          }
          break;
      default: vptr = NULL;
   }
   return (vptr);
}

pardef *parfind(pardef *parlist, char *parname) {
/*
 * Search through list of parameters, finding one that matches
 * This function if used by gpar*, so full parameter names are
 * compared.  parname cannot be an abbreviation.
 */

    pardef *curpar, *foundpar;
    int lenpar;
    char* tmpname, *curptr, *tmpstr;

    curpar = parlist;
    lenpar = strlen(parname); 
    foundpar = NULL;

    /* Compare lower case version */

    tmpname = stralloc(parname);
    if ( !tmpname ) return NULL;
    curptr = tmpname;
    while ( *curptr ) {
       *curptr = tolower(*curptr);
       curptr++;
    }

    while ( curpar && !foundpar ) {
       if ( strcmp(curpar->name, tmpname) == 0 ) {
          foundpar = curpar;
       }
       curpar = curpar->next;
    }

    if ( !foundpar ) {
       tmpstr = strcatalloc(" Unmatched parameter: ", tmpname);
       cxwrite(tmpstr, 10);
       free(tmpstr);
    }
    free(tmpname);
    return (foundpar);
}
   
pardef *parload (char *parfile) {
/*
 *  Given parameter file, return list of parameters
 */

   char *cptr;
   char linebuff[LINELEN];
   FILE *fp;
   pardef **setpar, *rootpar, *curpar;

   rootpar = NULL;

   fp = fopen(parfile, "r");
   if ( !fp ) {
      cxwrite("Failed to open parameter file", 15);
      cxwrite(parfile, 15);
      return NULL;
   }

   /* Read parameter file */

   setpar = &rootpar;

   while ( fgets(linebuff, LINELEN, fp) ) {

      if ( linebuff[0] != '#' ) {  /* Skip comment lines */
         *setpar = parinit();
         curpar = *setpar;
         if ( !curpar ) {
            parfree(rootpar);
            return NULL;
         }

         /* Parameter name */

         cptr = quotok(linebuff, ",");
         if ( cptr ) curpar->name = stralloc(cptr);
         if ( !curpar->name ) {
            cxwrite(" Failed to get parameter name", 5);
            cxwrite(parfile, 5);
            parfree(rootpar);
            return NULL;
         }

         /* Parameter type */

         cptr = quotok(NULL, ",");
         if ( cptr ) {
             if ( *cptr == 'b' || *cptr == 'i' || 
                  *cptr == 'r' || *cptr == 's' ) {
                curpar->type = *cptr;
             } 
         } 
         if ( !curpar->type ) {
            cxwrite(" Failed to get parameter type", 5);
            cxwrite(parfile, 5);
            parfree(rootpar);
            return NULL;
         }

         /* Mode */
             
         cptr = quotok(NULL, ",");
         if ( cptr ) curpar->mode = stralloc(cptr);
         if ( !curpar->mode ) {
            cxwrite(" Failed to get parameter mode", 5);
            cxwrite(parfile, 5);
            parfree(rootpar);
            return NULL;
         }
               
         /* Default value */
         
         cptr = quotok(NULL, ",");
         if ( cptr && *cptr ) {
            curpar->deflt = allocval(curpar->type, cptr);
            if ( !curpar->deflt ) {
               cxwrite(" Failed to get parameter default", 5);
               cxwrite(parfile, 5);
               parfree(rootpar);
               return NULL;
            }
         }

         /* Minimum value */

         cptr = quotok(NULL, ",");
         if ( cptr && *cptr ) {
            curpar->min = allocval(curpar->type, cptr);
            if ( !curpar->min ) {
               cxwrite(" Failed to get parameter minimum", 5);
               cxwrite(parfile, 5);
               parfree(rootpar);
               return NULL;
            }
         }

         /* Maximum value */

         cptr = quotok(NULL, ",");
         if ( cptr && *cptr ) {
            curpar->max = allocval(curpar->type, cptr);
            if ( !curpar->max ) {
               cxwrite(" Failed to get parameter maximum", 5);
               cxwrite(parfile, 5);
               parfree(rootpar);
               return NULL;
            }
         }

         /* Prompt string */

         cptr = quotok(NULL, ",");
         if ( cptr ) curpar->prompt = stralloc(cptr);
         if ( !curpar->prompt ) {
            cxwrite(" Failed to get parameter prompt", 5);
            cxwrite(parfile, 5);
            parfree(rootpar);
            return NULL;
         }
         setpar = &curpar->next;
      }
   }
   fclose(fp);

   /* Add help parameter */

   *setpar = parinit();
   curpar = *setpar;
   if ( !curpar ) {
      parfree(rootpar);
      return NULL;
   }
   curpar->name = stralloc("help");
   curpar->type = 'b';
   curpar->deflt = allocval(curpar->type, "no");

   return (rootpar);
}


cmddef *cmdload (char *cmdname) {
/*
 *  Add contents of parameter file into command list
 */

   char *xanadu, *locmd, *cptr;
   char filbuff[MAX_FILELEN];
   int i;

   /* Find parameter file */

   locmd = stralloc(cmdname);
   if ( !locmd ) return(0);
   cptr = locmd;
   while ( *cptr ) {
      *cptr = tolower(*cptr);
      cptr++;
   }

   xanadu = getenv("XANADU");
   if ( xanadu ) {
      sprintf(filbuff, "%s/image/ximage/pfiles/%s.par", xanadu, locmd);
   } else {
      cxwrite("XANADU not set", 5);
      return(NULL);
   }

   /* Add command entry into cmdlist */

   i = numcmds;
   numcmds++;
   if ( numcmds > MAX_CMDNUM ) {
      cxwrite(" Maximum number of commands exceeded", 5);
      numcmds--;
      return(NULL);
   }
   cmdlist[i] = cmdinit();
   if ( !cmdlist[i] ) {
      cxwrite(" Could not allocate command entry", 5);
      numcmds--;
      return(NULL);
   }
   cmdlist[i]->id = i;
   cmdlist[i]->name = locmd;
   cmdlist[i]->parlist = parload(filbuff);
 
   return(cmdlist[i]);
}

/* UNUSED
void fcmdload (char *cmdname, int *status) {
   if ( cmdload(cmdname) ) {
      *status = 0;
   } else {
      *status = -1;
   }
}
UNUSED  */

void printarg(argdef *curarg) {

   if ( !curarg ) return;
   printf ("-> ");
   if ( curarg->name ) printf ("name(%s) ", curarg->name);
   if ( curarg->value ) printf ("val(%s) ", curarg->value);
   printf ("\n");
   printarg(curarg->next);

}

argdef *argremove(argdef *lastarg, argdef *curarg, argdef **rootargptr) {
/* 
 *  Remove current argument from list
 *  Returns new current argument
 */
    argdef *tmparg;

    if ( curarg == *rootargptr ) {
       *rootargptr = curarg->next;
    } else {
       lastarg->next = curarg->next;
    }
    tmparg = curarg->next;
    if ( curarg->name ) free(curarg->name);
    if ( curarg->value ) free(curarg->value);
    free(curarg);
    return (tmparg);
}

argdef *argmove(argdef *lastarg, argdef *curarg, 
                argdef **rootargptr, argdef **setargptr) {
/* 
 *  Remove current argument from list
 *  Returns new current argument
 */
    argdef *tmparg;

    if ( curarg == *rootargptr ) {
       *rootargptr = curarg->next;
    } else {
       lastarg->next = curarg->next;
    }
    *setargptr = curarg;
    tmparg = curarg->next;
    curarg->next = NULL;
    
    return (tmparg);
}

int parassign(pardef *parlist, argdef **rootargptr) {
/*
 *  Set parameter values based on arguments
 *  If no corresponding argument, assume default
 *  *arglist is list of arguments with matched parameters stripped out
 */
    argdef *curarg, *lastarg;
    argdef *checkedargs, **setargptr;
    pardef *curpar;
    void *vptr;
    int valset;
    char *valstr, *tmpstr;
    static char trueval[] = "yes";

    lastarg = NULL;
    curpar = parlist;
    checkedargs = NULL;
    setargptr = &checkedargs;

    while ( curpar ) {

    /* Zap existing value for parameter */

       if ( curpar->value ) {
          free(curpar->value);
          curpar->value = NULL;
       }

    /* Run through checkedargs to make sure not an ambiguous match */

       curarg = checkedargs;
       while ( curarg ) {
          if ( abbrmatch(curpar->name, curarg->name) ) {
             tmpstr = strcatalloc(" Ambiguous parameter: ",
                                  curarg->name);
             cxwrite(tmpstr, 5);
             free(tmpstr);
             argfree(checkedargs);
             return 0;
          }
          curarg = curarg->next;
       }

    /* Look for matching arg */

       curarg = *rootargptr;
       valset = 0;
       while ( curarg ) {
          if ( abbrmatch(curpar->name, curarg->name) ) {
             if ( valset ) {
                tmpstr = strcatalloc(" Multiple use of same parameter: ",
                      curpar->name);
                cxwrite(tmpstr, 5);
                free(tmpstr);
                argfree(checkedargs);
                return 0;
             }
             valset = 1;

             /* Assign parameter value if matched */

             if ( curpar->value ) free(curpar->value);
             if ( curpar->type == 'b' ) {
                if ( curarg->value ) {
                   tmpstr = strcatalloc(
                     " Value given for logical parameter ignored: ", 
                     curpar->name);
                   cxwrite(tmpstr, 5);
                   free(tmpstr);
                }
                valstr = trueval;
             } else {
                valstr = curarg->value;
             }
             curpar->value = allocval(curpar->type, valstr);
             if ( !curpar->value ) {
                tmpstr = strcatalloc(" No value supplied for: ",
                                  curpar->name);
                cxwrite(tmpstr, 5);
                free(tmpstr);
                argfree(checkedargs);
                return 0;
             }

             /* Move current argument into checkedargs if matched */

             curarg = argmove(lastarg, curarg, rootargptr, setargptr);
             setargptr = &(*setargptr)->next;

          } else {
             lastarg = curarg;
             curarg = curarg->next;
          }
       }
       curpar = curpar->next;
    }
    argfree(checkedargs);

    /*
     * Run through argument list, checking for name=value types
     * Reassemble argument if value is non-null, passing equal
     * on as it is not part of a parameter
     */
    curarg = *rootargptr;
    lastarg = NULL;
    while ( curarg ) {
       if ( curarg->value ) {

          valstr = strcatalloc("=", curarg->value);
          free(curarg->value);
          curarg->value = NULL;

          tmpstr = strcatalloc(curarg->name, valstr);
          free(valstr);
          free(curarg->name);
          curarg->name = tmpstr;
          tmpstr = strcatalloc(" Treating unrecognized parameter as argument: ",
                               curarg->name);
          cxwrite(tmpstr, 10);
          free(tmpstr);

       } else {
          lastarg = curarg;
          curarg = curarg->next;
       }
    }

    /*
     *  Look for q-type parameters to assign remaining arguments to
     *  Also, if parameter not set, assign default
     */
    curarg = *rootargptr;
    lastarg = NULL;
    curpar = parlist;
    while ( curpar ) {
       /* 
        *  If there are arguments left, and a q-type parameter
        *  has not been set, use the argument
        */
       if ( curarg && curpar->mode && *curpar->mode == 'q' 
                   && !curpar->value ) {
          curpar->value = allocval(curpar->type, curarg->name);
          curarg = argremove(lastarg, curarg, rootargptr);
       }

       /* Use default, if no value set */

       if ( !curpar->value && curpar->deflt ) {
          switch ( curpar->type ) {
             case 'b':
                vptr = malloc(sizeof(int));
                if ( !vptr ) return 0;
                * (int *) vptr = * (int *) curpar->deflt;
                curpar->value = vptr;
                break;
             case 'i':
                vptr = malloc(sizeof(int));
                if ( !vptr ) return 0;
                * (int *) vptr = * (int *) curpar->deflt;
                curpar->value = vptr;
                break;
             case 'r':
                vptr = malloc(sizeof(double));
                if ( !vptr ) return 0;
                * (double *) vptr = * (double *) curpar->deflt;
                curpar->value = vptr;
                break;
             case 's':
                vptr = (void *) stralloc( (char *) curpar->deflt);
                if ( !vptr ) return 0;
                curpar->value = vptr;
                break;
             default:
                cxwrite(" parassign: bad type", 5);
                return 0;
          }
       }
       curpar = curpar->next;
    }

    return 1;
}


void cgparl(pardef *parlist, char *parname, int *parval, int *status) {
/*
 *  Return logical parameter (C)
 */
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 'b' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         *parval = * (int *) foundpar->value;
      }
   }
}

void gparl(int cmdid, char *parname, int *parval, int *status) {
/*
 *  Return logical parameter (FORTRAN)
 */

   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cgparl(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(gparl,GPARL,gparl,INT,STRING,PLOGICAL,PINT)

void cgpari(pardef *parlist, char *parname, int *parval, int *status) {
/*
 *  Return integer parameter (C)
 */
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 'i' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         *parval = * (int *) foundpar->value;
      }
   }
}

void gpari(int cmdid, char *parname, int *parval, int *status) {
/*
 *  Return integer parameter (FORTRAN)
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cgpari(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(gpari,GPARI,gpari,INT,STRING,PINT,PINT)

void cgpard(pardef *parlist, char *parname, double *parval, int *status) {
/*
 *  Return double parameter
 */
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 'r' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         *parval = * (double *) foundpar->value;
      }
   }
}

void gpard(int cmdid, char *parname, double *parval, int *status) {
/*
 *  Return double parameter (FORTRAN)
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cgpard(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(gpard,GPARD,gpard,INT,STRING,PDOUBLE,PINT)

void cgparr(pardef *parlist, char *parname, float *parval, int *status) {
/*
 *  Return real parameter (C)
 */
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 'r' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         *parval = * (double *) foundpar->value;
      }
   }
}
void gparr(int cmdid, char *parname, float *parval, int *status) {
/*
 *  Return real parameter (FORTRAN)
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cgparr(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(gparr,GPARR,gparr,INT,STRING,PFLOAT,PINT)

void cgpars(pardef *parlist, char *parname, char *parval, int maxparlen, 
           int *status) {
/*
 *  Return string parameter (C)
 */
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 's' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         safestrcpy(parval, (char *) foundpar->value, maxparlen);
      }
   }
}
void gnpars(int cmdid, char *parname, char *parval, int maxparlen, 
            int *status) {
/*
 *  Return string parameter (FORTRAN)
 *   -> maxparval argument prevents string overflow
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cgpars(cmdlist[cmdid]->parlist, parname, parval, maxparlen, status);
}
FCALLSCSUB5(gnpars,GNPARS,gnpars,INT,STRING,PSTRING,INT,PINT)

void cgparld(pardef *parlist, char *parname, double *parval, int *parnum,
             int maxnum, int *status) {
/*
 *  Return double array (a string parameter, entered as Tcl list) (C)
 *  parnum always modified: use to determine whether parval was set
 */
   Tcl_Obj *lstObj, *itemObj, **objv;
   pardef *foundpar;
   char *tmpstr, *liststr, **listary;
   int i, objc;
   double dd;

   *parnum = 0;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 's' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         liststr = stralloc( (char *) foundpar->value);
         listary = quosplit(liststr, ",");
         if ( listary[0] && listary[1] ) {
         /* 
          * If there are at least 2 in the array, must be ,-delimited 
          */
            i = 0;
            while ( listary[i] ) {
               itemObj = Tcl_NewStringObj(listary[i], -1);
               Tcl_GetDoubleFromObj(xm_interp, itemObj, &dd);
               if ( i+1 > maxnum ) {
                  *parnum = 0;
                  cxwrite("cgparld: Input list larger than array", 10);
                  *status = -1;
                  free (liststr);
                  free (listary);
                  return;
               }
               parval[i] = dd;
               i++;
            }
            *parnum = i;
            free (liststr);
            free (listary);
         } else {
            free (liststr);
            free (listary);
         /* 
          * Otherwise, let Tcl interpret as a list
          */
            lstObj = Tcl_NewStringObj( (char *) foundpar->value, -1 );
            Tcl_ListObjGetElements(xm_interp, lstObj, &objc, &objv);
            if ( objc > maxnum ) {
               cxwrite("cgparld: Input list larger than array", 10);
               *status = -1;
               return;
            }
            *parnum = objc;
            for ( i = 0; i < objc; i++ ) {
               Tcl_GetDoubleFromObj(xm_interp, objv[i], &dd);
               parval[i] = dd;
            }
         }
      }
   }
}
void gparld(int cmdid, char *parname, double *parval, int *parnum, 
            int maxnum, int *status) {
/*
 *  Return double array (a string parameter, entered as Tcl list) (C)
 *   -> maxnum argument prevents array overflow
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cgparld(cmdlist[cmdid]->parlist, parname, parval, parnum, maxnum, 
           status);
}
FCALLSCSUB6(gparld,GPARLD,gparld,INT,STRING,DOUBLEV,PINT,INT,PINT)

void cgparlr(pardef *parlist, char *parname, float *parval, int *parnum,
             int maxnum, int *status) {
/*
 *  Return real array (a string parameter, entered as Tcl list) (C)
 *  parnum always modified: use to determine whether parval was set
 */
   int i;
   double *buff;
   
   buff = malloc(maxnum*sizeof(double));
   if ( !buff ) {
      *status = -1;
      cxwrite("cgparlr: failed to allocate double prec buffer", 5);
      return;
   }
   cgparld(parlist, parname, buff, parnum, maxnum, status);
   for ( i = 0; i < *parnum; i++ ) {
      parval[i] = buff[i];
   }
   free(buff);
}
void gparlr(int cmdid, char *parname, float *parval, int *parnum, 
            int maxnum, int *status) {
/*
 *  Return real array (a string parameter, entered as Tcl list) (C)
 *   -> maxnum argument prevents array overflow
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cgparlr(cmdlist[cmdid]->parlist, parname, parval, parnum, maxnum, 
           status);
}
FCALLSCSUB6(gparlr,GPARLR,gparlr,INT,STRING,FLOATV,PINT,INT,PINT)

/*
 *  Modify parameter structure (for journalling)
 */

void csparl(pardef *parlist, char *parname, int parval, int *status) {
/*
 *  Set logical parameter (C)
 */
   void *vptr;
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 'b' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         * (int *) foundpar->value = parval;
      } else {
          vptr = malloc(sizeof(int));
          if ( vptr ) {
             foundpar->value = vptr;
             * (int *) vptr = parval;
          }
      }
   }
}

void sparl(int cmdid, char *parname, int parval, int *status) {
/*
 *  Set logical parameter (FORTRAN)
 */

   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   csparl(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(sparl,SPARL,sparl,INT,STRING,LOGICAL,PINT)

void cspari(pardef *parlist, char *parname, int parval, int *status) {
/*
 *  Set integer parameter (C)
 */
   void *vptr;
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 'i' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         * (int *) foundpar->value = parval;
      } else {
          vptr = malloc(sizeof(int));
          if ( vptr ) {
             foundpar->value = vptr;
             * (int *) vptr = parval;
          }
      }
   }
}

void spari(int cmdid, char *parname, int parval, int *status) {
/*
 *  Set integer parameter (FORTRAN)
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cspari(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(spari,SPARI,spari,INT,STRING,INT,PINT)

void cspard(pardef *parlist, char *parname, double parval, int *status) {
/*
 *  Set double parameter (C)
 */
   void *vptr;
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 'r' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         * (double *) foundpar->value = parval;
      } else {
          vptr = malloc(sizeof(double));
          if ( vptr ) {
             foundpar->value = vptr;
             * (double *) vptr = parval;
          }
      }
   }
}

void spard(int cmdid, char *parname, double parval, int *status) {
/*
 *  Set double parameter (FORTRAN)
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cspard(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(spard,SPARD,spard,INT,STRING,DOUBLE,PINT)

void csparr(pardef *parlist, char *parname, float parval, int *status) {
/*
 *  Set real parameter (C)
 */
   void *vptr;
   pardef *foundpar;
   char *tmpstr;
   double dd;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 'r' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      dd = parval;
      if ( foundpar->value ) {
         * (double *) foundpar->value = dd;
      } else {
          vptr = malloc(sizeof(double));
          if ( vptr ) {
             foundpar->value = vptr;
             * (double *) vptr = dd;
          }
      }
   }
}
void sparr(int cmdid, char *parname, float parval, int *status) {
/*
 *  Set real parameter (FORTRAN)
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   csparr(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(sparr,SPARR,sparr,INT,STRING,FLOAT,PINT)

void cspars(pardef *parlist, char *parname, char *parval, int *status) {
/*
 *  Set string parameter (C)
 */
   pardef *foundpar;
   char *tmpstr;

   if ( *status != 0 ) return;  /* If status.ne.0 don't modify variable */

   foundpar = parfind(parlist, parname);
   if ( foundpar ) {
      if ( foundpar->type != 's' ) {
         tmpstr = strcatalloc("Type mismatch: ", foundpar->name);
         cxwrite(tmpstr, 5);
         free(tmpstr);
         *status = -1;
         return;
      }
      if ( foundpar->value ) {
         free(foundpar->value);
      }
      foundpar->value = (void *) stralloc(parval);
   }
}

void spars(int cmdid, char *parname, char *parval, int *status) {
/*
 *  Set string parameter (FORTRAN)
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cspars(cmdlist[cmdid]->parlist, parname, parval, status);
}
FCALLSCSUB4(spars,SPARS,spars,INT,STRING,STRING,PINT)

/*
 *  Command argument retrieval from FORTRAN
 */

void numcarg(int cmdid, int *n, int *status) {
/*
 *  Retrieve number of command arguments
 */
   if ( *status != 0 ) return;

   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }

   *n = cmdlist[cmdid]->cmdargc;
}
FCALLSCSUB3(numcarg,NUMCARG,numcarg,INT,PINT,PINT)

void nextcarg(int cmdid, char *argstr, int maxlen, int *status) {
/*
 *  Retrieve next command arguments
 */
   if ( *status != 0 ) return;

   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }

   if ( !cmdlist[cmdid]->curarg ) {
      *status = -1;
      return;
   }
   safestrcpy(argstr, cmdlist[cmdid]->curarg->name, maxlen);
   cmdlist[cmdid]->curarg = cmdlist[cmdid]->curarg->next;
}
FCALLSCSUB4(nextcarg,NEXTCARG,nextcarg,INT,PSTRING,INT,PINT)

void catcarg(int cmdid, char *argstr, int maxlen, int *status) {
/*
 *  Concatenate remaining arguments into string and return
 */
   Tcl_DString *tmpdstr;

   if ( *status != 0 ) return;

   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }

   if ( !cmdlist[cmdid]->curarg ) {
      *status = -1;
      return;
   }

   tmpdstr = (Tcl_DString *)malloc(sizeof(Tcl_DString));
   Tcl_DStringInit(tmpdstr);
   while ( cmdlist[cmdid]->curarg ) {
      Tcl_DStringAppendElement(tmpdstr, cmdlist[cmdid]->curarg->name);
      cmdlist[cmdid]->curarg = cmdlist[cmdid]->curarg->next;
   }
   safestrcpy(argstr, Tcl_DStringValue(tmpdstr), maxlen);
   Tcl_DStringFree(tmpdstr);
   free(tmpdstr);
}
FCALLSCSUB4(catcarg,CATCARG,catcarg,INT,PSTRING,INT,PINT)

void cwrongargs(argdef *cmdargs) {
/*
 *  Standard error message for wrong number of arguments
 */
   char *msg;
   Tcl_DString *tmpdstr;
   argdef *curarg;

   curarg = cmdargs;
   if ( !curarg ) {
      cxwrite(" Wrong number of arguments", 10);
   } else {
      tmpdstr = (Tcl_DString *)malloc(sizeof(Tcl_DString));
      Tcl_DStringInit(tmpdstr);
      while ( curarg ) {
         Tcl_DStringAppendElement(tmpdstr, curarg->name);
         curarg = curarg->next;
      }
      msg = strcatalloc(" Wrong number of arguments: ",
                        Tcl_DStringValue(tmpdstr));
      cxwrite(msg, 10);
      free(msg);
      Tcl_DStringFree(tmpdstr);
      free(tmpdstr);
   }
}

void wrongargs(int cmdid, int *status) {
/*
 *  Standard error message for wrong number of arguments
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }

   cwrongargs(cmdlist[cmdid]->cmdargs);
   *status = -1;
}
FCALLSCSUB2(wrongargs,WRONGARGS,wrongargs,INT, PINT)

/*
 *  FORTRAN hook for journalling (journal.c has C implementation)
 */ 
void jrncmd(int cmdid, int *status) {
/*
 *  Journal command corresponding to cmdid (FORTRAN)
 */
   if ( cmdid < 0 || cmdid >= numcmds ) {
      cxwrite("Invalid command id", 5);
      *status = -1;
      return;
   }
   cjrncmd(cmdlist[cmdid], status);
}
FCALLSCSUB2(jrncmd,JRNCMD,jrncmd,INT,PINT)
