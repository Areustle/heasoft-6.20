#ifndef CMDDEF_H
#define CMDDEF_H


/*
 *  Parameter definition
 */

struct par {
   char *name;       /* Parameter name */
   char type;        /* Parameter type (b=bool, i=int, r=real, s=string) */
   char *mode;       /* Mode (q=query, l=learn, a=auto, h=hidden) [IGNORED] */
   void *deflt;      /* Default value */
   void *min;        /* Minimum value */
   void *max;        /* Maximum value */
   char *prompt;     /* Prompt string [IGNORED] */
   void *value;      /* Parameter value */
   struct par *next; /* Pointer to next parameter */
};

typedef struct par pardef;

/*
 *  Argument definition
 */

struct arg {
   char* name;        /* Parameter name */
   char* value;       /* Parameter value (string) */
   struct arg *next;  /* Pointer to next arg */
};
   
typedef struct arg argdef;

/*
 *  Command definition
 */

typedef struct {
   int id;          /* Command id (location in array) */
   char *name;      /* Command name */
   pardef *parlist; /* Parameter list */
   int cmdargc;     /* Number of command arguments */
   argdef *cmdargs; /* Command arguments */
   argdef *curarg;  /* Current command argument */
} cmddef;

/* Function prototypes */

void *allocval(char, char *);
void argfree(argdef *);
void parfree(pardef *);
void cmdfree(cmddef *);
argdef *arginit();
pardef *parinit();
cmddef *cmdinit();
cmddef *cmdload (char *);
pardef *parload (char *);
pardef *parfind(pardef *parlist, char *parname);
int parassign(pardef *parlist, argdef **rootargptr);
void cgparl(pardef *, char *, int *, int *);
void cgpari(pardef *, char *, int *, int *);
void cgpard(pardef *, char *, double *, int *);
void cgparr(pardef *, char *, float *, int *);
void cgpars(pardef *, char *, char *, int, int *);
void csparl(pardef *, char *, int, int *);
void cspari(pardef *, char *, int, int *);
void cspard(pardef *, char *, double, int *);
void csparr(pardef *, char *, float, int *);
void cspars(pardef *, char *, char *, int *);
void cwrongargs(argdef *);
int CParseParm(Tcl_Interp *, int, Tcl_Obj * CONST [], cmddef *);
void cjrncmd(cmddef *, int *);

#endif /* CMDDEF_H */
