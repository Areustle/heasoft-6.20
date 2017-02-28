/* These functions support the execution of the Yacc code contained in the
 * brcaldb.y file. They should be reintegrated to the brcaldb.y file when
 * DEC ultrix is no longer supported.  See the comments in the brcaldb.y file */

#include <stdio.h>

#include "ftools.h"

#include "cifdata.h"

/*
no longer needed since we pass -DHAVE_ALLOCA_H
(and since the current version of bison generates
correct conditionals to include it when appropriate)

#ifdef __sgi
#include <alloca.h>
#endif
*/
 
extern Cifdata *cifdata;
extern int *selrows;

/* These variables define the internal string for YY_INPUT */
extern char expr[];
extern char *exprptr, *exprlim;

extern int read_string;

extern int  result;
extern int  sren;
extern char tmpfilbuf[200];
extern int  errstat,srch,i;
extern int  BufLen_3;

#include "optr.h"

extern int parse_error;
extern int exprlen;

extern void cbrestart();
extern FILE *cbin;

int strgsel(s1,op,s2)
char *s1, *s2;
Optyp op;
{
    int srch;

    srch = strsel(&s1,&s2);

    if ((op == OPNE)) {
         if (srch) {
              return (0);
         }
         else {
              return (1);
         }
    }
    else {
         if (srch) {
              return (1);
         }
         else {
              return (0);
         }
    }
}

int strsel(sa,sb)
char **sa, **sb;
{
    char *s1, *s2;
    int front=0, back=0, ln1, ln2, i;
    char *ssptr, *tmpstr;

    s1 = *sa;
    s2 = *sb;

    ln1 = strlen(s1);
    ln2 = strlen(s2);

    for(i=0;i<ln2;i++)
        s2[i] = toupper(s2[i]);

    tmpstr = s1;
    s1 = (char *) malloc((ln1+1) * sizeof(char));

    for(i=0;i<ln1;i++)
        s1[i] = toupper(tmpstr[i]);

    s1[ln1] = '\0';

    if (s2[0] == '*') {
        front = 1;
        s2++;
        ln2--;
    }

    if (s2[ln2-1] == '*') {
        back = 1;
        ln2--;
    }

    if (ln1 < ln2) {
        free((char *) s1);
        return (0);
    }

    if ((!front) && (!back) && (strcmp(s1,s2) == 0)) {
        free((char *) s1);
        return 1;
    }

    ssptr = NULL;
    for (i=0;i<ln1;i++) {
        if (strncmp(&s1[i],s2,ln2) == 0) {
            ssptr = &s1[i];
            break;
        }
    }

    if (( front) && ( back) && (ssptr != NULL)) {
        free((char *) s1);
        return 1;
    }
    if ((!front) && ( back) && (ssptr == &s1[0])) {
        free((char *) s1);
        return 1;
    }
    if (( front) && (!back) && (ssptr == &s1[ln1-ln2])) {
        free((char *) s1);
        return 1;
    }

    free((char *) s1);
    return 0;
}

int intsel(i1,op,s)
int i1;
Optyp op;
char *s;
{
    int i2;

    sscanf(s,"%d",&i2);

    switch(op) {
        case OPEQ: if (i1 == i2) return (1); else return (0);
        case OPNE: if (i1 != i2) return (1); else return (0);
        case OPGT: if (i1 >  i2) return (1); else return (0);
        case OPLT: if (i1 <  i2) return (1); else return (0);
        case OPGE: if (i1 >= i2) return (1); else return (0);
        case OPLE: if (i1 <= i2) return (1); else return (0);
    }
}

int dblsel(d1,op,s)
double d1;
Optyp op;
char *s;
{
    double d2;

    sscanf(s,"%lf",&d2);

    switch(op) {
        case OPEQ: if (d1 == d2) return (1); else return (0);
        case OPNE: if (d1 != d2) return (1); else return (0);
        case OPGT: if (d1 >  d2) return (1); else return (0);
        case OPLT: if (d1 <  d2) return (1); else return (0);
        case OPGE: if (d1 >= d2) return (1); else return (0);
        case OPLE: if (d1 <= d2) return (1); else return (0);
    }
}

int datesel(d1,op,s)
double d1;
Optyp op;
char *s;
{
    double d2;
    int errstat = 0;

    Dt2mjd(s,1,&d2,&errstat);

    switch(op) {
        case OPEQ: if ((int)d1 == (int)d2) return (1); else return (0);
        case OPNE: if ((int)d1 != (int)d2) return (1); else return (0);
        case OPGT: if ((int)d1 >  (int)d2) return (1); else return (0);
        case OPLT: if ((int)d1 <  (int)d2) return (1); else return (0);
        case OPGE: if ((int)d1 >= (int)d2) return (1); else return (0);
        case OPLE: if ((int)d1 <= (int)d2) return (1); else return (0);
    }
        
}

int timesel(d1,op,s)
double d1;
Optyp op;
char *s;
{
    double d2;
    int errstat = 0;

    Tim2df(s,1,&d2,&errstat);

    switch(op) {
        case OPEQ: if ((d1-(double)((int)d1))==d2) return (1); else return (0);
        case OPNE: if ((d1-(double)((int)d1))!=d2) return (1); else return (0);
        case OPGT: if ((d1-(double)((int)d1))> d2) return (1); else return (0);
        case OPLT: if ((d1-(double)((int)d1))< d2) return (1); else return (0);
        case OPGE: if ((d1-(double)((int)d1))>=d2) return (1); else return (0);
        case OPLE: if ((d1-(double)((int)d1))<=d2) return (1); else return (0);
    }
        
}

prmssel(cbd,param,value)
char cbd[][71];
char *param;
char *value;
{
    int i;
    int plen;
    int reslt,errstat;

    for (i=0;i<strlen(param);i++)
        param[i] = toupper(param[i]);

    for (i=0;i<strlen(value);i++)
        value[i] = toupper(value[i]);

    for (i=0;i<9;i++) {
        if (strncmp(cbd[i],"NONE",4) == 0) break;
        plen = (int)(strchr(cbd[i],'(') - cbd[i]);
        if (strncmp(param,cbd[i],plen) == 0) {
            Cbdcs(cbd[i],value,&reslt,&errstat);
            if (reslt) return (1);
            return (0);
        }
    }
    return (1);
}

prmnsel(cbd,param,op,value)
char cbd[][71];
char *param;
Optyp op;
char *value;
{
    int i;
    int plen;
    int reslt,errstat;
    float rval;

    for (i=0;i<strlen(param);i++)
        param[i] = toupper(param[i]);

    for (i=0;i<9;i++) {
        if (strncmp(cbd[i],"NONE",4) == 0) break;
        plen = (int)(strchr(cbd[i],'(') - cbd[i]);
        if (strncmp(param,cbd[i],plen) == 0) {
            sscanf(value,"%f",&rval);
            switch(op) {
                case OPEQ: Cbdcr(cbd[i],"eq",rval,&reslt,&errstat); break;
                case OPNE: Cbdcr(cbd[i],"ne",rval,&reslt,&errstat); break;
                case OPGT: Cbdcr(cbd[i],"gt",rval,&reslt,&errstat); break;
                case OPLT: Cbdcr(cbd[i],"lt",rval,&reslt,&errstat); break;
                case OPGE: Cbdcr(cbd[i],"ge",rval,&reslt,&errstat); break;
                case OPLE: Cbdcr(cbd[i],"le",rval,&reslt,&errstat); break;
            }
            if (reslt) return (1);
            return (0);
        }
    }
    return (1);
}

cifselector(nrows,newrows,nnewrows,status)
/*selrows and cifdata are defined externally */
int nrows;
int newrows[];
int *nnewrows;
int *status;
{

    int j;

    *nnewrows = 0;

    begin_initial();
    cbrestart(cbin);

    for (sren=0;sren<nrows;sren++) {

        exprptr = expr;
        exprlim = expr + strlen(exprptr) + 1;
        exprlen = 0;

        read_string = 1;
 
        cbparse();
 
        if (parse_error != 0) {
            *status = 1;
            begin_initial();
            cbrestart(cbin);
            return;
        }

        /*printf("Result: %d\n",result);*/
 
        if (result) {
            newrows[*nnewrows] = selrows[sren];
            (*nnewrows)++;
        }
 
        begin_initial();
        cbrestart(cbin);
 
    }
 
}
