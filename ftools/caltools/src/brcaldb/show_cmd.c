#include <stdio.h>
#include <string.h>
#include "ftools.h"

#include "optr.h"
#include "y_tab.h"
#include "colm.h"
#include "cifdata.h"

extern Cifdata *cifdata;
extern int *selrows;

#define DISPLAYLEN 1500
char display[DISPLAYLEN];

Colmnode *colhead;
extern int sren;

char caldbstr[200];
int caldblen=0;
extern int BufLen_3;

void display_cl(char *rowstr, Colmnode *colhead, int wrap, int *cont_disp);

void show_cmd(colhead,nranges,rowrange1,rowrange2,wrap) 
Colmnode *colhead;
int nranges;
int rowrange1[15];
int rowrange2[15];
int wrap;
{

    Colmnode *colmnode;
    char *tmptitlestr;
    int cifdatalen, disp_cbd=0, cont_disp=1;
    char rowstr[7];
    int range,maxdisplen=0,displen,i;
    char contxt[160];
    int pgfstatus;

    display[0] = '\0';

    caldbstr[0]='\0';
    BufLen_3 = 199;
    Ctrlog("CALDB",5,caldbstr,&caldblen);
    if (strchr(caldbstr,'[') != (char *) NULL ) {
        caldblen = 6;
    }

    /* For each column... */
    for(colmnode=colhead;colmnode!=(Colmnode *) NULL;colmnode=colmnode->next) {

        /* get column title */
        gtcoltitle(colmnode -> col,&tmptitlestr,&(colmnode -> displen));

        /* search rows to be displayed to find largest display field */
        for (range=0;range<nranges;range++) {
            for (sren=rowrange1[range]-1;sren<rowrange2[range];sren++) {

                /* +2 for spaces between cols in display*/
                cifdatalen = gtcifdatalen(colmnode -> col,sren) +2;

                if (cifdatalen > colmnode -> displen) {
                    colmnode -> displen = cifdatalen;
                }
            }
        }

        /* calculate max size of display buffer needed */
        maxdisplen += colmnode -> displen;

        /* allocate display buffers for this field */
        for (i=1;i<=9;i++) {
            colmnode -> dispstr[i-1] = (char *) malloc((colmnode -> displen)+1);
        }

        /* copy field title to display buffer */
        strcpy((colmnode -> dispstr)[0], tmptitlestr);
        padsp((colmnode->dispstr)[0],(colmnode->displen)-strlen(tmptitlestr));
        free(tmptitlestr);
        colmnode -> ndispstr = 1;

        /* set CBD display flag */
        if ((colmnode -> col) == CBD) disp_cbd = 1;
    }

    if ((maxdisplen + 6) > (DISPLAYLEN - 1)) {
        strcpy(contxt,"Error: display buffer overflow");
        Fcerr(contxt);
        return;
    }

    /* Print titles */
    display_cl("      ",colhead,wrap,&cont_disp);
    Pgfout(-1,0," ",&pgfstatus);
    pgfstatus = 0;

    for (range=0;range<nranges;range++) {
        for (sren=rowrange1[range]-1;sren<rowrange2[range];sren++) {
            sprintf(rowstr,"%4d  ",sren+1);

            for(colmnode=colhead;colmnode!=(Colmnode *) NULL;
            colmnode=colmnode->next) {
                gtdispstr(sren,colmnode);
            }

            display_cl(rowstr,colhead,wrap,&cont_disp);
            if (!cont_disp) break;
        }
        if (!cont_disp) break;
    }

}

gtcoltitle(col,title,titlen)
int col;
char **title;
int *titlen;
{
    int bufsz = 0;

    switch (col) {
        case MISSION : 	bufsz = 10; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"MISSION  \0"); 
			break;
        case INSTRUM : 	bufsz = 7; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"INST  \0"); 
			break;
        case DETNAM  : 	bufsz = 11; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"DETECTOR  \0"); 
			break;
        case FILTER  : 	bufsz = 9; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"FILTER  \0"); 
			break;
        case DEVICE  : 	bufsz = 9; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"DEVICE  \0"); 
			break;
        case DIR     : 	bufsz = 12; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"DIRECTORY  \0"); 
			break;
        case CFILE   : 	bufsz = 7; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"FILE  \0"); 
			break;
        case CLASS   : 	bufsz = 8; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"CLASS  \0"); 
			break;
        case DATATYP : 	bufsz = 7; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"TYPE  \0"); 
			break;
        case CODENAM : 	bufsz = 10; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"DATASET  \0"); 
			break;
        case EXTNO   : 	bufsz = 6; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"EXT  \0"); 
			break;
        case DATE    : 	bufsz = 6; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"VSD  \0"); 
			break;
        case VSD     : 	bufsz = 6; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"VSD  \0"); 
			break;
        case TIME    : 	bufsz = 6; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"VST  \0"); 
			break;
        case VST     : 	bufsz = 6; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"VST  \0"); 
			break;
        case REFTIME : 	bufsz = 10; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"REFTIME  \0"); 
			break;
        case QUALITY : 	bufsz = 10; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"QUALITY  \0"); 
			break;
        case CALDATE : 	bufsz = 12; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"INST-DATE  \0"); 
			break;
        case DESCRIP : 	bufsz = 14; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"DESCRIPTION  \0"); 
			break;
        case CIF     :  bufsz = 6;
                        *title = (char *) malloc(bufsz);
                        strcpy(*title,"CIF  \0");
                        break;
        case CBD     : 	bufsz = 9; 
			*title = (char *) malloc(bufsz);
			strcpy(*title,"BOUNDS  \0"); 
			break;
    }

    *titlen = bufsz - 1;

}

int gtcifdatalen(col,row) 
int col,row;
{
    int i, maxcbdlen=0;

    switch (col) {
        case MISSION : return(cifdata[selrows[row]].telelen);
        case INSTRUM : return(cifdata[selrows[row]].instlen);
        case DETNAM  : return(cifdata[selrows[row]].detnlen);
        case FILTER  : return(cifdata[selrows[row]].filtlen);
        case DEVICE  : return(cifdata[selrows[row]].devlen);
        case DIR     : return(cifdata[selrows[row]].dirlen);
        case CFILE   : return((caldblen                      +
                               cifdata[selrows[row]].dirlen  +
                               cifdata[selrows[row]].filelen + 2));
        case CLASS   : return(cifdata[selrows[row]].claslen);
        case DATATYP : return(cifdata[selrows[row]].dtyplen);
        case CODENAM : return(cifdata[selrows[row]].cnamlen);
        case EXTNO   : return(2);
        case DATE    : return(8);
        case VSD     : return(8);
        case TIME    : return(8);
        case VST     : return(8);
        case REFTIME : return(12);
        case QUALITY : return(1);
        case CALDATE : return(8);
        case DESCRIP : return(cifdata[selrows[row]].desclen);
        case CIF     : return(cifdata[selrows[row]].ciflen);
        case CBD     : for (i=0;i<9;i++) {
                           if (cifdata[selrows[row]].cbdlen[i] > maxcbdlen) {
                               maxcbdlen = cifdata[selrows[row]].cbdlen[i];
                           }
                       }
                       return(maxcbdlen);
    }

}

gtdispstr(row,colmnode)
int row;
Colmnode *colmnode;
{
    int nsp, i, tmpfilelen, errstat=0;
    char tmpfilestr[200];

    for(i=0;i<9;i++) {
        (colmnode -> dispstr)[i][0] = '\0';
    }

    switch (colmnode -> col) {
        case MISSION : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].telescop);
            colmnode -> dispstr[0][cifdata[selrows[row]].telelen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].telelen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case INSTRUM : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].instrume);
            colmnode -> dispstr[0][cifdata[selrows[row]].instlen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].instlen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case DETNAM  : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].detnam);
            colmnode -> dispstr[0][cifdata[selrows[row]].detnlen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].detnlen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case FILTER  : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].filter);
            colmnode -> dispstr[0][cifdata[selrows[row]].filtlen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].filtlen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case DEVICE  : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_dev);
            colmnode -> dispstr[0][cifdata[selrows[row]].devlen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].devlen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case DIR     : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_dir);
            colmnode -> dispstr[0][cifdata[selrows[row]].dirlen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].dirlen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case CFILE   : 

            strcpy(tmpfilestr,cifdata[selrows[row]].cal_file);
            tmpfilestr[cifdata[selrows[row]].filelen] = '\0';

            Cpthnm("CALDB",cifdata[selrows[row]].cal_dir,tmpfilestr,&errstat);
            if (errstat != 0) errstat = 0;

            strcpy(colmnode -> dispstr[0],tmpfilestr);
            tmpfilelen = strlen(tmpfilestr);
            colmnode -> dispstr[0][tmpfilelen] = '\0';

            nsp = (colmnode -> displen) - tmpfilelen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case CLASS   : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_clas);
            colmnode -> dispstr[0][cifdata[selrows[row]].claslen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].claslen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case DATATYP : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_dtyp);
            colmnode -> dispstr[0][cifdata[selrows[row]].dtyplen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].dtyplen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case CODENAM : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_cnam);
            colmnode -> dispstr[0][cifdata[selrows[row]].cnamlen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].cnamlen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case EXTNO   : 

            sprintf(colmnode->dispstr[0],"%2d",cifdata[selrows[row]].cal_xno);
            colmnode -> dispstr[0][2] = '\0';

            nsp = (colmnode -> displen) - 2;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case DATE    : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_vsd);
            colmnode -> dispstr[0][8] = '\0';

            nsp = (colmnode -> displen) - 8;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case VSD     : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_vsd);
            colmnode -> dispstr[0][8] = '\0';

            nsp = (colmnode -> displen) - 8;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case TIME    : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_vst);
            colmnode -> dispstr[0][8] = '\0';

            nsp = (colmnode -> displen) - 8;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case VST     : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_vst);
            colmnode -> dispstr[0][8] = '\0';

            nsp = (colmnode -> displen) - 8;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case REFTIME : 

            sprintf(colmnode->dispstr[0],"%5.6f",cifdata[selrows[row]].ref_time);
            colmnode -> dispstr[0][12] = '\0';

            nsp = (colmnode -> displen) - 12;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case QUALITY : 

            sprintf(colmnode->dispstr[0],"%1d",cifdata[selrows[row]].cal_qual);
            colmnode -> dispstr[0][1] = '\0';

            nsp = (colmnode -> displen) - 1;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case CALDATE : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_date);
            colmnode -> dispstr[0][8] = '\0';

            nsp = (colmnode -> displen) - 8;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case DESCRIP : 

            strcpy(colmnode -> dispstr[0],cifdata[selrows[row]].cal_desc);
            colmnode -> dispstr[0][cifdata[selrows[row]].desclen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].desclen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case CIF     :

            strcpy(colmnode->dispstr[0],cifdata[selrows[row]].cif);
            colmnode -> dispstr[0][cifdata[selrows[row]].ciflen] = '\0';

            nsp = (colmnode -> displen) - cifdata[selrows[row]].ciflen;
            padsp(colmnode -> dispstr[0],nsp);

            nsp = colmnode -> displen;
            for(i=1;i<9;i++) {
                padsp(colmnode -> dispstr[i],nsp);
            }

            colmnode -> ndispstr = 1;

            break;

        case CBD     : 

            for (i=0;i<9;i++) {
                strcpy(colmnode -> dispstr[i],cifdata[selrows[row]].cal_cbd[i]);
                colmnode -> dispstr[i][cifdata[selrows[row]].cbdlen[i]] = '\0';

                nsp = (colmnode -> displen) - cifdata[selrows[row]].cbdlen[i];
                padsp((colmnode -> dispstr)[i],nsp);

            }

            colmnode -> ndispstr = 9;

            break;
    }
}

padsp(string,nsp)
char *string;
int nsp;
{
    int i;

    string += strlen(string);

    for (i=0;i<nsp;i++) {
        string[i] = ' ';
    }

    string[nsp] = '\0';
}

void display_cl(rowstr,colhead,wrap,cont_disp) 
char *rowstr;
Colmnode *colhead;
int wrap;
int *cont_disp;
{
    Colmnode *colmnode, *firstcol, *lastcol, *oldlastcol=0;
    int width=0;
    int maxndispstr = 1;
    int pgfstatus = 0;
    int i;

    *cont_disp = 1;

    if (!wrap) {  /* If no wrap, print whole dispaly buffer in one chunk */

        for (colmnode=colhead;colmnode!=(Colmnode *) NULL;
        colmnode=colmnode->next) {
            if ((colmnode -> ndispstr) > maxndispstr) {
                maxndispstr = (colmnode -> ndispstr);
            }
        }

        for (i=0;i<maxndispstr;i++) {
            display[0] = '\0';
            if (i == 0) {
                strcat(display,rowstr);
            }
            else {
                strcat(display,"      ");
            }

            for(colmnode=colhead;colmnode!=(Colmnode *) NULL;
            colmnode=colmnode->next) {
                strcat(display,colmnode->dispstr[i]);
            }

            Pgfout(-1,0,display,&pgfstatus);
            if (pgfstatus != 0) {
                *cont_disp = 0;
                return;
            }
        }

    }

    else { /* Otherwise, split display into 80 byte chunks */

        firstcol = colhead;
        while(1) {
            maxndispstr = 1;

            for(lastcol=firstcol;lastcol!=(Colmnode *) NULL;
            lastcol=lastcol->next) {
                if((width + lastcol->displen) > 76) /*80-6+2*/ {
                    width = 0;
                    break;
                }

                width += lastcol -> displen;
                oldlastcol = lastcol;

                if((lastcol -> ndispstr) > maxndispstr) {
                    maxndispstr = lastcol -> ndispstr;
                }

            }

            lastcol = oldlastcol;

            for(i=0;i<maxndispstr;i++) {
                display[0] = '\0';
                if ((firstcol == colhead) && (i == 0)) {
                    strcat(display,rowstr);
                }
                else {
                    strcat(display,"      ");
                }

                for(colmnode=firstcol;colmnode!=lastcol->next;
                colmnode=colmnode->next) {
                    strcat(display,(colmnode->dispstr)[i]);
                }

                Pgfout(-1,0,display,&pgfstatus);
                if (pgfstatus != 0) {
                    *cont_disp = 0;
                    return;
                }
            }

            firstcol = lastcol -> next;
            if (firstcol == (Colmnode *) NULL) break;

        }
    }
}
