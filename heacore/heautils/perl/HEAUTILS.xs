/*  Perl interface to Headas's heautils librarys */
/*  Author: Ziqin Pan                           */
/*  December 24, 2003                           */

/*  History:                                    */
/*         August 19, 2004, Ziqin Pan          */
/*            Add HDgtcalf wrapper              */


#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "headas_utils.h"
#include "util.h"

MODULE = HEACORE::HEAUTILS		PACKAGE = HEACORE::HEAUTILS		


int
HDgtcalf(tele,instr,detnam,filt,codenam,strtdate,strtime,stpdate,stptime,expr,maxret,fnamesize,filenamref,extnoref,onlineref,nret,nfound,status)
        const char * tele
        const char * instr
        const char * detnam
        const char * filt
        const char * codenam
        const char * strtdate
        const char * strtime
        const char * stpdate
        const char * stptime
        const char * expr
        int maxret
        int fnamesize
        int &nret
        int &nfound
        int &status
        PREINIT:
        char ** filenam;
        long * extno;
        char ** online;
        int i;
        AV * avref;
        CODE:

        filenam = (char**) calloc(maxret+1,sizeof(char*));
        for (i=0; i<maxret; i++) {
          filenam[i] = (char *) calloc(fnamesize,sizeof(char));
        }

        extno = (long *) calloc(maxret+1,sizeof(long));
        online = (char**) calloc(maxret+1,sizeof(char*));
        for (i=0; i<maxret; i++) {
          online[i] = (char *) calloc(fnamesize,sizeof(char));
        }

        RETVAL=HDgtcalf(tele,instr,detnam,filt,codenam,strtdate,strtime,stpdate,stptime,expr,maxret,fnamesize,filenam,extno,online,&nret,&nfound,&status);

        avref = (AV*)sv_2mortal((SV*)newAV());
        for (i=0; i<maxret; i++) {
             av_push(avref,newSViv((IV) extno[i]));
        }
        sv_setsv(ST(13),newRV((SV*) avref));

        avref = (AV*)sv_2mortal((SV*)newAV());
        for (i=0; i<maxret; i++) {
             av_push(avref,newSVpv(filenam[i],strlen(filenam[i])));
        }
        sv_setsv(ST(12),newRV((SV*) avref));

        avref = (AV*)sv_2mortal((SV*)newAV());
        for (i=0; i<maxret; i++) {
             av_push(avref,newSVpv(online[i],strlen(online[i])));
        }
        sv_setsv(ST(14),newRV((SV*) avref));

        for (i=0; i<maxret; i++) {
             if(filenam[i]) free(filenam[i]);
             if(online[i]) free(online[i]);
        }

        if(filenam) free(filenam);
        if(online) free(online);
        if(extno) free(extno);
        OUTPUT:
        nret
        nfound
        status
        RETVAL







int 
HDpar_stamp(fptr, hdunum, status)
	fitsfile * fptr
	int hdunum
	int &status
	OUTPUT:
		status
		RETVAL


void
_HDpar_note(par,formatted)
	const char * par
	const char * formatted
	CODE:
		HDpar_note(par,formatted);



void
set_history(headas_histpar)
	int headas_histpar

void
set_toolname(name)
	char *name

void
set_toolversion(vers)
	char *vers

int
headas_clobberfile(file)
	char *file
	OUTPUT:
		RETVAL
