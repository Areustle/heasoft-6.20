#include <string.h>

#include "ftools.h"

#include "cifdata.h"

extern int sren;
extern Cifdata *cifdata;
extern int *selrows;

extern int BufLen_3;

copy_cmd(nranges,range1,range2,dir)
int nranges, range1[15], range2[15];
char *dir;
{
    int range, errstat;
    char file[200];

    for(range=0;range<nranges;range++) {
        for(sren=range1[range]-1;sren<range2[range];sren++) {

            strcpy(file,cifdata[selrows[sren]].cal_file);
            file[cifdata[selrows[sren]].filelen] = '\0';

            BufLen_3 = 199;
            Cpthnm("CALDB",cifdata[selrows[sren]].cal_dir,file,&errstat);
            if (errstat != 0) errstat = 0;

            cpfile(file,dir);

        }
    }
}
