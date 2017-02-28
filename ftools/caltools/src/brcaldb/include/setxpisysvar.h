#ifdef unix
 
    strcpy(dsk,"$FTOOLS\0");
    strcpy(dir,"bin/\0");
 
#endif
 
#ifdef vms
 
    strcpy(dsk,"FTOOLS\0");
    strcpy(dir,"bin_host");
 
#endif
