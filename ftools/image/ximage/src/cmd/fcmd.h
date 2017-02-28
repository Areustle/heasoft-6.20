
Tcl_ObjCmdProc procfcmd;

#define NFCMD 59
static char* fcmd[NFCMD]= {           "chatter",    "bglevels",     "calccor",
                         "coord",         "cpd",         "map",      "marith",
         "moper",         "crop",
        "header",         "show",       "chmdb",      "iminfo",
       "skyview","finding_chart", 
               "pixel_to_ra_dec",      "offset",       "value",      "counts",
       "uplimit",        "ogrid",  "read_image", "write_image",
    "sum_images",       "rotate",     "rescale",       "remap",
          "flip",        "rebin",      "resize",       "slice",      "pimage",
      "pcontour",      "surface",         "cct",         "smc",      "colors", 
         "scale",     "viewport",      "levels",       "label",     "vplabel", 
         "title",    "timestamp",   "circlereg", "boxreg",  "close_pg_window", 
    "background",          "psf",      "excess",      "search",     
        "sosta","remove_sources",     "extract",      "smooth",  "vignetting", 
      "centroid",         "help","xan::cleanup"
};
