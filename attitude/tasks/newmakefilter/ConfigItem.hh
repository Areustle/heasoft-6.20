#ifndef CONFIG_ITEM_INCLUDED
#define CONFIG_ITEM_INCLUDED

extern "C" {
#include "fitsio.h"
}

/********************************************************************************
* This class represents the configuration
********************************************************************************/
class ConfigOptions {
    char incol[64];
    char infile[FLEN_FILENAME];
    char outcol[64];
    char interp[16];
    char calib[16];
    char comment[FLEN_COMMENT];
};

#endif // CONFIG_ITEM_INCLUDED