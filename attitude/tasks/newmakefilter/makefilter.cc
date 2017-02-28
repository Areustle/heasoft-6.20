/*****************************************************************************
* FTOOLS TASK:
*     makefilter
*
* DESCRIPTION:
*     Merge House Keeping files into a filter file. 
* 
* AUTHOR:
*     Ning Gan 
*       Raytheon STX
*       August, 1999
* 
* MODIFICATION HISTORY:
*
*     1999-09-01 Ning Gan V1.0.0 : The original program.
*     2003-05-20 M.Tripicco V1.1.0 : Enabled clobber parameter
*                                    Enabled FITS config file
*                                    Disabled file sorting
*                                    Tightened error condition checking
*                                      (eg for non-existent configuration file)
*     2003-08-27 M.Tripicco v1.2.0   fixed bug reopening ascii config file
*                                    added HISTORY kwd with version and date
*     2003-09-25 Ziqin Pan V1.3.0    adapted it to Headas.
*
*     2004-07-27 Ed Pier  v2.0       Did an almost complete rewrite
*
* Notes:
*
*
*****************************************************************************/
#include "headas.h"

#define TOOLSUB makefilter
#include "headas_main.c"

#include "HDException.hh"

#include  "config.hh"

/*************************************************************************
* main function
*************************************************************************/
int makefilter(void) {

    /********************************
    * Register taskname and version *
    ********************************/
    static char taskname[80] = "makefilter";
    static char version[8] = "2.0";
    set_toolname(taskname);
    set_toolversion(version);

    
    try {
        /**************************
        * read the parameter file *
        **************************/
        Param* param = new Param();

        /******************************
        * read the configuration file *
        ******************************/
        FilterFile* filter = Config::read(param);

        /************************
        * write the filter file *
        ************************/
        filter->write_header();
        while(filter->write_row());
        filter->close();
        
    } catch(HDException& e) {
        /****************************************
        * caught a HEAdas exception
        * return with the error status so that
        * the error stack will get dumped
        ****************************************/
        std::cerr << e.what()<<endl;
        return e.get_status();

    } catch(Exception& e) {
        /*****************************
        * an error occured somewhere *
        *****************************/
        std::cerr << e.what() << "\n"<<endl;
        return 0;
    }

    return 0;
    
} // end of makefilter function
