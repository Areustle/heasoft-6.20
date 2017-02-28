#include "ASCIIConfig.hh"

#include "headas.h"

ASCIIConfig::ASCIIConfig(const string& filename,
                         FITSFileBroker* fits_broker, FilterFile* filter)
            : Config(filename,fits_broker, filter) {
            
    /****************
    * open the file *
    ****************/
    in = new ifstream(filename.c_str());



} // end of constructor


/**************************************************************************
* returns information about the next column listed in the config file
**************************************************************************/
bool ASCIIConfig::read_next() throw(Exception) {

    /*********************
    * read the next line *
    *********************/
    char buf[256];
    for (int i=0; i<256; i++) {
        buf[i] ='\0';
    }
    in->getline(buf, 256, '\n');
    
    headas_chat(3, "Config: %s\n", buf);
    
    /*********************************
    * check the status of the stream *
    *********************************/
    if(in->eof() ) return false;

    if(! in->good() ) {
        throw Exception("I/O error reading ASCII configuration file "+filename,
                        __FILE__, __LINE__);
    }

    /************************************
    * status is good, so parse the line *
    ************************************/
    char* pt = buf;
    while(isspace(*pt)) pt++;
    if(strlen(pt)== 0 || pt[0] == '#' ) {
        /*********************************
        * blank line or comment - go on to the next *
        *********************************/
        return read_next();
    }


    char* incol = strtok(buf, "\t ");
   // strcpy(coptions->incol, pt);

    char* insuffix = strtok(NULL, "\t ");
    //strcpy(insuffix, pt);

    char* extension = strtok(NULL, "\t ");
    //strcpy(exten, pt);

    char* interp = strtok(NULL, "\t ");
    //strcpy(coptions->interp, pt);

    char* calib = strtok(NULL, "\t ");
    //strcpy(coptions->calib, pt);

    char* outcol = strtok(NULL, "\t ");
    //strcpy(coptions->outcol, pt);

    pt=outcol;
    while(*pt != '\0') pt++;
    pt++;
    while(isspace(*pt)) pt++;
    if(*pt == '/') pt++;
    while(isspace(*pt)) pt++;
    char* comment = pt;


    //strcpy(coptions->comment, pt);

//     cout << "incol="<<incol<<endl;
//     cout << "insuffix="<<insuffix<<endl;
//     cout << "extension="<<extension<<endl;
//     cout << "interp="<<interp<<endl;
//     cout << "calib="<<calib<<endl;
//     cout << "outcol="<<outcol<<endl;
//     cout << "comment="<<comment<<endl;
    

    /*****************************************
    * create an object to describe this line *
    *****************************************/
    add(incol,
        insuffix,
        extension,
        interp,
        calib,
        outcol,
        comment);

    return true;


    
} // end of read_next method



/**************************************************************************
* returns information about the next column listed in the config file
**************************************************************************/
void ASCIIConfig::close() throw(Exception) {

    ifstream* file = dynamic_cast<ifstream*>(in);
    if(file) file->close();


}
