#ifndef EXCEPTION_INCLUDED
#define EXCEPTION_INCLUDED


#include <stdexcept>
#include <string>
using namespace std;

/*****************************************************************************
* this exception indicates a generic problem with the configuration file.
*****************************************************************************/
class Exception : public runtime_error {

string file;
int line;


public:
    /**************
    * constructor *
    **************/
    Exception(const string& message, const char* file =NULL, int line =0)
     : runtime_error(assemble_error_message(message, file, line) ) {

        if(file) this->file = file;
        else     this->file = "unknown";
        this->line = line;

    } // end of constructor
    

    virtual ~Exception() throw() {}
    virtual const string& get_file() { return file; }
    virtual int           get_line() { return line; }
    
private:

    static string assemble_error_message(const string& message, const char* file, int line);


}; // end of Exceptionclass

#endif // EXCEPTION_INCLUDED
