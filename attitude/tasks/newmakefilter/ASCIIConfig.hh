#ifndef ASCII_CONFIG_INCLUDED
#define ASCII_CONFIG_INCLUDED

#include "config.hh"

#include <string>

class ASCIIConfig : public Config {

private:
    std::istream* in;

public:
    /**************
    * constructor *
    **************/
    ASCIIConfig(const string& filename,
                FITSFileBroker* fits_broker, FilterFile* filter);

protected:
    virtual bool read_next() throw(Exception);
    virtual void close() throw(Exception);



}; // end of ASCIIConfig class


#endif // ASCII_CONFIG_INCLUDED
