#ifndef PARAM_INCLUDED
#define PARAM_INCLUDED

#include "PILException.hh"

/******************************************************************************
* this class is a container for parmeter file values
******************************************************************************/
class Param {

private:

    string configure_file;
    string infile_root;
    string outfile;
    string mission;
    bool clobber;


public:
    Param() throw(PILException);
    
    virtual ~Param() {}
    
    virtual const string& get_configure_file() { return configure_file; }
    virtual const string& get_infile_root()    { return infile_root;    }
    virtual const string& get_outfile()        { return outfile;        }
    virtual const string& get_mission()        { return mission;        }
    virtual bool          get_clobber()        { return clobber;        }

protected:

    virtual string& read_string( const string& name) throw(PILException);
    virtual double  read_double( const string& name) throw(PILException);
    virtual bool    read_boolean(const string& name) throw(PILException);

}; // end of Param class


#endif // PARAM_INCLUDED
