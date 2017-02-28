#ifndef BAD_OP_EXCEPTION_INCLUDED
#define BAD_OP_EXCEPTION_INCLUDED

class Data;

#include "Exception.hh"

/*******************************************************************************
* This is the exception to throw when a math operation is not defined
* for a particular kind of data
*******************************************************************************/
class BadOpException : public Exception {

public:
   BadOpException(const string& operation, const Data* data);

private:
    static string assemble_message(const string& operation, const Data* data);


}; // end of BadOpException

#endif // BAD_OP_EXCEPTION_INCLUDED
