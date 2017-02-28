#ifndef HD_EXCEPTION_INCLUDED
#define HD_EXCEPTION_INCLUDED

#include "Exception.hh"

/*******************************************************************************
* This is an exception to be thrown when an error has been thrown into the
* HEAdas error handling system. The idea is to throw this exception to 
* pop yourself up to the top level tool function, where you catch this
* exception, print its message and then return with its status. Returning that way
* causes the HEAdas error stack to get dumped.
* I would have preferred to dump the error stack into the message of this extension,
* but the only way to get at the error stack is to write it to a FILE.
* I might change the way this class works later if better access is added
* to the HEAdas error system.
*******************************************************************************/
class HDException : public Exception {

private:
    int status;

public:
   HDException(const string& message, const char* file, int line);
   
   int get_status() { return status; }

private:
    static string assemble_error_message(const string& message);



}; // end of HDException

#endif // HD_EXCEPTION_INCLUDED

