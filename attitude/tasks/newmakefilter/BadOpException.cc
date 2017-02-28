#include "BadOpException.hh"

#include <typeinfo>

#include "Data.hh"

/*****************************************************************************
*
*****************************************************************************/
string BadOpException::assemble_message(const string& operation, const Data* data) {

    string message = operation + " is not defined";
    if(data != NULL) {
        message += " for ";
        message += typeid(*data).name();
        message += " values";
    }

    return message;

} // end of assemble_message static method

/*****************************************************************************
*
*****************************************************************************/
BadOpException::BadOpException(const string& operation, const Data* data)
              : Exception(assemble_message(operation, data)) {}
