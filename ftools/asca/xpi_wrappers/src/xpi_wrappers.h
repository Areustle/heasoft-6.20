#ifndef XPI_WRAPPERS_INCLUDED
#define XPI_WRAPPERS_INCLUDED

/**************************************************************************
***************************************************************************
* initialize the parameter interface
**************************************************************************/
void OpenDefaultPF(int argc, char* argv[], char* dummy);

/**************************************************************************
***************************************************************************
* do final cleanup stuff for the parameter interface
**************************************************************************/
void CloseDefaultPF(void);


/*************************************
* read parameter function prototypes *
*************************************/
void read_string_param(char* name, char* value, int dimen);
double read_double_param(char* name);
int read_int_param(char* name);
int read_boolean_param(char* name);

/**************************************
* write parameter function prototypes *
**************************************/
void write_int_param(char* name, int value);
void write_double_param(char* name, double value);

#endif XPI_WRAPPERS_INCLUDED
