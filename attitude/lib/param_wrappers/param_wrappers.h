#ifndef PARAM_WRAPPERS_INCLUDED
#define PARAM_WRAPPERS_INCLUDED

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

#endif /* PARAM_WRAPPERS_INCLUDED */
