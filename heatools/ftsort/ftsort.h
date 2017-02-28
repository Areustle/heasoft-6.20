typedef struct complex_data_types 
{
   double real;
   double imag;
}  complex;

typedef union fits_column_data_types 
{
   char** sptr;
   int* iptr;
   double* dptr;
   complex* cptr;
}  fits_array;

typedef struct fits_column_data {
   fits_array data;
   char* undef_flags;
   int dtype;
}  column;
   

typedef union fits_table_item_data_types 
{
   char* s;
   int i;
   double d;
   complex c;
}  fits_item;

typedef struct fits_table_item {
   fits_item data;
   char undef_flag;
   int dtype;
} tblItem;           
