struct selrowsnode { int *rows;
                     int nrows;
                     struct selrowsnode *next;
                   };

typedef struct selrowsnode Selrowsnode;

