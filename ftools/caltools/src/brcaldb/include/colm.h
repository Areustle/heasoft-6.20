struct colmnode { int col;
                  char *colstr;
		  int ndispstr;
		  char *dispstr[9];
		  int displen;
		  struct colmnode *next;
		};

typedef struct colmnode Colmnode;
