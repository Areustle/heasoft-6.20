struct instnode { char inst[11];
                  char cif[150];
                  struct instnode *next;
                };

typedef struct instnode Instnode;

struct cnfgnode { char mission[11];
                  Instnode *instnode;
		  struct cnfgnode *next;
		};

typedef struct cnfgnode Cnfgnode;
