struct insnode { char ins[11];
                 struct insnode *next;
               };

typedef struct insnode Insnode;

struct alsnode { char alias[11];
                 int  nins;
                 Insnode *insnode;
                 struct alsnode *next;
               };

typedef struct alsnode Alsnode;

struct alscfgnode { char mission[11];
                     Alsnode *alsnode;
		     struct alscfgnode *next;
		   };

typedef struct alscfgnode Alscfgnode;
