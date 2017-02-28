typedef enum { OPEQ, OPNE, OPGT, OPLT, OPGE, OPLE, OPAND, OPOR } Optyp;
typedef struct {
                Optyp optyp;
                char opstr[6];
               } Optr;
