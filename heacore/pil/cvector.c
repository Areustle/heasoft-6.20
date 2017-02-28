#include <stdio.h>
#include <pil.h>

float	f5[10];
double	d2[10];
int	i3[10];
char	yoyo[PIL_LINESIZE];


int	main(int argc, char **argv)
{ int	i, len, r;


r = PILInit(argc, argv);

len = 5;
r = PILGetReal4Vector("f5", len, &(f5[0]));
printf("PILGetReal4Vector(f5) returned %d\n", r);
printf("values read (%d entries found) :", len);
for (i=0; i<len; i++) printf(" %g", f5[i]);
printf("\n\n");

len = 2;
r = PILGetRealVector("d2", len, &(d2[0]));
printf("PILGetRealVector(f5) returned %d\n", r);
printf("values read (%d entries found) :", len);
for (i=0; i<len; i++) printf(" %g", d2[i]);
printf("\n\n");

len = 3;
r = PILGetIntVector("i3", len, &(i3[0]));
printf("PILGetIntVector(f5) returned %d\n", r);
printf("values read (%d entries found) :", len);
for (i=0; i<len; i++) printf(" %d", i3[i]);
printf("\n\n");

len = 3;
r = PILGetReal4VarVector("f5", &len, &(f5[0]));
printf("PILGetReal4VarVector(f5) returned %d\n", r);
printf("values read (%d entries found) :", len);
for (i=0; i<len; i++) printf(" %g", f5[i]);
printf("\n\n");

len = 10;
r = PILGetRealVarVector("d2", &len, &(d2[0]));
printf("PILGetRealVarVector(d2) returned %d\n", r);
printf("values read (%d entries found) :", len);
for (i=0; i<len; i++) printf(" %g", d2[i]);
printf("\n\n");

len = 10;
r = PILGetIntVarVector("i3", &len, &(i3[0]));
printf("PILGetIntVarVector(i3) returned %d\n", r);
printf("values read (%d entries found) :", len);
for (i=0; i<len; i++) printf(" %d", i3[i]);
printf("\n\n");

r = PILGetDOL("yoyo", &(yoyo[0]));
printf("PILGetDOL() returned %d\n", r);
printf("value read : %s", yoyo);
printf("\n\n");

r = PILClose(r);
printf("PILClose(r) returned %d\n", r);

return(PIL_OK);
}
