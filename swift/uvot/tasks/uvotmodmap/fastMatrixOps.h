void timeIncrement (char *msg);
int countAndSumMatrix (IntMatrix *pixMap, RealMatrix *observation,
	double *sum);
int countAndSumMatrixSection (IntMatrix *pixMap, RealMatrix *observation,
	int sectionX, int sectionY, int maxX, int maxY, int dx, int dy, 
	double *sum);
int fastClearOutsiders (IntMatrix *mask, IterateClearOutsiders *iSSD);
void fastCopyIntMatrix (IntMatrix *s, IntMatrix *d);
void fastIterateScale (RealMatrix *tile, IterateScale *scale);
double fast_sum (RealMatrix *r);
int fast_countStep (IntMatrix *mtrx, int dx, int dy);
void fast_int_matrix_section (IntMatrix * m, IntMatrix * o, int xMin, int yMin);
double fast_sumMaskStep (RealMatrix *rm, IntMatrix *im, int dx, int dy);

#define BASE_DATA(m,i) m->data+m->xDimension*(i+m->yAlias)+m->xAlias
