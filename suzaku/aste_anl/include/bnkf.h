#ifndef BNKFGETM
#define BNKFGETM(key,size,used,value)	{\
	static int index = 0;\
	int vsize = size;\
	bnkfget_(key, &index, &vsize, &used, value, sizeof(key)-1);\
}
#endif

#ifndef BNKFPUTM
#define BNKFPUTM(key,size,value)	{\
	static int index = 0;\
	int vsize = size;\
	bnkfput_(key, &index, &vsize, value, sizeof(key)-1);\
}
#endif
