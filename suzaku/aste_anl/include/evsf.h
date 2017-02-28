#ifndef EVSFM
#define EVSFM(key,index)	evsf_(key, &(index), sizeof(key)-1)
#endif

#ifndef EVSFSETM
#define EVSFSETM(key)	{\
	static int index = 0;\
	evsfset_(key, &index, sizeof(key)-1);\
}
#endif
