double
hostdouble(double v)
{
	static double z = 100.0;
	char buf[8], *p;
	if ( *(char*)&z ) return v;		/* for sun */
	p = (char*)&v;
	buf[0] = p[7];
	buf[1] = p[6];
	buf[2] = p[5];
	buf[3] = p[4];
	buf[4] = p[3];
	buf[5] = p[2];
	buf[6] = p[1];
	buf[7] = p[0];
	return *(double*)buf;
}

double
netdouble(double v)
{
	static double z = 100.0;
	char buf[8], *p;
	if ( *(char*)&z ) return v;		/* for sun */
	p = (char*)&v;
	buf[0] = p[7];
	buf[1] = p[6];
	buf[2] = p[5];
	buf[3] = p[4];
	buf[4] = p[3];
	buf[5] = p[2];
	buf[6] = p[1];
	buf[7] = p[0];
	return *(double*)buf;
}

void
hostdouble_array(double array[], int n)
{
	static double z = 100.0;
	int i;
	char buf[8], *p;
	if ( *(char*)&z ) return;		/* for sun */
	for (i = 0; i < n; i++) {
		p = (char*)&array[i];
		buf[0] = p[7];
		buf[1] = p[6];
		buf[2] = p[5];
		buf[3] = p[4];
		buf[4] = p[3];
		buf[5] = p[2];
		buf[6] = p[1];
		buf[7] = p[0];
		array[i] = *(double*)buf;
	}
}

void
netdouble_array(double array[], int n)
{
	static double z = 100.0;
	int i;
	char buf[8], *p;
	if ( *(char*)&z ) return;		/* for sun */
	for (i = 0; i < n; i++) {
		p = (char*)&array[i];
		buf[0] = p[7];
		buf[1] = p[6];
		buf[2] = p[5];
		buf[3] = p[4];
		buf[4] = p[3];
		buf[5] = p[2];
		buf[6] = p[1];
		buf[7] = p[0];
		array[i] = *(double*)buf;
	}
}
