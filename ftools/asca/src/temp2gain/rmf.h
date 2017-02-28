struct rmf {
	struct { double lo, hi; } e;
	int f_chan, n_chan;
	float *mat;
};

struct rmf *ascatool_read_rmf(int ilun, double rsplimit);
void ascatool_free_rmf(struct rmf *rsp);
