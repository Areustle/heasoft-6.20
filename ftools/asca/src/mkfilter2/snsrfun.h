#ifdef _INCLUDE_HPUX_SOURCE
extern  void ssas_to_sunvec(int SSAS_no,double sun_ang,double *sun_ssas);
extern  int nsas_to_sunvec(double *nsas_pix,double *sun_nsas);
extern  int NSAS_pixel_to_angle(double *pixel,double *NSAS_angle);
extern  void stt_to_starvec(unsigned int STT_no,double stt_h,double stt_v,double *star_stt);
extern  double gas_to_magvec(double *gas_data,double *mtq_bias_data,double *mag_vec);
#else
extern  void ssas_to_sunvec();
extern  int nsas_to_sunvec();
extern  int NSAS_pixel_to_angle();
extern  void stt_to_starvec();
extern  double gas_to_magvec();
#endif
