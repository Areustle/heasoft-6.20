#ifdef _HXD_TRNPI_UTIL_H_
#define _HXD_TRNPI_UTIL_H_

#define TRN_PH_CH 54

/*** DE rebinning correction ***/
void hxdtrnpi_DE_correct_Init(char *rebin_file_list, ASTE_HK **rebin_file,
			      int *istat);

void hxdtrnpi_DE_correct(int board, int *pha_tlm, double *pha_ae,
			 int rebin_setting[TRN_PH_CH] );

void hxdtrnpi_rebintable_read(ASTE_HK *rebin_file, double trntime,
			      int *rebin_table, int *istat);

void hxdtrnpi_calc_rdbin(int *binning_table, int *calc_rdbin, int board);

void hxdtrnpi_AEDE_correct(int *pha_ae, double *pha_adc);

void hxdtrnpi_DE_no_correct(int *pha_tlm, double *pha_ae);

void hxdtrnpi_Gain_correct_Init(char *gain_history_list,
				ASTE_HK **gain_history_file, int *istat);

void hxdtrnpi_Gain_correction(double *pi_ae, double *pi, int *gain_table,
			      int *istat);


#endif
