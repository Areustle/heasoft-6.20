/*
 SimASTE.h
   SimASTE utility functions

  2006-08-05 version 2.2	Y.ISHISAKI
	SimASTE_write_history_pname(), SimASTE_write_timestamp()
	SimASTE_write_photon_detect_info(), SimASTE_write_random_number_info()
	SimASTE_update_time_keys(), SimASTE_write_std_keys(), SimASTE_write_gti()
*/

#ifndef _SimASTE_H_
#define _SimASTE_H_

#define SimASTE_Mode_Discard	0
#define SimASTE_Mode_Weight		1

#ifdef __cplusplus
extern "C"
{
#endif

#ifdef _FITSIO_H
int SimASTE_write_history_pname(fitsfile *fp, char *pname);
int SimASTE_write_timestamp(fitsfile *fp);
int SimASTE_write_photon_detect_info(fitsfile *fp);
int SimASTE_write_random_number_info(fitsfile *fp);
int SimASTE_update_time_keys(
	fitsfile *fp, double tstart, double tstop, double expo);
#ifdef _AE_FITS_HEADER_UTIL_H_
int SimASTE_write_std_keys(fitsfile *fp, AE_STD_KEYS *v);
#endif	/* _AE_FITS_HEADER_UTIL_H_ */
#ifdef _ASTE_GTI_H_
int SimASTE_write_gti(fitsfile *fp, GTI_DATA *gp, AE_STD_KEYS *v);
#endif	/* _ASTE_GTI_H_ */
#endif	/* _FITSIO_H */

#ifdef __cplusplus
}
#endif

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/

#endif	/* _SimASTE_H_ */
