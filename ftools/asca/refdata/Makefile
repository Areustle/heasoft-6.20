HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_INSTALL_REFDATA	= earth.dat earth_old.dat faintdfe.tbl \
			  gis2_ano_on_flf_180295.fits \
			  gis3_ano_on_flf_180295.fits rti_gis_1024_040693.fits \
			  rti_gis_128_190594.fits rti_gis_256_200494.fits \
			  s0_teldef_ascalin.fits s1_teldef_ascalin.fits \
			  s2bev1.fits s2gridv3.fits s3bev1.fits s3gridv3.fits \
			  sisechos.fits sis_rdd_1999-06-24.fits \
			  xrt_ea_v1_1.fits xrt_ea_v2_0.fits xrt_psf_v1_0.fits \
			  xrt_psf_v1_1.fits xrt_psf_v2_0.fits

HD_INSTALL_EXTRA	= install-sisdata-files

include ${HD_STD_MAKEFILE}

SISDATA			= sisdata/sis0c0p20_290296.fits \
			  sisdata/sis0c0p40_290296.fits \
			  sisdata/sis0c1p20_290296.fits \
			  sisdata/sis0c1p40_290296.fits \
			  sisdata/sis0c2p20_290296.fits \
			  sisdata/sis0c2p40_290296.fits \
			  sisdata/sis0c3p20_290296.fits \
			  sisdata/sis0c3p40_290296.fits \
			  sisdata/sis1c0p20_290296.fits \
			  sisdata/sis1c0p40_290296.fits \
			  sisdata/sis1c1p20_290296.fits \
			  sisdata/sis1c1p40_290296.fits \
			  sisdata/sis1c2p20_290296.fits \
			  sisdata/sis1c2p40_290296.fits \
			  sisdata/sis1c3p20_290296.fits \
			  sisdata/sis1c3p40_290296.fits \
			  sisdata/sisechos_150301.fits \
			  sisdata/sisechos_290296.fits \
			  sisdata/sisph2pi_110397.fits \
			  sisdata/sisph2pi_290296.fits \
			  sisdata/sisph2pi_290301.fits \
			  sisdata/sisrddis_290296.fits

install-sisdata-files:
	@${HD_MAKE} hd-std-install-refdata \
		HD_INSTALL_REFDATA="${SISDATA}" \
		HD_REFDATA="${HD_REFDATA}/sisdata" \
		HD_TOP_REFDATA="${HD_TOP_REFDATA}/sisdata"
