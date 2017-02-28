
      DOUBLE PRECISION psfmtx(600,20,5,20)
      DOUBLE PRECISION emtx(20), thmtx(20), phimtx(5), rmtx(600)
      DOUBLE PRECISION ideve(20), idevth(20), idevphi(5), idevr(600)
      INTEGER nne, nnth, nnph, nnir

      COMMON /xrt_psf_data/ psfmtx, emtx, thmtx, phimtx, rmtx,
     &    nne, nnth, nnph, nnir
      COMMON /xrt_idev_data/ ideve, idevth, idevphi, idevr

