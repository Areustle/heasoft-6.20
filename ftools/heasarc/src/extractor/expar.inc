* Parameters for the extractor

* Maximum number of Good Time intervals and time windows combined

      INTEGER MAXGTI
      PARAMETER (MAXGTI=1000000)

      INTEGER MAXGTIS
      PARAMETER (MAXGTIS=MAXGTI)

* Maximum number of chips

      INTEGER MAXCCD
      PARAMETER (MAXCCD=20)

* Maximum number of n for subspace keywords value nDVALm

      INTEGER MAXSTYP
      PARAMETER (MAXSTYP=100)

* Maximum number of n for subspace keywords value nDVALm

      INTEGER MAXSVAL
      PARAMETER (MAXSVAL=100)

* Size of "stride" in each vector processing

      INTEGER STRIDE
      PARAMETER (STRIDE=1000)

* Max number of ignored keywords for output events file

      integer MAXIGNORE
      parameter (MAXIGNORE=100)

* size of a row in integer*4 in the output events file

      integer ROWBUFFERSIZE
      parameter (ROWBUFFERSIZE=1000)

* Max number of columns for event output

      integer MAXCOLS
      parameter (MAXCOLS=100)



