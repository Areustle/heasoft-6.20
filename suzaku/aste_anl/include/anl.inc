c
c anl.inc,v 1.1.1.1 1997/02/23 16:22:35 ishisaki Exp
c
c useful definitions
      Integer ANL_OK
      Parameter (ANL_OK = 0)
      Integer ANL_NG
      Parameter (ANL_NG = -1)

      Integer ANL_YES
      Parameter (ANL_YES = 1)
      Integer ANL_NO
      Parameter (ANL_NO = 0)

      Integer ANL_TRUE
      Parameter (ANL_TRUE = 1)
      Integer ANL_FALSE
      Parameter (ANL_FALSE = 0)

      Integer ANL_ENA
      Parameter (ANL_ENA = 1)
      Integer ANL_DIS
      Parameter (ANL_DIS = 0)

      Integer ANL_ON
      Parameter (ANL_ON = 1)
      Integer ANL_OFF
      Parameter (ANL_OFF = 0)

c flow control (obsolete)
      Integer ASCA_ANL_OK
      Parameter (ASCA_ANL_OK = 0)
      Integer ASCA_ANL_QUIT
      Parameter (ASCA_ANL_QUIT = -1)
      Integer ASCA_ANL_SKIP
      Parameter (ASCA_ANL_SKIP = -3)
      Integer ASCA_ANL_LOOP
      Parameter (ASCA_ANL_LOOP = -4)

      Integer ANL_QUIT
      Parameter (ANL_QUIT = -1)
      Integer ANL_SKIP
      Parameter (ANL_SKIP = -3)
      Integer ANL_LOOP
      Parameter (ANL_LOOP = -4)

c flow control (new)
      Integer ANL_ENDLOOP, ANL_DISCARD,
     &     ANL_NOCOUNT, ANL_NEWROOT
      Parameter (ANL_ENDLOOP=1, ANL_DISCARD=2,
     &     ANL_NOCOUNT=4, ANL_NEWROOT=8)

c event
      Integer EVENTID_OBS
      Parameter (EVENTID_OBS = 0)
      Integer EVENTID_PSEUDO
      Parameter (EVENTID_PSEUDO = -1)

c sensor ID
      Integer SENSOR_PSEUDO
      Parameter(SENSOR_PSEUDO = -1)
