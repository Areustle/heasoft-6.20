C*******************************************************************
      SUBROUTINE write_detnam(exp, camera, detstr,detstp,
     &                        ndet, detnam,status)
C*******************************************************************


c
c     Input 
      integer*2 camera, detstr, detstp, ndet
      integer*4 exp
c
c     Output parameters:
      integer status
      CHARACTER*(*) DETNAM
      INTEGER   LENACT
      character(80) context
      character(20)  first, second
c   
c Experiment LE:
      if (exp.eq.1) then
         detnam = 'CMA1'
      elseif (exp.eq.2) then
	 detnam = 'CMA2'
      endif
c
c Experiment ME: 
      if (exp.eq.3) then
         if (ndet.eq.3.or.ndet.eq.4) then
           if (detstr.eq.1.and.detstp.eq.4) then
		first = 'HALF1 '
	   elseif (detstr.eq.5.and.detstp.eq.8) then
		first='HALF2'
	   elseif (detstr.eq.1.and.detstp.eq.8) then
               first = 'CORN'
           endif
         elseif (ndet.eq.7 .or. ndet.eq.8) then
	   if (detstr.eq.1.and.detstp.eq.8) then
	       first= 'ALL '
           endif
         elseif (ndet.eq.2) then
	      if (detstr.eq.1) first = 'DET-A'
	      if (detstr.eq.2) first = 'DET-B'
	      if (detstr.eq.3) first = 'DET-C'
	      if (detstr.eq.4) first = 'DET-D'
	      if (detstr.eq.5) first = 'DET-E'
	      if (detstr.eq.6) first = 'DET-F'
	      if (detstr.eq.7) first = 'DET-G'
	      if (detstr.eq.8) first = 'DET-H'
	      if (detstp.eq.1)
     &           first = first(1:lenact(first))//', DET-A'
	      if (detstp.eq.2)
     &            first = first(1:lenact(first))//', DET-B'
	      if (detstp.eq.3)
     &            first = first(1:lenact(first))//', DET-C'
	      if (detstp.eq.4) 
     &            first = first(1:lenact(first))//', DET-D'
	      if (detstp.eq.5)
     &            first = first(1:lenact(first))//', DET-E'
	      if (detstp.eq.6) 
     &            first = first(1:lenact(first))//', DET-F'
	      if (detstp.eq.7)
     &            first = first(1:lenact(first))//', DET-G'
	      if (detstp.eq.8)
     &            first = first(1:lenact(first))//', DET-H'
         elseif (ndet.eq.1) then
	      if (detstr.eq.1) first = 'DET-A'
	      if (detstr.eq.2) first = 'DET-B'
	      if (detstr.eq.3) first = 'DET-C'
	      if (detstr.eq.4) first = 'DET-D'
	      if (detstr.eq.5) first = 'DET-E'
	      if (detstr.eq.6) first = 'DET-F'
	      if (detstr.eq.7) first = 'DET-G'
	      if (detstr.eq.8) first = 'DET-H'
         else
	   CALL xwrite(' detnam: Unknow ME det combination', 10)
           write(context,'('' detnam:Number of det.:'', i4)')ndet
	   CALL xwrite(context, 15)
	   WRITE(context, '('' detnam:start & stop det.:'',i4,1x,i4)') 
     &           detstr, detstp
           CALL xwrite(context, 15)
             status=1
             return
         ENDIF
C  
C        Decode second part ME detector
	 if (camera.eq.1) then
	     second= 'AR'
         elseif (camera.eq.2) then
	     second= 'XE'
	 elseif (camera.eq.3) then
	     second= 'AR+XE'
	 else
           write(context,'('' detnam: Unknown ME camera:'',i4)')
     &    camera
	   CALL xwrite(context, 15)
             status=1
             return
         endif
C         
C  Obtain ME string
         DETNAM = first(1:lenact(first))//' '//
     &     second(1:lenact(second))

      ENDIF
c
      If(exp.ne.3.and.exp.ne.1.and.exp.ne.2)then
           write(context,'('' detnam: Unknown experiment type:'',i4)')
     &     exp
	   CALL xwrite(context, 15)
      endif

      RETURN
      END
