c
        subroutine nhmake(equinox, rastr, decstr, size, disio, map,
     &                    altmap, nh1, nh2, altnh1, altnh2,
     &                    usemap, tchat, lchat, ierr) 
c
c
c By reading the Nh map evaluate an nh at a given position in the sky 
c
c  I  equinox   (d)  equinox
c  I  rastr     (c)  R.A. string
c  I  decstr    (c)  Declination string
c  I  size      (d)  size of the submap
c  I  disio     (d)  distance in degree
c  I  map       (c)  input file map
c  I  altmap    (c)  alternate input file map
c  I  usemap    (i)  which map to use
c  I  tchat     (i)  terminal chatness
c  I  lchat     (i)  log chatness
c  O  nh1       (r)  Average Nh
c  O  nh2       (r)  Weighted average nH
c  O  altnh1    (r)  Average Nh (alternate map)
c  O  altnh2    (r)  Weighted average nH (alternate map)
c  O  ierr      (i)  error status
c
c 
c
c Input paramaters
        INTEGER*4 usemap, tchat, lchat, ierr
        DOUBLE PRECISION equinox, disio, size
        CHARACTER*(*) rastr, decstr, map, altmap
c
c Output paramaters
        REAL*4 nh1, nh2, altnh1, altnh2
c
c Local paramaters
c
c Fits Integer
        INTEGER*4 iunit, block, naxes(2)
        INTEGER*4 bitpix, pcount, gcount, naxis
        INTEGER*4 status
c 
        INTEGER equi, fpix(2), lpix(2), inc(2), npix
        INTEGER nxpix, nypix, dim1, dim2, ntot
        INTEGER*4 j, i, k, ng
        REAL*4 buffer(65341), bnh(100,4)
        REAL*4 good(100,4), av, wei, wes, unav
c
c ra and dec , l and b
        DOUBLE PRECISION radeg, decdeg , lii , bii
        DOUBLE PRECISION nh, ra, dec, decrad
        DOUBLE PRECISION term1, term2, dist, defdist
        DOUBLE PRECISION la, ba
c
c FITS double 
        DOUBLE PRECISION xpix,ypix,xrval,yrval,xinc,yinc,rot
        DOUBLE PRECISION xrpix,yrpix,x1pix,y1pix
c
c 
        character(16) coordtype
        character(255) context
c
c
c FITS data & logical 
        LOGICAL anynull(65341), simple, extend, near(100)
        DATA pcount,block,bitpix/0,2880,-999/
      
       status = 0
       nh1 = 0.0
       nh2 = 0.0
       altnh1 = 0.0
       altnh2 = 0.0

c
c Loop on usemap
10     IF (usemap.EQ.1) THEN
           map = altmap
       ENDIF
       
       IF (usemap.EQ.0.OR.usemap.EQ.2) THEN
           context = '  >> Leiden/Argentine/Bonn (LAB) Survey '
     &            // 'of Galactic HI'
           CALL xwrite(context, 5)
       ELSE IF (usemap.EQ.1) THEN
           context = '  >> Dickey & Lockman (DL) HI in the Galaxy'
           CALL xwrite(context, 5)
       ENDIF

c
c NOTE equinox is real *8 from the par file but parsera, prec,tranc
c /xanlib/coords ask for an int*4, instead cgt want a real *8
c
c transform RA/DEC string => numerical values
       equi=int(equinox)
       CALL parsera(rastr, equi, radeg, ierr)
       IF (ierr.NE.0) THEN
          context='  parsera: Error traslate RA string'
          GOTO 999
       ENDIF 
       CALL parsedec(decstr, equi, decdeg, ierr)
       IF (ierr.NE.0) THEN
          context='  parsdec: Error traslate DEC string'
          GOTO 999
       ENDIF 

c
c ctg precess and translate coordinate from radec to lii bii
c note that the sys is hardcoded always fk4.
c transform RA/DEC to LII/BII
        CALL CTG(radeg,decdeg,'FK4',equinox,lii,bii)
c  
c Normal writing 
       WRITE(context,'(''  LII , BII'',1x,f10.6,1x,f11.6)') lii,bii 
       CALL xwrite(context, 5)

c
c Dealing with FITS 
c initialize arrays used later linux seems having problem otherwise
c
       do j=1,100
          do i=1,4
             bnh(j,i)=0.0
             good(j,i)=0.0
          enddo 
       enddo
c
       do j=1,65241
         buffer(j)=0.0
       enddo

       do j=1,100
         near(j)=.false.
       enddo
c
c Open fits file map 
       CALL getlun (iunit)
       CALL ftopen (iunit, map, 0, block, ierr)
       IF (ierr.NE.0) THEN
          context = ' Error opening input file ' // map
          call FTCLOS(iunit,ierr)
          call FRELUN(iunit)
          GOTO 999
       ENDIF
c
c get required keywords
      CALL ftghpr(iunit,3,simple,bitpix,naxis,naxes,pcount,gcount,
     &            extend, ierr)
       IF(ierr.NE.0) THEN
          context = ' Error getting the primary header'
          call FTCLOS(iunit,ierr)
          call FRELUN(iunit)
          GOTO 999
       ENDIF
       IF ( naxis.LT.2 ) THEN
         CALL XWRITE(' Error: Not a 2D image',10)
         CALL FTCLOS(iunit,ierr)
         CALL FRELUN(iunit)
       ENDIF
c
c read the value of wcs in the header
       CALL ftgics (iunit, xrval, yrval, xrpix, yrpix, xinc, yinc,
     &        rot, coordtype, ierr)
       IF(ierr.NE.0) THEN
          context = ' Error reading the WCS keywords '
          call FTCLOS(iunit,ierr)
          call FRELUN(iunit)
          GOTO 999
       ENDIF
c
c must be Lii and Bii to get 
c sensible answer 
       CALL ftxypx (lii,bii, xrval, yrval, xrpix, yrpix, 
     &        xinc, yinc, rot, coordtype,xpix, ypix, ierr) 
c
c the fitsio routine does not work on the map from e-mail
c because is a pixel offset : the map start at 0 instead the 
c routine assumes 1 using the e-mail transformation 
c now use the aitoff map instead
c
       x1pix=xpix
       y1pix=ypix
       WRITE(context,1100)x1pix,y1pix  
1100   FORMAT('  Requested position at X and Y pixel  ',f7.2,3x,f7.2)
       CALL XWRITE(context,8)
c
c change value to the near integer
       nxpix=nint(x1pix)
       nypix=nint(y1pix)
c
c make sure that 'size' is bigger than 'disio'
c if not set size = disio
       if(disio.gt.size)size=disio
c
c get the total number of pixels
       ntot=size/yinc 
c       write(*,*)'ntot', ntot
c
       fpix(1)=nint(x1pix-(float(ntot-1)/2))
       fpix(2)=nint(y1pix-(float(ntot-1)/2))
       lpix(1)=nint(x1pix+(float(ntot-1)/2))
       lpix(2)=nint(y1pix+(float(ntot-1)/2))
       inc(1)=1  
       inc(2)=1  
       WRITE(context,'(''   Bounds'',2i4,2x,''XY1'',2i4,2x,''XY2'')')
     &               fpix(1), lpix(1), fpix(2), lpix(2)
       CALL XWRITE(context,15)
c
c         write(*,*)'2 fpix(1),fpix(2),lpix(1),lpix(2)',
c     &             fpix(1),fpix(2),lpix(1),lpix(2)   
c
c
       IF(fpix(1).le.0)fpix(1)=1
       IF(fpix(2).le.0)fpix(2)=1
       IF(lpix(1).gt.naxes(1))lpix(1)=naxes(1)
       IF(lpix(2).gt.naxes(2))lpix(2)=naxes(2)
c
c       IF(fpix(1).eq.0)fpix(1)=fpix(1)+1
c       IF(fpix(2).eq.0)fpix(2)=fpix(2)+1
c
c    2,2 is the nint(x1pix) nint(y1pix)
c
c    1,3  2,3  3,3 
c    1,2  2,2  3,2
c    1,1  2,1  3,1
c
c read data from the map

       call ftgsve(iunit,0,naxis,naxes,fpix,lpix,inc,0.
     &            ,buffer,anynull,ierr)
c       do j=1,65241  
c         if (buffer(j).ne.0) 
c     &       write(*,*)'anynull, buffer(j)',anynull(j), buffer(j), j
c       ENDDO
       IF(ierr.NE.0) THEN
          context=' Error reading the image array'
          call FTCLOS(iunit,ierr)
          call FRELUN(iunit)
c          GOTO 999
        ENDIF 
        call FTCLOS(iunit,ierr)
        call FRELUN(iunit)
c
c Finish dealing with FITS
c
c 1- transform pix in l and b and in ra dec
c 2- find distance between input value and value for each pixel
c 3- calculate distance 
c 4- create a vector nh ,dist,ra,dec
c
c print the 9 point matrix 
c calculate total number of pixels and dimension
       npix=(lpix(1)-fpix(1)+1)*(lpix(2)-fpix(2)+1)
       dim1=lpix(1)-fpix(1)+1
       dim2=lpix(2)-fpix(2)+1
c
c       write(*,*)'5 npix, dim1, dim2',npix, dim1, dim2
c
       WRITE(context,'(''  Search nH in'',i3,''  X'',i3,'' box'')')
     &               dim1,dim2
       CALL XWRITE(context,8)
       WRITE(context,
     &     '(''  Each pixel is '', f6.3,'' deg '', f5.3,'' deg'')')
     &       abs(xinc),abs(yinc)
       CALL XWRITE(context,8)
       context='  nH calculated using all points within'
       CALL XWRITE(context,8)
       WRITE(context,'(''  '',f7.4,'' deg from input position'')')
     &       disio
       CALL XWRITE(context,8)
c
c i run in x (column) k run in y (row) 
       i=0
       k=0
       WRITE(context,
     &  '( ''   LII     BII    XPIX  YPIX    Dist      nH'')')    
       CALL XWRITE(context,15)
c
c set the variable to test for minimun distance 
       defdist=300.D0 
c
c assign to each pixel in the matrix the appropriate nh value
       DO j=1, npix
         IF(i.EQ.dim1)THEN
c
c zero i start next row increment k 
            i=0
            k=k+1
         ENDIF
c
c assign to pixel the l and b
         call ftwldp(dble(fpix(1)+i),dble(fpix(2)+k),xrval,yrval,
     &        xrpix,yrpix,xinc,yinc,rot,coordtype,la,ba, status)
c
c         write(*,*)'status ftwldp', status
c
c If the value is not defined in the map than check the status
c 
         IF(status.eq.0)THEN
c
c
c assign the ra and dec 
           call GTC (la,ba,'FK4',equinox,ra,dec)
c
c calculate distance in degree
           decrad=decdeg/57.29578d0
           term1= (ra-radeg)*dcos(decrad)*(ra-radeg)*dcos(decrad)
           term2= (dec-decdeg)*(dec-decdeg)
           dist=sqrt(term1+term2)
c
c
c bnh two index first is j (1-9) to indicate than 9 pixel goes into
c the calculation
c the second instead run 1-4 where 1=nh 2=dist 3=ra 4=dec in degree 
c of the l and b center of that pixel.
           bnh(j,1)=buffer(j)
           bnh(j,2)=dist
           bnh(j,3)=ra
           bnh(j,4)=dec
c
c           write(*,*)'ra,dec,buf(j),d',ra,dec,buffer(j),dist,j
c
           nh=buffer(j)
           WRITE(context,1000)
     &        la, ba, fpix(1)+i,fpix(2)+k, dist, nh
1000       FORMAT(1x, 2f7.2, 2x, 2i5, 2x, f7.4, 2x, 1pe10.2)
           CALL XWRITE(context,15)
c
c test for minumun distance 
           if (dist.lt.defdist)defdist=dist 
         ELSE
           WRITE(context,1300)fpix(1)+i,fpix(2)+k  
1300       FORMAT(' NH is not defined for pixel',i5,3x,i5 )
           CALL XWRITE(context,15)
           status=0
         ENDIF
         i=i+1
      ENDDO
c
c take all points within the requested distance 
c and weigth interpola between the closest
      DO j=1,npix 
         IF (bnh(j,2).le.disio.and.bnh(j,2).ne.0)THEN
            near(j)=.true.
         ELSE
           near(j)=.false.
         ENDIF 
c         write(*,*)'near(j) j ',near(j),j,disio, bnh(j,2) 
      ENDDO      
      i=1
      DO J=1,npix
         IF(near(j)) THEN
           IF(bnh(j,1).gt.0)THEN 
              good(i,1)=bnh(j,1)
              good(i,2)=bnh(j,2)
              good(i,3)=bnh(j,3)
              good(i,4)=bnh(j,4)
              i=i+1
           ENDIF
         ENDIF
      ENDDO
      ng=i-1
      av=0.
      unav=0.
      wes=0.
      WRITE(context,
     &      '(5x ''RA'', 7x,''DEC'', 6x,''Dist'',6x,''  nH '')')    
      CALL XWRITE(context,8)
c
c      write(*,*)' ng av wes', ng, av, wes
      DO i=1,ng 
c
c write points in use
         WRITE(context,2000) good(i,3), good(i,4), good(i,2),good(i,1)
2000     FORMAT(2x, f8.4, 1x, f8.4, 2x, f7.4, 2x, 1pe10.2)    
         CALL XWRITE(context,8)
c
c         write(*,*)good(i,1),good(i,2)
c
         wei=(disio-good(i,2))/disio 
         av=good(i,1)*wei+av
         unav=good(i,1)+unav 
         wes=wes + wei
c         write(*,*)'weithed', av, wei, wes     
      ENDDO 
c      write(*,*)'defdist',defdist
      If (ng.ne.0) then 
         IF (usemap.EQ.0.OR.usemap.EQ.2) THEN
             nh1 = unav/ng
             nh2 = av/wes
             write(context,
     &             '(''  LAB >> Average nH (cm**-2)'', 1pe10.2)') 
     &             unav/ng
             CALL XWRITE(context,4)
             write(context,
     &             '(''  LAB >> Weighted average nH (cm**-2)'', 
     &             1pe10.2)') av/wes
             CALL XWRITE(context,4)
         ELSE
             altnh1 = unav/ng
             altnh2 = av/wes
             write(context,
     &             '(''  DL >> Average nH (cm**-2)'', 1pe10.2)') 
     &             unav/ng
             CALL XWRITE(context,4)
             write(context,
     &             '(''  DL >> Weighted average nH (cm**-2)'', 
     &             1pe10.2)') av/wes
             CALL XWRITE(context,4)
         ENDIF
       else 
         nh1=0.0
         nh2=0.0
         altnh1 = 0.0
         altnh2 = 0.0
         write(context, 3000)disio 
3000     FORMAT('  No points are within ',
     &             f7.4,' deg. from input position')
         call xwrite(context, 4)
         write(context, 4000)disio
4000     FORMAT('  Try with a distance larger than',
     &             f7.4,' deg' )
c         context='  Try with a distance larger than 1 deg'  
         call xwrite(context, 4) 
         write(context,
     &        '(''  First good point is at distance'', f7.4, '' deg'')')
     &        defdist 
         call xwrite(context, 4) 
       endif

c
c
999   CONTINUE
      IF(ierr.ne.0) CALL xaerror(context,1) 
c
c Loop on usemap
      IF(usemap.EQ.2) THEN
          usemap = 1
          write(context,'('' '')')
          CALL XWRITE(context,4)
          GOTO 10
      ENDIF
      RETURN
      END
