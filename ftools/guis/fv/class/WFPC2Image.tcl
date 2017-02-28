# First draft 06/27/96    Jianjun

# constrct a FitsImage object
# FitsImage FitsImageObjName FitsFileObjName currentHDU 

itcl::class WFPC2Image {
    inherit FitsImage
    constructor {args} {
	eval FitsImage::constructor $args
    } {
	#puts $args 
    }

# all these override/specialize their parents' (FitsImage and Table) methods

    private method _readInTable {} 
    private method _saveTableToAscii  {win_ asciiFileName_} 
    private method _powMakeImage {} 
}



itcl::body WFPC2Image::_readInTable { } {

    set _DC(height)   20
    set _DC(width)     [expr (int(log10($_numRows))+6)*$g_charPix]
    set _DC(headroom) 20
    set _DC(footroom) 40
    set _DC(vscrollsize) 15
    set _DC(hscrollsize) 15
    set _DC(rightspace) 6
    set _DC(interline)    0
    set _DC(tmar)         6
    set _DC(lmar)         8
    set _DC(tabspace)     0

# flag that the imageTable is being displayed
    set _isImageTable 1

    if { $_imgType == 0 } {
	set _cellSize 8
    } elseif {$_imgType == 1} {
	set _cellSize 8
    } elseif {$_imgType == 2} {
	set _cellSize 16
    } elseif {$_imgType == 3} {
	set _cellSize 16
    } elseif {$_imgType == 4} {
	set _cellSize 20
    } else {
	set _cellSize 8
    }

    set _listPreSelectedColNames {}
    set _dispCols $_numCols
    set _tableType Image

# use fits command setrowstate to initialize the rowState
# usage   setrowstate totalNumOfRos startRow endRow status 
# (0:normal, 1:selected, 2: deleted)
    setarray rowState 0 [expr $_numRows-1] 0
    setarray _colNotchedState 0 [expr $_dispCols-1] 0

    set _absXPos(0) [expr $_DC(lmar) + $_DC(width)/2]
    for {set i 0} {$i < $_dispCols} {incr i} {
        set _valueTDIM($i) 0
	set _columnName($i) [expr $i+1]
	set _columnType($i) " "
	set _columnUnit($i) " "
	lappend _listPreSelectedColNames [expr $i+1]
	set _cellWidth($i) $_cellSize 
	set _cellPixWidth($i) [expr $g_charPix*(1+$_cellWidth($i))]
	set _absXPos([expr $i+1]) [expr $_absXPos($i) + $_cellPixWidth($i) \
				      +$_DC(rightspace)] 
    }

}


itcl::body WFPC2Image::_powMakeImage {} {
    global powWCS powFitsHeader powFitsHeaderCnt powPlotParam
    global xCount yCount
    global powWCSName powWCSTranslation powWCSLabel powWCSList
    global useWCSInfo


    # get the pow widget 
    if { [winfo exist .pow.pow]!=1 } { 
	powInit .dummy
    }

    regsub -all { } [urlTail $fileName] _ cleanFileName
    set imgIndex  ${cleanFileName}_[expr $currentHDU-1]
    set imgHandle ${cleanFileName}_[expr $currentHDU-1]

    for {set i 1} {$i <=4} {incr i} {
	set ii $i
	set dataAddressForPOW [$fFile loadImageSlice $i [expr $i-1]] 
# the last param is for copying  data
	powCreateData ${imgHandle}_$ii $dataAddressForPOW $_imgType \
		       [expr $_numCols*$_numRows] 1
	# free the data array
	$fFile freeImage $dataAddressForPOW
    }
    # get the wcs info and pass them to pow
    # for WFPC images, the wcs info are stored in the second extension.
#    $fFile move +1
#    set crval1 [$fFile get table crval1]
#    set crval2 [$fFile get table crval2]
#    set crpix1 [$fFile get table crpix1]
#    set crpix2 [$fFile get table crpix2]
#    set cd1_1  [$fFile get table cd1_1 ]
#    set cd1_2  [$fFile get table cd1_2 ]
#    set cd2_1  [$fFile get table cd1_1 ]
#    set cd2_2  [$fFile get table cd1_2 ]
    

    set x_0 1
    set y_0 1
    set incrx 1
    set incry 1
    set x_label ""
    set y_label ""
    set x_unit "pixels"
    set y_unit "pixels"


    # Get the WCS info (if needed) and pass them to pow

    set powWCSLabel(xlabel,$imgIndex,DEFAULT) ""
    set powWCSLabel(ylabel,$imgIndex,DEFAULT) ""
    set powWCSLabel(xunit,$imgIndex,DEFAULT) ""
    set powWCSLabel(yunit,$imgIndex,DEFAULT) ""

    $fFile moveToHDU 1
    set powWCS($imgIndex) [$fFile getWcs]
    set powWCSName($imgIndex) 0
    set useWCSInfo($imgeIndex) $fvPref::ifWCSInfo

    if { $fvPref::ifWCSInfo } {
       set result [$fFile getHeader2String]
       set cntList($imgIndex) [$fFile getHeaderKeyWord [lindex $result 0] $imgIndex]
       set powFitsHeaderCnt($imgIndex) [lindex $cntList($imgIndex) 1]
       if { [lindex $cntList($imgIndex) 0] > 0 } {
          #set powFitsHeader($imgIndex) [$fFile assembleWcsHeader $imgIndex]
          set powFitsHeader($imgIndex) [lindex $result 0]
          set powWCSInfo($imgIndex,DEFAULT) $powWCS($imgIndex)
          set wcsinfo $powWCS($imgIndex) 
          set x_label [lindex [lindex $wcsinfo 3] 0]
          set y_label [lindex [lindex $wcsinfo 3] 1]
          if { $x_unit=="pixels" } {set x_unit NULL}
          if { $y_unit=="pixels" } {set y_unit NULL}
       } else {
          set powFitsHeader($imgIndex) [$fFile assembleWcsHeader $imgIndex NOWCS]
          set powWCS($imgIndex) [$fFile getWcs]
          set wcsinfo $powWCS($imgIndex) 
       }
    } else {
       set powWCS($imgIndex) [$fFile getWcs]
       set powFitsHeader($imgIndex) ""
       set powFitsHeaderCnt($imgIndex) 0
    }

    set powWCSList($imgIndex) {}
    set powWCSList(${imgIndex}scope) {}

    for {set i 1} {$i <=4} {incr i} {
# position the frames
#    2 1
#    3 4
	switch $i {
	    "1" {
		set x_0 [expr 1+$_numCols]
		set y_0 [expr 1+$_numRows]
		set incrx 1
		set incry 1
	    }
	    "2" {
		set x_0 1
		set y_0 [expr 1+$_numRows]
		set incrx 1
		set incry 1
	    }
	    "3" {
		set x_0 1
		set y_0 1
	    }
	    "4" {
		set x_0 [expr 1+$_numCols]
		set y_0 1 
		set incrx 1
		set incry 1

	    }
	}

	set ii $i
        set powWCSName(${imgHandle}_$ii) 0
        set powFitsHeader(${imgHandle}_$ii) $powFitsHeader($imgIndex)
        set powFitsHeaderCnt(${imgHandle}_$ii) $powFitsHeaderCnt($imgIndex)
        set powWCS(${imgHandle}_$ii) $powWCS($imgIndex)
        set powFitsHeader(${imgHandle}_${ii}scope) $powFitsHeader($imgIndex)
        set powFitsHeaderCnt(${imgHandle}_${ii}scope) $powFitsHeaderCnt($imgIndex)
        set powWCS(${imgHandle}_${ii}scope) $powWCS($imgIndex)

        set powPlotParam(graphType,${imgHandle}_$ii) \
                      [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
        set powPlotParam(graphType,${imgHandle}_${ii}scope) \
                      $powPlotParam(graphType,${imgHandle}_$ii)
        set powPlotParam(zoomed,${imgHandle}_$ii) 0
        set powPlotParam(zoomed,${imgHandle}_${ii}scope) 0

        set xCount(${imgHandle}_$ii) 0
        set yCount(${imgHandle}_$ii) 0
        set xCount(${imgHandle}_${ii}scope) 0
        set yCount(${imgHandle}_${ii}scope) 0
        set powWCSName(${imgHandle}_$ii) 0
        set powWCSName(${imgHandle}_${ii}scope) 0
        set powWCSTranslation 0

	powCreateImage ${imgHandle}_$ii ${imgHandle}_$ii 0 0\
		       $_numCols $_numRows $x_0 \
		       $incrx $y_0 $incry $x_label $y_label counts

        if [info exists powWCSList(${imgHandle}_$ii)] {
           foreach name [lindex $powWCSList(${imgHandle}_$ii) 1] {
              $fFile assembleWcsLabel ${imgHandle}_$ii $name
           }
        } else {
           set powWCSList(${imgHandle}_$ii) {}
           lappend powWCSList(${imgHandle}_$ii) 1
           lappend powWCSList(${imgHandle}_$ii) {}
        }

        set powWCSList(${imgHandle}_${ii}scope) $powWCSList(${imgHandle}_${ii})

	set powRBmin(${imgHandle}_$ii) 0
	set powRBmax(${imgHandle}_$ii) 1000
    }

    # use one of the image list
    set powWCSList($imgIndex) $powWCSList(${imgHandle}_4)
    set powWCSList(${imgIndex}scope) $powWCSList(${imgHandle}_4)

    if [info exists powWCSList($imgIndex)] {
       foreach name [lindex $powWCSList($imgIndex) 1] {
          $fFile assembleWcsLabel $imgIndex $name
       }
    } else {
       set powWCSList($imgIndex) {}
       lappend powWCSList($imgIndex) 1
       lappend powWCSList($imgIndex) {}
    }

    set powWCSList(${imgHandle}) $powWCSList($imgIndex)
    set powWCSList(${imgHandle}scope) $powWCSList($imgIndex)

    set powWCSName(${imgHandle}) 0
    set powFitsHeader($imgHandle) $powFitsHeader($imgIndex)
    set powFitsHeaderCnt($imgHandle) $powFitsHeaderCnt($imgIndex)
    set powWCS($imgHandle) $powWCS($imgIndex)
    set powPlotParam(graphType,$imgHandle) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${imgHandle}scope) $powPlotParam(graphType,$imgHandle)
    set powPlotParam(zoomed,$imgHandle) 0
    set powPlotParam(zoomed,${imgHandle}scope) 0

    set xCount($imgHandle) 0
    set yCount($imgHandle) 0
    set xCount(${imgHandle}scope) 0
    set yCount(${imgHandle}scope) 0

    powCreateGraph $imgHandle NULL \
	[list ${imgHandle}_1  ${imgHandle}_2 ${imgHandle}_3 ${imgHandle}_4] \
		   $x_unit $y_unit \
		   $x_label $y_label \
		   [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1] 
}

itcl::body WFPC2Image::_saveTableToAscii  {win_ asciiFileName_} {

# setup the grouping 
  set tmpWidth  $_cellSize
  set tmpFirstCol(0) 1
  set groupCount  0  

  for {set  n 0} {$n < $_numCols-1} {incr n} {
      set tmpWidth [expr  $tmpWidth + $_cellSize]
      if { $tmpWidth > $_asciiColWidth} {
      incr groupCount 
      set tmpWidth  $_cellSize;
	  set tmpFirstCol($groupCount) [expr  $n+2]
    }
  }
  incr groupCount
  set tmpFirstCol($groupCount) [expr 1+$_numCols]

  $win_.f.fb configure -steps $groupCount
  for {set k 0} {$k< $groupCount} {incr k} {
      set nCols  [expr $tmpFirstCol([expr $k+1])-$tmpFirstCol($k)]
      $fFile saveImgToASCII $asciiFileName_ $k \
	  1 $_numRows $tmpFirstCol($k) $nCols $_cellSize
      if {[catch {$win_.f.fb step}] == 1} {
	  file delete $asciiFileName_
	  return
      }
  }
  destroy $win_

}


















