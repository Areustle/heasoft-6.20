# First draft 06/27/96    Jianjun

# CubeImage CubeImageName FitsFileObjName currentHDU firstSlice lastSlice

itcl::class CubeImage {
    inherit FitsImage
    constructor {args} {
        eval FitsImage::constructor [lrange $args 0 1]
    } {
	set _istart [lindex $args 2]
	set _iend   [lindex $args 3]
	set _cslice [lindex $args 4]
    }
    
#  all of these methods override/specialize methods defined in
#  FitsImage or its parent Table
    public method setFileName { fName_ }

    private method _readInTable {} 

    private method _readTableData {fCol_ fRow_ nCols_ nRows_} 
    private method _putRawData {col_ row_ val_}
    private method _getRawDataBlock { fCol_ fRow_ lCol_ lRow_ }
    private method _putRawDataBlock { fCol_ fRow_ data_ }

    private method _powMakeImage {} 
    private method _saoMakeImage {}
    private method _ds9MakeImage {}
    
#   these variables are only for use wihin this class

# which image slice to start the pow movie from
    private variable _istart
# which image slice to end   the pow movie at
    private variable _iend
# which cube slice to end   the pow movie at
    private variable _cslice
}


itcl::body CubeImage::setFileName { fName_ } {
    set fileName $fName_
    set rName [urlTail $fName_]
    set dName [getFullDirPath $fName_]
    wm title $_droot "fv: $_tableType\[$_istart\] of $rName\[[expr $currentHDU-1]\] in $dName"
    .fvwinkeeper signoff  $_droot
    .fvwinkeeper register $_droot "Image Table" [urlTail $fName_] $currentHDU $this
}


itcl::body CubeImage::_readInTable { } {
    global g_charPix
    
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


itcl::body CubeImage::_saoMakeImage {} {
   tk_messageBox -type ok -message "SAOtng can only load the first slice of a 3D image at this point" -icon warning
   FitsImage::_saoMakeImage
}
    

itcl::body CubeImage::_ds9MakeImage {} {
   # tk_messageBox -type ok -message "DS9 can only load the first slice of a 3D image at this point" -icon warning
   FitsImage::_ds9MakeImage
}
    

itcl::body CubeImage::_powMakeImage {} {
    global powWCS powFitsHeader powFitsHeaderCnt powPlotParam
    global xCount yCount
    global powWCSName powWCSTranslation powWCSLabel powWCSList
    global useWCSInfo

    # get the pow widget 
    if { [winfo exist .pow.pow] != 1 } { 
	powInit .dummy
    }

    regsub -all { } [urlTail $fileName] _ cleanFileName
    set imgIndex  ${cleanFileName}_[expr $currentHDU-1]
    set imgHandle ${cleanFileName}_[expr $currentHDU-1]_$_graphIDhighest
    incr _graphIDhighest

    for {set i $_istart} {$i <= $_iend} {incr i} {
# load slice of image without rotating
	set dataAddressForPOW [$fFile loadImageSlice $i 0] 
# the last param is for copying  data
	powCreateData ${imgHandle}_$i $dataAddressForPOW $_imgType \
		       [expr $_numCols*$_numRows] 1
	# free the data array
	$fFile freeImage $dataAddressForPOW
    }

    set powWCSName($imgIndex) 0
    set powWCSName($imgHandle) 0
    set x_0 1
    set y_0 1
    set incrx 1
    set incry 1
    set x_label ""
    set y_label ""
    set x_unit "pixels"
    set y_unit "pixels"
    if { ![catch {set tmp [$fFile getKeyword CTYPE1]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set x_label [string trim $v {' }]
    }
    if { ![catch {set tmp [$fFile getKeyword CTYPE2]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set y_label [string trim $v {' }]
    }
    if { ![catch {set tmp [$fFile getKeyword CUNIT1]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set x_unit [string trim $v {' }]
    }
    if { ![catch {set tmp [$fFile getKeyword CUNIT2]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set y_unit [string trim $v {' }]
    }

    # Get the WCS info (if needed) and pass them to pow

    set powWCS($imgIndex) [$fFile getWcs]
    set useWCSInfo($imgIndex) $fvPref::ifWCSInfo

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
       }

    } else {
       set powWCS($imgIndex) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
       set powFitsHeader($imgIndex) ""
       set powFitsHeaderCnt($imgIndex) 0
    }

#puts "powFitsHeader($imgIndex): $powFitsHeader($imgIndex)"
#puts "powWCS($imgIndex): $powWCS($imgIndex)"
#puts "powFitsHeader($imgIndex): $powFitsHeader($imgIndex)"
#puts "powFitsHeaderCnt($imgIndex): $powFitsHeaderCnt($imgIndex)"
#
    set powWCSList($imgIndex) {}
    set powWCSList(${imgIndex}scope) {}

    set imgList {}
    for {set i $_istart} {$i <= $_iend} {incr i} {
        if [info exists powWCS(${imgHandle}_$i)] {
           catch { unset powWCS(${imgHandle}_$i) }
        }
        set powFitsHeader(${imgHandle}_$i) $powFitsHeader($imgIndex)
        set powFitsHeaderCnt(${imgHandle}_$i) $powFitsHeaderCnt($imgIndex)
        catch { set powWCS(${imgHandle}_$i) $powWCS($imgIndex) }

        set powPlotParam(graphType,${imgHandle}_$i) \
              [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
        set powPlotParam(graphType,${imgHandle}_${i}scope) \
                        $powPlotParam(graphType,${imgHandle}_${i})
        set powPlotParam(zoomed,${imgHandle}_${i}) 0
        set powPlotParam(zoomed,${imgHandle}_${i}scope) 0

        set xCount(${imgHandle}_$i) 0
        set yCount(${imgHandle}_$i) 0
        set xCount(${imgHandle}_${i}scope) 0
        set yCount(${imgHandle}_${i}scope) 0
        set powWCSName(${imgHandle}_$i) 0
        set powWCSName(${imgHandle}_${i}scope) 0
        set powWCSTranslation 0
        catch {
	   powCreateImage ${imgHandle}_$i ${imgHandle}_$i 0 0\
		          $_numCols $_numRows $x_0 \
		          $incrx $y_0 $incry $x_label $y_label counts
        } err
        if [info exists powWCSList(${imgHandle}_${i})] {
           foreach name [lindex $powWCSList(${imgHandle}_$i) 1] {
              $fFile assembleWcsLabel ${imgHandle}_$i $name
           }
        } else {
           set powWCSList(${imgHandle}_${i}) {}
           lappend powWCSList(${imgHandle}_${i}) 1
           lappend powWCSList(${imgHandle}_${i}) {}
        }

        set powWCSList(${imgHandle}_${i}scope) $powWCSList(${imgHandle}_${i})
	lappend imgList ${imgHandle}_$i
        set powWCSLabel(xlabel,${imgHandle}_$i,DEFAULT) $x_label
        set powWCSLabel(ylabel,${imgHandle}_$i,DEFAULT) $y_label
        set powWCSLabel(xunit,${imgHandle}_$i,DEFAULT) $x_unit
        set powWCSLabel(yunit,${imgHandle}_$i,DEFAULT) $y_unit
    }

    # wcslist can be obtained from any one slice
    set powWCSList($imgIndex) $powWCSList(${imgHandle}_$_iend)
    set powWCSList(${imgIndex}scope) $powWCSList(${imgHandle}_$_iend)

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

    set powWCS(${imgHandle}) $powWCS($imgIndex)
    set powWCS(${imgHandle}scope) $powWCS($imgIndex)
    set powFitsHeader($imgHandle) $powFitsHeader($imgIndex)
    set powFitsHeaderCnt($imgHandle) $powFitsHeaderCnt($imgIndex)
    set powFitsHeader(${imgHandle}scope) $powFitsHeader($imgIndex)
    set powFitsHeaderCnt(${imgHandle}scope) $powFitsHeaderCnt($imgIndex)
    set powPlotParam(graphType,$imgHandle) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${imgHandle}scope) $powPlotParam(graphType,$imgHandle)
    set powPlotParam(zoomed,$imgHandle) 0
    set powPlotParam(zoomed,${imgHandle}scope) 0

    set xCount($imgHandle) 0
    set yCount($imgHandle) 0
    set xCount(${imgHandle}scope) 0
    set yCount(${imgHandle}scope) 0
    powCreateGraph $imgHandle NULL $imgList \
		   $x_unit $y_unit \
		   $x_label $y_label \
		   [lindex $fvPref::graphDispSize 0] \
                   [lindex $fvPref::graphDispSize 1]

    set powWCSLabel(xlabel,${imgHandle},DEFAULT) $x_label
    set powWCSLabel(ylabel,${imgHandle},DEFAULT) $y_label
    set powWCSLabel(xunit,${imgHandle},DEFAULT) $x_unit
    set powWCSLabel(yunit,${imgHandle},DEFAULT) $y_unit
    catch {unset imgList}

# play the movie
    if { $_istart != $_iend} {
	powMovie 
#	set g_movieParam(loop) 1
        after 1 powPlayMovie     
    }
}

##############################################
#
# Handle Reading/Writing/Formatting of Data
#

itcl::body CubeImage::_readTableData {fCol_ fRow_ nCols_ nRows_} {
    if { $_istart != $_iend } {
	error "Can only load one slice a time"
    }

    set _slice $_istart
    set fRow_ [expr $_numRows - $fRow_ - $nRows_ + 1]
    incr fCol_

    # image data will be loaded into a 2-d Tcl array _tableData
    # this cannot be done as a FitsFile method because Tcl does not
    #  allow arrays to be returned
    #  the array must be created locally
    set fitsfile [$fFile cget -fitsfile]
    $fitsfile load iblock "_tableData" $fRow_ $nRows_ $fCol_ $nCols_ $_istart $_cslice
}

itcl::body CubeImage::_putRawData { col_ row_ val_ } {
# Overrides IMAGE method so that the image $_slice is taken into account

   set realRow [expr $_numRows-$row_-1]

   set origin    [expr $_numRows*$_numCols*($_slice-1)]
   set firstElem [expr $realRow*$_numCols+$col_+1+$origin]

   $fFile putImage $firstElem 1 [list $val_]
   _readTableData $col_ $row_ 1 1
}

itcl::body CubeImage::_getRawDataBlock { fCol_ fRow_ lCol_ lRow_ } {
   # col/row zero-indexed

   set origin [expr $_numRows*$_numCols*($_slice-1)]

   set nElem [expr $lCol_ - $fCol_ + 1]
   for { set col 0 } { $col<$nElem } { incr col } {
      set _colData($col) {}
   }

   set row $fRow_
   for { set row $fRow_ } { $row <= $lRow_ } { incr row } {
      set realRow   [expr $_numRows-$row-1]
      set firstElem [expr $realRow*$_numCols+$fCol_+1+$origin]
      set col 0
      foreach datum [$fFile getImageAsList $firstElem $nElem] {
         lappend _colData($col) $datum
         incr col
      }
   }

   set data {}
   for { set col 0 } { $col<$nElem } { incr col } {
      lappend data $_colData($col)
   }

   return $data
}

itcl::body CubeImage::_putRawDataBlock { fCol_ fRow_ data_ } {
   # col/row zero-indexed

   set origin [expr $_numRows*$_numCols*($_slice-1)]

   set nCols [llength $data_]
   set nRows [llength [lindex $data_ 0]]

   for { set i 0 } { $i<$nRows } { incr i } {
      set rowData {}
      foreach cData $data_ {
         lappend rowData [lindex $cData $i]
      }
      set realRow   [expr $_numRows-$fRow_-1]
      set firstElem [expr $realRow*$_numCols+$fCol_+1+$origin]
      $fFile putImage $firstElem $nCols $rowData
      incr fRow_
   }
}

#
#  End Data handlers
#
##############################################
