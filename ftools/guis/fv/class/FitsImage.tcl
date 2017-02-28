# First draft 06/27/96    Jianjun


#--------------------------------------------------------------
# Modification history
# Ziqin Pan, Feb 18, 2004
# Add export as text on selected rows
#--------------------------------------------------------------

# FitsImage FitsImageObjName FitsFileObjName currentHDU 

itcl::class FitsImage {
    inherit Table
    constructor {args} {}
    destructor         {}

# this public method overrides the parent Table class
    public method setFileName { fName_ }

# these public methods are not in the parent and are unique to this class
    public method makeImage { }
    public method showCell {col_ row_}
    public method imagePlot {paramList_}
    public method _sPlotCmd {} 
    public variable imgHandle

# these protected methods override the parent Table class
    protected method _buildMenus {}
    protected method _postMenus {}
    protected method _drawTable {}
    protected method _readInTable {} 
    protected method _plotCmd {} 
    protected method _saveTableToAscii {win_ asciiFileName_} 
    protected method _constructDS9Image { tmpfile }

# data handlers
    protected method _readTableData {fCol_ fRow_ nCols_ nRows_} 
    protected method _writeTableData {col_ row_ val_}
    protected method _getFormattedData {col_ row_}
    protected method _getRawDataForDS9 {col_ row_}
    protected method _getRawData {col_ row_}
    protected method _getRawDataBlock { fCol_ fRow_ lCol_ lRow_ }
    protected method _getImageVectorData { fCol_ fRow_ lCol_ lRow_ }
    protected method _putRawDataBlock { fCol_ fRow_ data_ }
    protected method _putRawData {col_ row_ val_}

# these protected methods are not in the parent and are unique to this class
    protected method _powMakeImageFlip { direction } 
    protected method _powMakeImage {} 
    protected method _saoMakeImage {}
    protected method _ds9MakeImage {}
    protected method _ds9MakePlot  { rowNum_ fRow lRow xColumn_ yColumn_ }

# these protected variables are not in the parent and are unique to this class
    protected variable _imgType
    protected variable _bitpixKey
    protected variable _cellSize
    protected variable _isImageTable 0
    protected variable _slice 1
    protected variable _dims
    protected variable _imgNull
    protected variable _colData
    protected variable _imgForm
    protected variable _tableDataDS9
    #protected variable fFile
}


itcl::body FitsImage::constructor {args} {

# if _tableType exists, then we're creating a VectorTable, so skip these inits
    if { ![info exists _tableType] } {
	set fFile FitsExtension::[lindex $args 0]
	set _fatherFitsExtension [lindex $args 1]
	
	set currentHDU     [$fFile cget -currentHDU]
	set isFailedToCopy [$fFile cget -isFailedToCopy]
	set fileName       [$fFile getOrigName]
	
	# Image info
	set _dims [$fFile getImgInfo]
	if { [llength $_dims] > 1 } {
	    set _numCols [lindex $_dims 0]
	    set _numRows [lindex $_dims 1]
	} else {
	    # Display image vertically
	    set _numCols 1
	    set _numRows [lindex $_dims 0]
	}
	if { $_showRows > $_numRows } {
	    set _showRows $_numRows
	}
	if { $_showCols > $_numCols } {
	    set _showCols $_numCols
	}
	
        set _bitpixKey [$fFile getImgType]
	
	if { [catch { set tmpNul [$fFile getKeyword BLANK] }] } {
	    set _imgNull "NULL"
	} else {
	    set _imgNull [lindex [join $tmpNul] 1]
	}
	
	if { ![catch {$fFile getKeyword BZERO}] ||
	![catch {$fFile getKeyword BSCALE}] } {
	    set _imgType 4
	    # set _imgForm "% #.10E"
	    set _imgForm "%g"
	} else {
	    if { $_bitpixKey == "8" } {
		set _imgType 0 
		set _imgForm "%u"
	    } elseif {$_bitpixKey == "16"} {
		set _imgType 1 
		set _imgForm "%d"
	    } elseif {$_bitpixKey == "32"} {
		set _imgType 2 
		set _imgForm "%d"
	    } elseif {$_bitpixKey == "-32"} {
		set _imgType 3 
		# set _imgForm "% #.5f"
		set _imgForm "%g"
	    } elseif {$_bitpixKey == "-64"} {
		set _imgType 4 
		# set _imgForm "% #.10E"
		set _imgForm "%g"
	    } elseif {$_bitpixKey == "64"} {
		set _imgType 5 
	        set _imgForm "%s"
	    } else {
		puts "unsupported image format, treat as integer type"
		set _imgType 2
	    }
	}
	
	set _tableType Image
	$_fatherFitsExtension addChild $this table
    }
}

itcl::body FitsImage::destructor {} {
    if { $_tableType == "Image" } {

	#take care of the stuff on the canvas
	if { $_isImageTable } {
	    set _isBeingDestroyed 1
	    destroy $_droot
	}

	$_fatherFitsExtension freeChild $this
    }
    # else it's a VectorTable, so follow VectorTable's destructor's code
}


itcl::body FitsImage::setFileName { fName_ } {

    Table::setFileName $fName_
    .fvwinkeeper signoff  $_droot
    .fvwinkeeper register $_droot "Image Table" [urlTail $fName_] $currentHDU \
	    $this
}


itcl::body FitsImage::_drawTable {} {

    Table::_drawTable

    bind $_droot <<Plot>>        [itcl::code $this _plotCmd]
}


itcl::body FitsImage::_buildMenus {} {
    global isMac
    global g_titleFont
    
    if { $isMac } {
        set _mBar .mbar.itable
    } else {
        set _mBar $_droot.mbar
    }
    $_droot config -menu $_mBar
    
    if { ![winfo exists $_mBar] } {
       _buildNewMenus

       # Can only Plot an image
       $_mBar.tools add command -label "Plot..." \
             -command "doMenuEvent <<Plot>>" -font g_titleFont

    }
}


itcl::body FitsImage::_postMenus {} {
   Table::_postMenus
   update idle
}   


itcl::body FitsImage::_readInTable { } {
    global g_charPix

    set _DC(height)      20
    set _DC(width)       [expr (int(log10($_numRows))+6)*$g_charPix]
#    set _DC(headroom)    20
    set _DC(headroom)    80
    set _DC(footroom)    40
    set _DC(vscrollsize) 15
    set _DC(hscrollsize) 15
    set _DC(rightspace)   6
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
    } elseif {$_imgType == 4 || $_imgType == 5} {
	set _cellSize 20
    } else {
	set _cellSize 8
    }

    set _listPreSelectedColNames {}
    set _dispCols $_numCols

# use fits command setrowstate to initialize the rowState
# usage   setrowstate totalNumOfRos startRow endRow status 
# (0:normal, 1:selected, 2: deleted)
    setarray rowState 0 [expr $_numRows-1] 0
    setarray _colNotchedState 0 [expr $_dispCols-1] 0

    set _absXPos(0) [expr $_DC(lmar) + $_DC(width) + $_DC(rightspace)]
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


itcl::body FitsImage::makeImage { } {
    global g_hasSAOtng g_hasDS9

    if { $fvPref::imgDisplayer == "SAOtng" && $g_hasSAOtng } {
	set err [catch {_saoMakeImage} errmsg]
    } elseif { $fvPref::imgDisplayer == "DS9" && $g_hasDS9 } {
	set err [catch {_ds9MakeImage} errmsg]
    } else {
	set err [catch {_powMakeImage} errmsg]
    }
    
    # after the image is displayed. clean up   
    itcl::delete object $this
    if { $err } {
        error $errmsg
    }
}

itcl::body FitsImage::_saoMakeImage {} {
    global g_backupDir

    set err "none"

    set tmpfile $g_backupDir/saoimage.fits
    if { ![catch {$fFile copyCHDU $tmpfile} ] } {
	catch {exec xpaaccess SAOtng} result  
	if { [string range $result 0 1] == "no" } {
	    # start SAOtng if SAOtng isn't there
	    if { [catch {exec saotng &} saopid] } {
		file delete $tmpfile
	        if { [tk_dialog .saoError "SAOtng startup error" \
		        "Cannot start saotng!\nUse POW instead?" \
			question 0 Yes No] == 0 } {
		     _powMakeImage
                }
		return
	    } 
	    # wait till saotng is up
	    set flag  1
            set nSecs 0
	    while { $flag } {
                after 1000
                incr  nSecs
		catch {exec xpaaccess SAOtng} result
                if { [string range $result 0 2] == "yes" } {
                   set flag 0
                } else {
                   if { $nSecs > 10 } {
                      file delete $tmpfile
                      if { [tk_dialog .saoError "SAOtng startup error" \
                            "Cannot start saotng!\nUse POW instead?" \
                            question 0 Yes No] == 0 } {
                         _powMakeImage
                      }
                      return
                   }
                }
	    } 
	} 
	exec echo "flip y"  | xpaset SAOtng
	exec xpaset SAOtng fits < $tmpfile
	file delete $tmpfile 
    }
}
    
itcl::body FitsImage::_constructDS9Image { tmpfile } {
# Pan Chai.. this routine is been disabled
     set _dims [$fFile getImgInfo]
     set _bitpixKey [$fFile getImgType]
     set _extname ""
     catch { set _extname [$fFile getKeyword "EXTNAME"] } err

     set fh [$fFile cget -fitsfile]

     if { $_extname == "" } {
        catch {$fh load copyto $fileName $tmpfile} err
     } else {
        set _extname [string trim [lindex [split $_extname " "] 1] "\{'\}"]
        catch {$fh load copyto "$fileName\[$_extname\]" $tmpfile} err
     }
     return SUCCESS
}

itcl::body FitsImage::_ds9MakeImage {} {
    global g_backupDir

    set err "none"

    set tmpfile $g_backupDir/saoimage.fits

    set errorFlag [catch {
        file copy -force $fileName $tmpfile
    } err ]

    if { !$errorFlag } {
	catch {exec xpaaccess ds9} result  
	if { [string range $result 0 1] == "no" } {
	    # start DS9 if DS9 isn't there
	    if { [catch {exec ds9 &} ds9pid] } {
		file delete $tmpfile
	        if { [tk_dialog .saoError "DS9 startup error" \
		        "Cannot start DS9!\nUse POW instead?" \
			question 0 Yes No] == 0 } {
		     _powMakeImage
                }
		return
	    } 
	    # wait till ds9 is up
	    set flag  1
            set nSecs 0
	    while { $flag } {
                after 1000
                incr  nSecs
		catch {exec xpaaccess ds9} result
                if { [string range $result 0 2] == "yes" } {
                   set flag 0
                } else {
                   if { $nSecs > 10 } {
                      file delete $tmpfile
                      if { [tk_dialog .saoError "DS9 startup error" \
                            "Cannot start ds9!\nUse POW instead?" \
                            question 0 Yes No] == 0 } {
                         _powMakeImage
                      }
                      return
                   }
                }
	    } 
	} 
        set ds9Ext [expr $currentHDU - 1]
	exec xpaset ds9 fits "[file tail $fileName]\[$ds9Ext\]" < "$tmpfile"
	file delete $tmpfile 
    } else {
        file delete $tmpfile
	if { [tk_dialog .saoError "Can't create temporary file for DS9!\nTry POW instead?" \
              question 0 Yes No] == 0 } {
           _powMakeImage
        }
    }
}
    
itcl::body FitsImage::_powMakeImageFlip { direction } {
    global powWCS 
    global powWCSTranslation powPlotParam
    global xCount yCount
    global useWCSInfo

    # get the pow widget 
    if { [winfo exist .pow.pow] != 1 } { 
	powInit .dummy
    }

    set dataAddressForPOW [$fFile loadImage] 

    regsub -all { } [urlTail $fileName] _ cleanFileName
    set imgIndex  ${cleanFileName}_[expr $currentHDU-1]
    set imgHandle ${cleanFileName}_[expr $currentHDU-1]

    if { [llength $_dims] == 1 } {
       # Display image horizontally
       set nCols $_numRows
       set nRows $_numCols
    } else {
       set nCols $_numCols
       set nRows $_numRows
    }

# the last param is for copying  data
    powCreateDataFlip $imgHandle $dataAddressForPOW $_imgType [expr $nCols*$nRows] 1 $direction $nRows $nCols
# free the data array
    $fFile freeImage  $dataAddressForPOW
    
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

    set z_label "counts"
    if { ![catch {set tmp [$fFile getKeyword BUNIT]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set z_label [string trim $v {' }]
    }

    # Get the WCS info (if needed) and pass them to pow

    set useWCSInfo($imgHandle) $fvPref::ifWCSInfo
    set useWCSInfo(${imgHandle}scope) $fvPref::ifWCSInfo
    if { $fvPref::ifWCSInfo } {
       set wcsinfo [$fFile getWcs]
       set powWCS($imgIndex) $wcsinfo
       set x_label [lindex [lindex $wcsinfo 3] 0]
       set y_label [lindex [lindex $wcsinfo 3] 1]
       if { $x_unit=="pixels" } {set x_unit NULL}
       if { $y_unit=="pixels" } {set y_unit NULL}
    } else {
       set powWCS($imgIndex) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
    }

    # powWCSTraslation is a variable that will be set in PowUtils.c
    set powWCSTranslation 0

    set powPlotParam(graphType,$imgHandle) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${imgHandle}scope) $powPlotParam(graphType,$imgHandle)
    set powPlotParam(zoomed,$imgHandle) 0
    set powPlotParam(zoomed,${imgHandle}scope) 0
    set xCount($imgHandle) 0
    set yCount($imgHandle) 0
    set xCount(${imgHandle}scope) 0
    set yCount(${imgHandle}scope) 0

    powCreateImage $imgHandle $imgHandle 0 0 $nCols $nRows $x_0 \
		   $incrx $y_0 $incry $x_label $y_label $z_label

    if { $powWCSTranslation != 0 } {
       if { $powWCSTranslation == 1 } {
          tk_messageBox -icon warning -type ok \
                        -message "Unrecognized Coordinate System is ignored."
       } else {
          tk_messageBox -icon warning -type ok -message "$powWCSTranslation\n\ndefault to pixel coordinate."
       }
       set useWCSInfo($imgHandle) 0
       set useWCSInfo(${imgHandle}scope) 0
       set powWCS($imgIndex) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
       powCreateImage $imgHandle $imgHandle 0 0 $nCols $nRows $x_0 \
		   $incrx $y_0 $incry $x_label $y_label $z_label

       powCreateGraph $imgHandle NULL $imgHandle $x_unit $y_unit \
	    $x_label $y_label \
	    [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1] 
    } else {
       powCreateGraph $imgHandle NULL $imgHandle $x_unit $y_unit \
	    $x_label $y_label \
	    [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1] 
    }

# allows double clicking on an image to pop up table display centered on pixel
    .pow.pow bind img_$imgHandle <<DblBtnPress>> \
          "+returnCurrentImageInfo [$fFile getMaster] $currentHDU $imgHandle %x %y"
}

itcl::body FitsImage::_powMakeImage {} {
    global powWCS powRotation powWCSName
    global powWCSTranslation
    global powFitsHeader powFitsHeaderCnt powPlotParam
    global xCount yCount
    global graphHandleList
    global powWCSList powWCSLabel
    global useWCSInfo

    # get the pow widget 
    if { [winfo exist .pow.pow] != 1 } { 
	powInit .dummy
    }

    set dataAddressForPOW [$fFile loadImage] 

    regsub -all { } [urlTail $fileName] _ cleanFileName
    set imgIndex  ${cleanFileName}_[expr $currentHDU-1]
    set imgHandle ${cleanFileName}_[expr $currentHDU-1]_$_graphIDhighest

    set fileName [$fFile getOrigName]

    set idx -1
    if [info exist graphHandleList] {
       set idx [lsearch -glob $graphHandleList [list $fileName "${imgIndex}*"]]
    }

    if { $idx < 0 } {
       # if we are creating a brand new image
       set entry {}
       lappend entry $fileName
       lappend entry $imgHandle
       lappend graphHandleList $entry
       incr _graphIDhighest
    } else {
       set imgHandle [lindex [lindex $graphHandleList $idx] 1]
    }

    if [info exists powWCS($imgHandle)] {
       powDeleteImage $imgHandle $imgHandle
       powDeleteGraph $imgHandle NOPROMPT
       powDeleteCurve $imgHandle curve
       catch { unset powWCS($imgHandle) }
    }

    if { [llength $_dims] == 1 } {
       # Display image horizontally
       set nCols $_numRows
       set nRows $_numCols
    } else {
       set nCols $_numCols
       set nRows $_numRows
    }

# the last param is for copying  data
    powCreateData $imgHandle $dataAddressForPOW $_imgType [expr $nCols*$nRows] 1
# free the data array
    $fFile freeImage  $dataAddressForPOW
    
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

    set z_label "counts"
    if { ![catch {set tmp [$fFile getKeyword BUNIT]}] } { 
       set v [lindex [lindex $tmp 0] 1]
       set z_label [string trim $v {' }]
    }

    # Get the WCS info (if needed) and pass them to pow

    set powWCS($imgHandle) [$fFile getWcs]
    set powWCSName($imgHandle) 0
    set powWCSName(${imgHandle}scope) 0
    set useWCSInfo($imgHandle) $fvPref::ifWCSInfo
    set useWCSInfo(${imgHandle}scope) $fvPref::ifWCSInfo
    if { $fvPref::ifWCSInfo } {
       set result [$fFile getHeader2String]
       set cntList($imgHandle) [$fFile getHeaderKeyWord [lindex $result 0] $imgHandle]
#puts "result: $result"
#puts "cntList($imgHandle): $cntList($imgHandle)"
       set powFitsHeaderCnt($imgHandle) [lindex $cntList($imgHandle) 1]
       if { [lindex $cntList($imgHandle) 0] > 0 } {
          #set powFitsHeader($imgHandle) [$fFile assembleWcsHeader $imgHandle]
          set powFitsHeader($imgHandle) [lindex $result 0]
#[$fFile headerDebugDataPrint "Default" $powFitsHeader($imgHandle)]
          #set powWCS($imgHandle) [$fFile getWcs]
          set powWCSInfo($imgHandle,DEFAULT) $powWCS($imgHandle)
          set wcsinfo $powWCS($imgHandle)

          set x_label [lindex [lindex $wcsinfo 3] 0]
          set y_label [lindex [lindex $wcsinfo 3] 1]
          if { $x_unit=="pixels" } {set x_unit NULL}
          if { $y_unit=="pixels" } {set y_unit NULL}
 
          if { ![catch {set tmp [$fFile getKeyword CROTA2]}] } {
             set v [lindex [lindex $tmp 0] 1]
             set powRotation($imgHandle) [string trim $v {' }]
          }
       } else {
          set powFitsHeader($imgHandle) [$fFile assembleWcsHeader $imgHandle NOWCS]
          set powWCS($imgHandle) [$fFile getWcs]
       }

       if { [lindex $powWCS($imgHandle) end] == "none" } {
          # no projection, assume linear
          #set powWCS($imgHandle) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
          #set powFitsHeader($imgHandle) ""
          #set powFitsHeaderCnt($imgHandle) 0
       }
    } else {
       set powWCS($imgHandle) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
       set powFitsHeader($imgHandle) ""
       set powFitsHeaderCnt($imgHandle) 0
    }

    set powWCSTranslation 0
    set powWCSList($imgHandle) {}
    set powWCSList(${imgHandle}scope) {}
    set powPlotParam(graphType,$imgHandle) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${imgHandle}scope) $powPlotParam(graphType,$imgHandle)
    set powPlotParam(zoomed,$imgHandle) 0
    set powPlotParam(zoomed,${imgHandle}scope) 0
    set xCount($imgHandle) 0
    set yCount($imgHandle) 0
    set xCount(${imgHandle}scope) 0
    set yCount(${imgHandle}scope) 0

    powCreateImage $imgHandle $imgHandle 0 0 $nCols $nRows $x_0 \
              $incrx $y_0 $incry $x_label $y_label $z_label

    if [info exists powWCSList($imgHandle)] {
       foreach name [lindex $powWCSList($imgHandle) 1] {
          $fFile assembleWcsLabel $imgHandle $name
       }
    } else {
       set powWCSList($imgHandle) {}
       lappend powWCSList($imgHandle) 1
       lappend powWCSList($imgHandle) {}
    }

    set powWCSList(${imgHandle}scope) $powWCSList($imgHandle)
    if { $powWCSTranslation != 0 } {
       if { $powWCSTranslation == 1 } {
          tk_messageBox -icon warning -type ok \
                        -message "Unrecognized Coordinate System is ignored."
       } elseif { $fvPref::ifWCSInfo } {
          tk_messageBox -icon warning -type ok -message "$powWCSTranslation\n\ndefault to pixel coordinate."
       }
       set powWCSTranslation 0
       set useWCSInfo($imgHandle) 0
       set useWCSInfo(${imgHandle}scope) 0
       set powWCS($imgHandle) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
       powCreateImage $imgHandle $imgHandle 0 0 $nCols $nRows $x_0 \
              $incrx $y_0 $incry $x_label $y_label $z_label
       set powWCSList(${imgHandle}scope) $powWCSList($imgHandle)
       powCreateGraph $imgHandle NULL $imgHandle $x_unit $y_unit \
              $x_label $y_label \
              [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1] 
    } else {
       set errorFlag [ catch {
           powCreateGraph $imgHandle NULL $imgHandle $x_unit $y_unit \
                     $x_label $y_label [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1] 
       } err ]

       if { $fvPref::ifWCSInfo } {
          if $errorFlag {
       set useWCSInfo($imgHandle) 0
       set useWCSInfo(${imgHandle}scope) 0
#             catch { powDeleteGraph $imgHandle NOPROMPT
#                     powDeleteImage $imgHandle $imgHandle
#                     powDeleteCurve $imgHandle curve }
             set powWCS($imgHandle) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
             set powFitsHeader($imgHandle) ""
             set powFitsHeaderCnt($imgHandle) 0
             powCreateImage $imgHandle $imgHandle 0 0 $nCols $nRows $x_0 \
                             $incrx $y_0 $incry $x_label $y_label $z_label

             set errorFlag [ catch {
                 powCreateGraph $imgHandle NULL $imgHandle $x_unit $y_unit \
                           $x_label $y_label [lindex $fvPref::graphDispSize 0] \
                           [lindex $fvPref::graphDispSize 1] 
             } err ]
          }
       }
       if $errorFlag {
          tk_messageBox -icon error -type ok -message "Can't create image"
          return

       }
    }

    set powWCSLabel(xlabel,$imgHandle,DEFAULT) $x_label
    set powWCSLabel(ylabel,$imgHandle,DEFAULT) $y_label
    set powWCSLabel(xunit,$imgHandle,DEFAULT) $x_unit
    set powWCSLabel(yunit,$imgHandle,DEFAULT) $y_unit

# allows double clicking on an image to pop up table display centered on pixel
    .pow.pow bind img_$imgHandle <<DblBtnPress>> \
          "+returnCurrentImageInfo [$fFile getMaster] $currentHDU $imgHandle %x %y"

}

itcl::body FitsImage::_sPlotCmd {} {
    global g_GL

    set tmpName [namespace tail $this]

    FitsImgPlotSel .pltSel_$tmpName $this 1 $_numRows
    wm title .pltSel_$tmpName "Image Plot Selection"
    tkwait window .pltSel_$tmpName

    if { [info exist g_GL($this,plotImg)] == 0} {
	return
    }

    if { [llength $g_GL($this,plotImg)] == 0} {
	return
    }

    imagePlot $g_GL($this,plotImg)
}

itcl::body FitsImage::_plotCmd { } {
    global g_GL ifWCSInfo_old

    set ifWCSInfo_old $fvPref::ifWCSInfo
    set fvPref::ifWCSInfo 0

    set tmpName [namespace tail $this]

    FitsImgPlotSel .pltSel_$tmpName $this $_numCols $_numRows
    wm title .pltSel_$tmpName "Image Plot Selection"
    tkwait window .pltSel_$tmpName

    if { [info exist g_GL($this,plotImg)] == 0} {
	return
    }

    if { [llength $g_GL($this,plotImg)] == 0} {
	return
    }

    imagePlot $g_GL($this,plotImg)
}

itcl::body FitsImage::_getImageVectorData { fRow_ nRows_ fCol_ nCols_ } {
    incr fCol_

   # col/row zero-indexed
   set fitsfile [$fFile cget -fitsfile]
   $fitsfile load iblock "_tableDataDS9" $fRow_ $nRows_ $fCol_ $nCols_
}

itcl::body FitsImage::imagePlot {paramList_} {
    global g_backupDir
    global powWCS powPlotParam
    global powFitsHeader powFitsHeaderCnt
    global ifWCSInfo_old
    global xCount yCount
    global powWCSList powWCSLabel powWCSName
    global useWCSInfo

#puts "paramList_: $paramList_"
    set imgPlotType   [lindex $paramList_ 0]
    set imgPlotStart  [lindex $paramList_ 1]
    set imgPlotEnd    [lindex $paramList_ 2]
    set imgCurrgn     [lindex $paramList_ 3]

#puts "imgPlotStart: $imgPlotStart"
#puts "imgPlotEnd: $imgPlotEnd"
#puts "_numCols: $_numCols"
#puts "_numRows: $_numRows"

    if { $fvPref::imgDisplayer == "DS9" } {
       # display is for DS9

       set flag "NOT_YET"

       set tmpfile ${g_backupDir}/ds9PlotTemp.data

       if [file exists $tmpfile] {
          file delete -force $tmpfile
       }

       if { [catch {set newDataFile [open $tmpfile "w"]} err] } {
          error $err
          return
       }

       if { $imgPlotType == "row" } {
          _getImageVectorData $imgPlotStart [expr $imgPlotEnd - $imgPlotStart + 1] 0 $_numCols

          set xColumn_ "Column_Number"
          set yColumn_ "Value"

          for {set col 0} {$col < [expr $_numCols - 1]} {incr col} {
              set totalValue 0
              for {set row [expr $imgPlotStart - 1]} {$row < $imgPlotEnd} {incr row} {
                  set data [_getRawDataForDS9 $col $row]
                  set currentVal $data
                  if { $data == "NULL" } {
                     set currentVal 0
                  }
                  set totalValue [expr $totalValue + $currentVal]
              }
              puts $newDataFile [format "%s %s" $col [expr $totalValue / ($imgPlotEnd - $imgPlotStart + 1)]]
          }

       } else {
          _getImageVectorData 1 $_numRows [expr $imgPlotStart - 1] [expr $imgPlotEnd - $imgPlotStart + 1]

          set xColumn_ "Row_Number"
          set yColumn_ "Value"

          for {set row 0} {$row < [expr $_numRows - 1]} {incr row} {
              set totalValue 0
              for {set col [expr $imgPlotStart - 1]} {$col < $imgPlotEnd} {incr col} {
                  set data [_getRawDataForDS9 $col $row]
                  set currentVal $data
                  if { $data == "NULL" } {
                     set currentVal 0
                  }
                  set totalValue [expr $totalValue + $currentVal]
              }
              puts $newDataFile [format "%s %s" $row [expr $totalValue / ($imgPlotEnd - $imgPlotStart + 1)]]
          }
       }

       close $newDataFile

       set err "none"

       catch {exec xpaaccess ds9} result

       if { [string range $result 0 1] == "no" } {
          # start DS9 if DS9 isn't there 
          if { [catch {exec ds9 &} ds9pid] } {
             file delete $tmpfile
             if { [tk_dialog .saoError "DS9 startup error.. Cannot start DS9!\nUse POW instead?" \
                           question 0 Yes No] == 0 } {
                close $fFile
                set flag INCOMPLETE_DONE
             }
             close $fFile
             set flag DONE
          }


          if { $flag == "NOT_YET" } {
             # wait till ds9 is up
             set stopflag  1
             set nSecs 0
             while { $stopflag } {
                   after 1000
                   incr  nSecs
                   catch {exec xpaaccess ds9} result
                   if { [string range $result 0 2] == "yes" } {
                      set flag DONE
                      set stopflag 0
                   } else {
                      if { $nSecs > 10 } {
                         file delete $tmpfile
                         if { [tk_dialog .saoError "DS9 startup error.. Cannot start ds9!\nUse POW instead?" \
                                  question 0 Yes No] == 0 } {
                            close $fFile
                            set flag NOT_YET
                            break
                         } else {
                            close $fFile
                            set flag INCOMPLETE_DONE
                            break
                         }
                      }
                   }
             }
          }

       } else {
          set flag DONE
       }

       if { $flag == "DONE" } { 
          if { $imgPlotStart == $imgPlotEnd } {
             set displayName [format "%s(%s_%s)" [file tail $fileName] $imgPlotType $imgPlotStart]
          } else {
             set displayName [format "%s(%s_%s-%s)" [file tail $fileName] $imgPlotType $imgPlotStart $imgPlotEnd]
          }
#Ziqin Pan, April 15, 2004
#Use new version ds9 plot command instead of analysis plot
#
#          exec xpaset ds9 analysis plot $displayName $xColumn_ $yColumn_ 2 < $tmpfile
	  if {$imgCurrgn == 1} {
       	        catch {[exec xpaset -p ds9 plot close]}  err
          	set imgCurrgn 0
          }



          if {$imgCurrgn == 0} {
          exec xpaset ds9 plot new $displayName $xColumn_ $yColumn_ xy < $tmpfile
          }

          file delete $tmpfile
       }

       if { $flag != "NOT_YET" } return
    }

    set graphID [namespace tail $this] 
    set graphID ${graphID}_$_graphIDhighest

    if { $imgPlotType == "row" } {
	powCreateVectorEN x_$graphID ColumnNumber $_numCols 1 1 NULL
	if { [llength $_dims] == 1 } {
	    set dataInfoForPOW [$fFile loadImageMeanCols $imgPlotStart \
		    $imgPlotEnd $_slice]
	} else {
	    set dataInfoForPOW [$fFile loadImageMeanRows $imgPlotStart \
		    $imgPlotEnd $_slice]
       }
    } else { 
	powCreateVectorEN x_$graphID RowNumber $_numRows 1 1 NULL
	if { [llength $_dims] == 1 } {
	   set dataInfoForPOW [$fFile loadImageMeanRows $imgPlotStart \
		$imgPlotEnd $_slice]
	} else {
	   set dataInfoForPOW [$fFile loadImageMeanCols $imgPlotStart \
		$imgPlotEnd $_slice]
	}
    }

    set dataAddressForPOW [lindex $dataInfoForPOW 0]
    set dataType          [lindex $dataInfoForPOW 1]
    set numElements       [lindex $dataInfoForPOW 2]
    powCreateData y_d_$graphID $dataAddressForPOW $dataType $numElements 1
    $fFile freeColumn $dataAddressForPOW
    powCreateVector y_$graphID y_d_$graphID 0 $numElements 1	

    set xUnit NULL
    set yUnit NULL
    set x_label ""
    set x_unit "pixels"
    regsub -all { } [urlTail $fileName] _ cleanFileName
    set graphHandle ${cleanFileName}_[expr $currentHDU-1]_$_graphIDhighest
    set useWCSInfo($graphHandle) $fvPref::ifWCSInfo
    set useWCSInfo(${graphHandle}scope) $fvPref::ifWCSInfo


    set powWCSLabel(xlabel,$graphHandle,DEFAULT) ""
    set powWCSLabel(ylabel,$graphHandle,DEFAULT) ""
    set powWCSLabel(xunit,$graphHandle,DEFAULT) "pixels"
    set powWCSLabel(yunit,$graphHandle,DEFAULT) ""

    if { ![catch {set tmp [$fFile getKeyword CTYPE1]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set x_label [string trim $v {' }]
    }
    if { ![catch {set tmp [$fFile getKeyword CUNIT1]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set x_unit [string trim $v {' }]
    }

    if { $imgPlotType == "row" } {
	set xLabel "Column_Number"
    } else {
	set xLabel "Row_Number"
    }

    if { $fvPref::ifWCSInfo } {
       set xLabel $x_label
       set xUnit $x_unit
    }

    if { ![catch {set tmp [$fFile getKeyword BUNIT]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set yLabel [string trim $v {' }]
    } else {
       set yLabel "Counts"
    }

    set powWCSLabel(xlabel,$graphHandle,DEFAULT) $xLabel
    set powWCSLabel(ylabel,$graphHandle,DEFAULT) $yLabel
    set powWCSLabel(xunit,$graphHandle,DEFAULT) $xUnit
    set powWCSLabel(yunit,$graphHandle,DEFAULT) $yUnit

    if { [winfo exist .pow.pow]!=1 } { 
	powInit .dummy
    }

    set powWCSName($graphHandle) 0
    set powWCSName(${graphHandle}scope) 0

    set realGraphHandle $graphHandle
    if { $fvPref::ifWCSInfo } {
       set powWCS($graphHandle) [$fFile getWcs]
       if { $_numCols > 1 } {
          set result [$fFile getHeader2String]
          set powFitsHeader($graphHandle) [lindex $result 0]
          set cntList($graphHandle) [$fFile getHeaderKeyWord [lindex $result 0] $graphHandle]
          set powFitsHeaderCnt($graphHandle) [lindex $cntList($graphHandle) 1]
#puts "powFitsHeader($graphHandle): $powFitsHeader($graphHandle)"
#puts "cntList($graphHandle): $cntList($graphHandle)"
#puts "powFitsHeaderCnt($graphHandle): $powFitsHeaderCnt($graphHandle)"
          if { [lindex $cntList($graphHandle) 0] > 0 } {
             #set powFitsHeader($graphHandle) [$fFile assembleWcsHeader $graphHandle]
             #set powWCS($graphHandle) [$fFile getWcs]
             set powWCSInfo($graphHandle,DEFAULT) $powWCS($graphHandle)
             set wcsinfo $powWCS($graphHandle)

             set curveName "c0_$graphID"
             set powWCS($curveName) $wcsinfo
             set xLabel [lindex [lindex $wcsinfo 3] 0]
             set yLabel [lindex [lindex $wcsinfo 3] 1]
             if { $yLabel == "" } { set yLabel "Data" }
             set xUnit ""
             set yUnit "intensity"
             set curveName $curveName
          } else {
             set powFitsHeaderCnt($graphHandle) 0
             #set powWCS($graphHandle) [$fFile getWcs]
             set powWCS($curveName) $powWCS($graphHandle)
             set powFitsHeader($curveName) $powFitsHeader($graphHandle)
             set powFitsHeaderCnt($curveName) $powFitsHeaderCnt($graphHandle)
          }
       } else {
          #set powWCS($graphHandle) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
          #set powWCS($graphHandle) $wcsinfo
          set useWCSInfo($graphHandle) 0
          set useWCSInfo(${graphHandle}scope) 0
          set powFitsHeader($graphHandle) ""
          set powFitsHeaderCnt($graphHandle) 0
          set curveName $graphHandle
          set powWCS($curveName) $powWCS($graphHandle)
          set powFitsHeader($curveName) $powFitsHeader($graphHandle)
          set powFitsHeaderCnt($curveName) $powFitsHeaderCnt($graphHandle)
       }
    } else { 
        set powWCS($graphHandle) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
        set powFitsHeader($graphHandle) ""
        set powFitsHeaderCnt($graphHandle) 0
        set curveName $graphHandle
        set powWCS($curveName) $powWCS($graphHandle)
        set powFitsHeader($curveName) $powFitsHeader($graphHandle)
        set powFitsHeaderCnt($curveName) $powFitsHeaderCnt($graphHandle)
    }

    eval powCreateCurve $curveName x_$graphID "NULL" y_$graphID "NULL"
    set curves [list $curveName]

#puts "powFitsHeader($graphHandle): $powFitsHeader($graphHandle)"
    set powPlotParam(graphType,$graphHandle) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${graphHandle}scope) $powPlotParam(graphType,$graphHandle)
    set powPlotParam(zoomed,$graphHandle) 0
    set powPlotParam(zoomed,${graphHandle}scope) 0
    set xCount($graphHandle) 0
    set yCount($graphHandle) 0
    set xCount(${graphHandle}scope) 0
    set yCount(${graphHandle}scope) 0

    if [info exists powWCSList($graphHandle)] {
       foreach name [lindex $powWCSList($graphHandle) 1] {
          $fFile assembleWcsLabel $graphHandle $name
       }
    } else {
       set powWCSList($graphHandle) {}
       lappend powWCSList($graphHandle) 1
       lappend powWCSList($graphHandle) {}
    }
    set powWCSList(${graphHandle}scope) $powWCSList($graphHandle)
    if { $imgCurrgn && [powGetCurrentGraph]!="" } {
       set realGraphHandle [powGetCurrentGraph]
       powAddCurves [powGetCurrentGraph] $curveName
    } else {
       powCreateGraph $graphHandle $curves NULL $xUnit $yUnit \
	     $xLabel $yLabel  \
	     [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1]
    }


    if [info exists ifWCSInfo_old] {
       set fvPref::ifWCSInfo $ifWCSInfo_old
       set useWCSInfo $fvPref::ifWCSInfo
    }
    incr _graphIDhighest
    catch { powSetCurveOptions $realGraphHandle $curveName pDisp No lDisp Yes } err
}

# Highlight the center

itcl::body FitsImage::showCell {col_ row_} {
   if { $_droot == "" || ![winfo exists $_droot] } return
   if { [llength $_dims]==1 } {
      # Swap col_/row_
      set tmp $col_; set col_ $row_; set row_ $tmp
   }
   _jump [expr $row_ + $_showRows/2]
   _setHScroll [expr $col_-$_showCols/2-1]
   update idletask
   _setStartMark [expr $col_-$_firstCol] [expr $_numRows-$_firstRow+1-$row_]
}


##############################################
#
# Handle Reading/Writing/Formatting of Data
#

itcl::body FitsImage::_writeTableData {col_ row_ val_} {

   set tmpStr [string toupper [string trim $val_ " "]]

   if { $tmpStr == "NULL" } {

      # float and double do not need a BLANK key
      if { $_imgNull == "NULL" && $_imgType!=3 && $_imgType!=4 } {
         error "\nNo NULL value is defined. Please write a\
               BLANK keyword in the header first."
      }
      set val_ "NULL"

   }

   _putRawData $col_ $row_ $val_
}

itcl::body FitsImage::_readTableData {fCol_ fRow_ nCols_ nRows_} {
    set fRow_ [expr $_numRows - $fRow_ - $nRows_ + 1]
    incr fCol_
    
    # image data will be loaded into a 2-d Tcl array _tableData
    # this cannot be done as a FitsFile method because Tcl does not
    #  allow arrays to be returned
    #  the array must be created locally
    set fitsfile [$fFile cget -fitsfile]
    $fitsfile load iblock "_tableData" $fRow_ $nRows_ $fCol_ $nCols_
}

itcl::body FitsImage::_getFormattedData {col_ row_} {
    set val_ [_getRawData $col_ $row_]
    if { $val_=="NULL" } {
	return $val_
    }
    set errorFlag [ catch { 
        set returnStr [format $_imgForm $val_] 
    } err ]

    if { $errorFlag } {
       # can't recongize the format, return as it is..
       set returnStr $val_
    }

    return $returnStr
}

itcl::body FitsImage::_getRawDataForDS9 {col_ row_} {
   set v $_tableDataDS9($col_,$row_)
   if { $v!="NULL" } {
      if { $_imgType==3 } {
         return [format "%.7G" $v]
      } elseif { $_imgType==4 } {
         return [format "%.15G" $v]
      }
   }
   return $v
}

itcl::body FitsImage::_getRawData {col_ row_} {
   set v $_tableData($col_,[expr $_numRows-$row_-1])
   if { $v!="NULL" } {
      if { $_imgType==3 } {
         return [format "%.7G" $v]
      } elseif { $_imgType==4 } {
         return [format "%.15G" $v]
      } elseif { $_imgType == 5 } {
         # chai: 64 bits integer
         return [lindex [split $v "."] 0]
      }
   }
   return $v
}

itcl::body FitsImage::_putRawData {col_ row_ val_} {
   set realRow [expr $_numRows-$row_-1]
   set firstElem [expr $realRow*$_numCols+$col_+1]
   $fFile putImage $firstElem 1 [list $val_]
   _readTableData $col_ $row_ 1 1
}

itcl::body FitsImage::_getRawDataBlock { fCol_ fRow_ lCol_ lRow_ } {
   # col/row zero-indexed

   set nElem [expr $lCol_ - $fCol_ + 1]
   for { set col 0 } { $col<$nElem } { incr col } {
      set _colData($col) {}
   }

   set row $fRow_
   for { set row $fRow_ } { $row <= $lRow_ } { incr row } {
      set realRow   [expr $_numRows-$row-1]
      set firstElem [expr $realRow*$_numCols+$fCol_+1]
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

itcl::body FitsImage::_putRawDataBlock { fCol_ fRow_ data_ } {
   # col/row zero-indexed

   set nCols [llength $data_]
   set nRows [llength [lindex $data_ 0]]

   for { set i 0 } { $i<$nRows } { incr i } {
      set rowData {}
      foreach cData $data_ {
         lappend rowData [lindex $cData $i]
      }
      set realRow   [expr $_numRows-$fRow_-1]
      set firstElem [expr $realRow*$_numCols+$fCol_+1]
      $fFile putImage $firstElem $nCols $rowData
      incr fRow_
   }
}


#
#  End Data handlers
#
##############################################
itcl::body FitsImage::_saveTableToAscii {win_ asciiFileName_} {

# these vars are used
#    _exportFirstRow
#    _exportLastRow
#    _exportFirstCol
#    _exportLastCol
#    _exportCSV
#    _exportPrintRowNumbers
#    _exportCharBetweenCols

   for {set i  1} { $i <= $_numRows} {incr i} {
       set _selectedRowstmp($i) $_selectedRows([expr $_numRows -$i+1])
   }


    set _exportFormat [$win_.rb get]
    if { $_exportFormat == "csv" } {
        set _exportCSV 1
        set _exportFixedFormat 0
    } elseif { $_exportFormat == "userdefine" } {
        set _exportCSV 0
        set _exportFixedFormat 0
    }

    # Note: all rows and cols are 1-based
    # this matches the $fFile print commands below use 1-based, so no
    # change is needed

    ################################################################
    # set up the feedback bar

    set totalRowsPrint [expr $_exportLastRow - $_exportFirstRow + 1]

    # NOTE -- this is changed to 1000000000 rows

    # we will print 1000 rows at a time (or whatever is left)
    # numPrintBlocks = total blocks of 1000 or iterations in feedbar bar
    # if we have 2835 rows, we'll print:
    #     1 to 1000,  (first block)
    #  1001 to 2000,  (second block)
    #  2001 to 2835.  (third block)
    #
    # tables are meant to be printed in standard row order

    set numPrintBlocks [expr $totalRowsPrint/1000000000]
    if { ($totalRowsPrint - $numPrintBlocks*1000000000) > 0 } {
        incr numPrintBlocks
    }

    $win_.f.fdb configure -steps $numPrintBlocks


 set firstloop 1



 if { $_exportsel == 1 } {
    set _exportFirstRow 1
    set _exportLastRow  $_numRows
    set totalRowsPrint [expr $_exportLastRow - $_exportFirstRow + 1]
 }

 if { $_exportselc == 1 } {
    set _exportFirstCol 1
    set _exportLastCol  $_dispCols
 }

 if { $_exportsel == 3 } {

 for {set r $_numRows} { $r >=1 } { incr r -1} {


  if {$_selectedRowstmp($r) ==1 } {
    set  _exportFirstRow  $r
    while { $_selectedRowstmp($r) == 1} {
       incr r -1
       if { $r < 1 } {
           break
       }
    }
    set totalRowsPrint [expr  $_exportFirstRow - $r]
    set _exportFirstRow [expr $r + 1]



    for {set n 1} {$n <= $numPrintBlocks} {incr n} {
        if { $n == 1 && $firstloop == 1} {
            # write into a new file and write column names
            set filePrintMode 0
        } else {
            # append to that file and don't write column names
            set filePrintMode 2
        }

        set fRow [expr 1000000000*[expr $n-1] + 1]
        if { $n == $numPrintBlocks } {
            set nRows [expr $totalRowsPrint - $fRow + 1]
        } else {
            set nRows 1000000000
        }

        # shift by the first user-selected row
        set fRow [expr $fRow + $_exportFirstRow - 1]

        set fCol   $_exportFirstCol
        set nCols  [expr $_exportLastCol - $_exportFirstCol + 1]
        $fFile saveImgToASCII $asciiFileName_ $filePrintMode \
                $fRow $nRows $fCol $nCols \
                $_cellSize \
                $_exportCSV \
                $_exportPrintRowNumbers \
                $_exportCharBetweenCols \
                $_slice

        # if user presses cancel, stop
        if {[catch {$win_.f.fdb step}] == 1} {
            file delete $asciiFileName_
            return
        }
    }
    set firstloop 0
   }
  }
 } else {

    for {set n 1} {$n <= $numPrintBlocks} {incr n} {
        if { $n == 1 } {
            # write into a new file and write column names
            set filePrintMode 0
        } else {
            # append to that file and don't write column names
            set filePrintMode 2
        }

        set fRow [expr 1000000000*[expr $n-1] + 1]
        if { $n == $numPrintBlocks } {
            set nRows [expr $totalRowsPrint - $fRow + 1]
        } else {
            set nRows 1000000000
        }

        # shift by the first user-selected row
        set fRow [expr $fRow + $_exportFirstRow - 1]

        set fCol   $_exportFirstCol
        set nCols  [expr $_exportLastCol - $_exportFirstCol + 1]

        

        $fFile saveImgToASCII $asciiFileName_ $filePrintMode \
                $fRow $nRows $fCol $nCols \
                $_cellSize \
                $_exportCSV \
                $_exportPrintRowNumbers \
                $_exportCharBetweenCols \
                $_slice


        # if user presses cancel, stop
        if {[catch {$win_.f.fdb step}] == 1} {
            file delete $asciiFileName_
            return
        }
     }
 }
    destroy $win_
}

