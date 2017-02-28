# First draft 05/01/96    Jianjun

#construct a VectorTable object
# Vector VectorTableName FitsFileObjName currentHDU 

itcl::class VectorTable {
    inherit FitsImage

    constructor {args} {
	set _tableType "Vector Table"
	set _fatherFitsTable   [lindex $args 1]
	set fFile                [$_fatherFitsTable cget -fFile]
	set _fatherFitsExtension [$_fatherFitsTable getFatherFitsExtension]
	FitsImage::constructor $fFile $_fatherFitsExtension
    } {}
    destructor         {}

#  all these override/specialize the parent (FitsImage) methods

    public method imagePlot {paramList_}
    public method showCell {col_ row_}
    public method setFileName { fName_ }

    private method _readInTable {} 
    private method _drawTable {}
    private method _buildMenus {}
    private method _postMenus {}
    private method _realCloseCmd {}
    private method _powMakeImage {}

    ### data formatting
    private method _readTableData {fCol_ fRow_ nCols nRows_}
    private method _writeTableData {col_ row_ val_}
    private method _getFormattedData {col_ row_}
    private method _getRawData {col_ row_}
    private method _putRawData {col_ row_ val_}
    private method _getRawDataBlock { fCol_ fRow_ lCol_ lRow_ }
    private method _putRawDataBlock { fCol_ fRow_ data_ }

    private method _saveTableToAscii {win_ asciiFileName_}

# all these are distinct to this class (not present in parent Table class)
    private method _makeVectorImage   {}
    private method _plotAVectorRowWindow {}
    private method _plotAVectorRow {}

    # used in _plotAVectorRowWindow
    private variable _plotRowNum

    # _cellSize is gotten from _fatherFitsTable
    # and all _cellWidth($i) is set to _cellSize
    private variable _isVariableVec 0 
    # a vector table is based off a vector column in a father Table
    private variable _fatherFitsTable
    
    # all based on the _columnNull(i), _columnType(i)...of father table column
    private variable _colName
    private variable _colDim
    private variable _colNull
    private variable _colType
    private variable _colForm
    private variable _baseColNum
}

itcl::body VectorTable::constructor {args} {
    set colNum   [lindex $args 0]

# get some params from _fatherFitsTable
    set listFatherParams [$_fatherFitsTable passParamsToVectorTable]
    set _numRows       [lindex $listFatherParams 0]
    set isFailedToCopy [lindex $listFatherParams 1]
    set fileName       [lindex $listFatherParams 2]
    set currentHDU     [lindex $listFatherParams 3]

    set _colName  [$_fatherFitsTable getColumnInfo _columnName $colNum]
    set _colNull  [$_fatherFitsTable getColumnInfo _columnNull $colNum]
    set _colForm  [$_fatherFitsTable getColumnInfo _columnForm $colNum]
    set _colType  [$_fatherFitsTable getColumnInfo _columnType $colNum]
    set _colDim   [$_fatherFitsTable getColumnInfo _columnDim  $colNum]
    set _cellSize [$_fatherFitsTable getColumnInfo _cellWidth  $colNum]
    set _numCols  [$_fatherFitsTable getColumnInfo _columnVecSize $colNum]

#puts "_colName : $_colName"
#puts "_colNull : $_colNull"
#puts "_colForm : $_colForm"
#puts "_colType : $_colType"
#puts "_colDim : $_colDim "
#puts "_colSize : $_cellSize"
#puts "_colCols : $_numCols"

    # pick the variable length array.
    if { $_numCols < 0 } {
       set _numCols [expr -$_numCols]
       set _isVariableVec 1
    }
    set _dispCols $_numCols

    set _justStarted 1

    $_fatherFitsTable addChild $this

# In FitsImage.tcl constructor, _dims is gotten from the fFiles, *from which*
#  we get _numCols and _numRows
# Here we set _dims directly
    set _dims [list $_numCols $_numRows]
}

itcl::body VectorTable::destructor {} {

    .fvwinkeeper signoff $_droot

    set _isBeingDestroyed 1
    destroy $_droot

    $_fatherFitsTable freeChild $this
}


itcl::body VectorTable::setFileName { fName_ } {

   Table::setFileName $fName_
   .fvwinkeeper signoff  $_droot
   .fvwinkeeper register $_droot "Vector Table" [urlTail $fName_] $currentHDU \
         $this
}


itcl::body VectorTable::_drawTable {} {

   Table::_drawTable

   bind $_droot <<Plot>>        [itcl::code $this _plotAVectorRowWindow]
   bind $_droot <<MakeVImg>>    [itcl::code $this _makeVectorImage]
}

itcl::body VectorTable::_buildMenus {} {
    global isMac
    global g_titleFont
    
    if { $isMac } {
        set _mBar .mbar.vtable
    } else {
        set _mBar $_droot.mbar
    }
    $_droot config -menu $_mBar
    
    if { ![winfo exists $_mBar] } {

       _buildNewMenus

       $_mBar.tools add command -label "Plot..." \
             -command "doMenuEvent <<Plot>>" -font g_titleFont
       $_mBar.tools add command -label "Make Image" \
             -command "doMenuEvent <<MakeVImg>>" -font g_titleFont

    }
}


itcl::body VectorTable::_postMenus {} {

   Table::_postMenus

   if { $_isVariableVec } {
      $_mBar.tools entryconfig "Make Image" -state disabled
   } else {
      $_mBar.tools entryconfig "Make Image" -state normal
   }
   update idle
}   

itcl::body VectorTable::_readInTable {} {
    global g_charPix

    set _DC(height)   20
    set _DC(width)     [expr (int(log10($_numRows))+4)*$g_charPix]
#    set _DC(headroom) 20
    set _DC(headroom) 80
    set _DC(footroom) 40
    set _DC(vscrollsize) 15
    set _DC(hscrollsize) 15
    set _DC(rightspace) 6
    set _DC(interline)    0
    set _DC(tmar)         6
    set _DC(lmar)         8
    set _DC(tabspace)     0

# use fits command setarray to initialize the rowState
# usage   setrowstate totalNumOfRos startRow endRow status 
# (0:normal, 1:selected, 2: deleted)
    setarray rowState 0 [expr $_numRows-1] 0
    setarray _colNotchedState 0 [expr $_dispCols-1] 0

    set _absXPos(0) [expr $_DC(lmar) + $_DC(width) + $_DC(rightspace)]
    set _listPreSelectedColNames {}
    for {set i 0} {$i < $_numCols} {incr i} {
        set _valueTDIM($i) 0
	set _colNotchedState($i) 0
	set _columnName($i) [expr $i+1]
	set _columnType($i) " "
	set _columnUnit($i) " "
	lappend _listPreSelectedColNames [expr $i+1]
	set _cellWidth($i) $_cellSize 
	set _cellPixWidth($i) [expr $g_charPix*(1+$_cellWidth($i))]
	set _absXPos([expr $i+1]) [expr $_absXPos($i) + $_cellPixWidth($i) \
                                                    + $_DC(rightspace) ]
    }
}


itcl::body VectorTable::_makeVectorImage {} {
    global g_hasSAOtng g_hasDS9

    if { $fvPref::imgDisplayer == "SAOtng" && $g_hasSAOtng } {
       tk_messageBox -type ok -message "Vector columns cannot be\
             displayed with SAOtng. Please use POW instead."
    } elseif { $fvPref::imgDisplayer == "DS9" && $g_hasDS9 } {
       tk_messageBox -type ok -message "Vector columns cannot be\
             displayed with DS9. Please use POW instead."
    } else {
	_powMakeImage
    }    
}

itcl::body VectorTable::_powMakeImage {} {
   global powPlotParam
   global xCount yCount

   # get the pow widget 
    if { [winfo exist .pow.pow]!=1 } { 
	powInit .dummy
    }
    # load the entire vector table
    set tmpStr [$fFile loadVectorTableToDataAddressForPOW $_colName]
    set dataAddressForPOW [lindex $tmpStr 0]
    set dataType          [lindex $tmpStr 1]
    set dataSize          [lindex $tmpStr 2]

    regsub -all { } [urlTail $fileName] _ cleanFileName
    set imgHandle ${cleanFileName}_c$_colName

# the last param is for copying  data
    powCreateData $imgHandle $dataAddressForPOW $dataType $dataSize 1
# free the data array
    $fFile freeVTable $dataAddressForPOW   

    set powPlotParam(graphType,$imgHandle) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${imgHandle}scope) $powPlotParam(graphType,$imgHandle)
    set powPlotParam(zoomed,$imgHandle) 0
    set powPlotParam(zoomed,${imgHandle}scope) 0

    set xCount($imgHandle) 0
    set yCount($imgHandle) 0
    set xCount(${imgHandle}scope) 0
    set yCount(${imgHandle}scope) 0

    powCreateImage $imgHandle $imgHandle 0 0 $_numCols $_numRows 0 1 0 1 \
	    pixels pixels counts

    powCreateGraph $imgHandle NULL $imgHandle pixels pixels \
	    detector detector \
	    [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1]

# register for the linkage to table display
    .pow.pow bind img_$imgHandle <<DblBtnPress>> \
          "+returnCurrentImageInfo_ForVectorTables $this $imgHandle %x %y"
}

itcl::body VectorTable::_realCloseCmd {} {
    $_fatherFitsTable refresh 0
    itcl::delete object $this
}

##############################################
#
# Handle Reading/Writing/Formatting of Data
#

itcl::body VectorTable::_writeTableData {col_ row_ val_} {

   if { [regexp L $_colType] == 1} {
      if { ([regexp -nocase {[ftu]} $val_] == 0) || \
            ([string length $val_] !=1)} {
         error "Logical column can only have value T, F or U"
         return
      } else {
         set val_ [string toupper $val_]
      }
   }	
   set tmpStr [string toupper [string trim $val_ " "]]

   if { $tmpStr == "NULL" } {

      # float and double do not need a TNULL key for binary table
      if { $_colNull == "NULL" && \
            ![regexp A|D|E|F|C|M|d|e|f $_colType] } {
         error "\nNo NULL value is defined. Please write a\
               TNULLn keyword in the header first."
      }		
      set val_ "NULL"
         
   }

   _putRawData $col_ $row_ $val_
}

itcl::body VectorTable::_readTableData {fCol_ fRow_ nCols_ nRows_} {
   # col/row is 0-indexed

   if { $_isVariableVec } {
      # Need to read/write entire row in Variable-length columns
      set fCol_ 1
      set lCol_ $_numCols
   } else {
      incr fCol_
      set lCol_ [expr $fCol_+$nCols_-1]
   }
   incr fRow_
   for { set col $fCol_ } { $col <= $lCol_ } { incr col } {
       # table block data will be loaded into a 2-d Tcl array _tableData
       # this cannot be done as a FitsFile method because Tcl does not
       #  allow arrays to be returned
       #  the array must be created locally

       set fitsfile [$fFile cget -fitsfile]
       $fitsfile load tblock -noformat "_tableData" [list $_colName] $fRow_ \
	       $nRows_ $col $col
   }
}

itcl::body VectorTable::_getFormattedData {col_ row_} {
   set val [_getRawData $col_ $row_]
   if { $val=="NULL" || $val==" " } {
      return $val
   }

   if { [regexp C|M $_colType] } {
      foreach [list v1 v2] $val {} 
      return [format "$_colForm, $_colForm" $v1 $v2]
   } else {
      regsub -all {%[0-9]*[iuld]*} $_colForm "" result
      if { $result == "" } {
         # $_colForm == "%i" || $_colForm == "%d" ||
         # $_colForm == "%u" || $_colForm == "%ld"
         # $_colForm == "%xxi" || $_colForm == "%xxd" ||
         # $_colForm == "%xxu" || $_colForm == "%xxld"
         set token [split $val "."]
         set val [lindex $token 0]
      }
      return [format $_colForm $val]
   }
}

itcl::body VectorTable::_getRawData {col_ row_} {
   set v $_tableData($col_,$row_)
   if { $v!="NULL" && $v!=" " } {
      if { [regexp E $_colType] } {
         return [format "%.7G" $v]
      } elseif { [regexp D $_colType] } {
         return [format "%.15G" $v]
      }
   }
   return $v
}

itcl::body VectorTable::_putRawData {col_ row_ val_} {
   if { $_isVariableVec } {
      # variable length vectors want to write the entire vector at
      # once so, build up list of values

      set tmpVecList {}
      set _tableData($col_,$row_) $val_
      for { set i 0 } { $i < $_numCols } { incr i } {	
         if {$_tableData($i,$row_) == " "} break
         lappend tmpVecList $_tableData($i,$row_)
      }
      $fFile putTable $_colName 1 [expr $row_+1] $tmpVecList
   } else {
      $fFile putTable $_colName [expr $col_+1] [expr $row_+1] [list $val_]
   }
   _readTableData $col_ $row_ 1 1
}

itcl::body VectorTable::_getRawDataBlock { fCol_ fRow_ lCol_ lRow_ } {
   # col/row zero-indexed

#puts "_colName: $_colName"
   incr fRow_
   incr lRow_
   set range "${fRow_}-${lRow_}"
   set data {}
   for { set col $fCol_ } { $col <= $lCol_ } {} {
      incr col
      lappend data [$fFile getVectorTableAsRawList $_colName $col $range]
   }

   return $data
}

itcl::body VectorTable::_putRawDataBlock { fCol_ fRow_ data_ } {

   if { $_isVariableVec } {
      set col 1
   } else {
      set col [expr $fCol_+1]
   }

   set nRows [llength [lindex $data_ 0]]
   set nCols [llength $data_]

   for { set i 0 } { $i<$nRows } { incr i } {
      set rowData {}

      if { $_isVariableVec } {
         # Must insert start of row
         for { set c 0 } { $c<$fCol_ } { incr c } {
            lappend rowData [_getRawData $c $fRow_]
         }
      }

      foreach cData $data_ {
         lappend rowData [lindex $cData $i]
      }

      if { $_isVariableVec } {
         # Must append end of row
         for { set c [expr $fCol_+$nCols] } { $c<$_numCols } {incr c} {
            set v [_getRawData $c $fRow_]
            if { $v==" " } break
            lappend rowData $v
         }
      }

      incr fRow_
      $fFile putTable $_colName $col $fRow_ $rowData
   }
}


#
#  End Data handlers
#
##############################################

itcl::body VectorTable::imagePlot {paramList_} {

    set imgPlotType   [lindex $paramList_ 0]
    set imgPlotStart  [lindex $paramList_ 1]
    set imgPlotEnd    [lindex $paramList_ 2]
    set imgCurrgn     [lindex $paramList_ 3]

    if { $imgPlotType == "row" } {
        set _plotXstart 0
        set _plotXtotal $_numCols
        set _plotYstart [expr $imgPlotStart - 1]
        set _plotYtotal [expr $imgPlotEnd - $imgPlotStart]
    } else { 
        set _plotYstart 0
        set _plotYtotal $_numRows
        set _plotXstart [expr $imgPlotStart - 1]
        set _plotXtotal [expr $imgPlotEnd - $imgPlotStart]
    }
    _powMakeImage
}

# double click on image pixel displays the cell of the pixel
itcl::body VectorTable::showCell {col_ row_} {
   if { $_droot == "" || ![winfo exists $_droot] } return
   if { [llength $_dims]==1 } {
      # Swap col_/row_
      set tmp $col_; set col_ $row_; set row_ $tmp
   }
   _jump [expr $row_ - $_showRows/2]
   _setHScroll [expr $col_-$_showCols/2-1]
   update idletask
   _setStartMark [expr $col_ - $_firstCol] [expr $row_ - $_firstRow]
}

itcl::body VectorTable::_plotAVectorRowWindow {} {
    if { [winfo exist .vector_plotrow ] } {
	return
    }
    powToplevel .vector_plotrow .dummy
    wm title    .vector_plotrow "fv: Select Row To Plot"

    set _plotRowNum ""

    iwidgets::entryfield .vector_plotrow.entry \
	    -labeltext "Choose a row to plot (between 1 and $_numRows):" \
	    -textvariable [itcl::scope _plotRowNum]

    iwidgets::buttonbox .vector_plotrow.bbox
    .vector_plotrow.bbox add plot   -text "Plot" \
	    -command [itcl::code $this _plotAVectorRow]
    .vector_plotrow.bbox add cancel -text "Cancel" \
	    -command "destroy .vector_plotrow"

    grid configure .vector_plotrow.entry -row 0 -column 0 -sticky ew
    grid configure .vector_plotrow.bbox  -row 1 -column 0
}

itcl::body VectorTable::_plotAVectorRow {} {
    global g_backupDir powPlotParam
    global xCount yCount
    global powWCS powFitsHeader powFitsHeaderCnt
    global powWCSName powWCSTranslation powWCSLabel powWCSList

    if { $_plotRowNum == "" || $_plotRowNum < 1 || $_plotRowNum > $_numRows } {
	error "Row must be between 1 and $_numRows"
	return
    }

#puts "plotRowNum: $_plotRowNum"
    # zero-indexed

    set tokens [split $_plotRowNum "-"]
    set firstRow [expr [lindex $tokens 0] - 1]
    set lastRow [expr [lindex $tokens 0] - 1]
    if { [llength $tokens] > 1 } {
       set lastRow  [expr [lindex $tokens end] - 1]
    }
    set firstCol 0
    set lastCol [expr $_numCols - 1]

    set xColumn_ "Column"
    set yColumn_ "Value"

    set numPixels $_numCols
    # not sure if we should get this as a formatted or raw block
    set listRowData [_getRawDataBlock $firstCol $firstRow $lastCol $lastRow]
#puts "firstRow: $firstRow"
#puts "lastRow: $lastRow"
#puts "firstCol: $firstCol"
#puts "lastCol: $lastCol"
#puts "$listRowData"

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
  
       # load the data with specific rowNum_ into the tmpfile
       set range "$firstRow-$lastRow"
       set col 0
       set totalCol $_numCols

       for { set col 1 } { $col <= $totalCol } {incr col} {
           set totalValue 0
           for {set row 1} {$row <= [expr $lastRow - $firstRow + 1]} {incr row} {
               set data [lindex [lindex $listRowData [expr $col - 1]] [expr $row - 1]]
               set currentVal $data
               if { $data == "NULL" } {
                  set currentVal 0
               }
               set totalValue [expr $totalValue + $currentVal]
           }
           puts $newDataFile [format "%s %s" $col [expr $totalValue / ($lastRow - $firstRow + 1)]]
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
          if { $firstRow == $lastRow } {
             set displayName [format "%s(%s_%s)" [file tail $fileName] Row $firstRow]
          } else {
             set displayName [format "%s(%s_%s-%s)" [file tail $fileName] Row $firstRow $lastRow]
          }

          exec xpaset ds9 analysis plot $displayName $xColumn_ $yColumn_ 2 < $tmpfile
          file delete $tmpfile
       }

       if { $flag != "NOT_YET" } return
    }

    # reset lastCol and numPixels if 
    if { $_isVariableVec } {
	# find how large our row is
	for {set i 0} {$i < $_numCols} {incr i} {
	    if {[lindex $listRowData $i] == "{ }"} {
		if { $i == 0 } {
		    error "No data in row to plot"
		    return
		}
		set lastCol [expr $i - 1]
		set numPixels $i
		break
	    }
	}
	# get data of that one row, truncated before the last " "
	set listRowData [_getRawDataBlock $firstCol $firstRow \
		$lastCol $lastRow]
    }


    # create list from 1 to $numPixels
    set listPixels {}
    for { set i 1 } { $i <= $numPixels } { incr i } {
	lappend listPixels $i
    }

    powCreateDataFromList "xData" $listPixels
    powCreateDataFromList "yData" $listRowData

    powCreateVector "xVector" "xData" 0 NULL NULL
    powCreateVector "yVector" "yData" 0 NULL NULL

    regsub -all { } [urlTail $fileName] _ cleanFileName
    set plotName ${cleanFileName}_${_colName}_row$_plotRowNum

    set curveName c${_plotRowNum}_$plotName

    set powWCSName($curveName) 0
    set powWCSName(${curveName}scope) 0
    if { [info exists wcsinfo] && $wcsinfo!="" } {
       set result [$fFile getDummyHeader2String {} $RAColNum $DecColNum]
       set powFitsHeader($curveName) [lindex $result 0]
       set powWCS($curveName) [lindex $result 2]
       set powWCSInfo($curveName,DEFAULT) $powWCS($curveName)
       set cntList($curveName) [$fFile getHeaderKeyWord [lindex $result 0] $curveName]
       set powFitsHeaderCnt($curveName) [lindex $cntList($curveName) 1]
       set wcsinfo $powWCS($curveName)
    } else {
       set powWCS($curveName) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
       set powFitsHeader($curveName) ""
       set powFitsHeaderCnt($curveName) 0
    }

    set powPlotParam(wcsName,$curveName) "WCS"
    set powPlotParam(graphType,$curveName) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(zoomed,$curveName) 0
    set xCount($curveName) 0
    set yCount($curveName) 0

    powCreateCurve $curveName xVector NULL yVector NULL
    set curves [list $curveName]

    if { [winfo exist .pow.pow]!=1 } { 
	powInit .dummy
    }

    set x_unit ""
    set y_unit ""
    set x_label "Pixel"
    set y_label "Data"

    set powWCSLabel(xlabel,$plotName,DEFAULT) $x_label
    set powWCSLabel(ylabel,$plotName,DEFAULT) $y_label
    set powWCSLabel(xunit,$plotName,DEFAULT) $x_unit
    set powWCSLabel(yunit,$plotName,DEFAULT) $y_unit

    set powPlotParam(graphType,$plotName) "binary"
    set powPlotParam(graphType,${plotName}scope) "binary"
    set powPlotParam(zoomed,$plotName) 0
    set powPlotParam(zoomed,${plotName}scope) 0
    set xCount($plotName) 0
    set yCount($plotName) 0
    set xCount(${plotName}scope) 0
    set yCount(${plotName}scope) 0

    # wcslist can be obtained from any one slice
    if [info exists powWCSList($curveName)] {
       set powWCSList($plotName) $powWCSList($curveName)
       set powWCSList(${plotName}scope) $powWCSList($curveName)

       foreach name [lindex $powWCSList($plotName) 1] {
          $fFile assembleWcsLabel $plotName $name
       }
    } else {
       set powWCSList($plotName) {}
       lappend powWCSList($plotName) 1
       lappend powWCSList($plotName) {}
    }

    set powWCSList(${plotName}scope) $powWCSList($plotName)
    set powWCSName(${plotName}) 0
    set powWCSName(${plotName}scope) 0
    set powPlotParam(wcsName,$plotName) "WCS"

    set powWCS($plotName) $powWCS($curveName)
    set powWCS(${plotName}scope) $powWCS($curveName)
    set powFitsHeader($plotName) $powFitsHeader($curveName)
    set powFitsHeaderCnt($plotName) $powFitsHeaderCnt($curveName)
    set powFitsHeader(${plotName}scope) $powFitsHeader($curveName)
    set powFitsHeaderCnt(${plotName}scope) $powFitsHeaderCnt($curveName)
    # set powPlotColumnDataName($plotName) $powPlotColumnDataName($curveName)

    set powPlotParam(graphType,$plotName) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${plotName}scope) $powPlotParam(graphType,$plotName)
    set powPlotParam(zoomed,$plotName) 0
    set powPlotParam(zoomed,${plotName}scope) 0

    powCreateGraph $plotName $curves NULL $x_unit $y_unit $x_label $y_label \
	    [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1]

    powSetCurveOptions $plotName $curves pDisp No lDisp Yes
    destroy .vector_plotrow
}

################################################################

itcl::body VectorTable::_saveTableToAscii {win_ asciiFileName_} {

# these vars are used
#    _exportFirstRow
#    _exportLastRow 
#    _exportFirstCol
#    _exportLastCol 
#    _exportCSV
#    _exportPrintRowNumbers
#    _exportCharBetweenCols
    set _exportFormat [$win_.rb get]
    if { $_exportFormat == "csv" } {
	set _exportCSV 1
    } elseif { $_exportFormat == "userdefine" } {
	set _exportCSV 0
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
    
    # create list of print widths
    set listPrintWidths {}
    for {set i 0} {$i < $_dispCols} {incr i} {
	lappend listPrintWidths [expr $_cellWidth($i) + 1]
    }

 set firstloop 1

 if { $_exportsel == 1 } {
    set _exportFirstRow 1
    set _exportLastRow  $_numRows
    set totalRowsPrint [expr $_exportLastRow - $_exportFirstRow + 1]
 }

 if { $_exportsel == 3 } { 
  for {set r 1} { $r <=$_numRows } { incr r } {
   if {$_selectedRows($r) ==1 } {
    set  _exportFirstRow  $r
    while { $_selectedRows($r) == 1} {
       incr r
       if { $r > $_numRows } {
           break
       }
    }
    set totalRowsPrint [expr $r - $_exportFirstRow]

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


        if { ($_tableType == "Vector Table") } {
	$fFile saveVecToASCII $asciiFileName_ $filePrintMode \
		$fRow $nRows $fCol $nCols \
		$_colName \
		$_exportCSV \
		$_exportPrintRowNumbers \
		$_exportCharBetweenCols \
		$_isVariableVec
        } else {
            error "Table Type unknown.  Export as text not possible."
            return
        }

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

	set fCol   $_exportFirstCol
	set nCols  [expr $_exportLastCol - $_exportFirstCol + 1]

	# shift by the first user-selected row
	set fRow [expr $fRow + $_exportFirstRow - 1]
#okbox "$asciiFileName_ $filePrintMode $fRow $nRows $fCol $nCols	$_colName $_exportCSV $_exportPrintRowNumbers $_exportCharBetweenCols $_isVariableVec"
	$fFile saveVecToASCII $asciiFileName_ $filePrintMode \
		$fRow $nRows $fCol $nCols \
		$_colName \
		$_exportCSV \
		$_exportPrintRowNumbers \
		$_exportCharBetweenCols \
		$_isVariableVec
	
	# if user presses cancel, stop
	if {[catch {$win_.f.fdb step}] == 1} {
	    file delete $asciiFileName_
	    return
	}
    }
  }
    destroy $win_
}

