# construct a FitsTable object # FitsTable FitsTableObjName FitsFileObjName currentHDU 

#-------------------------------------------------------------------------------
# Modification History:
#   Ziqin Pan, Feb 18, 2004
#   Add the following row selection functions
#         1. Select  rows by using the mouse
#         2. Select all rows 
#         3. Invert selected rows
#         4. Select rows from expr
#         5. Delete the selected rows
#         6. Do statistics on selected rows
#         7. Do histogram on selected rows
#         8. Calculate value on selected rows and column
#         9. Keep selected rows after doing sorting 
#         10. Export selected rows and columns as text
#         11. Export selected rows and columns as HDU 
#-------------------------------------------------------------------------------


itcl::class Table {
    constructor {args} {}
    destructor         {}

    public method makeTable { extNum_ }
    public method refresh { {doChildren_ 1} }
    public method calculateCols {colname_ colform_ formula_ selonly_}
    public method getCalcCols {}
    public method scplotCmd {{xyname_ {}}}
    public method delRowsWithCondition {cond_}
    public method selRowsWithCondition {cond_ fRow_ nRows_ seland_ selor_}
    public method delRowsFromList {entry_}
    public method selRowsFromList {entry_ seland_ selor_}
    public method closeCmd {} 
    public method bringToFront {}
    public method setFileName { fName_ }
    public method addChild { child_ }
    public method freeChild { child_ }
    public method callHistogramDirectly { extNum_ fillFlag_ } 
    public method getColInfo { name } { return [$fFile getColInfo $name] }

# used by VectorTable
    public method getFatherFitsExtension {}
    public method getColumnInfo {name_ index_}

#   see protected sort methods below
    public method doSort { keys_ dirs_ unique_ }

# called within VectorTable's constructor
    public method passParamsToVectorTable {}

    public method getnumRows {}

    public variable fileName ""
    public variable isFailedToCopy 0
    public variable fFile
    public variable currentHDU

    public method trySaveASCII {}
    public method tryExport {}



# protected
    protected variable _extNum 1
    protected variable snum ""
    protected variable _overWriteImageValue
    protected variable _neverAskFlagValue "false"
    protected variable _overWriteImage no
    protected variable _frameXPos
    protected variable _endX 0
    protected variable _valueTDIM 
    protected variable _istart 0
    protected variable _iend   0
    protected variable _opCancel "false"
    protected method _determineOverWriteFlag {}
    protected method _openGroupFile { c r }
    protected method _plotVectorTableRow { c r }

    protected method _ds9MakeImage { fh colIdx slice xSize ySize }
    protected method _ds9MakePlot { rowNum_ fRow lRow xColumn_ yColumn_ {xeColumn_ ""} {yeColumn_ ""} }
    protected method _ds9MakePlot1 { rowNum_ range xColumn_ yColumn_ {xeColumn_ ""} {yeColumn_ ""} {inCurrGraph_ 0}}
    protected method _setOverWriteFlag { }
    protected method _setOpCancel { }
    protected method _drawTable {}
    protected method _plotCmd {}
    protected method _plotCols { xColumn_ xeColumn_ yColumn_ yeColumn_ \
          {inCurrGraph_ 0} {whichRows_ -} {plotSelOnly_ 0}}
    protected method _drawTableFrame {}
    protected method _getDataForAxis { axis_ axisExpression_ rows_ }

    protected method _relocateDividLine {x_ index_}
    protected method _finalDividLine {x_ index_}
    protected method _startDividLine {x_ index_}
    protected method _setVScroll {args}
    protected method _setHScroll {args}

    protected method _setupTable { }
    protected method _redrawTable {prevCols_}
    protected method _addColsToTable {prevCols_ prevRows_}
    protected method _delColsFrTable {prevCols_ prevRows_}
    protected method _addRowsToTable {prevCols_ prevRows_}
    protected method _delRowsFrTable {prevCols_ prevRows_}
    protected method _addColsToFile {inWin_} 
    protected method _addRowsToFile {inWin_} 
    protected method _updateLast { }
    protected method _colMenu { hdr i colName colNum }
    protected method _updateUnViewedColumns {} 
    protected method _moveColView { colName targetName option }
    protected method _addColView { colName_ addPos_ beforeAfter_ }
    protected method _delColView { colName_ }
    protected method _editColParamsWindow { { colNum0_ 0 } }
    protected method _isVScrollLocked {}
    private variable _isVScrollLocked1
    protected method _scrollChildren { firstRow { currentRow -1} }

    ################
    # Subclasses need to implement these methods...
    #          all col/row params are zero-based

    protected method _getRawData       {col_ row_}
    protected method _getFormattedData {col_ row_}
    protected method _readTableData      {fCol_ fRow_ nCols_ nRows_}
    protected method _getRawDataBlock  {fCol_ fRow_ lCol_ lRow_}
    protected method _putRawDataBlock  {fCol_ fRow_ data_}
    protected method _putRawData  {fCol_ fRow_ val_}

    #
    ################

    protected method _jumpFrEnt {}
    protected method _jump {lineNum_}
    protected method _selRowsFrExpr {ifDelete_}
    protected method _copyCell {col_ row_}
    protected method _modifyTableCell {col_ row_ val_}
    protected method _writeTableData {col_ row_ val_}
    protected method _updateColSel {col_}
    protected method _resizeCan {x_ y_}
    protected method _layoutCan {x_ y_}
    protected method _getXYSize {x_ y_}
    protected method _refacing {}
    protected method _saveAs {}
    # exports to ascii file
    protected method _saveASCII {}
    protected method _export {}
    protected method _calXPos {fCol_ nCols_}
    protected method _reframe {}
    protected method _setScrolls {}
    protected method _getLastFirstCol {}
    protected method _closeFrWm {w_} 
    protected method _realCloseCmd {} 
    protected method _saveFile {}
    protected method _updateNumRows {}
    protected method _updateNumCols {}
#    protected method _addCols {}
    public method _addCols {{colindex -1 } {colname {} } {colformat {}} { colunit {}} {dispformat {}} }
#    protected method _delCols {}
    protected method _delCols {{collist_ -1 }}
    protected method _delRows {}
    protected method _delRowsRange {}
#    protected method _tryDelCols {}
    public method _tryDelCols {{collist_ -1 }}
#    protected method _tryDelRows {}
    public method _tryDelRows {{range_ 0}}
#    protected method _addRows {}
    public method _addRows {{rowindex -1} {rownum -1}}
    protected method _openVectorTable { colNum_ }

# Ziqin Pan, Jan 2004
# Added functions for rows selection
    protected method _initSelRows {}
    protected method _selAllRows {}
    protected method _selRows {}
    public method _showselRows {}
    public method _showselCheck {}
    protected method _logicalWithselectedRows {rowlist_ seland_ selor_}
    protected method _setRowStartMark { row_}
    protected method _setRowEndMark { row_}
    protected method _unsetRowMark {}
    protected method _selInvert {}
    protected method _updateExportPanelState {}
    protected method _writeHist {ffile_ origName_}
    public method _parseToRowRange {start_end}
    protected method _saveTableToFile {win_ fitsFileName_ origName_} 
#
#

    protected method _editCell {} 
    protected method _selCell {}
    protected method _unselCell {}
    protected method _cellUp {} 
    protected method _cellDown {} 
    protected method _cellLeft {} 
    protected method _cellRight {} 
    protected method _pageUp {}
    protected method _pageDown {}
    protected method _jumpTo { colName } 
    protected method _calculateCmd {}
    protected method _collapse {seed_ bed_}
    protected method _sortRange {bed_}
    protected method _sortCmd {} 
    protected method _sortColumn {}

    protected method _calAbsXPos {}

    protected method _determineRealSlice { slice }
    protected method _getSortKey {key_}
    protected method _resetSKey {}
    protected method _cpyToCB {}
    protected method _cpyFrCB {}
    protected method _displaySlice { colIdx slice }
    protected method _createMovie { colIdx slice xSize ySize }
    protected method _getKeywordValue { keyword header }
    protected method _displayMovie { colIdx slice }
    protected method _makeHistogram { {extNum_ {}} }
    protected method _histoCmd {} 
    protected method _changeUniqLabel {}
    protected method _statCmd {}
    protected method _statSelCmd {}
    protected method _makeJustification {} 
    protected method _updateRestDisps {} 
    protected method _saveTableToAscii {win_ asciiFileName_} 

    protected method _setStartMark {col_ row_ {focus True} }
    protected method _buildMenus {}
    protected method _buildNewMenus {}
    protected method _postMenus {}
    protected method _updateHL {}
    protected common _colSel
    protected common _asciiColWidth 80
    protected common _psortkey 
    protected common _ssortkey 
    protected common _tsortkey 
    protected common _psortcheck 1
    protected common _ssortcheck 1
    protected common _tsortcheck 1
    protected common _addcolname ""
    protected common _addcolunit ""
    protected common _addcolform ""
    protected common _addcoldisp ""
    protected common _isUniqMerge 0
# this explains the format for _numEntry, which contains what actually
#   shows up on the screen
#         set id ${c}_${r}
#         set _numEntry($id) $val
    protected variable _arrayDim
    protected variable _ttype
    protected variable _tType
    protected variable _numEntry
    protected variable _currentCol
    protected variable _currentRow
    protected variable _cellVar
    protected variable _cstartx ""
    protected variable _cstarty ""
    protected variable _oldcstartx ""
    protected variable _oldcstarty ""
    protected variable _cendx ""
    protected variable _cendy ""

    protected variable _insColNum "End of Table"

    protected variable _showRows 20
    protected variable _showCols 6
    protected variable _firstRow 1 
    protected variable _firstCol 1
    protected variable _old_firstCol 1
    protected variable _lastFirstCol 1
    protected variable _droot ""
    protected variable _mBar


# Ziqin Pan, Jan 2004
# Add variables to support row selection
    public variable _selectedRows
    protected variable _rstart ""
    protected variable _rend ""
    protected variable _selectAllRows 0
    protected variable _statselonly 0
    protected variable _plotselonly 0
    protected variable _calcselonly 0
    protected variable _exportsel 0
    protected variable _exportselc 0
    protected variable _exportcolsel 0
    protected variable _fth 0
    protected variable _ftp 0
    protected variable _rowindex 1
    protected variable _rownum 1

#
#



# what does DC stand for? 
    protected variable _DC
    protected variable _xPos
    protected variable _yPos
    protected variable _absXPos
    protected variable _tableType
    protected variable _numCols
    protected variable _numRows
# arrays from 0 to $_dispCols - 1 that keep information on vector info
    protected variable _cellPixWidth
    # cell width in characters
    protected variable _buttonWidth
    protected variable _cellWidth
    protected variable _columnName
    protected variable _columnType
    protected variable _columnComment
    protected variable _columnUnit
    protected variable _columnTDisp
    protected variable _columnForm
    protected variable _columnWidth
    protected variable _columnNull
    protected variable _columnDim
    protected variable _columnVecSize
# a list (not an array) of column names
    protected variable _listColNames
    protected variable _unViewedColNames

# _rowState is NEVER used...I only keep it here because of fitsTcl
# (see fvTcl.c) -- Han
    protected variable _rowState
# array of whether a column is chosen by the user ("notched" in the table display)
    protected variable _colNotchedState
    protected variable _tableData

    protected variable _anyColSelected 0
    protected variable _anyRowsSelected 0
# _listPreSelectedColNames is the list of columns preselected by user to be
#  displayed using the right-mouse button on the Table button
    protected variable _listPreSelectedColNames
    protected variable _dispCols
    protected variable _startX
    protected variable _fatherFitsExtension
    protected common _graphIDhighest 0
    protected variable _isDirPlot 0
    protected variable _justStarted 1
    protected variable _isBeingDestroyed 0
    protected variable _myChildren {}

    protected method _readInTable { }
    protected method _setEndMark {col_ row_}
    protected method _unsetMark {}

# used only in statistics menu, see Table::_statCmd
    protected common _colSName
    protected common _colMin
    protected common _colMax
    protected common _colMean
    protected common _colFMin
    protected common _colFMax
    protected common _colStd
    protected common _colRowRange
    protected common _colNumVal

# only used in the file export menu, see Table::_saveASCII
    protected variable _exportRowRange
    protected variable _exportFirstRow
    protected variable _exportLastRow
    protected variable _exportFirstCol
    protected variable _exportLastCol
    protected variable _exportPrintRowNumbers
    protected variable _exportselRows
    protected variable _exportFormat
    protected variable _exportCSV
    protected variable _exportUserChooseChar
    protected variable _exportFixedFormat
    protected variable _exportCharBetweenCols

    protected variable _fitsFileLocationCol -1
    protected variable _expandFlag false
    protected variable sortRowResultList 
}

itcl::body Table::constructor {args} {
}

itcl::body Table::destructor {} {
}


itcl::body Table::getnumRows {} {
     return  $_numRows
}

itcl::body Table::makeTable { extNum_ } {
    global g_charPix

    set _extNum $extNum_
    set _DC(height)      20
    # width in pixels required to display row numbers
#    set _DC(width)       [expr ($_numRows !=0 )? \
#	                       (int(log10($_numRows))+4)*$g_charPix : 100 ] 
    set _DC(width)       [expr ($_numRows > 10000 )? \
	                       (int(log10($_numRows))+4)*$g_charPix : 8*$g_charPix ] 
    set _DC(headroom)    80
    set _DC(footroom)    40
    set _DC(vscrollsize) 15
    set _DC(hscrollsize) 15
    # space to the right of the data
    set _DC(rightspace)   6
    # space between lines
    set _DC(interline)    0

    # top margin
    set _DC(tmar)         6
    # left margin
    set _DC(lmar)         8

    # Whether vertical scrollers are locked to parent's
    set _isVScrollLocked1 False

    #Read in all the needed data
    _readInTable

    #Draw the table 
    _drawTable
    _buildMenus
}

itcl::body Table::_readInTable { } {
#puts "readInTable start"
    global g_charPix

    set maxWidth [expr int([winfo screenwidth .]/$g_charPix)-10]
    set _dispCols [llength $_listPreSelectedColNames]
    for {set i 0} {$i < $_dispCols} {incr i} {
	set tmpName [lindex $_listPreSelectedColNames $i]
        if { [catch {set listOfListOneColInfo [$fFile getColInfo $tmpName]}] } {
	    # error, so we won't display the column
            set _listPreSelectedColNames [lreplace $_listPreSelectedColNames $i $i]
            incr i -1
            incr _dispCols -1
        } else {
            set arrayColInfoLists($i) [lindex $listOfListOneColInfo 0]
        }
    }
    setarray _colNotchedState 0 [expr $_dispCols-1] 0
    set thisHeader [split [$fFile dumpHeader] \n]

    for {set i 0} {$i < $_dispCols} {incr i} {
        set _valueTDIM($i) 0
	set listOneColInfo $arrayColInfoLists($i)

# in the listOneColInfo list. the elements are
# Index 0 : column name
#       1 : column type (TFORM)
#       2 : column unit
#       3 : column TDISP
#       4 : column format
#       5 : column width
#       6 : is Zero
#       7 : is Scaled
#       8 : column null value

#puts "listOneColInfo: $listOneColInfo"
	set _columnName($i)  [lindex $listOneColInfo 0]
# TFORM -- don't blame me, I didn't name this
        # Pan Chai: ignore everything after the vaild TFORM value
        # i.e., if TFORMn = 16A23:18/032
        #          TFORMn should be 16A23 only
	set tformData  [lindex $listOneColInfo 1]
        regsub {^[0-9a-zA-Z]*} [lindex $listOneColInfo 1] "" restData

        if { [string length $restData] > 0 } {
           if { [string range $restData 0 0] == ":" } {
              set idx [string first $restData $tformData]
              set _columnType($i)  [string range $tformData 0 [expr $idx - 1]]
           } else {
              set _columnType($i)  [lindex $listOneColInfo 1]
           }
        } else {
	   set _columnType($i)  [lindex $listOneColInfo 1]
        }
      
# TUNIT
	set _columnUnit($i)  [lindex $listOneColInfo 2]
	set _columnTDisp($i) [lindex $listOneColInfo 3]

        set _columnComment($i) ""
        set idx [lsearch -glob $thisHeader "TTYPE*$_columnName($i)*"]
        if { $idx >= 0 } { 
           set commentTokenList [split [lindex $thisHeader $idx] "/"]
           if { [llength $commentTokenList] > 1 } {
              set _columnComment($i) [string trim [lindex $commentTokenList end]]
           }
        }

        if { [regexp J $_columnType($i)] } {
	   set _columnForm($i) "%.10G"
        } elseif { [regexp K $_columnType($i)] } {
           # chai: K for 64 bits
	   set _columnForm($i) "%s"
        } else {
	   set _columnForm($i)  [lindex $listOneColInfo 4]
        }
	set _columnWidth($i) [lindex $listOneColInfo 5]

        if { [regexp K $_columnType($i)] } {
           # chai: using double data type width for the cell if it is K type
	   set _columnWidth($i) 20
        }

	set _columnNull($i)  [lindex $listOneColInfo 8]

	# Check for TDIM keyword

	set colNo [lsearch $_listColNames $_columnName($i)]
	incr colNo
	if { [catch {set key [$fFile getKeyword TDIM$colNo]} err] } {
	    set _columnDim($i) 1
            set _valueTDIM($i) 0
	} else {
	    set key "[string trim [lindex [lindex $key 0] 1] {' ()}]"
	    set _columnDim($i) [split $key ,]
            set _arrayDim($i) [split $key ,]
            set _valueTDIM($i) [llength $_arrayDim($i)]
        }

       if { ![catch {set key [$fFile getKeyword TFORM$colNo]} err] } {
           set key "[string trim [lindex [lindex $key 0] 1] {' ()}]"
           set testKey $key
           set value 1
           scan $key "%d%s" value dataType
           if { $value > 1 &&  [string range $dataType 0 0] != "A"  && 
                               [string range $dataType 0 0] != "C"  &&
                               [string range $dataType 0 0] != "L"  &&
                               [string range $dataType 0 0] != "M"  &&
                               [string range $dataType 0 0] != "P"  &&
                               [string range $dataType 0 0] != "X" } {
              if { $_valueTDIM($i) <= 0 } {
                 set _valueTDIM($i) 1
              }
           } else {
              set testSequence [split $testKey "()"]
              if { [llength $testSequence] != 2 } {
                 set _valueTDIM($i) 0
              } else {
                 set idx [string first "P" [lindex $testSequence 0] ]
                 if { $idx >= 0 } { 
                    regsub -all {[0-9]} [lindex $testSequence 1] "" result
                    if { $result != "" } {
                       set _valueTDIM($i) 0
                    }
                 } else {
                    set _valueTDIM($i) 0
                 }
              }
           }
        }

        if { ![catch {set _ttype [$fFile getKeyword TTYPE$colNo]} err] } {
            set _ttype "[string trim [lindex [lindex $_ttype 0] 1] {' ()}]"
            set _tType($i) [lindex [split $_ttype ,] 0]
        }

	# Determine vector size of column

        if { $_tableType == "Binary Table" } {
	    # "PE(65)" means a variable vector table of max length 65
            if { [string index $_columnType($i) end] == ")" } {
		# remove letters PE
		regsub -all {[A-Z]} $_columnType($i) "" _columnVecSize($i)
		# Pull out variable length from (65)
		if { ![regexp {\((.*)\)} $_columnVecSize($i) dmy val] } {
		    set val 0
		}
		set _columnVecSize($i) [expr -$val]
	    } else {
		regsub -all {[A-Z].*} $_columnType($i) "" _columnVecSize($i)
		# most common scenario; e.g., "F4.2" or "I"
		if { $_columnVecSize($i) == "" } {
		    set _columnVecSize($i) 1
		}
	    }
            if { [regexp A $_columnType($i)] && $_columnVecSize($i) != 0 } {
                set token [split $_columnType($i) "A"]

                regsub -all {[A-Z]} [lindex $token 0] "" testString

                if { $testString <= 1 } {
	           set _columnVecSize($i) 1
                } elseif { [lindex $token 0] == "" && [lindex $token 1] == "" } {
		   set _columnVecSize($i) 1
                } elseif { [lindex $token 1] == "" } {
		   set _columnVecSize($i) 1
		   # set _columnVecSize($i) [lindex $token 0]
                } else {
		   set _columnVecSize($i) [expr [lindex $token 0] / [lindex $token 1]]
		   set _columnDim($i) [lindex $token 1]
		   set _columnWidth($i) [lindex $token 1]
                }
            }
	} else {
	    set _columnVecSize($i) 1
	}
	
        if { ![info exists _cellWidth($i)] } {
	    set headMax 0
	    foreach l [list xx$_columnName($i) $_columnType($i) $_columnUnit($i)] {
		set wdth [font measure g_titleFont $l]
		if { $wdth > $headMax } {set headMax $wdth}
	    }
	    # we now have header maximum in pixels
	    set headMax [expr int( 1.0*$headMax/$g_charPix + 0.999 )]
	    # we now have header maximum in characters
	    set _cellWidth($i) [expr ($headMax > $_columnWidth($i)) \
		    ? $headMax : $_columnWidth($i) ]
	    if { $_cellWidth($i) > $maxWidth } {
		set _cellWidth($i) $maxWidth
	    }
        }

    }
    
    _calAbsXPos
}

### why was this taken out? --Han
### I'm putting it back in (by uncommenting it)
#
itcl::body Table::_calAbsXPos {} {
#puts "_calAbsXPos start"
    global g_charPix
    set _absXPos(0) [expr $_DC(lmar) + $_DC(width) +$_DC(rightspace)]

    # this is to determine the size of a button inside the entry
    if ![winfo exists .dummyTest] {
       button .dummyTest -text "WWWWW" -font [list Helvetica 10]
    }
    set _buttonSize [winfo reqwidth .dummyTest]

    for {set i 0} {$i < $_dispCols} {incr i} {
	set _cellPixWidth($i) [expr $g_charPix*$_cellWidth($i)+4]
	set _absXPos([expr $i+1]) [expr $_absXPos($i)+$_cellPixWidth($i)+$_DC(rightspace) + 3] 
        if { $_valueTDIM($i) > 0 } {
	   # set _absXPos([expr $i+1]) [expr $_absXPos([expr $i + 1])+$_buttonSize]
        }
    }
}

itcl::body Table::_drawTable {} {
    global isMac
    global g_titleFont
    
    set _droot ".[namespace tail $this]"

    if { [winfo exists $_droot] } {
       destroy $_droot
    }
    
    powToplevel  $_droot  .dummy
    wm geometry $_droot +50+[winfo pointery .]

    setFileName $fileName

    set _firstRow 1
    set _firstCol 1
    set _fitsFileLocationCol -1

    _setupTable

# Ziqin Apr 01,2004

    frame         $_droot.table 
    bind $_droot <Destroy> +[itcl::code $this _closeFrWm %W]

    set can       $_droot.table.can
    set hdr       $_droot.table.hdr
    canvas         $hdr \
	-width $_DC(xsize) \
	-height $_DC(headroom) -highlightthickness 0
    canvas        $can \
	-width $_DC(xsize) \
	-height  $_DC(ysize) \
	-relief ridge -borderwidth 5
    bind $can <Configure> +[itcl::code $this _resizeCan %w %h]
    bind $can <Home>    [itcl::code $this _jumpTo 0]
    bind $can <Up>    [itcl::code $this _cellUp]
    bind $can <Down>  [itcl::code $this _cellDown]
    bind $can <Left>  [itcl::code $this _cellLeft]
    bind $can <Right> [itcl::code $this _cellRight]
    bind $can <Prior> [itcl::code $this _pageUp]
    bind $can <Next>  [itcl::code $this _pageDown]
    bind $can <KeyPress> "\
          focus $_droot.usrEntry.sel_e; \
          event generate $_droot.usrEntry.sel_e <KeyPress> -when now \
          -keysym %K -keycode %k"

    scrollbar  $_droot.table.vscroll -width $_DC(vscrollsize) -orient vertical \
	-command [itcl::code $this _setVScroll]
    scrollbar  $_droot.table.hscroll -width $_DC(hscrollsize) -orient horizontal \
	-command [itcl::code $this _setHScroll]


    if { $_numRows } {
       $_droot.table.vscroll set 0.0 \
	     [expr double($_showRows)/$_numRows] 
    } else {
       $_droot.table.vscroll set 0.0 1.0
    }
    if { $_listPreSelectedColNames == "" } {
       $_droot.table.hscroll set 0.0 1.0
    } else {
       $_droot.table.hscroll set 0.0 \
	     [expr double($_showCols)/[llength $_listPreSelectedColNames]]
    }


    pack $_droot.table.hdr      -side top    -fill x  -anchor w
    pack $_droot.table.vscroll  -side right  -fill y 
    pack $_droot.table.hscroll  -side bottom -fill x 
    pack $_droot.table.can      -side top    -fill both  -expand 1


    frame         $_droot.usrEntry  -height $_DC(footroom) \
	-relief raised -borderwidth 0
    entry 	    $_droot.usrEntry.goto_e -width 10 \
	-relief sunken -bd 2 -font g_titleFont
    bind $_droot.usrEntry.goto_e <Return> [itcl::code $this _jumpFrEnt]
    button 	    $_droot.usrEntry.goto_b -text "Go to:" \
	-command [itcl::code $this _jumpFrEnt] -font g_titleFont

    label $_droot.usrEntry.sel -text "  Edit cell:" -font g_titleFont
    entry $_droot.usrEntry.sel_e -width 22 \
	-textvariable [itcl::scope _cellVar] -relief sunken -bd 2 -font g_titleFont
    bind $_droot.usrEntry.sel_e <Return> [itcl::code $this _editCell]

    if { ($_tableType == "Image") || ($_tableType == "Vector Table") } {
        checkbutton $_droot.usrEntry.lock -text "Lock to Parent   "  \
	    -font g_titleFont  \
	    -variable [itcl::scope _isVScrollLocked1 ] \
	    -selectcolor $fvPref::checkBBgColor  \
	    -activeforeground black -activebackground $fvPref::globalBgColor \
	    -command {} 
    }
    
    pack $_droot.usrEntry.goto_b -side left 
    pack $_droot.usrEntry.goto_e -side left
    pack $_droot.usrEntry.sel    -side left
    pack $_droot.usrEntry.sel_e  -side left 
    catch { pack $_droot.usrEntry.lock   -side right }
    pack $_droot.usrEntry        -side bottom -fill x 
    pack $_droot.table           -side top    -fill both -expand 1

# draw the lines and entries ... 
    _drawTableFrame

    # Bind window to menu events

    bind $_droot <<PostMenus>>   [itcl::code $this _postMenus]

    bind $_droot <<SaveFile>>    [itcl::code $this _saveFile]
    bind $_droot <<SaveFileAs>>  [itcl::code $this _saveAs]
    bind $_droot <<Export>>      [itcl::code $this _export]
    bind $_droot <<ExportAs>>      [itcl::code $this _saveASCII]
    bind $_droot <<CloseWindow>> [itcl::code $this closeCmd]

    bind $_droot <<Copy>>        [itcl::code $this _cpyToCB]
    bind $_droot <<Paste>>       [itcl::code $this _cpyFrCB]
  
}


itcl::body Table::_setupTable { } {

    set lastCol [llength $_listPreSelectedColNames]

    if {$_showRows > $_numRows} {
	set _showRows $_numRows
    }

    if {$_showCols > $lastCol} {
	set _showCols $lastCol
    }

    if { $_firstCol > [expr $lastCol -$_showCols+1]} {
	set _firstCol [expr $lastCol -$_showCols+1]
    }
    if { $_firstRow > [expr $_numRows -$_showRows+1]} {
	set _firstRow [expr $_numRows -$_showRows+1]

    }
    # the - $_DC(rightspace)/2 appears to be a hack, so that the table looks prettier
    # and not so frayed at the right margin...taking it out will show you what I mean
    set _DC(xsize) [expr $_absXPos([expr $_firstCol-1+$_showCols]) \
		       - $_absXPos([expr $_firstCol-1]) + $_absXPos(0) \
		       - $_DC(rightspace)/2 ]
    if { $_DC(xsize) > [winfo screenwidth .] } {
       set _DC(xsize) [expr [winfo screenwidth .] - 20]
    }

    set _DC(ysize) [expr 2*$_DC(tmar) + $_showRows*($_DC(height)+$_DC(interline))]

    _getXYSize $_DC(xsize) $_DC(ysize)

    _getLastFirstCol

    _updateLast
}

itcl::body Table::_getLastFirstCol {} {
    # base on the size of the canvas, find out the beginning of the
    # last page of the table.
    set tmpWidth  [expr $_DC(lmar) + $_DC(width) + $_DC(rightspace)]
    for { set i $_dispCols} {$i > 0} {incr i -1} {
	set tmpWidth [expr $tmpWidth+$_cellPixWidth([expr $i-1])+$_DC(rightspace) + 10]
	if { $tmpWidth > $_DC(xsize) } {
	    break
	}
    }
    set _lastFirstCol [expr $i +1]
}

itcl::body Table::_buildMenus {} {
#puts "_buildMenus start"
    global isMac
    global g_titleFont
    
    if { $isMac } {
        set _mBar .mbar.table
    } else {
        set _mBar $_droot.mbar
    }
    $_droot config -menu $_mBar
    
    if { ![winfo exists $_mBar] } {
       _buildNewMenus
    }
}


itcl::body Table::_buildNewMenus {} {
#puts "_buildNewMenus start"
   global isMac isWin
   global g_titleFont

   menu $_mBar -font g_titleFont
   if { $isMac } {
      set evtWndw ""
      set cmdkey "Cmd"
      $_mBar add cascade -menu $_mBar.apple
      $_mBar add cascade -menu $_mBar.file  -label File
      $_mBar add cascade -menu $_mBar.edit  -label Edit
      $_mBar add cascade -menu $_mBar.tools -label Tools
      $_mBar add cascade -menu .mbar.wind  -label Windows
      $_mBar add cascade -menu $_mBar.help  -label Help
      buildAppleStyleMenu $_mBar.apple
   } else {
      set evtWndw $_droot
      set cmdkey "Alt"
      $_mBar add cascade -menu $_mBar.file  -label File
      $_mBar add cascade -menu $_mBar.edit  -label Edit
      $_mBar add cascade -menu $_mBar.tools -label Tools
      $_mBar add cascade -menu $_mBar.help  -label Help
   }
   
   # FILE

   if { $isMac } {
      buildFileMenu $_mBar.file
      if { ($_tableType == "Binary Table") || ($_tableType == "ASCII Table")} {
      $_mBar.file entryconfig "Export" -label "Export HDU..." 
      }
      $_mBar.file entryconfig "ExportAs" -label "Export as Text..." \
            -state normal -font g_titleFont
      $_mBar.file entryconfig "Save As..." -state normal -font g_titleFont
      $_mBar.file entryconfig "Close"      -state normal -font g_titleFont
   } else {
      menu $_mBar.file -tearoff False
      $_mBar.file add command -label "Save" -underline 0 \
            -command "doMenuEvent <<SaveFile>>" -accelerator "$cmdkey+S" -font g_titleFont
      if { ($_tableType == "Binary Table") || ($_tableType == "ASCII Table")} {
      $_mBar.file add command -label "Export HDU..." \
            -command "doMenuEvent <<Export>>" -font g_titleFont
      }
      $_mBar.file add command -label "Export as Text..." \
            -command "doMenuEvent <<ExportAs>>" -font g_titleFont
      $_mBar.file add command -label "Close" \
            -command "doMenuEvent <<CloseWindow>>" -accelerator "$cmdkey+W" -font g_titleFont
   }
   
   # EDIT

   buildEditMenu $_mBar.edit

   # TOOLS

   menu $_mBar.tools -tearoff False

   # HELP

   buildHelpMenu $_mBar.help tableDisplay "Table Display"
        
   # Configure the Post Commands

   if { $isMac || $isWin } {
            
       # The Mac and Windows post all menus at once, so we only need
       # to do the post on the top level
            
       $_mBar configure -postcommand "doMenuEvent <<PostMenus>> $evtWndw"

   } else {
            
       # Unix has to post each individual menu

       $_mBar.file configure -postcommand \
               "doMenuEvent <<PostMenus>> $evtWndw"
       $_mBar.edit configure -postcommand \
               "doMenuEvent <<PostMenus>> $evtWndw"
       $_mBar.tools configure -postcommand \
               "doMenuEvent <<PostMenus>> $evtWndw"
   }
}


itcl::body Table::_postMenus {} {
#puts "_postMenus start"
   
   if { [$fFile isFileChanged] && ![$fFile isReadOnly] } {
      $_mBar.file entryconfigure Save -state normal
   } else {
      $_mBar.file entryconfigure Save -state disabled
   }

   set clipType [lindex [fvClipBoard report] 0]
   if { ($clipType == "table") && !$isFailedToCopy } {
      $_mBar.edit entryconfigure "Paste" -state normal
   } else {
      $_mBar.edit entryconfigure "Paste" -state disabled
   }

   if { $_cstartx == "" } {
      $_mBar.edit entryconfigure "Copy" -state disabled
   } else {
      $_mBar.edit entryconfigure "Copy" -state normal
   }

}


itcl::body Table::scplotCmd {{xyname_ {}}} {
    global g_charPix

    set _DC(height)      20
    set _DC(width)       [expr ($_numRows !=0 )? \
	                       (int(log10($_numRows))+4)*$g_charPix : 100 ] 
    set _DC(headroom)    80
    set _DC(footroom)    40
    set _DC(vscrollsize) 15
    set _DC(hscrollsize) 15
    # space to the right of the data
    set _DC(rightspace)   6
    # space between lines
    set _DC(interline)    0

    # top margin
    set _DC(tmar)         6
    # left margin
    set _DC(lmar)         8

    set _listPreSelectedColNames $_listColNames
# let the destructor know
    set _isDirPlot 1
#
    _readInTable

#  Ziqin Pan , Feb 18, 2004  
#  add to support row selection
     _initSelRows
#
#

    set argc [llength $xyname_]
    if { $argc<4 || $argc>6 } {
       _plotCmd
    } elseif { $argc>5 } {
       eval _plotCols $xyname_
    } else {
       if { $argc==4 } {
          set flag 0
          set range "-"
          set sel 0
          foreach {x xe  y ye } $xyname_ {}
       } elseif { $argc==5} {
          set range "-"
          set sel 0
          foreach {x xe y  ye flag} $xyname_ {}
       } else {
          set sel 0
          foreach {x xe y  ye flag range} $xyname_ {}
       }
 
 
       _plotCols $x $xe $y $ye $flag $range $sel
    }

    itcl::delete object $this
}

itcl::body Table::_plotCmd {} {
#puts "_plotCmd start"

    set nameList [list RowNumber ElementNumber]
    set vecList [list 1 1]


    for {set i 0} {$i < $_dispCols} {incr i} {
	if { $_columnVecSize($i) > 0 } {
#            lappend vecList  $_columnVecSize($i)
	    lappend vecList  $_columnDim($i)
	    lappend nameList [lindex $_listPreSelectedColNames $i]
	}
    }

    if { [winfo exists .pltSel] } {
       .pltSel quitCmd
    }
    set _ftp [FitsPlotSel .pltSel $this $nameList $vecList [itcl::code $this _plotCols]]
    wm title .pltSel "Select Plot Columns"
    _showselCheck
    tkwait window .pltSel
    set _ftp 0
}

itcl::body Table::_getDataForAxis { axis_ axisExpression_ whichRows_ } {
    if { $axisExpression_ == "RowNumber" } {
	set axisExpression_ "#ROW"
	set dataType 41
	set nelem [range count $whichRows_ $_numRows]
	set dim 1
    } else {
	if { [catch {set xinfo [$fFile getExprInfo $axisExpression_]} err] } {
	    error "Cannot plot expression for $axis_.\n\n$err"
	}
	set dataType [lindex $xinfo 0]
	set nelem    [lindex $xinfo 1]
	set dim      [lindex $xinfo 2]

	if { $dataType!=41 && $dataType!=82 } {
	    error "Cannot plot expression type for $axis_.\
		    Expression must evaluate to INT or REAL"
	}
	if { $nelem == -1 } {
	    #  Scalar constants could be applied to either nRows or nElems
	    #  so just return a solitary value and expand later as needed
	    set whichRows_ 1
	} elseif { $nelem < -1 } {
	    #  Treat vector constants as a regular vector column
	    set nelem [expr -$nelem]
	}
    }
    set dataInfoForPOW [$fFile loadExpr $axisExpression_ NULL $whichRows_]
    set dataPtr     [lindex $dataInfoForPOW 0]
    set dataType    [lindex $dataInfoForPOW 1]
    set numElements [lindex $dataInfoForPOW 2]

    return [list $dataPtr $dataType $numElements $nelem $dim]
}

itcl::body Table::callHistogramDirectly { extNum_ fillFlag_ } {
    global g_histoParam

    set nameList $_listPreSelectedColNames
    set tmpName [namespace tail $this]
#    set ft [FitsHistoParam .his_$tmpName $fFile $nameList $this]
    set _fth [FitsHistoParam .his_$tmpName $fFile $nameList $this]
    if { $fillFlag_ == "true" } {
#       $ft insertDefaultValue
       $_fth insertDefaultValue
       wm title .his_$tmpName "fv: Histogram"
       tkwait window .his_$tmpName
       if { $g_histoParam == "" } return
       # _makeHistogram $extNum_
    } else {
       wm title .his_$tmpName "fv: Histogram"
       tkwait window .his_$tmpName
    }
    set _fth 0
#   Ziqin Pan
#    itcl::delete object $this

    closeCmd

#   
}

itcl::body Table::_histoCmd {} {
    global g_histoParam

    set g_histoParam ""

    set nameList $_listPreSelectedColNames
    set tmpName [namespace tail $this]

    set _fth [FitsHistoParam .his_$tmpName $fFile $nameList $this]
     
    wm title .his_$tmpName "fv: Histogram"
    _showselCheck
    tkwait window .his_$tmpName
    set _fth 0


    if { $g_histoParam == "" } return
    # _makeHistogram
}

itcl::body Table::_setOpCancel {} {
    set _opCancel true
    destroy .di
}

itcl::body Table::_setOverWriteFlag {} {
    set _opCancel false
    set _overWriteImage $_overWriteImageValue 
    destroy .di
}

itcl::body Table::_determineOverWriteFlag {} {
    if [winfo exists .di] {
       destroy .di
    }

    if [winfo exists .pow.pow] {
       powToplevel  .di .dummy
       wm title .di "fv: Ask to Replace Selected Image"

       label .di.label -text "Replace current selected image?" -font g_titleFont
       label .di.empty -text "                             " -font g_titleFont
       radiobutton .di.yes -variable [itcl::scope _overWriteImageValue] -value "yes" \
                           -font g_titleFont -text "Yes"
       radiobutton .di.no  -variable [itcl::scope _overWriteImageValue] -value "no" \
                           -font g_titleFont -text "No"

       checkbutton .di.check  -text "Don't ask anymore on remaining \nsession of current FITS file." \
                              -onvalue true -offvalue false \
                              -justify left \
                              -font g_titleFont -variable [itcl::scope _neverAskFlagValue]

       button .di.ok -text OK -command [itcl::code $this _setOverWriteFlag]
       button .di.cancel -text Cancel -command [itcl::code $this _setOpCancel]

       grid .di.label  -row 0 -column 0 -pady 5 -padx 5 -sticky news -columnspan 5
       grid .di.yes    -row 1 -column 1 -padx 1 -pady 1 -sticky news
       grid .di.no     -row 1 -column 3 -padx 1 -pady 1 -sticky news
       grid .di.empty  -row 2 -column 0 -padx 1 -pady 1 -sticky news -columnspan 5
       grid .di.check  -row 3 -column 0 -padx 1 -pady 1 -sticky news -columnspan 5
       grid .di.ok     -row 4 -column 1 -padx 1 -pady 1 -sticky nes
       grid .di.cancel -row 4 -column 3 -padx 1 -pady 1 -sticky nws

       if { $_overWriteImageValue == "yes" } {
          .di.yes select
       } else {
          .di.no select
       }

       tkwait window .di
    }

}

itcl::body Table::_ds9MakeImage { fh colIdx slice xSize ySize } {
     global g_backupDir
    
     set fCol_ $colIdx

     set filemode 2
     set tmpfile ${g_backupDir}/ds9Temp.fit

     if [file exists $tmpfile] {
        file delete -force $tmpfile
     }

     if { [catch {set newfits [fits open $tmpfile $filemode]} err] } {
        error $err
        return
     }

     # command: fitsFile put ihd -p bitpix naxes axis_list
     # $newfits put ihd -p 32 2 {$xSize $ySize}
     set key [$fFile getKeyword TFORM[expr $colIdx + 1]]
     set datavalue [string trim [lindex [lindex $key 0] 1] {' ()}]
     scan $datavalue "%d%s" value dataType

     switch $dataType {
            "L" {
              set dataForm 8
            }
            "X" {
              set dataForm 8
            }
            "I" {
              set dataForm 16
            }
            "J" {
              set dataForm 32
            }
            "A" {
              set dataForm 8
            }
            "E" {
              set dataForm -32
            }
            "D" {
              set dataForm -64
            }
            "B" {
              set dataForm 8
            }
            "C" {
              set dataForm -32
            }
            "M" {
              set dataForm -64
            }
            "P" {
              set dataForm -64
            }
     }

#puts "value: $value"

     $newfits put ihd -p $dataForm 2 {$xSize $ySize}
     $newfits put keyword "NAXIS1  $xSize"
     $newfits put keyword "NAXIS2  $ySize"
     
     set datatList {}
     for {set i 1} {$i <= [expr $xSize * $ySize]} {incr i} {
         lappend dataList [$fh get vtable -noformat $_tType($colIdx) $i $slice]
     }


     catch { $newfits put image 1 1 $dataList } err
     $newfits close

     set err "none"

     catch {exec xpaaccess ds9} result   

     if { [string range $result 0 1] == "no" } {
        # start DS9 if DS9 isn't there 
        if { [catch {exec ds9 &} ds9pid] } {
           file delete $tmpfile 
           if { [tk_dialog .saoError "DS9 startup error.. Cannot start DS9!\nUse POW instead?" \
                         question 0 Yes No] == 0 } {
              close $fFile
              return NO_YET
           }
           close $fFile
           return DONE
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
                    if { [tk_dialog .saoError "DS9 startup error.. Cannot start ds9!\nUse POW instead?" \
                             question 0 Yes No] == 0 } {
                       close $fFile
                       return NO_YET
                    }
                    close $fFile
                    return DONE
                 }
              }
        }

     }

     set displayName [format "%s\(%s\)" [file tail $fileName] $slice]
     exec xpaset ds9 fits $displayName < $tmpfile
     file delete $tmpfile

     return DONE
}

itcl::body Table::_determineRealSlice { slice } {
     if ![info exists sortRowResultList] {
        return $slice
     }
     return [lindex $sortRowResultList [expr $slice + - 1]]
}

itcl::body Table::_displaySlice { colIdx slice } {
    global g_titleFont
    global g_backupDir
    global xFactor yFactor
    global currgn powFirstTimeThroughFlag powPlotParam currimg
    global xCount yCount
    global powFitsHeader powFitsHeaderCnt powPlotParam
    global powWCSList powWCSLabel powWCSName powWCS
    global useWCSInfo powWCSInfo
    global powWCSToken

    if { [string first "ftp" $fileName] >= 0 } {
       set fileName [format "%s/%s/%s" $::env(HOME) $g_backupDir [file tail $fileName]]
    }

    set slice [expr $_firstRow + $slice]
    set slice [_determineRealSlice $slice]

    set xSize [lindex $_arrayDim($colIdx) 0]
    set ySize [lindex $_arrayDim($colIdx) 1]
    set fh [fits open $fileName 0] 
    $fh move +[expr ${_extNum} - 1]

    regsub -all { } [urlTail $fileName] _ cleanFileName
    set graphHandle [format "%s\(%s_%s)_%s" ${cleanFileName} $_tType($colIdx) $slice $_graphIDhighest]

    # tempFile is now a temporary file created in backup directory fvtmp via fits_copy_cell2image
    set tempFile ${g_backupDir}/_[pid]_[clock seconds]
    if { $fvPref::imgDisplayer == "DS9" } {
        if { [catch {exec ds9 &} ds9pid] } {
        }
    } else {
       set tempFile "NONE"
       if { [winfo exist .pow.pow]!=1 } { 
	   powInit .dummy
       }

       set oldgn ""

       if { [info exists currimg] && $currimg!="NULL" && $currimg!="" } {
          if { $_neverAskFlagValue == "false" } {
             _determineOverWriteFlag
             if { $_opCancel == "true" } return
          }
          # the graph is not already on .pow.pow, if _overWriteImage is yes, then delete the previous
          # selected image. 
          if { $_overWriteImage == "yes" } {
             if { $currgn != "powDef" } {
                set oldgn $currgn
                set xo $powPlotParam(xo,$currgn)
                set yo $powPlotParam(yo,$currgn)
             }
             set currgn ""
          }
       }
    }

    # set graphHandle ${cleanFileName}_[expr $currentHDU-1]_$_graphIDhighest

    # call fits_copy_cell2image to translate the keyword and wcs
    set result [$fFile translateKeyWords $_tType($colIdx) $slice DEFAULT $tempFile]
    if { $fvPref::imgDisplayer == "DS9" } {
       catch {exec xpaaccess ds9} result
       after 1000
       exec xpaset ds9 fits $graphHandle < $tempFile
       return
    }

    # fitsObj load arrayRow <column name> <row number> <number element> <null string> <first element>
    set alist [$fh load arrayRow $_tType($colIdx) $slice [expr $xSize * $ySize] NULL 1]

    eval powCreateData $graphHandle $alist
    set wcsinfo [lindex $result 2]
    set powWCS($graphHandle) $wcsinfo
    set powWCS(${graphHandle}scope) $powWCS($graphHandle)
    set powWCS(imgobj_$graphHandle) $powWCS($graphHandle)

    set useWCSInfo(${graphHandle}scope) $fvPref::ifWCSInfo
    set useWCSInfo($graphHandle) $fvPref::ifWCSInfo

    #set result [$fFile getHeader2String]
    #set cntList($graphHandle) [$fFile getHeaderKeyWord [lindex $result 0] $graphHandle]
    set cntList($graphHandle) [$fFile getHeaderKeyWord [lindex $result 0] $graphHandle]
    set powFitsHeaderCnt($graphHandle) [lindex $cntList($graphHandle) 1]
    if { [lindex $cntList($graphHandle) 0] > 0 } {
       set powFitsHeader($graphHandle) [lindex $result 0]
       set powWCSInfo($graphHandle,DEFAULT) $powWCS($graphHandle)
    } else {
       set powFitsHeader($graphHandle) [$fFile assembleWcsHeader $graphHandle NOWCS]
    }

    set x_label "X"
    set y_label "Y"

#puts "powWCS($graphHandle): $powWCS($graphHandle)"
#puts "powFitsHeader($graphHandle): $powFitsHeader($graphHandle)"
    set powFitsHeader(imgobj_$graphHandle) $powFitsHeader($graphHandle)
    set powFitsHeaderCnt(imgobj_$graphHandle) $powFitsHeaderCnt($graphHandle)

    set useWCSInfo(imgobj_$graphHandle) $useWCSInfo($graphHandle)
    set powWCSName(imgobj_$graphHandle) 0
    powCreateImage imgobj_$graphHandle $graphHandle 0 0 $xSize $ySize 1 1 1 1 $x_label $y_label value

    set powWCSList(imgobj_$graphHandle) {}

    if [info exist powWCSToken($graphHandle)] {
       for { set wi 1 } { $wi < [llength $powWCSToken($graphHandle)] } { incr wi } {
           # call fits_copy_cell2image to translate the keyword and wcs for other projection
           set dest [lindex $powWCSToken($graphHandle) $wi]
           set result [$fFile translateKeyWords $_tType($colIdx) $slice $dest $tempFile]
           set powWCSInfo($graphHandle,$dest) [lindex $result 2]
#puts "powWCSInfo($graphHandle,$dest): $powWCSInfo($graphHandle,$dest)"
#powDebugDataPrint "$graphHandle" [lindex $result 0]
       }
       lappend powWCSList(imgobj_$graphHandle) [llength $powWCSToken($graphHandle)]]
       lappend powWCSList(imgobj_$graphHandle) $powWCSToken($graphHandle)
    } else {
       lappend powWCSList(imgobj_$graphHandle) 1
       lappend powWCSList(imgobj_$graphHandle) {}
    }

    set powWCSName($graphHandle) 0
    set powWCSList($graphHandle) $powWCSList(imgobj_$graphHandle)
    set powWCSList(${graphHandle}scope) $powWCSList(imgobj_$graphHandle)

#puts "powWCSList($graphHandle): $powWCSList($graphHandle)"
    set powPlotParam(graphType,$graphHandle) [string tolower [lindex [$fh info hdutype] 0]]
    set powPlotParam(graphType,${graphHandle}scope) $powPlotParam(graphType,$graphHandle)
    set powPlotParam(zoomed,$graphHandle) 0
    set powPlotParam(zoomed,${graphHandle}scope) 0

    set xCount($graphHandle) 0
    set yCount($graphHandle) 0
    set xCount(${graphHandle}scope) 0
    set yCount(${graphHandle}scope) 0

    set x_unit "pixels"
    set y_unit "pixels"
    set z_label "counts"

#powDebugDataPrint "$graphHandle" $powFitsHeader($graphHandle)

    for {set wi 0} {$wi < [lindex $powWCSList($graphHandle) 0]} {incr wi} {
        set wcsCoord [lindex $powWCSList($graphHandle) 1]
        set wcsName [lindex $wcsCoord $wi]
        set appendStr ""
        if { $wcsName != "DEFAULT" } {
           set appendStr $wcsName
        }

        if { ![catch {set tmp [_getKeywordValue CTYPE1$appendStr $powFitsHeader($graphHandle)]}] } {

           set x_label $tmp
           if { $appendStr == "" } {
              set wcstmp_x [string trim [lindex [lindex $wcsinfo 3] 0]]
              if { $wcstmp_x != "" } {
                 set x_label $wcstmp_x
              }
           }
        }
        if { ![catch {set tmp [_getKeywordValue CTYPE2$appendStr $powFitsHeader($graphHandle)]}] } {
           set y_label $tmp
           if { $appendStr == "" } {
              set wcstmp_y [string trim [lindex [lindex $wcsinfo 3] 1]]
              if { $wcstmp_y != "" } {
                 set y_label $wcstmp_y
              }
           }
        }
        if { ![catch {set tmp [_getKeywordValue CUNIT1$appendStr $powFitsHeader($graphHandle)]}] } {
           if { $tmp != "" } {
              set x_unit $tmp
           }
        }
        if { ![catch {set tmp [_getKeywordValue CUNIT2$appendStr $powFitsHeader($graphHandle)]}] } {
           if { $tmp != "" } {
              set y_unit $tmp
           }
        }
        if { ![catch {set tmp [_getKeywordValue BUNIT$appendStr $powFitsHeader($graphHandle)]}] } {
           if { $tmp != "" } {
              set z_label $tmp
           }
        }

        set powWCSLabel(xlabel,imgobj_$graphHandle,$wcsName) $x_label
        set powWCSLabel(ylabel,imgobj_$graphHandle,$wcsName) $y_label
        set powWCSLabel(xunit,imgobj_$graphHandle,$wcsName) $x_unit
        set powWCSLabel(yunit,imgobj_$graphHandle,$wcsName) $y_unit

        set powWCSLabel(xlabel,$graphHandle,$wcsName) $x_label
        set powWCSLabel(ylabel,$graphHandle,$wcsName) $y_label
        set powWCSLabel(xunit,$graphHandle,$wcsName) $x_unit
        set powWCSLabel(yunit,$graphHandle,$wcsName) $x_unit
    }

    # Pan Chai - and we use DEFAULT wcs to construct first image/plot
    powCreateGraph $graphHandle NULL imgobj_$graphHandle \
                   $powWCSLabel(xunit,imgobj_$graphHandle,DEFAULT) \
                   $powWCSLabel(yunit,imgobj_$graphHandle,DEFAULT) \
                   $powWCSLabel(xlabel,imgobj_$graphHandle,DEFAULT) \
                   $powWCSLabel(ylabel,imgobj_$graphHandle,DEFAULT) \
                   [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 0] 

    if [info exists xo] {
       powMoveGraphTo $graphHandle $xo $yo .pow.pow
       foreach el [list xTop yTop xBot yBot xo yo graphWidth graphHeight xmagstep ymagstep] {
          unset powPlotParam($el,$oldgn) 
       }
       powDeleteGraph $oldgn NOPROMPT
       powDeleteImage $oldgn $oldgn
       # set _graphIDhighest [expr $_graphIDhighest - 1]
       powReconfigureToplevel 1
    }

    if { [info exists xFactor] && [info exists yFactor] } {
       #powStretchGraph $graphHandle $xFactor $yFactor
    }

    incr _graphIDhighest
    catch {$fh close} err
    set powPlotParam(zoomed,$currimg) 0
}

itcl::body Table::_getKeywordValue { keyword header } {
     set k 0
     set tokenStr ""
     for {set i 0} {$i < [string length $header]} {incr i 80} {
        set currentStr [string range $header $i [expr $i + 79]]
        set strToken [split $currentStr "="]
        set sKeyword [lindex $strToken 0]
        if { [string trim $sKeyword] == $keyword } {
           set tokenStr $currentStr
           break
        }
        incr k
     }

     set valueStr [lindex [split [string range $tokenStr 10 end] "/"] 0]
     regsub -all {'} $valueStr {} returnValue
     return [string trim $returnValue]
}

itcl::body Table::_displayMovie { colIdx slice } {
    global g_titleFont

    set slice [expr $_firstRow + $slice]
    set slice [_determineRealSlice $slice]

    set xSize    [lindex $_arrayDim($colIdx) 0]
    set ySize    [lindex $_arrayDim($colIdx) 1]
    set _istart  1
    set _iend    [lindex $_arrayDim($colIdx) 2]

    # image selection
    if { [winfo exist .fv_imgsel] == 1 } {
       destroy .fv_imgsel
    }
    powToplevel  .fv_imgsel .dummy
    wm title     .fv_imgsel "fv: Image Selection"

    label .fv_imgsel.label -text "The image has $_iend slices" \
            -relief ridge -borderwidth 4 -font g_titleFont
 
    label .fv_imgsel.sl  -width  5  -text "Start" -font g_titleFont
    entry .fv_imgsel.se  -width 10  -textvariable [itcl::scope _istart] -font g_titleFont
    label .fv_imgsel.el  -width  5  -text "End" -font g_titleFont
    entry .fv_imgsel.ee  -width 10  -textvariable [itcl::scope _iend] -font g_titleFont
   
    button .fv_imgsel.anim -text "Animate" \
                           -command "[itcl::code $this _createMovie $colIdx $slice $xSize $ySize]
                                     destroy .fv_imgsel" -font g_titleFont
    button .fv_imgsel.cancel -text "Cancel" \
                             -command "destroy .fv_imgsel" -font g_titleFont
    button .fv_imgsel.help -text "Help" \
                             -command "hhelp 3D-ImageDisplay" -font g_titleFont
 
 
    grid .fv_imgsel.label  -column 0 -row 0  -columnspan 4 -sticky "snew"
    grid .fv_imgsel.sl     -column 0 -row 1
    grid .fv_imgsel.se     -column 1 -row 1
    grid .fv_imgsel.el     -column 2 -row 1
    grid .fv_imgsel.ee     -column 3 -row 1
    grid .fv_imgsel.anim   -column 0 -row 2
    grid .fv_imgsel.cancel -column 1 -row 2
    grid .fv_imgsel.help   -column 3 -row 2
}

itcl::body Table::_createMovie { colIdx slice xSize ySize } {
    global powWCS powFitsHeader powFitsHeaderCnt powPlotParam
    global xCount yCount
    global powWCSName powWCSTranslation powWCSLabel powWCSList
    global useWCSInfo
    global g_backupDir

    if { [string first "ftp" $fileName] >= 0 } {
       set fileName [format "%s/%s/%s" $::env(HOME) $g_backupDir [file tail $fileName]]
    }

    regsub -all { } [urlTail $fileName] _ cleanFileName
    # set imgHandle ${cleanFileName}_[expr $currentHDU-1]_$_graphIDhighest
    set imgHandle ${cleanFileName}_[expr $currentHDU-1]

    if { [winfo exist .pow.pow]!=1 } { 
	powInit .dummy
    }

    # open the fits file
    set fh [fits open $fileName 0] 
   
    # move down on HDU to get to the really data
    $fh move +[expr ${_extNum} - 1]

    set x_label "X"
    set y_label "Y"
    set x_unit "pixel"
    set y_unit "pixel"
    if { $fvPref::ifWCSInfo } {
       set result [$fFile getHeader2String]
       set cntList($imgHandle) [$fFile getHeaderKeyWord [lindex $result 0] $imgHandle]

       set powFitsHeaderCnt($imgHandle) [lindex $cntList($imgHandle) 1]
       if { [lindex $cntList($imgHandle) 0] > 0 } {
          #set powFitsHeader($imgHandle) [$fFile assembleWcsHeader $imgHandle]
          set powFitsHeader($imgHandle) [lindex $result 0]
          set powWCSInfo($imgHandle,DEFAULT) $powWCS($imgHandle)
          set wcsinfo $powWCS($imgHandle)

          set x_label [lindex [lindex $wcsinfo 3] 0]
          set y_label [lindex [lindex $wcsinfo 3] 1]
          if { $x_unit=="pixels" } {set x_unit NULL}
          if { $y_unit=="pixels" } {set y_unit NULL}
       } else {
          set powFitsHeader($imgHandle) [$fFile assembleWcsHeader $imgHandle NOWCS]
          set powWCS($imgHandle) [$fFile getWcs]
       }

    } else {
       set powWCS($imgHandle) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
       set powFitsHeader($imgHandle) ""
       set powFitsHeaderCnt($imgHandle) 0
    }

    set powWCSList($imgHandle) {}
    set powWCSList(${imgHandle}scope) {}

    set imgList {}
    for {set i [expr $_istart - 1]} {$i < $_iend} {incr i} {
        # fitsObj load arrayRow <column name> <row number> <number element> <null string> <first element>
        set alist [$fh load arrayRow $_tType($colIdx) $slice [expr $xSize * $ySize] NULL [expr $i * $xSize * $ySize + 1]]
        eval powCreateData ${imgHandle}_$i $alist
        if [info exists powWCS(${imgHandle}_$i)] {
           catch { unset powWCS(${imgHandle}_$i) }
        }
        set powFitsHeader(${imgHandle}_$i) $powFitsHeader($imgHandle)
        set powFitsHeaderCnt(${imgHandle}_$i) $powFitsHeaderCnt($imgHandle)
        catch { set powWCS(${imgHandle}_$i) $powWCS($imgHandle) }

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

        powCreateImage ${imgHandle}_$i ${imgHandle}_$i 0 0 $xSize $ySize 1 1 1 1 pixels pixels value
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

        lappend imgList ${imgHandle}_$i
    }

    set powPlotParam(graphType,$imgHandle) [string tolower [lindex [$fh info hdutype] 0]]
    set powPlotParam(graphType,${imgHandle}scope) $powPlotParam(graphType,$imgHandle)
    set powPlotParam(zoomed,$imgHandle) 0
    set powPlotParam(zoomed,${imgHandle}scope) 0

    set xCount($imgHandle) 0
    set yCount($imgHandle) 0
    set xCount(${imgHandle}scope) 0
    set yCount(${imgHandle}scope) 0

    powCreateGraph $imgHandle NULL $imgList $x_unit $y_unit \
                   $x_label $y_label [lindex $fvPref::graphDispSize 0] \
                       [lindex $fvPref::graphDispSize 1]

    if { [info exists xFactor] && [info exists yFactor] } {
       #powStretchGraph $imgHandle $xFactor $yFactor
    }

    catch {$fh close} err
    # incr _graphIDhighest

    powMovie
    after 1 powPlayMovie
}

itcl::body Table::_makeHistogram { {extNum_ {}} } {
    global g_histoParam
    global g_histoFileID
    global g_backupDir
    global g_fitsFileMode

    set oldMode $g_fitsFileMode
    # Set Read-Only flag
    set g_fitsFileMode 1

    set fileName ""
    set fileName [file join $g_backupDir histo.tmp$g_histoFileID]
    if ![file exists $fileName] {
       # eval $fFile makeHistogram $g_histoParam
    }

    set foundFlag false
    set fftmp [openFitsFile $fileName]
    incr g_histoFileID

    $fftmp plotHisto
    $fftmp changeFile
    if { $extNum_ != "" } {
       $fftmp plotData $extNum_
    }

    set g_fitsFileMode $oldMode
}

# called by FitsPlotSel::_plotCmd
# any of the input axes (x, xe, y, ye) can also be expressions
# not just a column name
#
itcl::body Table::_plotCols { xColumn_ xeColumn_ yColumn_ yeColumn_ {inCurrGraph_ 0} {whichRows_ "-"} {plotSelOnly_ 0}} {

    global powWCS powFitsHeader powFitsHeaderCnt
    global xFactor yFactor powPlotParam currimg
    global powPlotColumnDataName
    global xCount yCount
    global powWCSName powWCSTranslation powWCSLabel powWCSList
    global useWCSInfo
 
    set xColumn $xColumn_
    set yColumn $yColumn_
    set cbCurrgn 0
    if { $_ftp != 0 } {
       set cbCurrgn [$_ftp returnAddMyCurve]
    }

    if { $whichRows_ == "" || $whichRows_ =="-" } {
        set whichRows_ "1-$_numRows"
    }

    if { $plotSelOnly_ == 1 } {
        set selrange ""
        set tmplist [split $whichRows_ ","]
        set numlist [llength $tmplist]

        for { set k 0 } {$k <$numlist } {incr k} {
            set start_end  [lindex $tmplist $k]
            if { $selrange =="" } {
                append  selrange [_parseToRowRange $start_end]
            } else {
                append  selrange "," [_parseToRowRange $start_end]
            }
        }
        set whichRows_ $selrange
     }

    set inputXcolumn $xColumn_
    set inputYcolumn $yColumn_

    if { $fvPref::imgDisplayer == "DS9" } {
       # display is for DS9
#       if { $xeColumn_ != "" || $yeColumn_ != "" } {
#          tk_messageBox -icon error -type ok \
#                        -message "ds9 can't display either X error and Y error, try POW instead."
#          return
#       }
 
       # remove constraint of using all CAPs for column name
       # set xColumn_ [string toupper $xColumn_ ]
       # set yColumn_ [string toupper $yColumn_ ]

       set colIdx -1
       foreach name $_listPreSelectedColNames {
             if { [string first $name $xColumn_] >= 0 } {
                  set colIdx [lsearch $_listPreSelectedColNames  $name ]
                  break
             }
       }

       if { $colIdx < 0 } {
             foreach name $_listPreSelectedColNames {
                if { [string first $name $yColumn_] >= 0 } {
                   set colIdx [lsearch $_listPreSelectedColNames  $name ]
                   break
                }
             }
       }

       if { $colIdx < 0 } {
          # both xcolumn and ycoloumn are not in the _listPreSelectedColNames
          tk_messageBox -icon error -type ok \
                        -message "Please select one of the following:\n\n$_listPreSelectedColNames"
          return
       }
#puts $whichRows_

       set flag "NOT_YET"
#       set flag [_ds9MakePlot NONE 1 $_numRows $xColumn_ $yColumn_ $xeColumn_ $yeColumn_]
       set flag [_ds9MakePlot1 NONE $whichRows_ $xColumn_ $yColumn_ $xeColumn_ $yeColumn_ $inCurrGraph_]
       if { $flag == "DONE" } return
    }

    if { $whichRows_ == "-" } {
	set nRows $_numRows
    } else {
	set nRows [range count $whichRows_ $_numRows]
    }

    regsub -all { } [urlTail $fileName] _ cleanFileName
    
    set graphID table_${cleanFileName}_$_graphIDhighest
    
    #
    #  Analyze the expressions
    #

    foreach axis [list x y xe ye] {
	set axisExpression [subst \$${axis}Column_]

#puts "axisExpression: $axisExpression"
	
	# for example: set exprData(x,expr) $xColumn_
	set exprData($axis,expr) $axisExpression
	if { $axisExpression == "" || $axisExpression == "NULL" } {
	    
	    #  No expression supplied.  OK for errorbars, not allowed for x/y
	    
	    if { $axis == "x" || $axis == "y" } {
		error "[string toupper $axis] axis is empty"
	    } else {
		# empty errorbar
		set exprData($axis,nelem) 0
		set exprData($axis,name1) "NULL"
	    }
	    
	} elseif { $axisExpression == "ElementNumber" } {
	    
	    #   Have to delay data creation until we know how big
	    #   the other axes are
	    
	    set exprData($axis,nelem) 1
	    set exprData($axis,name1) "NULL"
	    
	} else {
	    
	    #  nelem value will be -1 for scalar constants
	    set eData [_getDataForAxis $axis $axisExpression $whichRows_]
	    
	    foreach p [list ptr type len nelem dim] v $eData {
		set exprData($axis,$p) $v
		set $p $v
	    }
	    
	}
    }

    #
    #  Create the vectors
    #
    
    set nelem [getmax $exprData(x,nelem)  $exprData(y,nelem) \
	    $exprData(xe,nelem) $exprData(ye,nelem)]
    if { $nelem == 1 } {
	# All parameters are scalars
	set nelem $nRows
    } elseif { $nelem < 1 } {
	# All parameters are constants or blank, plot just solitary point
	set nelem 1
    }
    foreach axis [list x y xe ye] {
	if { $exprData($axis,expr) == "ElementNumber" } {
	    
	    set exprData($axis,cnt)   1
	    set exprData($axis,name1) ${axis}1_v_$graphID
	    set exprData($axis,len)   $nelem
	    powCreateVectorEN $exprData($axis,name1) ElementNumber_$graphID \
		    $nelem 1 1 NULL

            set powPlotColumnDataNameTmp 1_v_$graphID
	    
	} elseif { $exprData($axis,nelem) == -1 } {
	    
	    set val [ptr2lst $exprData($axis,ptr) $exprData($axis,type) 1]
	    set exprData($axis,cnt)   1
	    set exprData($axis,name1) ${axis}1_v_$graphID 
	    set exprData($axis,len)   $nelem
	    powCreateVectorEN $exprData($axis,name1) ${axis}_$graphID \
		    $nelem $val 0 NULL
	    fits free $exprData($axis,ptr)
            set powPlotColumnDataNameTmp 1_v_$graphID
	    
	} elseif { $exprData($axis,nelem) != 0 } {
	    
	    #   Test for vector-length compatibility amongst axes
#puts "exprData($axis,len):  $exprData($axis,len)"
	    if { $exprData($axis,nelem) != $nelem && \
		    ($exprData($axis,len) != $nelem || \
		    $exprData($axis,nelem) != 1) } {
		error "[string toupper $axis] axis data length is\
			incompatible with other axes"
	    }
	    
	    powCreateData ${axis}_$graphID $exprData($axis,ptr) \
		    $exprData($axis,type) $exprData($axis,len) -1
	    
            set powPlotColumnDataNameTmp _$graphID
	    set cnt 0
	    set offset 0
	    while { $offset < $exprData($axis,len) } {
		incr cnt
		set exprData($axis,name$cnt) ${axis}${cnt}_v_$graphID
		powCreateVector $exprData($axis,name$cnt) ${axis}_$graphID \
			$offset $nelem 1
		incr offset $nelem
	    }
	    set exprData($axis,cnt) $cnt
	    
	} else {
	    
	    set exprData($axis,cnt) 1
	    
	}
    }
    

    # get the x and y units
    set xUnit NULL
    set yUnit NULL
    if { $_tableType != "Vector Table"} {
	for {set i 0} {$i < $_dispCols} {incr i} {
	    if { $_columnName($i) == $xColumn_} {
		set xUnit $_columnUnit($i)
		if {$xUnit == ""} {set xUnit NULL}
	    }
	    if { $_columnName($i) == $yColumn_} {
		set yUnit $_columnUnit($i)
		if {$yUnit == ""} {set yUnit NULL}
	    }
	}
    }
    
    if { [string range $yColumn_ 0 0] == "\$" } {
       set yColumn_ [string range $yColumn_ 1 [expr [string length $yColumn_] - 2]]
    }
    if { [string range $xColumn_ 0 0] == "\$" } {
       set xColumn_ [string range $xColumn_ 1 [expr [string length $xColumn_] - 2]]
    }

    set gColumnName $yColumn_
    set gWhichRows  $whichRows_
    set lWhichRows [split $gWhichRows ","]
    if { [llength $lWhichRows] > 1 } {
       set gWhichRows ""
       append gWhichRows [lindex $lWhichRows 0]
#       append gWhichRows ","
#       append gWhichRows [lindex $lWhichRows 1]
       append gWhichRows "..."
    }


    if { $whichRows_ == "" || $whichRows_ == "-" } {
       set graphHandle ${cleanFileName}_[expr $currentHDU-1]_$_graphIDhighest
    } else {
       foreach name $_listPreSelectedColNames {
           if { [string first $name $gColumnName] == 0 } {
              set gColumnName $name
              break
           }
       }
       set graphHandle [format "%s\(%s_%s)_%s" ${cleanFileName} $gColumnName $gWhichRows $_graphIDhighest]
    } 
    
    regsub -all { } $graphHandle "" graphHandle
    set oldplot ""

    if { (![info exists currimg] || $currimg=="NULL" || $currimg=="") && $cbCurrgn == 0 } {
       if { $_neverAskFlagValue == "false" } {
          _determineOverWriteFlag
          if { $_opCancel == "true" } return
       }

       # the graph is not already on .pow.pow, if _overWriteImage is yes, then delete the previous
       # selected image. 
       if { $_overWriteImage == "yes" } {
          if { [powGetCurrentGraph] != "powDef" } {
             set currplot [powGetCurrentGraph]
             set oldplot $currplot
             set xo $powPlotParam(xo,$currplot)
             set yo $powPlotParam(yo,$currplot)
          }
       }
    }

    if { [winfo exist .pow.pow]!=1 } { 
	powInit .dummy
    }

    set powWCSLabel(xlabel,$graphHandle,DEFAULT) $xColumn_
    set powWCSLabel(ylabel,$graphHandle,DEFAULT) $yColumn_
    set powWCSLabel(xunit,$graphHandle,DEFAULT) $xUnit
    set powWCSLabel(yunit,$graphHandle,DEFAULT) $yUnit

    # get the wcs info if it exists
    set useWCSInfo(${graphHandle}scope) $fvPref::ifWCSInfo
    set useWCSInfo($graphHandle) $fvPref::ifWCSInfo

    if { $_tableType != "Vector Table" && $fvPref::ifWCSInfo } {
	# Get the WCS info (if needed) and pass them to pow
	
	set RAColNum [lsearch $_listColNames $xColumn_]
	set DecColNum [lsearch $_listColNames $yColumn_]
	if { ($RAColNum != -1) && ($DecColNum != -1) } {
	    incr RAColNum
	    incr DecColNum
	    
            # for now, wcsinfo at plot is set to none
	    set wcsinfo [$fFile getWcs {} $RAColNum $DecColNum]
	    if { [lindex $wcsinfo 4] != "none" } {
               set testCnt 0
               foreach item [lindex $wcsinfo 4] {
                   if { [llength $item] == 0 } {
                      incr testCnt
                   }
               }
               if { $testCnt != [llength [lindex $wcsinfo 4]] } {
                  set xColumn_ [lindex [lindex $wcsinfo 3] 0]
	          set yColumn_ [lindex [lindex $wcsinfo 3] 1]
	          set xUnit "NULL"
	          set yUnit "NULL"
                  set powWCSLabel(xlabel,$graphHandle,DEFAULT) $xColumn_
                  set powWCSLabel(ylabel,$graphHandle,DEFAULT) $yColumn_
                  set powWCSLabel(xunit,$graphHandle,DEFAULT) $xUnit
                  set powWCSLabel(yunit,$graphHandle,DEFAULT) $yUnit
               }
	    }
	}
    }
    
    foreach axis [list x y xe ye] {
	set ${axis}i 1
	set looping($axis) 1
    }
    set curves ""
    set cnt 0

    set powWCSName($graphHandle) 0
    while { $looping(x) || $looping(y) || $looping(xe) || $looping(ye) } {
	incr cnt
	set curveName c${cnt}_$graphHandle
        set powWCSName($curveName) 0
        set powWCSName(${curveName}scope) 0
        set useWCSInfo($curveName) $fvPref::ifWCSInfo
        set useWCSInfo(${curveName}scope) $fvPref::ifWCSInfo
        if { [info exists wcsinfo] && $wcsinfo!="" } {
           set result [$fFile getDummyHeader2String {} $RAColNum $DecColNum]
           set powFitsHeader($curveName) [lindex $result 0]
           set powWCS($curveName) [lindex $result 2]
           set powWCSInfo($curveName,DEFAULT) $powWCS($curveName)
           set cntList($curveName) [$fFile getHeaderKeyWord [lindex $result 0] $curveName]
           set powFitsHeaderCnt($curveName) [lindex $cntList($curveName) 1]
           set wcsinfo $powWCS($curveName)
        } else {
           set useWCSInfo($curveName) 0
           set useWCSInfo(${curveName}scope) 0
           set powWCS($curveName) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
           set powFitsHeader($curveName) ""
           set powFitsHeaderCnt($curveName) 0
        }
        set powPlotParam(wcsName,$curveName) "WCS"
        set powPlotParam(graphType,$curveName) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
        set powPlotParam(zoomed,$curveName) 0
        set powPlotColumnDataName($curveName) $powPlotColumnDataNameTmp
        set xCount($curveName) 0
        set yCount($curveName) 0

        set powPlotParam(wcsName,$curveName) "WCS"
	powCreateCurve $curveName $exprData(x,name$xi) $exprData(xe,name$xei) \
		$exprData(y,name$yi) $exprData(ye,name$yei)

        if [info exists powWCSList($curveName)] {
           foreach name [lindex $powWCSList($curveName) 1] {
              $fFile assembleWcsLabel $curveName $name
           }
        } else {
           set powWCSList($curveName) {}
           lappend powWCSList($curveName) 1
           lappend powWCSList($curveName) {}
        }

        set powWCSList(${curveName}scope) $powWCSList(${curveName})

	lappend curves $curveName
	foreach axis [list x y xe ye] {
	    incr ${axis}i
	    if { [subst \$${axis}i] > $exprData($axis,cnt) } {
		set ${axis}i 1
		set looping($axis) 0
	    }
	}
    }
    
    set realGraphHandle $graphHandle

    # wcslist can be obtained from any one slice
    if [info exists powWCSList($curveName)] {
       set powWCSList($graphHandle) $powWCSList($curveName)
       set powWCSList(${graphHandle}scope) $powWCSList($curveName)

       foreach name [lindex $powWCSList($graphHandle) 1] {
          $fFile assembleWcsLabel $graphHandle $name
       }
    } else {
       set powWCSList($graphHandle) {}
       lappend powWCSList($graphHandle) 1
       lappend powWCSList($graphHandle) {}
    }

    set powWCSList(${graphHandle}scope) $powWCSList($graphHandle)
    set powWCSName(${graphHandle}scope) 0
    set powPlotParam(wcsName,$graphHandle) "WCS"
    set powWCS($graphHandle) $powWCS(c1_$graphHandle)
    set powWCS(${graphHandle}scope) $powWCS(c1_$graphHandle)
    set powFitsHeader($graphHandle) $powFitsHeader(c1_$graphHandle)
    set powFitsHeaderCnt($graphHandle) $powFitsHeaderCnt(c1_$graphHandle)
    set powFitsHeader(${graphHandle}scope) $powFitsHeader(c1_$graphHandle)
    set powFitsHeaderCnt(${graphHandle}scope) $powFitsHeaderCnt(c1_$graphHandle)
    set powPlotColumnDataName($graphHandle) $powPlotColumnDataName(c1_$graphHandle)

    set powPlotParam(graphType,$graphHandle) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${graphHandle}scope) $powPlotParam(graphType,$graphHandle)
    set powPlotParam(zoomed,$graphHandle) 0
    set powPlotParam(zoomed,${graphHandle}scope) 0
    set xCount($graphHandle) 0
    set yCount($graphHandle) 0
    set xCount(${graphHandle}scope) 0
    set yCount(${graphHandle}scope) 0

    if { $inCurrGraph_ && [powGetCurrentGraph]!="" } {
       powAddCurves [powGetCurrentGraph] $curves
    } else {
       if { $powFitsHeader($graphHandle) == "" } {
          set useWCSInfo($graphHandle) 0
          set useWCSInfo(${graphHandle}scope) 0
       }
       set errorFlag [ catch { powCreateGraph $graphHandle $curves NULL $xUnit $yUnit \
                               $xColumn_ $yColumn_ \
                               [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1] } err]

       if { $errorFlag } {
           catch { powDeleteGraph $graphHandle NOPROMPT
                   powDeleteImage $graphHandle $graphHandle
                   powDeleteCurve $graphHandle curve }

           set wcsinfo ""
           set powWCS($curveName) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
           set powFitsHeader($curveName) ""
           set powFitsHeaderCnt($curveName) 0

           set powWCS($graphHandle) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
           set powWCS(${graphHandle}scope) $powWCS($curveName)
           set powFitsHeader($graphHandle) $powFitsHeader($curveName)
           set powFitsHeaderCnt($graphHandle) $powFitsHeaderCnt($curveName)
           set powFitsHeader(${graphHandle}scope) $powFitsHeader($curveName)
           set powFitsHeaderCnt(${graphHandle}scope) $powFitsHeaderCnt($curveName)
           set powPlotParam(FixedAspect,$graphHandle) "no"

           set xCount($graphHandle) 0
           set yCount($graphHandle) 0
           set xCount(${graphHandle}scope) 0
           set yCount(${graphHandle}scope) 0

           set errorFlag [ catch { powCreateGraph $graphHandle $curves NULL $xUnit $yUnit \
                                   $xColumn $yColumn \
                                   [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1] } err ]

           if { $errorFlag } {
              tk_messageBox -icon error -type ok -message "Can't plot graph"
              return
           }
       }
    }

    if { $xColumn_ == "ElementNumber" || $yColumn_ == "ElementNumber" ||
         $xColumn_ == "RowNumber" || $yColumn_ == "RowNumber" } {
       powSetCurveOptions $graphHandle $curves pDisp No lDisp Yes
    } else {
#puts "exprData(x,name1): $exprData(x,name1)"
#puts "exprData(y,name1): $exprData(y,name1)"
       # we need to determine if xColumn_ or yColumn_ increased monotonic
       set key [$fFile getKeyword NAXIS2]
       set value "[string trim [lindex [lindex $key 0] 1] {' ()}]"
       regsub -all {[a-zA-Z]} $value {} lRow
#puts "value: $value"
       # set lRow [string range $value 0 [expr [string length $value] - 2]]
       # set lRow $value

       set checkFlag PASSED

       set range "1-$lRow"
#puts "_listPreSelectedColNames: $_listPreSelectedColNames"
       foreach name $_listPreSelectedColNames {

           if { [string first $name $inputXcolumn] >= 0 } {
#              if { $name != $inputXcolumn} {
#                 set checkFlag FAILED
#              }
              set searchX $name
           }
           if { [string first $name $inputYcolumn] >= 0 } {
#              if { $name != $inputYcolumn} {
#                 set checkFlag FAILED
#              }
              set searchY $name
           }
       }

       if [info exists searchX] {
          set dataX [$fFile getVectorTableAsRawList $searchX 1 $range]
          set dataY [$fFile getVectorTableAsRawList $searchY 1 $range]

          # first step, compare the first two elements
          foreach dataList [list $dataX $dataY] {
              if { $checkFlag == "FAILED" } break
              set tokens [split $dataList " "]
              set direction up
              set previousValue -99

              if { [lindex $tokens 0] > [lindex $tokens 1] } {
                 set direction down
                 set previousValue 9999999
              }

              set i 1
              foreach value $tokens {
                 incr i
                 if { $direction == "down" && $value > $previousValue } {
                    set checkFlag FAILED
                    break
                 }
                 if { $direction == "up" && $value < $previousValue } {
                    set checkFlag FAILED
                    break
                 }
                 set previousValue $value
              }
              if { $checkFlag == "PASSED" } break
          }
       }

       if { $checkFlag == "FAILED" } {
          powSetCurveOptions $realGraphHandle $curves pDisp Yes lDisp No
       } else {
          if { $xeColumn_ != "" || $yeColumn_ != "" } {
             powSetCurveOptions $realGraphHandle $curves pDisp Yes lDisp No
          } else {
             powSetCurveOptions $realGraphHandle $curves pDisp No lDisp Yes
          }
       }

    }

    if [info exist xo] {
       powMoveGraphTo $graphHandle $xo $yo .pow.pow
       foreach el [list xo yo graphWidth graphHeight xmagstep ymagstep] {
          unset powPlotParam($el,$oldplot)
       }
       powDeleteGraph $oldplot NOPROMPT
       powDeleteImage $oldplot $oldplot
       powDeleteCurve $oldplot curve
       # set _graphIDhighest [expr $_graphIDhighest - 1]
       powReconfigureToplevel 1
    }

    if { [info exists xFactor] && [info exists yFactor] } {
       #powStretchGraph $graphHandle $xFactor $yFactor
    }

    incr _graphIDhighest
}

itcl::body Table::_relocateDividLine {x_ index_ } {
# note index_ is not used, but I don't know why  --Han
    set dx [expr $x_ - $_startX]
    set _startX $x_
    $_droot.table.can move tmpLine $dx 0
}

itcl::body Table::_startDividLine {x_ index_ } {
# note index_ is not used, but I don't know why  --Han
    if { $x_ > $_endX } {
       set _endX $x_
    }
    $_droot.table.can create line $x_ 0 $x_ $_DC(ysize) \
	    -width 4 -fill red -tag tmpLine
    set _startX $x_
}

itcl::body Table::_finalDividLine {x_ index_} {
# note index_ *is* used in this method
#puts "finalDividLine start"
    global g_charPix

    $_droot.table.can delete tmpLine
    set dx [expr $x_ - $_xPos($index_)]

    if { $index_ == 0} {
        set moveX $dx
        set _DC(width) [expr $_DC(width)+$dx]
        $_droot.table.can move check [expr $moveX/2] 0
        $_droot.table.can itemconfigure check -width $_DC(width)
# _DC(width) is an important param. it will effect several other things.
# Need to do extra here
        _calAbsXPos
        _calXPos $_firstCol $_showCols
        _getLastFirstCol
    } else {
        set prIndex [expr $index_ -1 ]
        set dx1 [expr $x_ - $_xPos($prIndex)]
        set prAbsIndex [expr $prIndex + $_firstCol -1 ]

        if { [expr abs($dx)] < $g_charPix} return

        if { $x_ < $_xPos($prIndex) } return

        set tmp $_cellPixWidth($prAbsIndex)
        set _cellWidth($prAbsIndex) [expr $dx1/$g_charPix ]
        _calAbsXPos
        set moveX [expr $_cellPixWidth($prAbsIndex) -$tmp]

        _calXPos $_firstCol $_showCols
        _getLastFirstCol

        $_droot.table.hdr itemconfigure tempColData_$prIndex \
            -width $_cellPixWidth($prAbsIndex)

        $_droot.table.can itemconfigure entry_$prIndex \
               -width $_cellPixWidth($prAbsIndex)
        $_droot.table.can move entry_$prIndex [expr $moveX/2]  0

        if [winfo exists $_droot.table.can.tdim${prIndex}_0] {
           $_droot.table.can itemconfigure tdim_$prIndex \
               -width $_cellPixWidth($prAbsIndex)
           $_droot.table.can move tdim_$prIndex [expr $moveX/2]  0
        }

        $_droot.table.hdr move tempColData_$prIndex [expr $moveX/2]  0
    }
    for {set i $index_} {$i <= $_showCols} {incr i} {
        $_droot.table.can move colLine_$i $moveX 0
        $_droot.table.can move entry_$i $moveX 0

        if [winfo exists $_droot.table.can.tdim${i}_0] {
           $_droot.table.can move tdim_$i $moveX 0
        }
        $_droot.table.hdr move tempColData_$i $moveX 0
    }

    _setHScroll [expr $_firstCol-1]
    set _endX 0
    _reframe
}


itcl::body Table::_calXPos {fCol_ nCols_} {
#puts "_calXPos start"
    set errExt [ catch {set key [$fFile getKeyword EXTNAME ]} ]

    if { $errExt == 0  && [string first "GROUPING" $key] >= 0  } { set errExt true }

    if { $errExt == "true" } {
       set _xPos(0) [expr [expr $_DC(lmar) + $_DC(width) + \
                                $_DC(rightspace)] * 2 + $_DC(width)/4]
    } else {
       set _xPos(0) [expr $_DC(lmar) + $_DC(width) + $_DC(rightspace)]
    }

    set tmpPos $_absXPos([expr $fCol_-1])

    for {set i 0} {$i < $nCols_} {incr i} {
        set colIndex [expr $fCol_+$i]
        set _xPos([expr $i+1]) [expr $_absXPos($colIndex)-$tmpPos+$_xPos(0)]
    }
}

itcl::body Table::_colMenu { hdr i colName colNum } {
    catch { destroy $_droot.table.hdr.menu$i.menu }
    catch { destroy $_droot.table.hdr.menu$i.viewcolmenu }
    catch { destroy $_droot.table.hdr.menu$i.sort }
    
    if { [catch {$hdr.menu$i configure -text "Modify" -menu $hdr.menu$i.menu}] } {
	menubutton $hdr.menu$i -text "Modify" \
	    -relief raised -font g_titleFont -menu $hdr.menu$i.menu
    }


    menu $hdr.menu$i.menu -tearoff False
    $hdr.menu$i.menu add cascade -label "Sort" \
	-menu $hdr.menu$i.menu.sort \
	-underline 0 -font g_titleFont

    menu $hdr.menu$i.menu.sort -tearoff False
    $hdr.menu$i.menu.sort add command -label "Ascending" \
       -underline 0 -command [itcl::code $this doSort $colName 1 0 ] \
       -font g_titleFont
    $hdr.menu$i.menu.sort add command -label "Descending" \
       -underline 0 -command [itcl::code $this doSort $colName 0 0 ] \
       -font g_titleFont

    menu $hdr.menu$i.menu.showcol -tearoff False
    set ct 0
    set columnbreak 0
    foreach name $_unViewedColNames {
	$hdr.menu$i.menu.showcol add command -columnbreak $columnbreak \
	    -label "$name" -underline 0 -font g_titleFont \
	    -command [itcl::code $this _addColView $name $colName "after"]
	set columnbreak 0
	incr ct
	if { $ct >= 20 } {
	    set columnbreak 1
	    set ct 0
	}
    }

    menu $hdr.menu$i.menu.jumpto -tearoff False
    $hdr.menu$i.menu.jumpto add command \
	-label "Beginning of Table" -underline 0 -font g_titleFont \
	-command [itcl::code $this _jumpTo 0]
    set ct 0
    set columnbreak 0

    set _otherColNameList {}
    foreach name $_listPreSelectedColNames {
        if { $name != $colName } {
            lappend _otherColNameList $name
        }
    }

    foreach name $_otherColNameList {
	$hdr.menu$i.menu.jumpto add command -columnbreak $columnbreak \
	    -label "$name" -underline 0 -font g_titleFont \
	    -command [itcl::code $this _jumpTo $name]
	set columnbreak 0
	incr ct
	if { $ct >= 20 } {
	    set columnbreak 1
	    set ct 0
	}
    }

    $hdr.menu$i.menu.jumpto add command \
	-label "End of Table" -underline 0 -font g_titleFont \
	-command [itcl::code $this _jumpTo "end"]

    $hdr.menu$i.menu add cascade \
	-label "Jump To Column.." -underline 0 -font g_titleFont \
	-menu $hdr.menu$i.menu.jumpto

    if { [llength $_listPreSelectedColNames] <= 1 } {
       $hdr.menu$i.menu entryconfigure 2 -state disabled 
    }

    menu $hdr.menu$i.menu.movebefore -tearoff False
    set ct 0
    set columnbreak 0

    set _otherColNameList {}
    foreach name $_listPreSelectedColNames {
        if { $name != $colName } {
            lappend _otherColNameList $name
        }
    }

    foreach name $_otherColNameList {
	$hdr.menu$i.menu.movebefore add command -columnbreak $columnbreak \
	    -label "$name" -underline 0 -font g_titleFont \
	    -command [itcl::code $this _moveColView $colName $name before]
	set columnbreak 0
	incr ct
	if { $ct >= 20 } {
	    set columnbreak 1
	    set ct 0
	}
    }

    $hdr.menu$i.menu.movebefore add command \
	-label "End of Table" -underline 0 -font g_titleFont \
	-command [itcl::code $this _moveColView $colName "END" after]

    $hdr.menu$i.menu add cascade \
	-label "Move Before.." -underline 0 -font g_titleFont \
	-menu $hdr.menu$i.menu.movebefore

    $hdr.menu$i.menu add cascade \
	-label "Show Column... " -underline 0 -font g_titleFont \
	-menu $hdr.menu$i.menu.showcol
    
    if { [llength $_unViewedColNames] <= 0 } {
       $hdr.menu$i.menu entryconfigure 3 -state disabled 
    }

    $hdr.menu$i.menu add command -label "Hide Column" \
       -underline 0 -command [itcl::code $this _delColView $colName ] \
       -font g_titleFont

    if { [llength $_listPreSelectedColNames] <= 1 } {
       $hdr.menu$i.menu entryconfigure 4 -state disabled 
    }

    $hdr.menu$i.menu add command -label "Insert New Column..." \
        -underline 0 -font g_titleFont \
        -command [itcl::code $this _addCols [expr $i + 1]]

    $hdr.menu$i.menu add command -label "Delete..." \
	-underline 0 -command [itcl::code $this _tryDelCols $colName ] \
       -font g_titleFont

    $hdr.menu$i.menu add command -label "Parameters..." \
       -underline 0 -command [itcl::code $this _editColParamsWindow $colNum ] \
       -font g_titleFont

}

itcl::body Table::_moveColView { colName targetName option } {
      _delColView $colName
      _addColView $colName $targetName $option
}

itcl::body Table::_updateUnViewedColumns {} {
    set _unViewedColNames {}

    if { [catch { set tmplist $_listColNames }] == 0 } {
	foreach name $_listColNames {
	    set tmpPos [lsearch -exact $_listPreSelectedColNames $name]
	    if { $tmpPos == -1 } {
		lappend _unViewedColNames $name
	    }
	}
    }

}

itcl::body Table::_drawTableFrame {} {
    global g_titleFont

    set hdr $_droot.table.hdr
    set can $_droot.table.can
    set _expandFlag false

   # calculate the _xPos
    _calXPos $_firstCol $_showCols

#puts "tableType: $_tableType"
#puts "showCols: $_showCols"
    set deleteFlag false

    if { ($_tableType == "Image") || ($_tableType == "Vector Table") } {
	for {set i 0} {$i < $_showCols } {incr i} {
	    set tmpColName [lindex $_listPreSelectedColNames [expr $_firstCol-1+$i]]
	    set tmpColNum  [expr $_firstCol-1+$i]
	    set tmpColWid($i)   $_cellPixWidth($tmpColNum)
	    set tmpColXPos($i)  [expr $_xPos($i) + $_cellPixWidth($tmpColNum)/2 ]
            if [winfo exists $hdr.name$i] {
               destroy $hdr.name$i
               set deleteFlag true
            }
	    checkbutton $hdr.name$i -text $tmpColName -font g_titleFont \
		-variable [itcl::scope _colSel($this,$i)] \
		-selectcolor $fvPref::checkBBgColor  \
		-activeforeground black -activebackground $fvPref::globalBgColor \
		-command [itcl::code $this _updateColSel $i]
	    $hdr create window $tmpColXPos($i) \
		[expr $_DC(height)/2] -width $_cellPixWidth($i) \
		-window $hdr.name$i -tags tempColData_$i
	}
# presumably more standard "Table" tables
    } else {

        set _fitsFileLocationCol [lsearch -exact $_listPreSelectedColNames "MEMBER_LOCATION"] 

	_updateUnViewedColumns

	for {set i 0} {$i < $_showCols} {incr i} {
	    set tmpColNum  [expr $_firstCol-1+$i]
	    set tmpColName [lindex $_listPreSelectedColNames $tmpColNum ]
	    set tmpColXPos($i)  [expr $_xPos($i) + $_cellPixWidth($tmpColNum)/2 ]
	    set tmpColWid($i)  $_cellPixWidth($tmpColNum)

            if [winfo exists $hdr.name$i] {
               catch { destroy $hdr.name$i }
               catch { destroy $hdr.type$i }
               catch { destroy $hdr.unit$i }
               catch { destroy $hdr.vecB$i }
               catch { destroy $hdr.menu$i }
               set deleteFlag true
            }
	    checkbutton $hdr.name$i -text $_columnName($tmpColNum)  \
		-font g_titleFont -variable [itcl::scope _colSel($this,$i)] \
		-selectcolor $fvPref::checkBBgColor  \
		-activeforeground black -activebackground $fvPref::globalBgColor \
		-command [itcl::code $this _updateColSel $i]
	    label $hdr.type$i -text $_columnType($tmpColNum) \
		-font g_titleFont
	    label $hdr.unit$i -text $_columnUnit($tmpColNum)  \
		-font g_titleFont

	    # Create column header menu
	    _colMenu $hdr $i $tmpColName $tmpColNum

	    if { $_columnVecSize($tmpColNum) > 1 || $_columnVecSize($tmpColNum) < 0 } {
		$hdr.menu$i.menu insert 0 command \
		    -label "Expand" -underline 0 -font g_titleFont \
		    -command [itcl::code $this _openVectorTable $tmpColNum] 

                set _expandFlag true
	    }
	    $hdr create window $tmpColXPos($i) [expr $_DC(height)/2] \
		-width $tmpColWid($i) \
		-window $hdr.name$i -tags tempColData_$i
	    $hdr create window $tmpColXPos($i) [expr $_DC(height)*3/2] \
		-width $tmpColWid($i) \
		-window $hdr.type$i -tags tempColData_$i
	    $hdr create window $tmpColXPos($i) [expr $_DC(height)*5/2] \
		-width $tmpColWid($i) \
		-window $hdr.unit$i -tags tempColData_$i
# CM Begin
	    $hdr create window $tmpColXPos($i) [expr $_DC(height)*7/2] \
		-width $tmpColWid($i) \
		-window $hdr.menu$i -tags tempColData_$i
# CM End
#	    $hdr create window $tmpColXPos($i) [expr $_DC(height)*9/2] \
#		-width $tmpColWid($i) \
#		-window $hdr.vecB$i -tags tempColData_$i
	}
    }

    set _yPos(0) $_DC(tmar)
# Ziqin 
#         _updateNumRows
          _initSelRows
        if { $deleteFlag == "true" } {
           catch { destroy $hdr.lselAll   }
           catch { destroy $hdr.selAll    }
           catch { destroy $hdr.selInvert }
        }

        label $hdr.lselAll -text "Select" \
		-font g_titleFont
        $hdr create window [expr $_DC(lmar)+25+[$hdr.lselAll cget -width]/2] [expr $_DC(height)*3/2] \
	    -width [expr [$hdr.lselAll cget -width]] -height [expr $_DC(height)-1] \
       	    -window $hdr.lselAll -tags check

        checkbutton $hdr.selAll -text "All"  \
		-font g_titleFont  \
                -variable [itcl::scope _selectAllRows ] \
		-selectcolor $fvPref::checkBBgColor  \
		-activeforeground black -activebackground $fvPref::globalBgColor \
		-command [itcl::code $this _selAllRows ]

        button $hdr.selInvert -text "Invert" \
                -font g_titleFont  \
                -justify left \
		-activeforeground black -activebackground $fvPref::globalBgColor \
                -command [itcl::code $this _selInvert ]

	$hdr create window [expr $_DC(lmar)+25+[$hdr.selAll cget -width]/2] \
	    [expr $_DC(height)*5/2]  -window  $hdr.selAll  \
	    -width [expr [$hdr.selAll cget -width]] -height [expr $_DC(height)-1] \
	    -tags check

	$hdr create window [expr $_DC(lmar)+25+[$hdr.selInvert cget -width]/2] \
	    [expr $_DC(height)*7/2]  -window  $hdr.selInvert  \
	    -width [expr [$hdr.selInvert cget -width]] -height [expr $_DC(height)-1] \
	    -tags check

         set indexwidth 56

#         puts $indexwidth
#         if { $indexwidth < $_DC(width) } {
#                set indexwidth $_DC(width)
#         }
#         puts $_DC(width)

#
#	    -width [expr $_DC(width)*2] -height [expr $_DC(height)-1] 
#	$hdr create window [expr $_DC(lmar)+$_DC(width)] 

    for {set j 0} {$j < $_showRows} {incr j} {
	set realRowNum [expr $_firstRow+$j]
	if { $_tableType == "Image"} {
	    set rowIndex [expr $_numRows - $realRowNum+1]
	} else {
	    set rowIndex $realRowNum
	}
        if { $deleteFlag == "true" } {
           catch { destroy $can.b$j }
        }
	label $can.b$j -text $rowIndex -font g_titleFont  

# Modify by Ziqin Pan , Jan 2004
         bind $can.b$j <Button-1> [itcl::code $this _setRowStartMark $j]
         bind $can.b$j <Button-3> [itcl::code $this _setRowEndMark $j]
         bind $can.b$j <Shift-Button-1> [itcl::code $this _setRowEndMark $j]
         bind $can.b$j <Button-2> [itcl::code $this _unsetRowMark ]
# Modify end




	set _yPos([expr $j+1]) [expr $_DC(tmar) + ($j+1)*($_DC(height) + $_DC(interline))]
	$can create window [expr $_DC(lmar)+$_DC(width)/2] \
	    [expr $_yPos($j)+$_DC(height)/2]  -window  $can.b$j  \
	    -width $_DC(width) -height [expr $_DC(height)-1] \
	    -tags check
#	$can create window [expr $_DC(lmar)+$indexwidth/2] \
#	    [expr $_yPos($j)+$_DC(height)/2]  -window  $can.b$j  \
#	    -width $indexwidth -height [expr $_DC(height)-1] \
#	    -tags check

	if { [catch {set key [$fFile getKeyword EXTNAME]}] == 0 } {
           if { [string first "GROUPING" $key] >= 0 } {
              if { $deleteFlag == "true" } {
                 catch { destroy $can.button$realRowNum }
              }
              button $can.button$realRowNum -text "Open" \
                     -font [list Helvetica 10 bold] \
                     -command [itcl::code $this _openGroupFile $_fitsFileLocationCol $realRowNum]

              # create consistence OPEN button
   	      $can create  window [expr $_DC(width)*2+$_DC(lmar)-10] \
	                          [expr $_yPos($j)+$_DC(height)/2] \
		                  -window $can.button$realRowNum \
                                  -height [expr $_DC(height)-5] \
                                  -tags open_$j
           }
        }



    }

    # set the row divide line size
    set _DC(xsize) [expr $_DC(xsize) + $_DC(width) + $_DC(lmar) + 12 ]

    set tmpHeight [expr $_DC(height) -1]
    
    for {set j 0} {$j < $_showRows } {incr j} {
	set tmpColYPos($j) [expr $_yPos($j)+$_DC(height)/2] 

	for {set i 0} {$i < $_showCols} {incr i} {
	    set id ${i}_$j
            if { $deleteFlag == "true" } {
               catch { destroy $can.e$id }
            }

            entry $can.e$id -width $_cellWidth($i) -state disabled \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor \
		-textvariable [itcl::scope _numEntry($id)] \
		-relief sunken -bd 0 -justify right -font g_entryFont
#		-textvariable [itcl::scope _numEntry($id)] \
#hello
            bind $can.e$id <Button-1> [itcl::code $this _setStartMark $i $j]
	    bind $can.e$id <Button-3> [itcl::code $this _setEndMark $i $j]
	    bind $can.e$id <Shift-Button-1> [itcl::code $this _setEndMark $i $j]
	    bind $can.e$id <Button-2> [itcl::code $this _unsetMark ]

            set _buttonWidth 0
            if { $_valueTDIM($i) > 0 } {
               set slice $j
#puts "_valueTDIM($i) = $_valueTDIM($i)"   
               if { $deleteFlag == "true" } {
                  catch { destroy $can.tdim$id }
               }
               switch $_valueTDIM($i) {
              
                      1 {
                           button $can.tdim$id -text "Plot" \
                                  -font [list Helvetica 10] \
                                  -command [itcl::code $this _plotVectorTableRow $i $slice]
                        }
                      2 {
                           button $can.tdim$id -text "Image" \
                                  -font [list Helvetica 10] \
                                  -command [itcl::code $this _displaySlice $i $slice]
                        }
                      3 {
                           button $can.tdim$id -text "Movie" \
                                  -font [list Helvetica 10] \
                                  -command [itcl::code $this _displayMovie $i $slice]
                        }
                default {
                           button $can.tdim$id -text "Expand" \
                                  -font [list Helvetica 10] \
                                  -command [itcl::code $this _openVectorTable $i]
                        }
   
               }
   
               set _buttonWidth [winfo reqwidth $can.tdim$id]
   	       $can create  window $tmpColXPos($i) $tmpColYPos($j) \
                                   -window $can.tdim$id \
		                   -width  $tmpColWid($i) \
                                   -height $tmpHeight \
                                   -tag tdim_$i
            }

   	    $can create  window $tmpColXPos($i) $tmpColYPos($j) \
                                -height $tmpHeight \
                                -width  $tmpColWid($i) \
                                -window $can.e$id -tags entry_$i

            set _xPos([expr $i + 1]) $tmpColXPos($i)
   	}

    }

    _reframe
}

itcl::body Table::_redrawTable {prevCols_} {

   _calXPos $_firstCol $_showCols

   # Reposition/Resize the header/cells

   set minCols [getmin [list $prevCols_ $_showCols]]
   set idx [expr $_firstCol-1]
   for {set i 0} {$i < $minCols } {incr i; incr idx} {
      foreach {oldx oldy} [$_droot.table.can coord entry_$i] {}
      set entryExist true
      if ![info exist oldx] {
         set entryExist false
         foreach {oldx oldy} [$_droot.table.hdr coord tempColData_$i] {}
      }

      set dx [expr $_xPos($i)+$_cellPixWidth($idx)/2 - $oldx]

      if { $entryExist == "false" } {
         unset oldx
      }
      $_droot.table.can move entry_$i   $dx 0

      if [winfo exists $_droot.table.can.tdim${i}_0] {
         $_droot.table.can move tdim_$i $dx  0
      }

      $_droot.table.hdr move tempColData_$i $dx 0

      $_droot.table.can itemconfigure entry_$i   -width $_cellPixWidth($idx)
      if [winfo exists $_droot.table.can.tdim${i}_0] {
         $_droot.table.can itemconfigure tdim_$i   -width $_cellPixWidth($idx)
      }
      $_droot.table.hdr itemconfigure tempColData_$i -width $_cellPixWidth($idx)
   }

   # Reposition the last lines
   for {set i 0} {$i <= $minCols } {incr i} {
      foreach {oldx oldy} [$_droot.table.can coord colLine_$i] {}
      set dx [expr $_xPos($i)-$_DC(rightspace)/2-1 - $oldx]
      $_droot.table.can move  colLine_$i $dx 0
      $_droot.table.can raise colLine_$i
   }

   # Do we need to add/delete any columns?
   if { $prevCols_ > $_showCols } {
      _delColsFrTable $prevCols_ $_showRows
   } elseif { $prevCols_ < $_showCols} {
      _addColsToTable $prevCols_ $_showRows 
   }
}

itcl::body Table::_setVScroll {args} {
#puts "_setVScroll start"

    set old_firstRow $_firstRow
# unselected the previously selected cell
    _unselCell
#
    if { [llength $args]==1 } {
	set _firstRow [expr $args+1]
    } else {
	set stype [lindex $args 2]
	set snum  [lindex $args 1]
	if {$snum == ""} return

	if { [string match "page*" $stype] } {
	    set tmpjump [expr $snum*$_showRows]
	} elseif { [string match "unit*" $stype] } {
	    set tmpjump $snum
	} elseif { $stype == ""} {
	    set tmpjump [expr 1+round($snum*$_numRows)-$_firstRow]
	} else {
	    error "unknown scroll command $stype"
	    return
	}
	set _firstRow [expr $_firstRow+$tmpjump]
    }

    if {$_firstRow < 1} {
	set _firstRow 1
    }

    if {$_firstRow > [expr $_numRows-$_showRows+1] } {
	set _firstRow [expr $_numRows-$_showRows+1]
    }

    _updateLast

    _refacing
# reselect cell
    _selCell
    _showselRows

    # Check for locked children
    _scrollChildren $_firstRow
}

itcl::body Table::_refacing {} {
#puts "_refacing start"

    if { $_numRows } {
       $_droot.table.vscroll set [expr double($_firstRow-1)/$_numRows]  \
	     [expr double($_firstRow + $_showRows - 1)/$_numRows]
    } else {
       $_droot.table.vscroll set 0.0 1.0
    }

    for {set j 0} {$j < $_showRows} {incr j} {
	if { $_tableType == "Image" } {
	    set rowIndex [expr $_numRows -  ($_firstRow+$j) +1]
	} else {
	    set rowIndex [expr $j+$_firstRow]
	}	
	$_droot.table.can.b$j configure -text $rowIndex
# Add by Ziqin Pan , Aug 03,  2005
         bind $_droot.table.can.b$j <Button-1> [itcl::code $this _setRowStartMark $j]
         bind $_droot.table.can.b$j <Button-3> [itcl::code $this _setRowEndMark $j]
         bind $_droot.table.can.b$j <Shift-Button-1> [itcl::code $this _setRowEndMark $j]
         bind $_droot.table.can.b$j <Button-2> [itcl::code $this _unsetRowMark ]
# Add end
    }
}

itcl::body Table::_setScrolls {} {
#puts "_setScrolls start"
#horizontal
   if { $_listPreSelectedColNames == "" } {
      $_droot.table.hscroll set 0.0 1.0
   } else {
      $_droot.table.hscroll set [expr ($_firstCol-1.0)/[llength $_listPreSelectedColNames] ] \
	    [expr ($_firstCol+$_showCols-1.0)/[llength $_listPreSelectedColNames]]
   }
#vertical is set in _refacing 
}


itcl::body Table::_setHScroll {args} {
#puts _setHScroll
    set _old_firstCol $_firstCol
    set oldShowCols $_showCols
    set oldShowRows $_showRows
# unselected the cell
    _unselCell

    if { [llength $args]==1 } {
	set _firstCol [expr $args+1]
    } else {
	set stype [lindex $args 2]
	set snum  [lindex $args 1]
	if {$snum == ""} return

	if { [string match "page*" $stype] } {
	    set tmpjump [expr $snum*$_showCols]
        } elseif { [string match "unit*" $stype] } {
	    set tmpjump $snum
	} elseif { $stype == ""} {
	    set tmpjump [expr 1+round($snum*[llength $_listPreSelectedColNames])-$_firstCol]
	} else {
	    error "unknown scroll command $stype"
	    return
	}
	set _firstCol [expr $_firstCol+$tmpjump]
    }

    if {$_firstCol < 1} {
	set _firstCol 1
    } elseif {$_firstCol > $_lastFirstCol } {
	set _firstCol $_lastFirstCol
    }

 # determine how many cols and rows to display
    _getXYSize $_DC(xsize) $_DC(ysize)

    if { [expr $_firstCol+$_showCols-1] > $_dispCols } {
	set $_firstCol $_old_firstCol
	return
    }

    _redrawTable $oldShowCols

# update the table entries
    _updateLast
    _updateUnViewedColumns

    if { ($_tableType == "Image") || ($_tableType == "Vector Table")} {
        for {set i 0} {$i < $_showCols} {incr i} {
	    set tmpColNum [expr $_firstCol+$i-1]
	    set [itcl::scope _colSel($this,$i)] $_colNotchedState($tmpColNum)
            set tmpColName [lindex $_listPreSelectedColNames [expr $i+$_firstCol-1]]
            $_droot.table.hdr.name$i configure \
                -text $tmpColName  -command [itcl::code $this _updateColSel $i]
        }
    } else {
        for {set i 0} {$i < $_showCols} {incr i} {
            set tmpColName [lindex $_listPreSelectedColNames [expr $i+$_firstCol-1]]
	    set tmpColNum [expr $_firstCol+$i-1]
	    set [itcl::scope _colSel($this,$i)] $_colNotchedState($tmpColNum)
            $_droot.table.hdr.name$i configure \
                -text $_columnName($tmpColNum) -command [itcl::code $this _updateColSel $i]
            $_droot.table.hdr.type$i configure \
                -text $_columnType($tmpColNum)
            $_droot.table.hdr.unit$i configure \
                -text $_columnUnit($tmpColNum)

# CM Begin
	    _colMenu $_droot.table.hdr $i $tmpColName $tmpColNum

	    if { $_columnVecSize($tmpColNum) > 1 || $_columnVecSize($tmpColNum) < 0 } {
		$_droot.table.hdr.menu$i.menu insert 0 command \
		    -label "Expand" -underline 0 -font g_titleFont \
		    -command [itcl::code $this _openVectorTable $tmpColNum] 
	    }
        }
    }

# reselected the previously selected cell
    _selCell
    _showselRows
#
    if { $_listPreSelectedColNames=="" } {
       $_droot.table.hscroll set 0.0 1.0
    } else {
       $_droot.table.hscroll set [expr ($_firstCol-1.0)/[llength $_listPreSelectedColNames] ] \
	     [expr ($_firstCol+$_showCols-1.0)/[llength $_listPreSelectedColNames]]
    }

# justify
    _makeJustification
    set _endX 0
    _reframe
}

itcl::body Table::_addColsToTable {prevCols_ prevRows_} {
    set hdr $_droot.table.hdr
    set can $_droot.table.can

    _calXPos $_firstCol $_showCols
    if { ($_tableType == "Image") || ($_tableType == "Vector Table") } {
#puts "prevCols_: $prevCols_"
#puts "_showCols: $_showCols"
	for {set i $prevCols_} {$i < $_showCols} {incr i} {
	    set absColIndex [expr $_firstCol -1+$i]
	    set tmpColName [lindex $_listPreSelectedColNames $absColIndex]
	    checkbutton $hdr.name$i -text $tmpColName -font g_titleFont \
		-variable [itcl::scope _colSel($this,$i)] \
		-selectcolor $fvPref::checkBBgColor  \
		-activeforeground black -activebackground $fvPref::globalBgColor \
		-command [itcl::code $this _updateColSel $i]
	    $hdr create window [expr $_xPos($i) + \
  	        $_cellPixWidth($absColIndex)/2] \
		[expr $_DC(height)/2] -width  $_cellPixWidth($absColIndex)  \
		-window $hdr.name$i -tags tempColData_$i

	    for {set j 0} {$j < $_showRows } {incr j} {
		set id ${i}_$j
		if { [winfo exist $can.e$id] == 0} { 
		    entry $can.e$id \
			-width $_cellWidth($absColIndex) \
			-font g_entryFont -state disabled \
                        -disabledforeground black \
                        -disabledbackground $fvPref::globalBgColor \
		        -textvariable [itcl::scope _numEntry($id)] \
			-relief sunken -bd 0 -justify right
		
		    bind $can.e$id <Button-1> [itcl::code $this _setStartMark $i $j]
		    bind $can.e$id <Button-3> [itcl::code $this _setEndMark $i $j]
                    bind $can.e$id <Shift-Button-1> [itcl::code $this _setEndMark $i $j]
#		    bind $can.e$id <Button-2> [itcl::code $this _unsetMark]


                }

                $can create  window \
                    [expr $_xPos($i)+$_cellPixWidth($absColIndex)/2]\
                    [expr $_yPos($j)+$_DC(height)/2]  \
                                      -height [expr $_DC(height) -1] \
                                      -width  $_cellPixWidth($absColIndex) \
                                      -window $can.e$id -tags entry_$i
	    }
	  
	}

	for {set i $prevCols_} {$i <= $_showCols} {incr i} {	
	    set tmpx [expr $_xPos($i)-$_DC(rightspace)/2-1]
	    
            if { $tmpx > $_endX } {
               set _endX $tmpx
            }

	    $can create line $tmpx 0 $tmpx $_DC(ysize) -width 3 \
		-tags colLine_$i
	    $can bind colLine_$i <B1-Motion> \
		[itcl::code $this _relocateDividLine %x $i]
	    $can bind colLine_$i <Button-1> \
		[itcl::code $this _startDividLine %x $i]
	    $can bind colLine_$i <ButtonRelease-1> \
		[itcl::code $this _finalDividLine %x $i]
	}

    } else {  
        if [winfo exists $can] {
           for {set c 0} {$c<=[llength $_listPreSelectedColNames]} {incr c} {
              for {set r 0} {$r<$_showRows} {incr r} {
                  catch {destroy $can.tdim${c}_${r}} 
              }
           }
        }

	_updateUnViewedColumns

	for {set i $prevCols_} {$i < $_showCols} {incr i} {
	    set absColIndex [expr $_firstCol -1+$i]
	    set tmpColName [lindex $_listPreSelectedColNames $absColIndex]
	    set tmpColNum  [expr $_firstCol-1+$i]
	    checkbutton $hdr.name$i -text $_columnName($tmpColNum) \
                -font g_titleFont \
		-variable [itcl::scope _colSel($this,$i)] \
		-selectcolor $fvPref::checkBBgColor  \
		-activeforeground black \
                -activebackground $fvPref::globalBgColor \
		-command [itcl::code $this _updateColSel $i]
	    label $hdr.type$i -text $_columnType($tmpColNum) -font g_titleFont
	    label $hdr.unit$i -text $_columnUnit($tmpColNum) -font g_titleFont

	    # Create column header menu
	    _colMenu $hdr $i $tmpColName $tmpColNum

	    if { $_columnVecSize($tmpColNum) > 1 || $_columnVecSize($tmpColNum) < 0 } {
		$hdr.menu$i.menu insert 0 command \
		    -label "Expand" -underline 0 -font g_titleFont \
		    -command [itcl::code $this _openVectorTable $tmpColNum] 

	    }
	    $hdr create window [expr $_xPos($i) + $_cellPixWidth($absColIndex)/2]\
		[expr $_DC(height)/2] -width $_cellPixWidth($absColIndex)  \
		-window $hdr.name$i -tags tempColData_$i
	    $hdr create window [expr $_xPos($i) + $_cellPixWidth($absColIndex)/2]\
		[expr $_DC(height)*3/2 ] -width $_cellPixWidth($absColIndex)  \
		-window $hdr.type$i -tags tempColData_$i
	    $hdr create window [expr $_xPos($i) + $_cellPixWidth($absColIndex)/2]\
		[expr $_DC(height)*5/2 ] \
		-width $_cellPixWidth($absColIndex)  \
		-window $hdr.unit$i -tags tempColData_$i            
# CM Begin
	    $hdr create window [expr $_xPos($i) + $_cellPixWidth($absColIndex)/2]\
		[expr $_DC(height)*7/2 ] \
		-width $_cellPixWidth($absColIndex) \
		-window $hdr.menu$i -tags tempColData_$i
# CM End
#	    $hdr create window [expr $_xPos($i) + $_cellPixWidth($absColIndex)/2]\
#		[expr $_DC(height)*9/2 ] \
#		-width $_cellPixWidth($absColIndex) \
#		-window $hdr.vecB$i -tags tempColData_$i   

	    for {set j 0} {$j < $_showRows } {incr j} {
               set id ${i}_$j
               catch {entry $can.e$id \
                     -width $_cellWidth($absColIndex) \
                     -font g_entryFont -state disabled \
                     -disabledforeground black \
                     -disabledbackground $fvPref::globalBgColor \
                     -textvariable [itcl::scope _numEntry($id)] \
                     -relief sunken -bd 0 -justify right}
               bind $can.e$id <Button-1> [itcl::code $this _setStartMark $i $j]
               bind $can.e$id <Button-3> [itcl::code $this _setEndMark $i $j]
               bind $can.e$id <Shift-Button-1> [itcl::code $this _setEndMark $i $j]

               set _buttonWidth 0
               if { $_valueTDIM($i) > 0 } {
                  set slice $j 
    
                  # catch {$can delete tdim_$i}
                  # catch {destroy $can.tdim$id}

                  switch $_valueTDIM($i) {
   
                         1 {
                              button $can.tdim$id -text "Plot" \
                                     -font [list Helvetica 10] \
                                     -command [itcl::code $this _plotVectorTableRow $i $slice ]
                           }
                         2 {
                              button $can.tdim$id -text "Image" \
                                     -font [list Helvetica 10] \
                                     -command [itcl::code $this _displaySlice $i $slice ]
                           }
                         3 {
                              button $can.tdim$id -text "Movie" \
                                     -font [list Helvetica 10] \
                                     -command [itcl::code $this _displayMovie $i $slice ]
                           }
                   default {
                              button $can.tdim$id -text "Expand" \
                                     -font [list Helvetica 10] \
                                     -command [itcl::code $this _openVectorTable $i]
                           }
   
                  }
   
                  set _buttonWidth [winfo reqwidth $can.tdim$id]
                  $can create  window \
			[expr $_xPos($i)+$_cellPixWidth($absColIndex)/2]\
			[expr $_yPos($j)+$_DC(height)/2]  \
                                      -window $can.tdim$id \
                                      -width  $_cellPixWidth($absColIndex) \
                                      -height [expr $_DC(height)-1] \
                                      -tag tdim_$i
               }
     
               $can create  window \
                    [expr $_xPos($i)+$_cellPixWidth($absColIndex)/2]\
                    [expr $_yPos($j)+$_DC(height)/2]  \
                                      -height [expr $_DC(height) -1] \
                                      -width  $_cellPixWidth($absColIndex) \
                                      -window $can.e$id -tags entry_$i
               set _xPos([expr $i + 1]) [expr $_xPos($i)+$_cellPixWidth($absColIndex)/2]

	    }

	}

	for {set i $prevCols_} {$i <= $_showCols} {incr i} {
	    set tmpx [expr $_xPos($i)-$_DC(rightspace)/2-1]
	    
            if { $tmpx > $_endX } {
               set _endX $tmpx
            }
	    $can create line $tmpx 0 $tmpx $_DC(ysize) -width 3 \
		-tags colLine_$i
            $can bind colLine_$i <B1-Motion> \
		[itcl::code $this _relocateDividLine %x $i]
	    $can bind colLine_$i <Button-1> \
		[itcl::code $this _startDividLine %x $i]
	    $can bind colLine_$i <ButtonRelease-1> \
		[itcl::code $this _finalDividLine %x $i]
	}
    }
    _setHScroll [expr $_firstCol-1]
}

itcl::body Table::_delColsFrTable {prevCols_ prevRows_} {
    set hdr $_droot.table.hdr
    set can $_droot.table.can
    
    for {set i $_showCols} {$i < $prevCols_} {incr i} {
	$hdr delete tempColData_$i
	$can delete entry_$i
	$can delete tdim_$i
	catch {destroy $hdr.name$i}
	catch {destroy $hdr.type$i}
	catch {destroy $hdr.unit$i}
	catch {destroy $hdr.vecB$i}
	catch {destroy $hdr.menu$i}
	for {set j 0} {$j < $prevRows_ } {incr j} {
	    set id ${i}_$j
	    catch {destroy $can.e$id }
	    catch {destroy $can.tdim$id  }
	}
    }
    
    for {set i [expr 1+$_showCols]} {$i <= $prevCols_} {incr i} {
	$can delete colLine_$i
    }
}

itcl::body Table::_addRowsToTable {prevCols_ prevRows_} {
    set can $_droot.table.can

    _calXPos $_firstCol $_showCols

    for {set j $prevRows_} {$j < $_showRows} {incr j} {
	set realRowNum [expr $_firstRow + $j]
	if { $_tableType == "Image" } {
	    set rowIndex [expr $_numRows -  $realRowNum +1]
	} else {
	    set rowIndex $realRowNum
	}
	label $can.b$j -text $rowIndex -font g_titleFont
	set _yPos([expr $j+1])  [expr $_yPos($j)+ $_DC(height) + $_DC(interline) ]
	$can create window [expr $_DC(lmar)+$_DC(width)/2] \
	    [expr $_yPos($j)+$_DC(height)/2]  -window \
	    $_droot.table.can.b$j  \
	    -width $_DC(width) -height [expr $_DC(height)-1] \
	    -tags check

	if { [catch {set key [$fFile getKeyword EXTNAME]}] == 0 } {
           if { [string first "GROUPING" $key] >= 0 } {
              button $can.button$realRowNum -text "Open" \
                     -font [list Helvetica 10 bold] \
                     -command [itcl::code $this _openGroupFile $_fitsFileLocationCol $realRowNum]

              # create consistence OPEN button
   	      $can create  window [expr $_DC(width)*2+$_DC(lmar)-10] \
	                          [expr $_yPos($j)+$_DC(height)/2] \
		                  -window $can.button$realRowNum \
                                  -height [expr $_DC(height)-5] \
                                  -tags open_$j
           }
        }

	for {set i 0} {$i < $prevCols_ } {incr i} {
	    set absColIndex [expr $_firstCol -1 +$i]
	    set id ${i}_$j
	    entry $can.e$id -width $_cellWidth($absColIndex) \
		-textvariable [itcl::scope _numEntry($id)] -state disabled \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor \
		-relief sunken -bd 0 -justify right -font g_entryFont
	    bind $can.e$id <Button-1> [itcl::code $this _setStartMark $i $j]
	    bind $can.e$id <Button-3> [itcl::code $this _setEndMark $i $j]
	    bind $can.e$id <Shift-Button-1> [itcl::code $this _setEndMark $i $j]
#	    bind $can.e$id <Button-2> [itcl::code $this _unsetMark]

            set _buttonWidth 0

            if { $_valueTDIM($i) > 0 } {
               set slice $j
 
               # catch {$can delete tdim_$i}
               # catch {destroy $can.tdim$id}

               switch $_valueTDIM($i) {

                      1 {
                           button $can.tdim$id -text "Plot" \
                                  -font [list Helvetica 10] \
                                  -command [itcl::code $this _plotVectorTableRow $i $slice ]
                        }
                      2 {
                           button $can.tdim$id -text "Image" \
                                  -font [list Helvetica 10] \
                                  -command [itcl::code $this _displaySlice $i $slice]
                        }
                      3 {
                           button $can.tdim$id -text "Movie" \
                                  -font [list Helvetica 10] \
                                  -command [itcl::code $this _displayMovie $i $slice]
                        }
                default {
                           button $can.tdim$id -text "Expand" \
                                  -font [list Helvetica 10] \
                                  -command [itcl::code $this _openVectorTable $i]
                        }

               }

               set _buttonWidth [winfo reqwidth $can.tdim$id]
               $can create  window \
			[expr $_xPos($i)+$_cellPixWidth($absColIndex)/2]\
			[expr $_yPos($j)+$_DC(height)/2]  \
                                   -window $can.tdim$id \
                                   -width  $_cellPixWidth($absColIndex) \
                                   -height [expr $_DC(height)-1] \
                                   -tag tdim_$i
            }
     
            # set _xPos([expr $i + 1]) [expr $_xPos($i) + $width + 3] 
            # set width 0

	    $can create  window [expr $_xPos($i)+$_cellPixWidth($absColIndex)/2]\
                                [expr $_yPos($j)+$_DC(height)/2]  \
                                -height [expr $_DC(height) -1] \
                                -width  $_cellPixWidth($absColIndex) \
                                -window $can.e$id -tags entry_$i
	}

        # Pan - change the end X of horizational line
	$can create line 0 $_yPos($j) $_endX $_yPos($j)  -tags rowLine_$j
	# $can create line 0 $_yPos($j) $_DC(xsize) $_yPos($j)  -tags rowLine_$j
    }
    
    if { $_numRows } {
       $_droot.table.vscroll set [expr double($_firstRow-1)/$_numRows]  \
	     [expr ($_firstRow + $_showRows - 1.0)/$_numRows]
    } else {
       $_droot table.vscroll set 0.0 1.0
    }
}

itcl::body Table::_delRowsFrTable {prevCols_ prevRows_} {
#puts "_delRowsFrTable start"
    if {$prevRows_ > $_showRows} {
	set can $_droot.table.can
	
	for {set j $_showRows} {$j < $prevRows_} {incr j} {
	    catch {destroy $can.b$j}
	    catch {destroy $can.button[expr $j+1]}

            $can delete rowLine_[expr $j+1]
	    for {set i 0} {$i < $_showCols } {incr i} {
		set id ${i}_$j
		catch {destroy $can.e$id }
		catch {destroy $can.tdim$id }
	    }
	    
	}
	
    }
    _refacing
}

itcl::body Table::_updateLast { } {
   set insertButton false
   set can $_droot.table.can

   # Update visible cells with current values

#puts "_firstCol: $_firstCol"
#puts "_firstRow: $_firstRow"
   # _readTableData [expr $_firstCol-1] [expr $_firstRow-1] $_showCols $_showRows
   _readTableData [expr $_firstCol-1] [expr $_firstRow-1] \
                  [llength $_listPreSelectedColNames] $_showRows

   set col [expr $_firstCol-1]
   # command in fitsTcl
   # updateCell 

   if [winfo exists $can] {
      for {set c 0} {$c<=[llength $_listPreSelectedColNames]} {incr c} {
         for {set r 0} {$r<$_showRows} {incr r} {
             catch {destroy $can.tdim${c}_${r}} 
         }
      }
   }

   for {set c 0} {$c<$_showCols} {incr c} {

      set row [expr $_firstRow-1]
      set absColIndex [expr $_firstCol -1 +$c]

      for {set r 0} {$r<$_showRows} {incr r} {
         set id ${c}_${r}

         if [winfo exists $can] {
 
            set oldx 0
            set oldy 0
            foreach {oldx oldy} [$_droot.table.can coord colLine_$c] {}

            if { $_valueTDIM($absColIndex) > 0 } {
               set slice $r
               switch $_valueTDIM($absColIndex) {

                   1 {
                        button $can.tdim${absColIndex}_${r} -text "Plot" \
                               -font [list Helvetica 10] \
                               -command [itcl::code $this _plotVectorTableRow $col $slice ]
                     }
                   2 {
                        button $can.tdim${absColIndex}_${r} -text "Image" \
                               -font [list Helvetica 10] \
                               -command [itcl::code $this _displaySlice $col $slice]
                     }
                   3 {
                        button $can.tdim${absColIndex}_${r} -text "Movie" \
                               -font [list Helvetica 10] \
                               -command [itcl::code $this _displayMovie $col $slice]
                     }
                   default {
                              button $can.tdim${absColIndex}_${r} -text "Expand" \
                                                 -font [list Helvetica 10] \
                               -command [itcl::code $this _openVectorTable $absColIndex]

                              # $can.tdim$id configure -state disable
                           }

               }

               $can create  window \
                            [expr $oldx+$_cellPixWidth($absColIndex)/2 + 6] \
                            [expr $_yPos($r)+$_DC(height)/2]  \
                            -window $can.tdim${absColIndex}_${r} \
                            -width  $_cellPixWidth($absColIndex) \
                            -height [expr $_DC(height) -1] \
                            -tag tdim_$absColIndex

               set _xPos([expr $c + 1]) [expr $_xPos($c)+$_cellPixWidth($absColIndex)/2]
            }

         }

         set val [_getFormattedData $col $row]
         if { [string length $val]>$_cellWidth($col) } {
            # set val "*"
         }

         set _numEntry($id) $val
         incr row
      }
      incr col
   }
}

itcl::body Table::_openGroupFile {c r} {

       set fileName [_getRawData $c [expr $_firstRow + $r - 2]]

       if { [string range $fileName 0 0] == "/" } {
          # full path
          set fullPathName $fileName
       } else {
          set token [split [wm title $_droot ] " "]
          set dName [lindex $token [expr [llength $token] - 1]]
          set fullPathName [format "%s%s" $dName $fileName]
       }

       if ![file exists $fullPathName] {
          foreach ext { gz Z z zip } {
            if [file exists $fullPathName.$ext] {
               set fullPathName $fullPathName.$ext
               break
            }
          }
       }

       set flagErr [catch {
           set result [FitsFile #auto $fullPathName 1]
       } err]

       if { $flagErr } {
          tk_messageBox -icon error -type ok -message $err
       }
 
}

itcl::body Table::_jumpFrEnt {} {
#puts "_jumpFrEnt start"
   _jump [$_droot.usrEntry.goto_e get]
}

itcl::body Table::_jump {lineNum_} {
#puts "_jump start"
    if { $lineNum_ == "" } {
	puts "Please give me a number"
	return
    }
    if { $lineNum_<1 }        { set lineNum_ 1        }
    if { $lineNum_>$_numRows } { set lineNum_ $_numRows }
    if { $_tableType == "Image" } {
       set realRow [expr $_numRows-$lineNum_]
    } else {
       set realRow [expr $lineNum_-1]    
    }
    _setVScroll $realRow
    if { [info exists _currentCol] } {
       _setStartMark [expr $_currentCol-$_firstCol+1] \
             [expr $realRow-$_firstRow+1]
    } else {
       _setStartMark 0 [expr $realRow-$_firstRow+1]
    }
}

itcl::body Table::_selRowsFrExpr { ifDelete_ } {
#puts "_selRowsFrExpr start"
#   never used, not even initialized
#   global g_rowselentry

   if { $ifDelete_ } {
      set cal ".cald_[namespace tail $this]" 
      if { [winfo exist $cal] == 1 } {
	 raise $cal
	 return 
      }
	    
      # pick out the variable-length vector columns from the calculator
      set calList {}
      for {set i 0} {$i < $_dispCols} {incr i} {
	 if { $_columnVecSize($i) > 0 } {
	    lappend calList [lindex $_listPreSelectedColNames $i]
	 }
      }
      FitsDelCalculator $cal $this $calList
      $cal configure -title "fv: Delete Rows"
   } else {
      set cal ".cald_[namespace tail $this]" 
      if { [winfo exist $cal] == 1 } {
	 raise $cal
	 return 
      }
	    
      # pick out the variable-length vector columns from the calculator
      set calList {}
      for {set i 0} {$i < $_dispCols} {incr i} {
	 if { $_columnVecSize($i) > 0 } {
	    lappend calList [lindex $_listPreSelectedColNames $i]
	 }
      }
      FitsSelCalculator $cal $this $calList 0 $_numRows
      $cal configure -title "fv: Select Rows From Expr"
   }
}

itcl::body Table::_addRows {{rowindex -1} { rownum -1} } {
#puts "_addRows start"
    global g_titleFont
    set addWin .addRow
    catch {destroy .addRow}
    powToplevel $addWin .dummy
    wm title $addWin "fv: Add Rows"

    iwidgets::entryfield $addWin.after -textvariable [itcl::scope _rowindex] -labeltext "After row \#" \
          -labelpos nw -width 10 -labelfont g_titleFont
    iwidgets::entryfield $addWin.number -textvariable [itcl::scope _rownum] -labeltext "Number of rows" \
          -labelpos nw -width 10 -labelfont g_titleFont
    iwidgets::buttonbox $addWin.bb

    if { $rowindex !="" && $rowindex >-1} {
         if { $rownum !="" && $rownum > -1 } {
           set _rowindex $rowindex
           set _rownum $rownum
           _addRowsToFile $addWin
           return
         }
    }

    $addWin.bb add OK -text OK -command [itcl::code $this _addRowsToFile $addWin] -font g_titleFont
    $addWin.bb add Cancel -text Cancel -command "destroy $addWin" -font g_titleFont
    pack $addWin.after  -fill x
    pack $addWin.number -fill x
    pack $addWin.bb     -fill x
    tkwait window $addWin
}


itcl::body Table::_delRowsRange {} {
     set selrange ""
     set start_end "1-$_numRows"
     append selrange [_parseToRowRange $start_end]
     if { $selrange !="" } {
     for { set i 1 } { $i <= $_numRows } { incr i } {
          set _selectedRows($i) 0
     }

     $fFile delRowsRange $selrange 

     $fFile changeFile


     refresh
     _updateRestDisps
     }

}


itcl::body Table::_delRows {} {
   
   set ndeleted 0
   for {set i 1} {$i <= $_numRows} {incr i} {
            if { $_selectedRows($i) == 1 } {
                set j $i
                while { ($j <=$_numRows) && ($_selectedRows($j) == 1)} { 
                      set _selectedRows($j) 0
                      incr j
                }
                set n0 $i
                set n1 [expr $j-1]
                set nn [expr $j-$i]

                $fFile delRows [expr $n0-$ndeleted] $nn
                incr ndeleted $nn
		if { $_firstRow >= $n1 } {
	    		incr _firstRow [expr -$nn]
		} elseif { $_firstRow >= $n0 } {
	    		incr _firstRow [expr -($_firstRow-$n0)]
		} else {

		}
		if { $_firstRow < 1} {
	    		set _firstRow 1
		}
                set i $j
            }
    }

   set _numRows [expr $_numRows - $ndeleted]
   $fFile changeFile

   refresh
   _updateRestDisps
# puts $_numRows

}
              
     
itcl::body Table::_logicalWithselectedRows { rowlist seland selor} {
   set sellist {}
 

   for {set i 0} {$i <= $_numRows} {incr i} {
        set tmpselrows($i) 0
        if { $_selectedRows($i) ==1 } {
            lappend sellist $i
        }
   }
    
#   for {set i 0} {$i < [llength $rowlist]} {incr i} {
#        set j [expr 1+[lindex $rowlist $i]]
#        set tmpselrows($j) 1
#   }

   foreach j $rowlist {
        set tmpselrows([expr $j+1]) 1
   }


   if { $seland == 1} {
#   	for {set i 0} {$i <= $_numRows} {incr i} {
#        set _selectedRows($i) [expr $_selectedRows($i) && $tmpselrows($i)] 
#   	}
        foreach i $sellist {
             set _selectedRows($i) [expr $_selectedRows($i) && $tmpselrows($i)]
        }
   } elseif { $selor == 1} {
   	for {set i 0} {$i <= $_numRows} {incr i} {
        set _selectedRows($i) [expr $_selectedRows($i) || $tmpselrows($i)] 
   	}
   } else {
   	for {set i 0} {$i <= $_numRows} {incr i} {
        set _selectedRows($i) $tmpselrows($i) 
   	}
   }
   
        
   _showselRows
}
               



itcl::body Table::selRowsWithCondition {cond fRow nRows seland selor} {
#puts "selRowsWithCondition start"
   set rowlist [$fFile selRowsWithCondition $cond $fRow $nRows]

   _logicalWithselectedRows $rowlist $seland $selor

#   set sellist {}
 

#   for {set i 0} {$i <= $_numRows} {incr i} {
#        set tmpselrows($i) 0
#        if { $_selectedRows($i) ==1 } {
#            lappend sellist $i
#        }
#   }
    
##   for {set i 0} {$i < [llength $rowlist]} {incr i} {
##        set j [expr 1+[lindex $rowlist $i]]
##        set tmpselrows($j) 1
##   }

#   foreach j $rowlist {
#        set tmpselrows([expr $j+1]) 1
#   }


#   if { $seland == 1} {
##   	for {set i 0} {$i <= $_numRows} {incr i} {
##        set _selectedRows($i) [expr $_selectedRows($i) && $tmpselrows($i)] 
##   	}
#        foreach i $sellist {
#             set _selectedRows($i) [expr $_selectedRows($i) && $tmpselrows($i)]
#        }
#   } elseif { $selor == 1} {
#   	for {set i 0} {$i <= $_numRows} {incr i} {
#        set _selectedRows($i) [expr $_selectedRows($i) || $tmpselrows($i)] 
#   	}
#   } else {
#   	for {set i 0} {$i <= $_numRows} {incr i} {
#        set _selectedRows($i) $tmpselrows($i) 
#   	}
#   }
   
        
#   _showselRows
}

itcl::body Table::delRowsWithCondition {cond} {
#puts "delRowsWithCondition start"
   $fFile delRowsWithCondition $cond
   $fFile changeFile

   refresh
   _updateRestDisps
}

itcl::body Table::selRowsFromList {entry seland selor} {

# get rid of all the white spaces
    regsub -all " " $entry "" tmpEntry
# break it apart 
    set tmpS [split  $tmpEntry  ","]
# deal with different parts and make a list
    foreach range $tmpS {
	if { [regexp {^[0-9]+$} $range] == 1 } {
	    if {$range <= $_numRows} {
		lappend rangeList [list $range $range]
	    } else {
		lappend rangeList [list $_numRows $_numRows]
	    }
	} else {

	    if {[regsub -- "-" $range "," range1] == 0} {
	
	    } else {
		set tmpPos [string first "," $range1]
	    
		if { $_tableType == "Image"} {
		    set n1 [expr $_numRows +1 - \
				[string range  $range1 [expr $tmpPos+1] end ] ]
		    set n2 [expr $_numRows +1 - \
				[string range  $range1 0 [expr $tmpPos-1]] ]
		} else {
		    set n1 [string range  $range1 0 [expr $tmpPos-1]] 
		    set n2 [string range  $range1 [expr $tmpPos+1] end ] 
		}
		if { $n1 < 1 } {
		    set n1 1
		}
		if { $n2 > $_numRows } {
		    set n2 $_numRows
		}
		if { $n1 > $n2} {
		    lappend rangeList [list $n2 $n1]
		} else {
		    lappend rangeList [list $n1 $n2]
		}
	    }
	}
    }

    for { set i 0 } {$i <=$_numRows} { incr i} {
          set tmplist($i)  0
    }

    foreach r $rangeList {
           set n0 [lindex $r 0]
           set n1 [lindex $r 1]
           for { set i $n0 } { $i <=$n1 } {incr i} {
                set tmplist($i) 1
           }
    }

    if { ($seland !=1) && ($selor !=1) } {
           for {set i 0 } { $i <= $_numRows } {incr i} {
                set _selectedRows($i) $tmplist($i)
           }
    } elseif { $seland ==1 } {
           for {set i 0 } { $i <= $_numRows } {incr i} {
                set _selectedRows($i) [expr $_selectedRows($i) &&  $tmplist($i)]
           }
    } else {
    	foreach r $rangeList {
       	   set n0 [lindex $r 0]
           set n1 [lindex $r 1]
           for { set i $n0 } { $i <=$n1 } {incr i} {
                set _selectedRows($i) 1
           }
    	}
#           for {set i 0 } { $i <= $_numRows } {incr i} {
#                set _selectedRows($i) [expr $_selectedRows($i) ||  $tmplist($i)]
#           }
    }
    _showselRows
          

}



itcl::body Table::delRowsFromList {entry} {
#puts "delRowsFromList start"
#  don't delete if its an image extension
    if { $_tableType == "Image"} {
	error "Cannot delete rows in an image"
	return
    }
# warning
    if { [llength $_listPreSelectedColNames] != [llength $_listColNames] } {
	if { [promptMsg "You are editing only part of the table. \n
Do you want to delete rows from all the columns?" \
		 return Yes No] == "CANCEL"} {
	    return
	}
    }
# get rid of all the white spaces
    regsub -all " " $entry "" tmpEntry 
# break it apart 
    set tmpS [split  $tmpEntry  ","]
# deal with different parts and make a list
    foreach range $tmpS {
	if { [regexp {^[0-9]+$} $range] == 1 } {
	    if {$range <= $_numRows} {
		lappend rangeList [list $range $range]
	    } else {
		lappend rangeList [list $_numRows $_numRows]
	    }
	} else {

	    if {[regsub -- "-" $range "," range1] == 0} {
	
	    } else {
		set tmpPos [string first "," $range1]
	    
		if { $_tableType == "Image"} {
		    set n1 [expr $_numRows +1 - \
				[string range  $range1 [expr $tmpPos+1] end ] ]
		    set n2 [expr $_numRows +1 - \
				[string range  $range1 0 [expr $tmpPos-1]] ]
		} else {
		    set n1 [string range  $range1 0 [expr $tmpPos-1]] 
		    set n2 [string range  $range1 [expr $tmpPos+1] end ] 
		}
		if { $n1 < 1 } {
		    set n1 1
		}
		if { $n2 > $_numRows } {
		    set n2 $_numRows
		}
		if { $n1 > $n2} {
		    lappend rangeList [list $n2 $n1]
		} else {
		    lappend rangeList [list $n1 $n2]
		}
	    }
	}
    }
# now merge the list chosen.
    
    set numRange [llength $rangeList]
    set leftList $rangeList
    set resultList ""

    while { $numRange > 1} {
	set seedRange [lindex $leftList 0]
	set leftList  [lrange  $leftList 1 end]
	set result [_collapse $seedRange $leftList]
	set leftList [lindex $result 1]

	if { [lindex $result 0] == 0 } {
	    lappend resultList $seedRange
	}
	incr numRange -1
    }
    lappend resultList [lindex $leftList 0]
    set resultList [_sortRange $resultList]

# if deletion happens before the current _firstRow, then the current _firstRow will change.
# 
    foreach r $resultList {
	set n0 [lindex $r 0]
	set n1 [lindex $r 1]
	set nn [expr $n1-$n0+1]
# really delete rows in the FITS file
	$fFile delRows $n0 $nn
	$fFile changeFile
	if { $_firstRow >= $n1 } {
	    incr _firstRow [expr -$nn]
	} elseif { $_firstRow >= $n0 } {
	    incr _firstRow [expr -($_firstRow-$n0)]
	} else {

	}
	if { $_firstRow < 1} {
	    set _firstRow 1
	}
	
    }

    refresh
    _updateRestDisps
}

itcl::body Table::_collapse {seed bed} {
#puts "_collapse start"
    set flag 0
    set numBedElem [llength $bed]
    set sLow  [lindex $seed 0]
    set sHigh [lindex $seed 1]
#                  |  seed  |
# case 1              **|********|**
# case 2                | ****   | 
# case 3           *****|**      |
# case 4                |     ***|**
#  * bed
    for {set i 0} {$i < $numBedElem} {incr i} {
	set eLow  [lindex [lindex $bed $i] 0] 
	set eHigh [lindex [lindex $bed $i] 1] 
# case 1 
	if { ($sLow > $eLow) && ($sHigh < $eHigh) } {
	    set flag 1
# case 2
	} elseif {($sLow< $eLow) && ($sHigh > $eHigh)} {
	    set flag 1
	    set bed [lreplace $bed $i $i [list $sLow $sHigh]]
# case 3
	} elseif {($sLow>=$eLow) && ($sLow<=$eHigh) && ($sHigh>=$eHigh)  } {
	    set flag 1
	    set bed [lreplace $bed $i $i [list $eLow $sHigh]]
# case 4
	} elseif {($sHigh>=$eLow) && ($sHigh<=$eHigh) && ($sLow<=$eLow)  } {
	    set flag 1
	    set bed [lreplace $bed $i $i [list $sLow $eHigh]]
	} else {

	}
    }
# flag = 1, _collapsed
    return [list $flag $bed]
}

itcl::body Table::_sortRange {bed} {
#puts "_sortRange start"
#
# bed should be a list of ranges, and they are not supposed to have overlaps
# overlap has been delt with in _collapse.
#
    foreach i $bed {
	lappend idxList [lindex $i 0]
    }
    set sortedList [lsort -decreasing -integer $idxList]

    for {set j 0} {$j < [llength $sortedList] } {incr j} {
	set tmpIdx [lsearch -exact $idxList [lindex $sortedList $j]]
	if { $tmpIdx == -1} {
	    error "Error sorting"
	    return
	} else {
	    lappend outList [lindex $bed $tmpIdx]
	}
    }
# in a reversed order, since cfitsio should delete the last row first
    return $outList
}


#   Changes contents of a table cell and writes
#   it to file if available. col_ and row_ are zero-indexed, indicating
#   true row/column of cell to change
#
itcl::body Table::_modifyTableCell {col_ row_ val_} {
#puts "_modifyTableCell start"
   if { $isFailedToCopy } return

   _writeTableData $col_ $row_ $val_

   set id [expr $col_ - $_firstCol + 1]_[expr $row_ - $_firstRow + 1]
   set _numEntry($id) [_getFormattedData $col_ $row_]

   $fFile changeFile
}



itcl::body Table::_updateColSel {col} {
    set realColNum [expr $_firstCol+$col-1] 
    set _colNotchedState($realColNum) $_colSel($this,$col)
}



itcl::body Table::_resizeCan {x y} {
#puts "_resizeCan start, $x, $y"

    set _DC(geoX) [winfo width  $_droot] 
    set _DC(geoY) [winfo height $_droot]
    set _DC(geoX0) [winfo x $_droot]
    set _DC(geoY0) [winfo y $_droot]
# 
    _unselCell
#
#puts "screen info: [winfo screenwidth .]"
#puts "_DC(geoX): $_DC(geoX)"

    if {$_DC(geoX) < 350 } {
	set _DC(geoX) 350
	set x 350
    }

    # wm geometry $_droot [set _DC(geoX)]x[set _DC(geoY)]+[set _DC(geoX0)]+[set _DC(geoY0)]
    # Pan Chai - so Table will not jump during resizeCan
    wm geometry $_droot [set _DC(geoX)]x[set _DC(geoY)]
    set _DC(xsize) $x 
    set _DC(ysize) $y

    $_droot.table.hdr configure -width $_DC(xsize)
    $_droot.table.hdr configure -width 10
    $_droot.table.can configure -width $_DC(xsize) -height $_DC(ysize)   
    $_droot.table.can configure -width 10 -height $_DC(ysize)   


    _getLastFirstCol

#  determine number of rows and number of columns to display
    _layoutCan $_DC(xsize) $_DC(ysize) 

# to prevent _readTableData to be called twice
    if { $_justStarted == 1 } {
	set _justStarted 0
	return
    }

    _reframe 
    _refacing 
    _setScrolls
    _updateLast
    _selCell
    _showselRows
}

itcl::body Table::_reframe {} {
    for {set i 0} {$i<=$_showCols} {incr i} {
        set _tempXPos($i) $_xPos($i)
    }
    _calXPos $_firstCol $_showCols

    for {set i 0} {$i<=$_showCols} {incr i} {
        if { $_tempXPos($i) > $_xPos($i) } {
           set _xPos($i) $_tempXPos($i)
        }
    }

    set tmpYPos [expr $_showRows*$_DC(height) + $_DC(tmar) + 1] 

    for {set i 0} {$i<=$_showCols} {incr i} {
	set tmpXPos [expr $_xPos($i)-$_DC(rightspace)/2-1]

        if { $i == $_showCols } {
	   set tmpXPos [expr $_xPos($i)-$_DC(rightspace)/2-1 + 10]
        }

	$_droot.table.can delete colLine_$i

        # Pan - change the end X of horizational line
        if { $tmpXPos > $_endX } {
           set _endX $tmpXPos
        }

        $_droot.table.can configure -width [ expr $_xPos($i) + 3 ]

	$_droot.table.can create line $tmpXPos 0 $tmpXPos $tmpYPos -width 3 -tags colLine_$i

	$_droot.table.can bind colLine_$i <B1-Motion> \
	    [itcl::code $this _relocateDividLine %x $i]
	$_droot.table.can bind colLine_$i <Button-1> \
	    [itcl::code $this _startDividLine %x $i]
	$_droot.table.can bind colLine_$i <ButtonRelease-1> \
		[itcl::code $this _finalDividLine %x $i]
    }    

    for {set j 0} {$j<=$_showRows} {incr j} {
	$_droot.table.can delete rowLine_$j

        # Pan - change the end X of horizational line
	# $_droot.table.can create line 0 $_yPos($j) $_DC(xsize) $_yPos($j) -tags rowLine_$j
	$_droot.table.can create line 0 $_yPos($j) $_endX $_yPos($j) -tags rowLine_$j
    }

    _makeJustification
}

itcl::body Table::_getXYSize {x y} {


    for {set i $_firstCol} {$i <= $_dispCols} {incr i} {
	set tabXSize [expr $_absXPos($i) - $_absXPos([expr $_firstCol-1]) \
			  + $_DC(lmar) + $_DC(width) + $_DC(rightspace)/2 ]
	if { $tabXSize > $x } break
    }
    set _showCols [expr $i - $_firstCol]

    for {set j 1} {$j <= $_numRows} {incr j} {
	set tabYSize [expr 2*$_DC(tmar) + ($j)*($_DC(height) + $_DC(interline))]
	if { $tabYSize > $y } break
    }
    set _showRows [expr $j - 1]

    if { $_showRows < 1} {
	set _showRows 1
    }

    if { $_showCols < 1} {
	set _showCols 1
    }
    
    set lastCol [llength $_listPreSelectedColNames]

    if {$_showRows > $_numRows} {
	set _showRows $_numRows
    }

    if {$_showCols > $lastCol} {
	set _showCols $lastCol
    }

    if { $_firstCol > [expr $lastCol -$_showCols+1]} {
	set _firstCol [expr $lastCol -$_showCols+1]
    }
    if { $_firstRow > [expr $_numRows -$_showRows+1]} {
	set _firstRow [expr $_numRows -$_showRows+1]
    }
}

itcl::body Table::_layoutCan {x y} {
    set oldShowCols $_showCols
    set oldShowRows $_showRows
    set _old_firstCol $_firstCol

    _getXYSize $x $y

    if { $_showRows > $oldShowRows } {
	_addRowsToTable $oldShowCols $oldShowRows
    } elseif { $_showRows <  $oldShowRows} {
	_delRowsFrTable $oldShowCols $oldShowRows
    } 

    _calXPos $_old_firstCol $_showCols
    if { $_showCols > $oldShowCols } {
	_addColsToTable $oldShowCols $oldShowRows
    } elseif { $_showCols <  $oldShowCols} {
	_delColsFrTable $oldShowCols $oldShowRows
    } 
   
}

itcl::body Table::_updateExportPanelState {} {

   catch {
      if { $_exportsel == 2 } {
         .table_saveas.r.frow configure -state normal
         .table_saveas.r.lrow configure -state normal
      } else {
         .table_saveas.r.frow configure -state disable
         .table_saveas.r.lrow configure -state disable
      }
   
      if { $_exportselc == 2 } {
         .table_saveas.r.fcol configure -state normal
         .table_saveas.r.lcol configure -state normal
      } else {
         .table_saveas.r.fcol configure -state disable
         .table_saveas.r.lcol configure -state disable
      }
   }
}

itcl::body Table::trySaveASCII {} {
    _saveASCII
}


itcl::body Table::_saveASCII {} {
    global g_titleFont

    set t [urlTail $fileName]
    set r [file root $t]
    set e [file ext $t]
    if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
	set e [file ext $r]
	set r [file root $r]
    }
    set sugName "${r}_t[expr $currentHDU-1].txt"

    set asciiFileName [getSelectedFileName $sugName]
    if { $asciiFileName == "" } return
    if { [file exist $asciiFileName] == 1 } {
	set feedBack [promptMsg "File $asciiFileName exists.\n overwrite?" \
         return Yes No] 

	if { $feedBack == "CANCEL" } return
    } 


##### saveas table options
    catch {destroy .table_saveas}
    set fileWin .table_saveas

    powToplevel $fileWin  .dummy
    wm title $fileWin "fv: Export to file options"
    if { ($_tableType == "ASCII Table") || ($_tableType == "Binary Table") } {
	wm geometry $fileWin 460x420
    } else {
	wm geometry $fileWin 460x420
    }

    frame $fileWin.f 

    iwidgets::feedback $fileWin.f.fdb -labeltext "Saved"  -labelfont g_titleFont
    pack $fileWin.f.fdb -fill x
    iwidgets::buttonbox $fileWin.f.bb
    pack $fileWin.f.bb -side top

    $fileWin.f.bb add continue -text "Save" \
	-command [itcl::code $this _saveTableToAscii $fileWin $asciiFileName]  -font g_titleFont
    $fileWin.f.bb add quit  -text "Cancel" \
	-command "destroy $fileWin" -font g_titleFont

##  reset these variables 
    set _exportFirstRow 1
    set _exportLastRow  $_numRows
    set _exportFirstCol 1
    set _exportLastCol  $_dispCols
    set _exportPrintRowNumbers 0
    set _exportCSV 1
    set _exportCharBetweenCols " "
    set _exportFixedFormat 0
    set _exportFormat "csv"

    frame $fileWin.r 

    radiobutton $fileWin.r.all \
		-text "All" -font g_titleFont \
                -variable [itcl::scope _exportsel] -value 1 \
                -command [itcl::code $this _updateExportPanelState] 

    if { ($_tableType == "Image") || ($_tableType == "Vector Table") } {
       radiobutton $fileWin.r.range \
		   -text "From:" -font g_titleFont \
                   -variable [itcl::scope _exportsel] -value 2 \
                   -command [itcl::code $this _updateExportPanelState]
    }
    radiobutton $fileWin.r.sel \
		-text "Selection" -variable [itcl::scope _exportsel] \
                -font g_titleFont -value 3 \
                -command [itcl::code $this _updateExportPanelState]
    $fileWin.r.all select

    label $fileWin.r.l -text "Rows:"
    grid $fileWin.r.l -column 0 -row 0 -pady 10 -padx 20
    grid $fileWin.r.all  -column 0 -row 1 -sticky w  -padx 20
    if { ($_tableType == "Image" ) || ($_tableType == "Vector Table")  } {
       grid $fileWin.r.range -column 0 -row 2 -sticky w -padx 20
    }
    grid $fileWin.r.sel  -column 0 -row 3 -sticky w -padx 20

    # not sure if member variables is the best way, or passing variables
    # along in a [itcl::code $this type args] way, if that's even possible
    if { ($_tableType == "Image") || ($_tableType == "Vector Table")   } {
       iwidgets::entryfield $fileWin.r.frow -labeltext "" \
                       -textvariable [itcl::scope _exportFirstRow] \
                       -textfont g_titleFont -labelfont g_titleFont

       iwidgets::entryfield $fileWin.r.lrow -labeltext "To:" \
                       -textvariable [itcl::scope _exportLastRow] \
                       -textfont g_titleFont -labelfont g_titleFont

       grid $fileWin.r.frow -column 1 -row 2 -sticky w
       grid $fileWin.r.lrow  -column 2 -row 2 -sticky w
    }

    pack $fileWin.r -side top

    # if user wants to choose columns, he should pre-select using right button
    if { ($_tableType == "ASCII Table") || ($_tableType == "Binary Table") || \
         ($_tableType == "Vector Table") || ($_tableType == "Image")} {
	# don't allow user to pick column
    	radiobutton $fileWin.r.allc \
		-text "All" -font g_titleFont \
                -variable [itcl::scope _exportselc] -value 1 \
                -command [itcl::code $this _updateExportPanelState] 
    	radiobutton $fileWin.r.selc \
		-text "Selection" -variable [itcl::scope _exportselc] \
                -font g_titleFont -value 2 \
                -command [itcl::code $this _updateExportPanelState]
    	$fileWin.r.allc select

    	label $fileWin.r.lc -text "Columns:"

        if { ($_tableType == "Image") || ($_tableType == "Vector Table") } {
           radiobutton $fileWin.r.crange \
                      -text "From:" -font g_titleFont \
                      -variable [itcl::scope _exportselc] -value 2 \
                      -command [itcl::code $this _updateExportPanelState]
	   iwidgets::entryfield $fileWin.r.fcol -labeltext "" \
	   	      -textvariable [itcl::scope _exportFirstCol] \
                      -textfont g_titleFont -labelfont g_titleFont
	   iwidgets::entryfield $fileWin.r.lcol -labeltext "To:" \
		      -textvariable [itcl::scope _exportLastCol] \
                      -textfont g_titleFont -labelfont g_titleFont
    	   grid $fileWin.r.lc -column 0 -row 4 -pady 10  -padx 20
    	   grid $fileWin.r.allc  -column 0 -row 5 -sticky w -padx 20
    	   grid $fileWin.r.crange  -column 0 -row 6 -sticky w -padx 20
    	   grid $fileWin.r.fcol  -column 1 -row 6 -sticky w 
    	   grid $fileWin.r.lcol  -column 2 -row 6 -sticky w 
        } else {
    	   grid $fileWin.r.lc -column 1 -row 0 -pady 10  -padx 50
    	   grid $fileWin.r.allc  -column 1 -row 1 -sticky w -padx 50
    	   grid $fileWin.r.selc  -column 1 -row 3 -sticky w -padx 50
        }
    } else {
	iwidgets::entryfield $fileWin.fcol -labeltext "First Column:" \
		-textvariable [itcl::scope _exportFirstCol] \
                -textfont g_titleFont -labelfont g_titleFont
	iwidgets::entryfield $fileWin.lcol -labeltext "Last Column:" \
		-textvariable [itcl::scope _exportLastCol] \
                -textfont g_titleFont -labelfont g_titleFont
	pack $fileWin.fcol  -side top
	pack $fileWin.lcol  -side top
    }

    checkbutton $fileWin.selcb -text "Use Selected Rows" \
	    -variable  [itcl::scope _exportselRows] \
	    -selectcolor $fvPref::checkBBgColor  \
	    -activeforeground black -activebackground $fvPref::globalBgColor \
	    -justify left -font g_titleFont

    checkbutton $fileWin.cb -text "Print Row Numbers" \
	    -variable  [itcl::scope _exportPrintRowNumbers] \
	    -selectcolor $fvPref::checkBBgColor  \
	    -activeforeground black -activebackground $fvPref::globalBgColor \
	    -justify left -font g_titleFont
    pack $fileWin.cb    -side top    -fill x -expand 1

    iwidgets::radiobox $fileWin.rb -labeltext "Output Format" \
	    -selectcolor $fvPref::checkBBgColor  \
	    -foreground black -background $fvPref::globalBgColor -labelfont g_titleFont

    if { ($_tableType == "ASCII Table") || ($_tableType == "Binary Table") } {
	$fileWin.rb add fixed -text "Fixed Width Columns" -font g_titleFont
    }

    $fileWin.rb add csv \
	    -text "CSV format (\"7\",\"8\"...format)" -font g_titleFont
    $fileWin.rb add userdefine \
	    -text "User-defined separator (specify below)" -font g_titleFont
    $fileWin.rb select csv
    pack $fileWin.rb -fill both

    iwidgets::entryfield $fileWin.ins -labeltext \
        "Column Separator (for example: , or | ... default is \" \" ):" \
        -textvariable [itcl::scope _exportCharBetweenCols] -textfont g_titleFont \
        -labelfont g_titleFont
    pack $fileWin.ins   -side top    -fill x -expand 1
    pack $fileWin.f     -side bottom -fill x

    catch {
       .table_saveas.r.frow configure -state disable
       .table_saveas.r.lrow configure -state disable
       .table_saveas.r.fcol configure -state disable
       .table_saveas.r.lcol configure -state disable }
}

itcl::body Table::_writeHist { ffile_ origName_ } {
   global env
   global isWin

   set date [clock format [clock seconds] \
            -format %Y-%m-%dT%H:%M:%S]

      if { [info exists env(USER)] } {
         set uname $env(USER)
      } elseif { [info exists env(USERNAME)] } {
         set uname $env(USERNAME)
      } elseif { [info exists env(LOGNAME)] } {
         set uname $env(LOGNAME)
      } else {
         set uname "???"
      }

    $ffile_ put history "  File exported from $origName_ by user \'$uname\' with fv  on $date"
}

itcl::body Table::_saveTableToFile {win_ fitsFileName_ origName_} {
 set firstrow 0
 set delrows 0
 set tdelrows 0

 if { $_exportsel == 1 } {
       $win_.f.fdb configure -steps 1
       set _exportLastRow $_numRows
       set _exportFirstRow 1
       $fFile copyCHDU  $fitsFileName_
		     if {[catch {$win_.f.fdb step}] == 1} {
	          	file delete $fitsFileName_
	          	return
	      	     }
       set outfFile [fits open $fitsFileName_ 1]
       if { $_tableType == "Image" } {
         $outfFile move 1
       } else {
         $outfFile move 2
       }
          
        _writeHist $outfFile $origName_
        $outfFile checksum update
#       $outfFile close
 } elseif {$_exportsel == 2} {
       $win_.f.fdb configure -steps 2
       $fFile copyCHDU $fitsFileName_
       set outfFile [fits open $fitsFileName_ 1]
       if { $_tableType == "Image" } {
         $outfFile move 1
       } else {
         $outfFile move 2
       }
       
       set delrows [expr $_exportFirstRow -1]
       set firstrow 1
       $outfFile delete rows $firstrow $delrows
		     if {[catch {$win_.f.fdb step}] == 1} {
	          	file delete $fitsFileName_
	          	return
	      	     }
       
       set firstrow [expr $_exportLastRow - $delrows +1]
       set delrows [expr $_numRows - $_exportLastRow ]
       $outfFile delete rows $firstrow $delrows
		     if {[catch {$win_.f.fdb step}] == 1} {
	          	file delete $fitsFileName_
	          	return
	      	     }
        _writeHist $outfFile $origName_
        $outfFile checksum update
#       $outfFile close
 } else {
       $fFile copyCHDU $fitsFileName_
       set outfFile [fits open $fitsFileName_ 1] 
#       set outfFile [openFitsFile $fitsFileName_]
       if { $_tableType == "Image" } {
         $outfFile move 1
       } else {
         $outfFile move 2
       }

       set nblock 1
       for { set i  1 } {$i <=$_numRows } { incr i } {
            if { $_selectedRows($i) == 1 } {
                 set _selectedRows($i) 0
            } else {
                 set _selectedRows($i) 1
            }
       }

#       set nblock [expr $_numRows/1000000]

#       if { $_numRows > [expr $nblock*1000000] } {
#            incr nblock
#       }

       $win_.f.fdb configure -steps $nblock

#       for { set i 1 } { $i <=$nblock } { incr i} {
#           set start [expr ($i-1)*1000000 +1]
#           set end [expr $i*100000]
#           if { $end > $_numRows } {
#                set end $_numRows
#           }
#           set start_end "$start-$end"
           set start_end "1-$_numRows"
 
           set selrange [_parseToRowRange $start_end]
           $outfFile delete rows -range $selrange
	   if {[catch {$win_.f.fdb step}] == 1} {
	       	file delete $fitsFileName_
	       	return
           }

#       }

#       for { set i  1 } {$i <=$_numRows } { incr i } {
#            if { $_selectedRows($i) == 0} {
#                 incr nblock
#                    while {$_selectedRows($i) == 0 } {
#                         incr i
#                         if { $i > $_numRows} {
#                           break
#                         }
#                    }
#             }
#       }
#       $win_.f.fdb configure -steps $nblock
         
#       for { set i  1 } {$i <=$_numRows } { incr i } {
#              if { $_selectedRows($i) == 0} {
#                    set firstrow [expr $i - $tdelrows]
##                    puts "first : $i"
#                    while {$_selectedRows($i) == 0 } {
#                         incr i
#                         if { $i > $_numRows} {
#                           break
#                         }
#                    }
##                    puts "last: $i"
#                    set delrows [expr $i - $firstrow - $tdelrows]
#
#                    incr tdelrows $delrows
#                    $outfFile delete rows $firstrow $delrows
#		     if {[catch {$win_.f.fdb step}] == 1} {
#	          	file delete $fitsFileName_
#	          	return
#	      	     }
#              }
#        }
#        $outfFile put history    "  Exported from $origName_"
        _writeHist $outfFile $origName_
        $outfFile checksum update
#        $outfFile close
  }
      

 set collist {}


 if { $_exportcolsel == 2 } {
         for {set i 1} { $i < $_exportFirstCol} {incr i} {
            lappend collist [lindex $_listPreSelectedColNames [expr $i -1]]
         }
         for {set i [expr $_exportLastCol +1]} { $i <= $_dispCols} {incr i} {
            lappend collist [lindex $_listPreSelectedColNames [expr $i -1]]
         }

         $outfFile delete cols $collist
 } elseif {$_exportcolsel == 3} {
         for {set i 0} { $i < $_dispCols} {incr i} {
             if {$_colNotchedState($i) == 0} {
                lappend collist [lindex $_listPreSelectedColNames $i]
             }
         }
         $outfFile delete cols $collist
  }
        

    destroy $win_
}


itcl::body Table::_saveTableToAscii {win_ asciiFileName_} {

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
	set _exportFixedFormat 0
    } elseif { $_exportFormat == "fixed" } {
	set _exportCSV 0
	set _exportFixedFormat 1
    } elseif { $_exportFormat == "userdefine" } {
	set _exportCSV 0
	set _exportFixedFormat 0
    }

    set collist {}
    set collistwidths {}

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


	         if { ($_tableType == "ASCII Table") || ($_tableType == "Binary Table") } {
                    if { $_exportselc == 1 } {
                       set collist $_listPreSelectedColNames
                       set collistwidths $listPrintWidths
                    } else {
                       set collist {}
                       set collistwidths {}
         	       for {set i 0} { $i < $_dispCols} {incr i} {
                           if {$_colNotchedState($i) == 1} {
                	      lappend collist [lindex $_listPreSelectedColNames $i]
                              lappend collistwidths [lindex $listPrintWidths $i]
             		   }
         	       }
                    }

                    $fFile saveTabToASCII $asciiFileName_ $filePrintMode \
		                          $fRow $nRows \
		                          $collist $collistwidths \
		                          $_exportFixedFormat \
		                          $_exportCSV \
		                          $_exportPrintRowNumbers \
		                          $_exportCharBetweenCols
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

	   # shift by the first user-selected row
	   set fRow [expr $fRow + $_exportFirstRow - 1]
	   if { ($_tableType == "ASCII Table") || ($_tableType == "Binary Table") } {
              if { $_exportselc == 1 } {
                 set collist $_listPreSelectedColNames
                 set collistwidths $listPrintWidths
              } else {
                 set collist {}
                 set collistwidths {}
                 for {set i 0} { $i < $_dispCols} {incr i} {
                     if {$_colNotchedState($i) == 1} {
                        lappend collist [lindex $_listPreSelectedColNames $i]
                        lappend collistwidths [lindex $listPrintWidths $i]
                     }
   	         }
              }

	      $fFile saveTabToASCII $asciiFileName_ $filePrintMode \
		    $fRow $nRows \
		    $collist $collistwidths \
		    $_exportFixedFormat \
		    $_exportCSV \
		    $_exportPrintRowNumbers \
		    $_exportCharBetweenCols

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
    }
    destroy $win_
}

itcl::body Table::tryExport {} {
    _export
}


itcl::body Table::_export {} {
    global g_titleFont

    set t [urlTail $fileName]
    set r [file root $t]
    set e [file ext $t]
    if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
	set e [file ext $r]
	set r [file root $r]
    }
    set origName "${r}.fits\[[expr $currentHDU-1]\]"
    set sugName "${r}_t[expr $currentHDU-1].fits"

    set fitsFileName [getSelectedFileName $sugName]
    if { $fitsFileName == "" } return
    if { [file exist $fitsFileName] == 1 } {
	set feedBack [promptMsg "File $fitsFileName exists.\n overwrite?" \
         return Yes No] 

	if { $feedBack == "CANCEL" } return
    } 


##### saveas table options
    catch {destroy .table_saveas}
    set fileWin .table_saveas

    powToplevel $fileWin  .dummy
    wm title $fileWin "fv: Export to file options"
    if { ($_tableType == "ASCII Table") || ($_tableType == "Binary Table") } {
	wm geometry $fileWin 400x350
    } else {
	wm geometry $fileWin 400x350
    }

    frame $fileWin.f 

    iwidgets::feedback $fileWin.f.fdb -labeltext "Saved"  -labelfont g_titleFont
    pack $fileWin.f.fdb -fill x
    iwidgets::buttonbox $fileWin.f.bb
    pack $fileWin.f.bb -side top

    $fileWin.f.bb add continue -text "Save" \
	-command [itcl::code $this _saveTableToFile $fileWin $fitsFileName $origName]  -font g_titleFont
    $fileWin.f.bb add quit  -text "Cancel" \
	-command "destroy $fileWin" -font g_titleFont

##  reset these variables 
    set _exportFirstRow 1
    set _exportLastRow  $_numRows
    set _exportFirstCol 1
    set _exportLastCol  $_dispCols
    set _exportPrintRowNumbers 0
    set _exportCSV 1
    set _exportCharBetweenCols " "
    set _exportFixedFormat 0
    set _exportFormat "csv"

    frame $fileWin.r 

    frame $fileWin.r.r
    frame $fileWin.r.c

    # Pan Chai: below is for itcl trace method only.
    #           without using itcl, it will be 
    #                   "trace variable _exportsel w _updateExportPanelState"
    # does not seems to do anything.. Do we even need this code?
    trace variable [itcl::scope _exportsel] w "itcl::code $this _updateExportPanelState" 

    radiobutton $fileWin.r.r.all \
		-text "All" -font g_titleFont \
                -variable [itcl::scope _exportsel] -value 1 -command [itcl::code $this _updateExportPanelState] 
#    radiobutton $fileWin.r.range \
#		-text "From:" -font g_titleFont -variable [itcl::scope _exportsel] -value 2 \
#                           -command [itcl::code $this _updateExportPanelState]
    radiobutton $fileWin.r.r.sel \
		-text "Selection" -variable [itcl::scope _exportsel] -font g_titleFont -value 3 \
                           -command [itcl::code $this _updateExportPanelState]
    $fileWin.r.r.all select

#    pack $fileWin.allrb  -side top
#    pack $fileWin.rangerb  -side top
#    pack $fileWin.selrb  -side top
    label $fileWin.r.r.l -text "Rows:"
    grid $fileWin.r.r.l -column 0 -row 0 -pady 10  -sticky w 
    grid $fileWin.r.r.all  -column 0 -row 1 -sticky w
#    grid $fileWin.r.range -column 0 -row 2 -sticky w 
    grid $fileWin.r.r.sel  -column 0 -row 3 -sticky w

#    frame $fileWin.c 

    radiobutton $fileWin.r.c.all \
		-text "All" -font g_titleFont \
                -variable [itcl::scope _exportcolsel] -value 1 -command [itcl::code $this _updateExportPanelState] 
#    radiobutton $fileWin.c.range \
#		-text "From:" -font g_titleFont -variable [itcl::scope _exportcolsel] -value 2 \
#                           -command [itcl::code $this _updateExportPanelState]
    radiobutton $fileWin.r.c.sel \
		-text "Selection" -variable [itcl::scope _exportcolsel] -font g_titleFont -value 3 \
                           -command [itcl::code $this _updateExportPanelState]
    $fileWin.r.c.all select

#    pack $fileWin.allrb  -side top
#    pack $fileWin.rangerb  -side top
#    pack $fileWin.selrb  -side top
    label $fileWin.r.c.l -text "Columns:"
    grid $fileWin.r.c.l -column 1 -row 0 -pady 10 -sticky w  
    grid $fileWin.r.c.all  -column 1 -row 1 -sticky w  
#    grid $fileWin.c.range -column 0 -row 2 -sticky w  
    grid $fileWin.r.c.sel  -column 1 -row 3 -sticky w 
#    iwidgets::entryfield $fileWin.c.fcol -labeltext "" \
#        -textvariable [itcl::scope _exportFirstCol] -textfont g_titleFont -labelfont g_titleFont
#    iwidgets::entryfield $fileWin.c.lcol -labeltext "To:" \
#        -textvariable [itcl::scope _exportLastCol] -textfont g_titleFont -labelfont g_titleFont
#    grid $fileWin.c.fcol -column 1 -row 2 -sticky w
#    grid $fileWin.c.lcol  -column 2 -row 2 -sticky w

    # not sure if member variables is the best way, or passing variables
    # along in a [itcl::code $this type args] way, if that's even possible
#    iwidgets::entryfield $fileWin.frow -labeltext "First Row:" 
#    iwidgets::entryfield $fileWin.r.frow -labeltext "" \
#        -textvariable [itcl::scope _exportFirstRow] -textfont g_titleFont -labelfont g_titleFont
#    iwidgets::entryfield $fileWin.lrow -labeltext "Last Row:" 
#    iwidgets::entryfield $fileWin.r.lrow -labeltext "To:" \
#        -textvariable [itcl::scope _exportLastRow] -textfont g_titleFont -labelfont g_titleFont
#    iwidgets::entryfield $fileWin.rowrange -labeltext "Row Range:" \
#        -textvariable [itcl::scope _exportRowRange] -textfont g_titleFont -labelfont g_titleFont
#    grid .fv_imgsel.label  -column 0 -row 0  -columnspan 4 -sticky "snew"
#    grid $fileWin.r.frow -column 1 -row 2 -sticky w
#    grid $fileWin.r.lrow  -column 2 -row 2 -sticky w
#    pack $fileWin.frow  -side top
#    pack $fileWin.lrow  -side top
#    pack $fileWin.rowrange  -side top

#    grid $fileWin.r -column 0 -row 0
#    grid $fileWin.c -column 1 -row 0

    pack $fileWin.r.r -side left -padx 40
    pack $fileWin.r.c -side right -padx 40
    pack $fileWin.r -side top 
#    pack $fileWin.c -side top

    # if user wants to choose columns, he should pre-select using right button
    if { ($_tableType == "ASCII Table") || ($_tableType == "Binary Table") } {
	# don't allow user to pick column
    } else {
#	iwidgets::entryfield $fileWin.fcol -labeltext "First Column:" \
#		-textvariable [itcl::scope _exportFirstCol] -textfont g_titleFont -labelfont g_titleFont
#	iwidgets::entryfield $fileWin.lcol -labeltext "Last Column:" \
#		-textvariable [itcl::scope _exportLastCol] -textfont g_titleFont -labelfont g_titleFont
#	pack $fileWin.fcol  -side top
#	pack $fileWin.lcol  -side top
    }


#    pack $fileWin.f     -side bottom -fill x
     pack $fileWin.f     -side bottom -fill x
#    grid $fileWin.f     -column 0  -row 7 -columnspan 3 -pady 10 -ipadx 90 
}


itcl::body Table::_saveFile {} {

# Save File
    $fFile save
    _updateHL
}

itcl::body Table::_saveAs {} {
    $fFile saveAs
}

itcl::body Table::_updateHL {} {
# Override this function in descendents
}

itcl::body Table::_closeFrWm {w} {
    if { $w == $_droot && !$_isBeingDestroyed } {
	closeCmd
    } 
}

itcl::body Table::closeCmd {} {
   foreach i $_myChildren {
       itcl::delete object $i
   }
   _updateHL
   _realCloseCmd
}

itcl::body Table::setFileName { fName_ } {
#    set fileName $fName_
    if ![winfo exists $_droot] return
    set rName [urlTail $fName_]
    set dName [getFullDirPath $fName_]
    wm title $_droot "fv: $_tableType of $rName\[[expr $currentHDU-1]\] in $dName"
    # wm geometry $_droot 400x200
}

itcl::body Table::_realCloseCmd {} {

# let the winkeeper know it's gone
    if { $_isDirPlot == 0 } {
	.fvwinkeeper signoff $_droot
    }

    itcl::delete object $this
}


itcl::body Table::_updateNumRows {} {
#Ziqin Apr 01,04
    if { $_tableType == "Image"} {
        if { [llength [$fFile getImgInfo]] == 1 } {
	    set _numRows  [lindex [$fFile getImgInfo] 0]      
        } else {
	    set _numRows  [lindex [$fFile getImgInfo] 1]      
        }
    } else {
	set _numRows  [$fFile getTableInfo nrows]
    }

}

itcl::body Table::_initSelRows {} {
    _updateNumRows
    for {set i 0} { $i <= $_numRows} {incr i} {
        set _selectedRows($i) 0
    }

}

itcl::body Table::_updateNumCols {} {
   # Do nothing here.  Override in subclasses
}



itcl::body Table::_addCols {{colindex -1 } {colname {} } {colformat {}} { colunit {}} {dispformat {}} } {
    # Init the variables
    set _addcolname ""
    set _addcolunit ""
    set _addcoldisp ""
    set _addcolform ""

    set addWin .addCol
    catch {destroy .addCol}
    powToplevel $addWin .dummy
    wm title $addWin "fv : add column info"
    wm geometry $addWin +[winfo pointerx .]+[winfo pointery .]

    iwidgets::entryfield $addWin.name -labeltext "Column Name" -labelpos w \
	-validate "validColName %c" -fixed 68 -width 10 \
	-textvariable [itcl::scope _addcolname]
# the format list
    if { $_tableType == "Binary Table"} {
	set formatList {{1A - ASCII string} {1L - logical (T or F)} \
		    {1X - bit} {1B - 1 byte unsigned integer} \
		    {1I - 2 byte integer}  {1J - 4 byte integer}\
		    {1K - 8 byte integer} \
		    {1E - 4 byte real} {1D - 8 byte double} \
		    {1C - 8 byte complex} {1M - 16 byte double complex} }
    } elseif { $_tableType == "ASCII Table"} {
	set formatList \
	    {{A10 - 10 chars ASCII string} {I10 - 10 digits integer} \
		 {F10.2  - 10 digits real with two sig digits} \
		 {E10.2  - 10 digits real with exp} \
		 {D20.10 - 20 digits double with 10 sig digits}}
    } else {
	set formatList { }
    }

    iwidgets::combobox $addWin.form -labeltext "Column Format" -labelpos w \
	-textvariable [itcl::scope _addcolform] -completion 0
    eval $addWin.form insert list end $formatList

    iwidgets::entryfield $addWin.unit -labeltext "Column Unit" -labelpos w \
	-fixed 68 -width 10 -textvariable [itcl::scope _addcolunit]
    iwidgets::entryfield $addWin.disp -labeltext "Display Format" -labelpos w \
	-fixed 68 -width 10 -textvariable [itcl::scope _addcoldisp]

    set tmpList $_listPreSelectedColNames
    set tmpList [lappend tmpList "End of Table"]
    # needs to be reset in case it was altered last time
    set _insColNum "End of Table"
    iwidgets::combobox $addWin.colist -labeltext "Insert Before" -labelpos w \
	-editable 1 -textvariable [itcl::scope _insColNum]
    eval $addWin.colist insert list end $tmpList

    if { $colindex !=-1 && $colname != "" } {
        set _addcolname $colname
        set _addcolform $colformat
        set _addcolunit $colunit
        set _addcoldip  $dispformat
        if { $colindex == "-" } {
            set colindex "End of Table"
        }
        set _insColNum $colindex
        _addColsToFile $addWin
        return
    }

    if { $colindex != -1 } {
	$addWin.colist selection clear 0 end
	$addWin.colist selection set $colindex
    }

    grid $addWin.name   -row 1 -column 1 -sticky ew -padx 10 -pady 2
    grid $addWin.form   -row 2 -column 1 -sticky ew -padx 10 -pady 2
    grid $addWin.unit   -row 3 -column 1 -sticky ew -padx 10 -pady 2
    grid $addWin.disp   -row 4 -column 1 -sticky ew -padx 10 -pady 2
    grid $addWin.colist -row 5 -column 1 -sticky ew -padx 10 -pady 2
    iwidgets::Labeledwidget::alignlabels \
          $addWin.name $addWin.unit $addWin.form $addWin.colist

    iwidgets::buttonbox $addWin.bb

    $addWin.bb add OK -text OK -command [itcl::code $this _addColsToFile $addWin]
    $addWin.bb add Cancel -text Cancel -command "destroy $addWin"

    grid $addWin.bb     -row 6 -column 1

    tkwait window $addWin
}


itcl::body Table::_addRowsToFile {inWin} {
    set rowIdx [$inWin.after get]
    set rowNum [$inWin.number get]
    regsub -all " " $rowIdx "" tmpIdx
    regsub -all " " $rowNum "" tmpNum
# delete the input window
    destroy $inWin

    if { ($tmpIdx == "") || ($tmpNum == "") } {
	error "Please input number"
	return
    } elseif { $tmpIdx=="end" } {
       set tmpIdx $_numRows
    }
    #  We asked for row "after", but CFITSIO inserts "before"
    incr tmpIdx
#    if { $tmpIdx > $_numRows} {
#	$fFile addRow $tmpNum
#    } else {
#	$fFile insertRow $tmpIdx $tmpNum
#    }
    if { $tmpIdx > $_numRows} {
        $fFile addRow $tmpNum
        for { set i 1} { $i <=$tmpNum } {incr i} {
          set _selectedRows([expr $i+$_numRows]) 0
        }
    } else {
        $fFile insertRow $tmpIdx $tmpNum
        for { set i $_numRows } { $i >= $tmpIdx } { set i [expr $i -1] } {
              set _selectedRows([expr $i + $tmpNum]) $_selectedRows($i)
        }

        for { set i 0 } { $i < $tmpNum } { incr i} {
              set _selectedRows([expr $tmpIdx +$i]) 0
        }
    }

    $fFile changeFile
    refresh
    _updateRestDisps
}

# Add an existing table column to the view
itcl::body Table::_addColView { colName_ addPos_ beforeAfter_ } {
    if { $addPos_ == "END" } {
       set tmpIdx [llength $_listPreSelectedColNames]
    } else {
       set tmpIdx  [lsearch $_listPreSelectedColNames $addPos_]

       if { $beforeAfter_ == "after" } {
	   incr tmpIdx
       }

       if { ($tmpIdx == -1) } {
	   error "No such column: $addPos_"
	   return
       }
    }

    set _listPreSelectedColNames [linsert $_listPreSelectedColNames $tmpIdx  $colName_]

    for {set j $_dispCols} {$j > $tmpIdx} {incr j -1} {
	set _cellWidth($j) $_cellWidth([expr $j-1])
    }
    if { $j < $_dispCols } {
	unset _cellWidth($j)
    }

    refresh
}

# Remove the column from the view (but don't delete it from the table)
itcl::body Table::_delColView { colName_ } {
    set tmpIdx  [lsearch $_listPreSelectedColNames $colName_]

    if { ($tmpIdx == -1) } {
	error "No such column: $colName_"
	return
    }

    set _listPreSelectedColNames [lreplace $_listPreSelectedColNames $tmpIdx  $tmpIdx]

    for {set j $tmpIdx} {$j < [expr $_dispCols-1]} {incr j} {
	set _cellWidth($j) $_cellWidth([expr $j+1])
    }
    unset _cellWidth([expr $_dispCols-1])

    refresh
}


itcl::body Table::_addColsToFile {inWin} {
#puts "_addColsToFile start"

    set addPos  $_insColNum
# delete the input window
    destroy $inWin
    
    set tmpcolform [lindex $_addcolform 0] 

    regsub -all " " $_addcolname "" tmpName
    regsub -all " " $_addcolunit "" tmpUnit
    regsub -all " " $_addcoldisp "" tmpDisp
    regsub -all " " $tmpcolform "" tmpForm
    if { ($tmpName == "") || ($tmpForm == "")} {
	error "Column name/form can't be blank"
	return
    }

    if { $addPos == "End of Table" } {
	$fFile addColumn $_addcolname $tmpcolform
	lappend _listPreSelectedColNames $_addcolname
	lappend _listColNames   $_addcolname
        if { $tmpUnit != "" } {
	   $fFile putKwd "TUNIT[llength $_listColNames] '$tmpUnit'" 1 
        }
        if { $tmpDisp != "" } {
	   $fFile putKwd "TDISP[llength $_listColNames] '$tmpDisp'" 1 
        }
    } else {
	set tmpIdx  [lsearch $_listPreSelectedColNames $addPos]
	set tmpIdx1 [lsearch $_listColNames   $addPos]
        if { ($tmpIdx == -1) || ($tmpIdx1 == -1)} {
	    error "No such column"
	    return
	}
        $fFile insertCol [expr $tmpIdx1+1] $_addcolname $tmpcolform
        set _listPreSelectedColNames [linsert $_listPreSelectedColNames $tmpIdx  $_addcolname]
        set _listColNames   [linsert $_listColNames   $tmpIdx1 $_addcolname]
        if { $tmpUnit != "" } {
           $fFile putKwd "TUNIT[expr $tmpIdx1+1] '$tmpUnit'" 1 
        }
        if { $tmpDisp != "" } {
	   $fFile putKwd "TDISP[expr $tmpIdx1+1] '$tmpDisp'" 1 
        }
        for {set j $_dispCols} {$j > $tmpIdx} {incr j -1} {
           set _cellWidth($j) $_cellWidth([expr $j-1])
        }
        unset _cellWidth($j)
    }

# let the fFile know that the file has been changed
    $fFile changeFile
# update the displayed table and children
    refresh
    _updateRestDisps
}

itcl::body Table::refresh { {doChildren_ 1} } {

    if ![info exists _DC(xsize)] return

    # Re-read table information
    _readInTable

    # Check the number of rows and columns in the table
    set oldNumCols $_numCols
    set oldNumRows $_numRows
    _updateNumCols
    _updateNumRows
    if { $oldNumCols != $_numCols || $oldNumRows != $_numRows } {
       _layoutCan $_DC(xsize) $_DC(ysize) 
       _reframe
    }
    
    # recalculate the _firstCol of the last page
    _getLastFirstCol

    # redraw the table and more
    _setHScroll [expr $_firstCol-1]
    _refacing

    # Update any children of this table, if they exist
    if { $doChildren_ } {
       foreach child $_myChildren {
          $child refresh
       }
    }
}


itcl::body Table::_delCols {{collist -1}} {
    if { $_tableType == "Image"} {
	error "Cannot delete columns in an image"
	return
    }
# clean the cell selection
    _unselCell
    set delColList {}
    set tmpList {}
    set j 0
#    for {set i 0} { $i < $_dispCols} {incr i} {
#	if { $_colNotchedState($i) == 1 } {
#	    lappend delColList [lindex $_listPreSelectedColNames $i]
#	} else {
#	    lappend tmpList [lindex $_listPreSelectedColNames $i]
#            set _cellWidth($j) $_cellWidth($i)
#            incr j
#	}
#    }

    for {set i 0} { $i < $_dispCols} {incr i} {
        if { $collist == -1} {
           if { $_colNotchedState($i) == 1 } {
               lappend delColList [lindex $_listPreSelectedColNames $i]
           } else {
               lappend tmpList [lindex $_listPreSelectedColNames $i]
               set _cellWidth($j) $_cellWidth($i)
               incr j
           }
        } else {
           if { [lsearch -exact [string toupper $collist] [lindex $_listPreSelectedColNames $i]] !=-1 } {
               lappend delColList [lindex $_listPreSelectedColNames $i]
           } elseif { [lsearch -exact [string tolower $collist] [lindex $_listPreSelectedColNames $i]] !=-1 } {
               lappend delColList [lindex $_listPreSelectedColNames $i]
           } else {
               lappend tmpList [lindex $_listPreSelectedColNames $i]
               set _cellWidth($j) $_cellWidth($i)
               incr j
           }
        }
    }

    for {} {$j < $_dispCols} {incr j} {
       unset _cellWidth($j)
    }
    set _listPreSelectedColNames $tmpList
#   update the _firstRow : updateFirst is a fitsTcl command
    set _firstCol [updateFirst -c $_firstCol $_dispCols]
#   delete row in the file
    $fFile delCols $delColList
    $fFile changeFile
# then update table info
#    _updateNumCols
# refresh the table and children
    refresh
    _updateRestDisps
# reselect the cell
    _selCell
    _showselRows
}
itcl::body Table::_tryDelRows { {range 0} } {
    if { $range !=0 } {
        selRowsFromList $range 0 0
    }
    promptMsg \
	  "Table rows will be permanently deleted.\nAre you sure?" \
	  [itcl::code $this _delRowsRange] Yes No
#	  [itcl::code $this _delRows] Yes No
}

itcl::body Table::_tryDelCols {{collist -1}} {

    promptMsg \
	  "Table columns will be permanently deleted.\nAre you sure?" \
	  [itcl::code $this _delCols $collist] Yes No
}


########################################################
#
#        Row Selection
#        Ziqin Pan, Jan 2004
#

itcl::body Table::_setRowStartMark {row} {
    set _rstart  [expr $_firstRow + $row ]
    set _rend  [expr $_firstRow + $row]
    _selRows
    _showselRows
}

itcl::body Table::_setRowEndMark {row} {
    set _rend  [expr $_firstRow + $row ]
    _selRows
    _showselRows
}

itcl::body Table::_unsetRowMark {} {
    set _rstart ""
    set _rend "" 
}

itcl::body Table::_showselCheck {} {
    set rowsel 0

    for {set j 1} {$j <= $_numRows} {incr j} {
#           set realRowNum [expr $_firstRow+$j]
           if { ($_selectedRows($j) ==1) } {
                 set rowsel 1
                 break
           }
    }
    if { $rowsel == 1} {
         if {$_fth !=0 } {
              $_fth turnsel 1
         }
         if {$_ftp !=0 } {
              $_ftp turnsel 1
         }
    } else {
         if {$_fth !=0 } {
              $_fth turnsel 0
         }
         if {$_ftp !=0 } {
              $_ftp turnsel 0
         }
    }
}

itcl::body Table::_showselRows {} {

    set rowsel 0

    for {set j 0} {$j < $_showRows} {incr j} {
           set realRowNum [expr $_firstRow+$j]
           if { ($_selectedRows($realRowNum) ==1) } {
                 $_droot.table.can.b$j  configure -background yellow
                 set rowsel 1
           } else {
                 $_droot.table.can.b$j  configure -background $fvPref::globalBgColor 
           }
    }

    _showselCheck

}

itcl::body Table::_selInvert {}  {
     for {set j 0} {$j <= $_numRows} {incr j} {
          if { ($_selectedRows($j) == 1)} {
              set _selectedRows($j) 0
          } else {
              set _selectedRows($j) 1
          }
     } 
     _showselRows
}

itcl::body Table::_selAllRows {} {


     if { ($_selectAllRows == 1 ) } {
           for {set j 0} {$j <= $_numRows} {incr j} {
               set _selectedRows($j) 1
           } 
     } else {
           for {set j 0} {$j <= $_numRows} {incr j} {
               set _selectedRows($j) 0
           } 
     }

     _showselRows
}

    

itcl::body Table::_selRows {} {
    if { $_rstart > $_rend } {
         set tmp_rstart  $_rend
         set tmp_rend  $_rstart
    } else {
         set tmp_rstart  $_rstart
         set tmp_rend  $_rend
    }

    if { ($_rstart == "") || ($_rend =="") } {
        return
    } elseif { ($_rstart == $_rend)} {
           if { ($_selectedRows($_rstart) == 0) } {
                 set _selectedRows($_rstart) 1
           } else {
                 set _selectedRows($_rstart) 0
           }
    } else {
        for {set i $tmp_rstart} { $i <= $tmp_rend} { incr i} { 
           if { ($_selectedRows($_rstart) == 0) } {
                 set _selectedRows($i) 0
           } else {
                 set _selectedRows($i) 1
           }
        }
    }
}

              



      

########################################################
#
#        Cursor  Motion  Selection
#

itcl::body Table::_setEndMark {col row} {
    _unselCell
#
    if { $_oldcstartx == ""} return
    if { $_oldcstarty == ""} return

    set _cendx   [expr $_firstCol + $col -1]
    set _cendy   [expr $_firstRow + $row -1]
    if { $_cendx <= $_oldcstartx} {
	set _cstartx $_cendx
	set _cendx $_oldcstartx
    } else {
        set _cstartx $_oldcstartx
    }
    if { $_cendy <= $_oldcstarty} {
	set _cstarty $_cendy
	set _cendy $_oldcstarty
    } else {
        set _cstarty $_oldcstarty
    }
    
    _selCell
    _showselRows
}

itcl::body Table::_unsetMark {} {
    _unselCell
    set _oldcstartx ""
    set _oldcstarty ""
    set _cstartx ""
    set _cstarty ""
    set _cendx ""
    set _cendy ""
}

itcl::body Table::_setStartMark {col row {focus True}} {
    _unsetMark
    set _cstartx [expr $_firstCol + $col -1]
    set _cstarty [expr $_firstRow + $row -1]
    set _oldcstartx $_cstartx
    set _oldcstarty $_cstarty

    _copyCell $col $row
    if { $focus } {
	focus $_droot.table.can
    }
}

itcl::body Table::_copyCell {col_ row_} {
    _unselCell
    set _currentCol [expr $_firstCol+ $col_ -1]
    set _currentRow [expr $_firstRow+ $row_ -1]
    if { $_currentCol<0 } {
       set _currentCol 0
    } elseif { $_currentCol >= $_dispCols } {
       set _currentCol [expr $_dispCols-1]
    }
    if { $_currentRow<0 } {
       set _currentRow 0
    } elseif { $_currentRow >= $_numRows } {
       set _currentRow [expr $_numRows-1]
    }
    _selCell
    _showselRows
}

itcl::body Table::_unselCell {} {

# individual cell
    if { [info exist _currentCol] } {
	set tmpCol [expr $_currentCol - $_firstCol+1]
	set tmpRow [expr $_currentRow - $_firstRow+1]
	if { ($tmpCol >= $_showCols) || ($tmpCol < 0) || ($tmpRow >= $_showRows) \
		 || ($tmpRow < 0) } {
	    ;
	} else {
	    set id ${tmpCol}_$tmpRow
	    $_droot.table.can.e$id configure -disabledbackground $fvPref::globalBgColor
	    set _cellVar ""
	}
    }
# a block of table
    for {set i 0} { $i < $_showCols} { incr i} { 
       for {set j 0} { $j < $_showRows} { incr j} { 
          set id ${i}_$j
          $_droot.table.can.e$id configure -disabledbackground $fvPref::globalBgColor
       }
    }
}

itcl::body Table::_selCell {} {
#puts "_selCell start"

    if { [info exist _currentCol] } {
	set tmpCol [expr $_currentCol - $_firstCol+1]
	set tmpRow [expr $_currentRow - $_firstRow+1]
	if { ($tmpCol >= $_showCols) || ($tmpCol < 0) || \
              ($tmpRow >= $_showRows) || ($tmpRow < 0) } {
           $_droot.usrEntry.sel_e config -state disabled
	} else {
           $_droot.usrEntry.sel_e config -state normal
	    set id ${tmpCol}_$tmpRow
	    $_droot.table.can.e$id configure -disabledbackground yellow
            set _cellVar [_getRawData $_currentCol $_currentRow]
            $_droot.usrEntry.sel_e selection range 0 end
	}
    }

# a block of table
    if { ($_cstartx == "") || ($_cstarty == "") || ($_cendx == "")  \
	     || ($_cendy == "") } {
	return
    } else {
	for {set i 0} {$i< $_showCols} {incr i} {
	    set tmpCol [expr $_firstCol +$i -1]
	    if { ($tmpCol >= $_cstartx) && ($tmpCol<=$_cendx) } { 
		for {set j 0} { $j < $_showRows} { incr j} { 
		    set tmpRow  [expr $_firstRow+$j-1]
		    if { ($tmpRow >= $_cstarty) && ($tmpRow<=$_cendy) } {  
			set id ${i}_$j
			$_droot.table.can.e$id configure -disabledbackground green 		
		    }
		}
	    }
	}
    }

}



itcl::body Table::_cellUp {} {
# if no cell is selected do nothing.
    if { [info exist _currentRow] == 0} return
# if up to the first row do nothing.
    if { $_currentRow <= 0 } return

# if the current row is on the last page scroll up
    if { $_currentRow < $_firstRow} {
	incr _firstRow -1 
	refresh 0
    }
#
    set tmpCol [expr $_currentCol - $_firstCol+1] 
    set tmpRow [expr $_currentRow - $_firstRow+1]
    _scrollChildren $_firstRow [expr $_currentRow-1]
    _setStartMark $tmpCol [expr $tmpRow-1]
}

itcl::body Table::_cellDown {} {
# if no cell selected, do nothing
    if { [info exist _currentRow] == 0} return
# if down to the last row, do nothing
    if { $_currentRow >  $_numRows  } return    
    if { $_currentRow >= [expr $_firstRow + $_showRows -2]} {
	incr _firstRow 
	refresh 0
    }
    set tmpCol [expr $_currentCol - $_firstCol+1] 
    set tmpRow [expr $_currentRow - $_firstRow+1]
    _scrollChildren $_firstRow [expr $_currentRow+1]
    _setStartMark $tmpCol [expr $tmpRow+1]
}

itcl::body Table::_pageUp {} {
    _unselCell

    if { [expr $_firstRow - $_showRows +1 ] < 1} {
	set _firstRow 1
    } else {
	set _firstRow [expr $_firstRow - $_showRows + 1 ]
    }

    refresh 0
    _selCell
    _showselRows
    
    _scrollChildren $_firstRow $_currentRow
}

itcl::body Table::_pageDown {} {
    _unselCell

    if { [expr $_firstRow + $_showRows -1] >  [expr $_numRows - $_showRows +1]} {
	set _firstRow [expr $_numRows - $_showRows +1]
    } else {
	set _firstRow [expr $_firstRow + $_showRows -1]
    }
    refresh 0
    _selCell
    _showselRows

    _scrollChildren $_firstRow $_currentRow
}

itcl::body Table::_jumpTo { colName } {
    set colIdx 0
    if { $colName != "0" && $colName != "end" } {
       set colIdx [lsearch -exact $_listPreSelectedColNames $colName]
    }

    if { $colName == "end" } {
       set colIdx [expr [llength $_listPreSelectedColNames] - 1 ]
    }

    _setHScroll $colIdx
}

itcl::body Table::_cellLeft {} {
# if no cell is selected do nothing.
    if { [info exist _currentCol] == 0} return
# if left to the first row do nothing.
    if { $_currentCol <= 0  } return    
# real move
    if { $_currentCol < $_firstCol } {
 	_setHScroll [expr $_firstCol -2]
    }    
#
    set tmpCol [expr $_currentCol - $_firstCol+1] 
    set tmpRow [expr $_currentRow - $_firstRow+1]
    _setStartMark [expr $tmpCol-1] $tmpRow
}

itcl::body Table::_cellRight {} {
# if no cell is selected do nothing.
    if { [info exist _currentCol] == 0} return
# if right to the last row do nothing.
    if { $_currentCol > $_dispCols  } return    
# real move
    while { $_currentCol >= [expr $_firstCol+$_showCols-2] \
          && [expr $_firstCol+$_showCols]<=$_dispCols } {
       _setHScroll $_firstCol
    }    

#
    set tmpCol [expr $_currentCol - $_firstCol+1] 
    set tmpRow [expr $_currentRow - $_firstRow+1]
    _setStartMark [expr $tmpCol+1] $tmpRow
}

itcl::body Table::_editCell {} {
   if { [$_droot.usrEntry.sel_e cget -state]=="disabled" } return
   if { $_cellVar=="" } {
      # Reset value
      _selCell
   } else {
      _modifyTableCell $_currentCol $_currentRow $_cellVar
   }
   focus $_droot.table.can
   $_droot.usrEntry.sel_e selection range 0 end
}


#
#                            End  Cursor Motion Selection
#
###########################################################

itcl::body Table::getFatherFitsExtension {} {
    return $_fatherFitsExtension
}

# called by VectorTable to get details of the column that gave birth to the
#    VectorTable
itcl::body Table::getColumnInfo {name_ index_} {
    return [set ${name_}($index_)]
}


itcl::body Table::_calculateCmd {} {

    set cal ".cal_[namespace tail $this]"
    if { [winfo exist $cal] == 1 } {
	raise $cal
	return 
    }
    set calList [getCalcCols]
    FitsCalculator $cal $this $calList
    $cal configure -title "fv: Calculator"
}

itcl::body Table::getCalcCols {} {
# pick out the variable-length and zero-lngth vector columns from the calculator
    set calList {}
    for {set i 0} {$i < $_dispCols} {incr i} {
	if { $_columnVecSize($i) > 0 } {
	    lappend calList [lindex $_listPreSelectedColNames $i]
	}
    }
    return $calList
}

itcl::body Table::calculateCols {colname_ colform_ formula_ calselonly_} {
    if { $calselonly_ == 0 } {
       set selrange "1-$_numRows"
       set result [catch {set isNew \
                   [$fFile addColumnFrExpr $colname_ $colform_ $formula_ $selrange] } err] 
    } else {
                set selrange ""
                set start_end "1-$_numRows"
                append selrange [_parseToRowRange $start_end]

                if { $selrange !="" } {
                set fileID [open out w 0600]
                close $fileID
                set result [catch {set isNew \
                   [$fFile addColumnFrExpr $colname_ $colform_ $formula_ $selrange] } err] 
                } else {
                   set isNew 0
                   set result 0
                }
    }
    
       

    if { $result == 1 } {
	error "Calculation failed\n$err"
    } 
    $fFile changeFile

    if { $isNew == 1 } {
	lappend _listPreSelectedColNames $colname_
    }

    _updateNumCols
    _reframe
    refresh
    _updateRestDisps
}

itcl::body Table::_updateRestDisps {} {
   # Update any other windows attached to this extension
   $_fatherFitsExtension updateDisps $this
}


itcl::body Table::_sortCmd {} {

    global g_titleFont
    catch {destroy .fvsort}
    powToplevel .fvsort .dummy
    wm title .fvsort "fv: Sort"

    _resetSKey
    set [itcl::scope _isUniqMerge] 0
    
    label   .fvsort.label -text "Select column name for sorting"  -font g_titleFont
    label   .fvsort.lbl -text "Columns" -font g_titleFont
    listbox .fvsort.lb -yscrollcommand ".fvsort.sb set" -font g_titleFont
    scrollbar .fvsort.sb -command ".fvsort.lb yview"

    checkbutton .fvsort.fkeyc -variable [itcl::scope _psortcheck] \
	-selectcolor $fvPref::checkBBgColor  \
        -font g_titleFont \
	-activeforeground black -activebackground $fvPref::globalBgColor \
        -width 0 	
    checkbutton .fvsort.skeyc -variable [itcl::scope _ssortcheck]  \
	-selectcolor $fvPref::checkBBgColor  \
        -font g_titleFont \
	-activeforeground black -activebackground $fvPref::globalBgColor \
        -width 0	

    checkbutton .fvsort.tkeyc -variable [itcl::scope _tsortcheck]  \
	-selectcolor $fvPref::checkBBgColor  \
        -font g_titleFont \
	-activeforeground black -activebackground $fvPref::globalBgColor \
        -width 0	

    frame   .fvsort.fsep -relief raised -borderwidth 2 -height 4
    frame   .fvsort.fsep1 -relief raised -borderwidth 2 -height 4
    checkbutton .fvsort.merge -variable [itcl::scope _isUniqMerge] \
	-selectcolor $fvPref::checkBBgColor -text  No \
        -font g_titleFont \
	-activeforeground black -activebackground $fvPref::globalBgColor \
        -width 0 -command [itcl::code $this _changeUniqLabel]

    label   .fvsort.mlabel -text "Delete multiple rows that have the same sort column values?" \
            -font g_titleFont

    button .fvsort.fkeyb -text "Primary Column" -command [itcl::code $this _getSortKey 1] -font g_titleFont
    button .fvsort.skeyb -text "Second Column" -command [itcl::code $this _getSortKey 2] -font g_titleFont
    button .fvsort.tkeyb -text "Third Column" -command [itcl::code $this _getSortKey 3] -font g_titleFont

    entry .fvsort.fkeye -textvariable [itcl::scope _psortkey]  -font g_titleFont
    entry .fvsort.skeye -textvariable [itcl::scope _ssortkey] -font g_titleFont
    entry .fvsort.tkeye -textvariable [itcl::scope _tsortkey] -font g_titleFont
    label .fvsort.ascend  -text "Ascending ?" -font g_titleFont
    label .fvsort.help  -text "Select Column(s) for sorting" -font g_titleFont

    iwidgets::buttonbox .fvsort.bb 
    .fvsort.bb add sort -text "Sort" -command [itcl::code $this _sortColumn] -font g_titleFont
    .fvsort.bb add clear -text "Clear" -command [itcl::code $this _resetSKey] -font g_titleFont
    .fvsort.bb add cancel -text "Close" -command "destroy .fvsort" -font g_titleFont
    .fvsort.bb add help -text "Help" -command "hhelp sortColumn" -font g_titleFont
    
    foreach i $_listPreSelectedColNames {
	.fvsort.lb insert end $i
    }
    grid configure .fvsort.lbl -column 0 -row 0 -sticky "snew" 
    grid configure .fvsort.lb -column 0 -row 1 -sticky "snew"  -rowspan 7
    grid configure .fvsort.sb -column 1 -row 1 -sticky "snew"  -rowspan 7
    grid configure .fvsort.ascend -column 2 -row 0 -sticky "snew" 
    grid configure .fvsort.help  -column 3 -row 0 -sticky "snew" -columnspan 2

    grid configure .fvsort.fkeyc -column 2 -row 1 -sticky "snew" 
    grid configure .fvsort.fkeyb -column 3 -row 1 -sticky "snew" 
    grid configure .fvsort.fkeye -column 4 -row 1 -sticky "snew"

    grid configure .fvsort.skeyc -column 2 -row 2 -sticky "snew"
    grid configure .fvsort.skeyb -column 3 -row 2 -sticky "snew"
    grid configure .fvsort.skeye -column 4 -row 2 -sticky "snew"

    grid configure .fvsort.tkeyc -column 2 -row 3 -sticky "snew"
    grid configure .fvsort.tkeyb -column 3 -row 3 -sticky "snew"
    grid configure .fvsort.tkeye -column 4 -row 3 -sticky "snew"

    grid configure .fvsort.fsep -column 2 -row 4 -sticky "ew" \
	-columnspan 3
    grid configure .fvsort.merge -column 2 -row 5 -sticky "snew"
    grid configure .fvsort.mlabel -column 3 -row 5 -columnspan 2 \
    -sticky "w"
    grid configure .fvsort.fsep1 -column 2 -row 6 -sticky "ew" \
	-columnspan 3
 
    grid configure .fvsort.bb    -column 2 -row 7 -sticky "snew" -columnspan 3
}

itcl::body Table::_resetSKey {} {
    set _psortkey ""
    set _ssortkey ""
    set _tsortkey ""
    set _psortcheck 1
    set _ssortcheck 1
    set _tsortcheck 1
}

itcl::body Table::_getSortKey {key_} {
    

    if { [catch {set curSel [lindex $_listPreSelectedColNames [.fvsort.lb curselection]]} \
	      err] == 1 } {
	error "Please Select a column name from left-hand window first"
	return
    }

    switch $key_ {
	1 {
	    set _psortkey $curSel
	}
	2 {
	    set _ssortkey $curSel
	}
	3 {
	    set _tsortkey $curSel
	}
	default {
	    error "Unknown key"
	    return
	}
    }
}

itcl::body Table::_sortColumn {} {
    set sortkey ""
    set sortcheck ""

    if { $_psortkey == "" } {
	error "You need to select at least the primary key for sorting"
	return
    } else {
	lappend sortkey $_psortkey
	lappend sortcheck $_psortcheck
    }
    
    if { $_ssortkey != ""} {
	lappend sortkey $_ssortkey
	lappend sortcheck $_ssortcheck
	
    }
    if { $_tsortkey != ""} {
	lappend sortkey $_tsortkey
	lappend sortcheck $_tsortcheck
    }

    doSort $sortkey $sortcheck $_isUniqMerge
    destroy .fvsort
}

itcl::body Table::doSort { keys_ dirs_ unique_ } {
   if { [catch {set rowlist [$fFile sort $keys_ $dirs_ $unique_ ]} err] } {
      error "Sort failed: $err"
      return
   }

   set rowlist [lindex $rowlist 0] 
   set rowlen [llength $rowlist]

   for { set i  0 } { $i <$rowlen } { incr i} {
       set j [lindex $rowlist $i]
       set tmplist([expr $i +1]) $_selectedRows($j)
   }
   for { set i 1} { $i <=$rowlen } { incr i} {
       
       set _selectedRows($i) $tmplist($i)
   }

   set sortRowResultList $rowlist

#   _showselRows

   $fFile changeFile
   refresh
   _updateRestDisps
}


itcl::body Table::_cpyToCB {} {
    
    # if nothing is selected return
    if { $_cstartx == "" } return
    
    set colStart $_cstartx
    if { $_cendx=="" } {
        set colEnd $_cstartx
    } else {
        set colEnd $_cendx
    }

    set rowStart $_cstarty
    if { $_cendy=="" } {
        set rowEnd $rowStart
    } else {
        set rowEnd $_cendy
    }

    set clipType "table"

    set dx [expr $colEnd-$colStart+1]
    set dy [expr $rowEnd-$rowStart+1]
    set dataList [_getRawDataBlock $colStart $rowStart $colEnd $rowEnd]

    fvClipBoard register $clipType $fileName "${dx}x${dy}" $dataList
}

itcl::body Table::_cpyFrCB {} {
# make sure the insert point is present
    if { [info exist _currentCol] == 0} {
	error "Please highlight the insert point"
	return 
    }
# get info from the clipboard
    if { [fvClipBoard hasRec] } {
	set cbinfo [fvClipBoard report]
	set clipType [lindex $cbinfo 0]
	set clipData [lindex $cbinfo 2]
	if { $clipType == "table" } {
           $fFile changeFile
           # Clip to Table size
           set nCols [llength $clipData]
           if { [expr $nCols+$_currentCol] > $_numCols } {
              set clipData [lrange $clipData 0 [expr $_numCols-$_currentCol-1] ]
           }
           set nRows [llength [lindex $clipData 0]]
           if { [expr $nRows+$_currentRow] > $_numRows } {
              set lRow [expr $_numRows-$_currentRow-1]
              set newData {}
              foreach c $clipData {
                 lappend newData [lrange $c 0 $lRow]
              }
              set clipData $newData
           }
           _putRawDataBlock $_currentCol $_currentRow $clipData
	} else {
	    error "The clipboard currently contains \"$clipType\" data.  Cannot\
                paste this into a table."
	} 

    }
    refresh
}

itcl::body Table::_changeUniqLabel {} {
    if { $_isUniqMerge == 1} {
	.fvsort.merge configure -text Yes
    } else {
	.fvsort.merge configure -text No
    }
}


itcl::body Table::_statCmd {} {
    global g_titleFont

    catch {destroy .fvstat}
    powToplevel .fvstat .dummy
    wm title .fvstat "fv: Column Statistics"  

    label   .fvstat.lbl -text "Select column:"  -font g_titleFont
    listbox .fvstat.lb -yscrollcommand ".fvstat.sb set" -font g_titleFont
    scrollbar .fvstat.sb -command ".fvstat.lb yview" 
    foreach i $_listPreSelectedColNames {
	.fvstat.lb insert end $i
    }
    bind .fvstat.lb <Double-Button-1> [itcl::code $this _statSelCmd]
    bind .fvstat.lb <ButtonRelease-1> [itcl::code $this _statSelCmd]

    label .fvstat.namel -text "Column Name" -font g_titleFont
    label .fvstat.numl -text "Number of values" -font g_titleFont
    label .fvstat.minl -text "Min" -font g_titleFont
    label .fvstat.maxl -text "Max" -font g_titleFont
    label .fvstat.meanl -text "Mean" -font g_titleFont
    label .fvstat.fminl -text "First minimum in row" -font g_titleFont
    label .fvstat.fmaxl -text "First maximum in row" -font g_titleFont
    label .fvstat.stdl -text "Standard deviation" -font g_titleFont
    label .fvstat.rowrangel -text "Row range" -font g_titleFont

    checkbutton .fvstat.selonly -text "Use selected rows"  \
		-font g_titleFont  \
                -variable [itcl::scope _statselonly ] \
		-selectcolor $fvPref::checkBBgColor  \
		-activeforeground black -activebackground $fvPref::globalBgColor \
		-command [itcl::code $this _statSelCmd]

    set [itcl::scope  _colSName($this)] ""
    set [itcl::scope  _colNumVal($this)] ""
    set [itcl::scope  _colMin($this)] ""
    set [itcl::scope  _colMax($this)] ""
    set [itcl::scope  _colMean($this)] ""
    set [itcl::scope  _colFMin($this)] ""
    set [itcl::scope  _colFMax($this)] ""
    set [itcl::scope  _colStd($this)] ""
    set _colRowRange($this) ""

    entry .fvstat.namee -textvariable [itcl::scope _colSName($this)] \
        -disabledforeground black \
        -disabledbackground $fvPref::globalBgColor \
	-relief raised -borderwidth 1 -stat disabled -font g_titleFont
    entry .fvstat.nume -textvariable [itcl::scope _colNumVal($this)] \
        -disabledforeground black \
        -disabledbackground $fvPref::globalBgColor \
	-relief raised -borderwidth 1 -stat disabled -font g_titleFont
    entry .fvstat.mine -textvariable [itcl::scope _colMin($this)] \
        -disabledforeground black \
        -disabledbackground $fvPref::globalBgColor \
	-relief raised -borderwidth 1 -stat disabled -font g_titleFont
    entry .fvstat.maxe -textvariable [itcl::scope _colMax($this)] \
        -disabledforeground black \
        -disabledbackground $fvPref::globalBgColor \
	-relief raised -borderwidth 1 -stat disabled -font g_titleFont
    entry .fvstat.meane -textvariable [itcl::scope _colMean($this)] \
        -disabledforeground black \
        -disabledbackground $fvPref::globalBgColor \
	-relief raised -borderwidth 1 -stat disabled -font g_titleFont
    entry .fvstat.fmine -textvariable [itcl::scope _colFMin($this)] \
        -disabledforeground black \
        -disabledbackground $fvPref::globalBgColor \
	-relief raised -borderwidth 1 -stat disabled -font g_titleFont
    entry .fvstat.fmaxe -textvariable [itcl::scope _colFMax($this)] \
        -disabledforeground black \
        -disabledbackground $fvPref::globalBgColor \
	-relief raised -borderwidth 1 -stat disabled -font g_titleFont
    entry .fvstat.stde  -textvariable [itcl::scope _colStd($this)] \
        -disabledforeground black \
        -disabledbackground $fvPref::globalBgColor \
	-relief raised -borderwidth 1 -stat disabled -font g_titleFont
    entry .fvstat.rowrange -textvariable [itcl::scope _colRowRange($this)] -font g_titleFont
    bind .fvstat.rowrange <Return> +[itcl::code $this _statSelCmd]

    button .fvstat.ok -text Close -command "destroy .fvstat" -font g_titleFont
    button .fvstat.help -text Help -command "hhelp columnStatistics" -font g_titleFont

    grid configure .fvstat.lbl -column 0 -row 0 -columnspan 2 \
	-sticky "snew"
    grid configure .fvstat.lb -column 0 -row 1 -rowspan 10 \
	-sticky "snew" 
    grid configure .fvstat.sb -column 1 -row 1 -rowspan 10 \
	-sticky "snw"

#add by ziqin pan

    grid configure .fvstat.namel -column 2 -row 0 \
	-sticky "snw"
    grid configure .fvstat.numl -column 2 -row 1 \
	-sticky "snw"
    grid configure .fvstat.minl -column 2 -row 2 \
	-sticky "snw"
    grid configure .fvstat.maxl -column 2 -row 3 \
	-sticky "snw"
    grid configure .fvstat.meanl -column 2 -row 4 \
	-sticky "snw"
    grid configure .fvstat.fminl -column 2 -row 5 \
	-sticky "snw"
    grid configure .fvstat.fmaxl -column 2 -row 6 \
	-sticky "snw"
    grid configure .fvstat.stdl -column 2 -row 7 \
	-sticky "snw"
    grid configure .fvstat.rowrangel -column 2 -row 8  \
	-sticky "snw"

    grid configure .fvstat.namee -column 3 -row 0 \
	-sticky "snew"
    grid configure .fvstat.nume -column 3 -row 1 \
	-sticky "snew"
    grid configure .fvstat.mine -column 3 -row 2 \
	-sticky "snew"
    grid configure .fvstat.maxe -column 3 -row 3 \
	-sticky "snew"
    grid configure .fvstat.meane -column 3 -row 4 \
	-sticky "snew"
    grid configure .fvstat.fmine -column 3 -row 5 \
	-sticky "snew"
    grid configure .fvstat.fmaxe -column 3 -row 6 \
	-sticky "snew"
    grid configure .fvstat.stde -column 3 -row 7 \
	-sticky "snew"    
    grid configure .fvstat.rowrange -column 3 -row 8 \
	-sticky "snew"    

#add by Ziqin Pan, Feb 18, 2004
    grid configure .fvstat.selonly -column 2 -row 9 \
        -sticky "snew" -columnspan 2 -pady 10
#

    grid configure .fvstat.ok -column 2 -row 10 \
	-sticky "s"    
    grid configure .fvstat.help -column 3 -row 10 \
	-sticky "s"    

    
}

#Add by Ziqin Pan, Jan, 2004
itcl::body Table::_parseToRowRange {start_end} {
        set tmplist [split $start_end "-"]
        set tmplistlen  [llength $tmplist]


        if {$tmplistlen <1 } {
            set start 1
            set end $_numRows
        } elseif { $tmplistlen ==1} {
             set start [lindex $tmplist 0] 
             set end $start
        } else {
             set start [lindex $tmplist 0]
             set end [lindex $tmplist 1]
        }

        if { $start < 1 } {
             set start 1
        }
        if { $end < 1 } {
             set end 1
        }
        if { $start > $_numRows } {
             set start $_numRows
        }
  
        if { $end > $_numRows } {
             set end $_numRows
        }

        set selrange ""
        for {set i $start} { $i <= $end} {incr i} {
             if { $_selectedRows($i) == 1} {
                 for {set j $i} { $j <= $end} {incr j} {
                    if { $_selectedRows($j) == 0} {
                         break
                    }
                 }
                 if { $selrange =="" } {
                    append selrange  $i "-" [expr $j - 1] 
                 } else {
                    append selrange ","  $i "-" [expr $j - 1]
                 }
                 set i  $j
             }
        }

        
        return $selrange
}

itcl::body Table::_statSelCmd {} {

    set tmpindex [.fvstat.lb curselection]
    if {$tmpindex == "" } return

    set tmpName [.fvstat.lb get $tmpindex]

#    set _colRowRange($this) [string trim $_colRowRange($this) " " ]

    if { $_colRowRange($this) == "" } {
	set tmprange "1-$_numRows"
    } else {
#	set tmprange [split $_colRowRange($this) ","]
	set tmprange $_colRowRange($this)
    }

    if { $_statselonly == 0 } {
        set paramList [$fFile getColStat $tmpName 1 $tmprange]
    } else {
        set selrange ""
        set tmplist [split $tmprange ","]
        set numlist [llength $tmplist]

  
        for { set k 0 } {$k <$numlist } {incr k} {
            set start_end  [lindex $tmplist $k]
            if { $selrange =="" } {
                append  selrange [_parseToRowRange $start_end]
            } else {
                append  selrange "," [_parseToRowRange $start_end]
            }
        }

        if { $selrange == "" } {
              set paramList {"" "" "" "" "" "" 0}
        } else {
              set paramList [$fFile getColStat $tmpName 1 $selrange]
        }

    }
        

   .fvstat.namee  configure -state normal
   .fvstat.mine  configure -state normal
   .fvstat.maxe  configure -state normal
   .fvstat.meane configure -state normal
   .fvstat.fmine configure -state normal
   .fvstat.fmaxe configure -state normal
   .fvstat.stde  configure -state normal

    set [itcl::scope  _colSName($this)] $tmpName
    set [itcl::scope  _colMin($this)] [lindex $paramList 0]
    set [itcl::scope  _colFMin($this)] [lindex $paramList 1]
    set [itcl::scope  _colMax($this)] [lindex $paramList 2]
    set [itcl::scope  _colFMax($this)] [lindex $paramList 3]
    set [itcl::scope  _colMean($this)] [lindex $paramList 4]
    set [itcl::scope  _colStd($this)] [lindex $paramList 5]
    set [itcl::scope  _colNumVal($this)] [lindex $paramList 6]

   .fvstat.namee  configure  -state disabled
   .fvstat.mine  configure  -state disabled
   .fvstat.maxe  configure  -state disabled
   .fvstat.meane configure  -state disabled
   .fvstat.fmine configure  -state disabled
   .fvstat.fmaxe configure  -state disabled
   .fvstat.stde  configure  -state disabled

}

# column of string can be left or right justified.
itcl::body Table::_makeJustification {} {

    for {set i 0} {$i < $_showCols} {incr i} {
	set tmpIndex [expr $_firstCol + $i -1]
	if { [regexp A $_columnType($tmpIndex)] == 1} {
	    for {set j 0} { $j< $_showRows} {incr j} {
		set id ${i}_$j
                if { $fvPref::ifLeftJustifyString == 0} { 
		   $_droot.table.can.e$id configure -justify right
                } else {
		   $_droot.table.can.e$id configure -justify left
                }
	    }
	} else {
	    for {set j 0} { $j< $_showRows} {incr j} {
		set id ${i}_$j
		$_droot.table.can.e$id configure -justify right
	    }

	}
    }
}

itcl::body Table::bringToFront {} {
   if { $_droot == "" || ![winfo exists $_droot] } {return 0}
   if { ![winfo ismapped $_droot] } {
       wm deiconify $_droot
   }
   raise $_droot
   focus $_droot
   return 1
}


itcl::body Table::_openVectorTable { colNum_ } {
   set fV [VectorTable #auto $colNum_ $this]
   $fV makeTable $_extNum
}
itcl::body Table::_ds9MakePlot1 { rowNum_ range xColumn_ yColumn_ {xeColumn_ ""} {yeColumn_ ""} {inCurrGraph_ 0}} {
     global g_backupDir
     global DS9plotName
    
#puts _ds9MakePlot1
#puts "_listPreSelectedColNames: $_listPreSelectedColNames"
#puts "xColumn_: $xColumn_"
#puts "yColumn_: $yColumn_"
     set tmpfile ${g_backupDir}/ds9PlotTemp_[clock seconds].data

#puts "tmpfile: $tmpfile"

     if [file exists $tmpfile] {
        file delete -force $tmpfile
     }

     if { [catch {set newDataFile [open $tmpfile "w"]} err] } {
        puts $err
        error $err
        return
     }

     # load the data with specific rowNum_ into the tmpfile
     # set range "[expr $rowNum_ - 1]-[expr $rowNum_ - 1]"
     #set range "$fRow-$lRow"
     set col 0
     set totalCol 2

     set colIdx -1
     set colIdxY -1

     foreach name $_listPreSelectedColNames {
          if { [string first $name $xColumn_] >= 0 } {
             set searchX $name
             set colIdx [lsearch $_listPreSelectedColNames $name]
             break
          }
     }

     foreach name $_listPreSelectedColNames {
          if { [string first $name $yColumn_] >= 0 } {
             set searchY $name
             set colIdxY [lsearch $_listPreSelectedColNames $name]
             break
          }
     }


     set findXColumn true

     if { $colIdx < 0 } {
        set findXColumn false
     }

     set findYColumn true
     if { $colIdxY < 0 } {
        set findYColumn false
     }

     if { $findYColumn == "true" && $findXColumn == "false" } {
        set colIdx $colIdxY
        set xColumn_ $yColumn_
     }

     set key [$fFile getKeyword TFORM[expr $colIdx + 1]]
#puts "key: $key"
     set value "[string trim [lindex [lindex $key 0] 1] {' ()}]"
#puts "value: $value"
     # set value [string range $value 0 [expr [string length $value] - 2]]
     regsub -all {[a-zA-Z]} $value {} value

#puts "after value: $value"

     if { $value == "" } {
        set value 1
     }

     # take care x error or y error

     set errorIdxX -1
     set errorIdxY -1

     foreach name $_listPreSelectedColNames {
        if { [string first $name $xeColumn_] >= 0 } {
           set errorIdxX [lsearch $_listPreSelectedColNames $name]
           break
        }
     }

     foreach name $_listPreSelectedColNames {
        if { [string first $name $yeColumn_] >= 0 } {
           set errorIdxY [lsearch $_listPreSelectedColNames $name]
           break
        }
     }

     set findXerr true
     if { $errorIdxX < 0 || $xeColumn_ ==""} {
        set findXerr false
     }

     set findYerr true
     if { $errorIdxY < 0 || $yeColumn_ ==""} {
        set findYerr false
     }

#puts "check value: $value, range: $range"
     set col 1

#     for { set col 1 } { $col <= $value } {incr col} {
         set data [_getDataForAxis "x" $xColumn_ $range]
#puts "data : $data"
         set tokens [ptr2lst [lindex $data 0] [lindex $data 1] [lindex $data 2]]

         if { $colIdxY >= 0 } {
            set data2 [_getDataForAxis "y" $yColumn_ $range]
            set tokens2 [ptr2lst [lindex $data2 0] [lindex $data2 1] [lindex $data2 2]]
         }
#puts "data2 : $data2"

         if { $errorIdxX >=0 } {
            set errData [_getDataForAxis "xe" $xeColumn_ $range]
            set errTokens [ptr2lst [lindex $errData 0] [lindex $errData 1] [lindex $errData 2]]
         }
         if { $errorIdxY >=0 } {
            set errData2 [_getDataForAxis "ye" $yeColumn_ $range]
            set errTokens2 [ptr2lst [lindex $errData2 0] [lindex $errData2 1] [lindex $errData2 2]]
         }

         set monoflag 1

         for { set i 0 } { $i < [llength $tokens] } {incr i} {
             if { $findXColumn == "true" && $findYColumn == "true" } {
                set finalStr [format "%s %s" [lindex $tokens $i] [lindex $tokens2 $i]]
             } elseif { $xColumn_ == "RowNumber" } {
                set finalStr [format "%s %s" [expr $i + 1] [lindex $tokens $i]]
             } elseif  { $yColumn_ == "RowNumber" } {
                set finalStr [format "%s %s" [lindex $tokens $i] [expr $i + 1]]
             } else {
                if { $findXColumn == "false" } {
                   set finalStr [format "%s %s" $col [lindex $tokens $i]]
                } else {
                   set finalStr [format "%s %s" [lindex $tokens $i] $col]
                }
             }

             if { $findXerr } {
                set finalStr [format "%s %s" $finalStr [lindex $errTokens $i] ]
             }

             if { $findYerr } {
                set finalStr [format "%s %s" $finalStr [lindex $errTokens2 $i] ]
             }


             set finalTokens [split $finalStr " "]
             if { $monoflag == 1 } {
                if { $i ==0 } {
                    set xval [lindex $finalTokens 0]
                } elseif { $i == 1} { 
                    set xval2 [lindex $finalTokens 0]
                } else {
                    set xval3 [lindex $finalTokens 0]
                    if { $xval <= $xval2 && $xval2 <= $xval3 } {
                    } elseif { $xval >= $xval2 && $xval2 >= $xval3 } {
                    } else {
                       set monoflag 0
                    }
#puts "1:$xval, 2:$xval2, 3:$xval3, mono:$monoflag"
                    set xval $xval2
                    set xval2 $xval3
                }
             }
             

             puts $newDataFile $finalStr
         }
#     }

     close $newDataFile

     set err "none"

     catch {exec xpaaccess ds9} result   

#puts "result: $result"
     if { [string range $result 0 1] == "no" } {
        # start DS9 if DS9 isn't there 
        if { [catch {exec ds9 &} ds9pid] } {
           file delete $tmpfile 
           if { [tk_dialog .saoError "DS9 startup error.. Cannot start DS9!\nUse POW instead?" \
                         question 0 Yes No] == 0 } {
              close $fFile
              return NO_YET
           }
           close $fFile
           return DONE
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
                    if { [tk_dialog .saoError "DS9 startup error.. Cannot start ds9!\nUse POW instead?" \
                             question 0 Yes No] == 0 } {
                       close $fFile
                       return NO_YET
                    }
                    close $fFile
                    return DONE
                 }
              }
        }

     }

     if { $rowNum_ == "NONE" } {
        set displayName [file tail $fileName]
     } else {
        set displayName [format "%s\(%s\)" [file tail $fileName] [expr $rowNum_ + 1]]
     }
         
#puts "ready to call DS9"
#puts "displayName: $displayName"
     if {$inCurrGraph_ == 1} {

        set ds9plotList {}
        catch {exec xpaget ds9 plot} result
        set ds9plotList [split $result " "]
#puts "ds9plotList: $ds9plotList"

        if ![info exists DS9plotName] {
           set inCurrGraph_ 0
        } else {
#puts "DS9plotName: $DS9plotName"
           if { [lsearch -exact $ds9plotList $DS9plotName] < 0 } {
              set inCurrGraph_ 0
           } else {
              if { $findXerr  && $findYerr } {
                 exec xpaset ds9 plot $DS9plotName data xyexey < $tmpfile
              } elseif { $findXerr } {
                 exec xpaset ds9 plot $DS9plotName data xyex < $tmpfile
              } elseif { $findYerr } {
                 exec xpaset ds9 plot $DS9plotName data xyey < $tmpfile
              } else {
                 exec xpaset ds9 plot $DS9plotName data xy < $tmpfile
              }
           }
        }
     }


    if {$inCurrGraph_ == 0} { 
       set DS9plotName ds9plot_[clock seconds]
#puts "ready to print"
       if { $findXerr  && $findYerr } {
          exec xpaset ds9 plot new name $DS9plotName $displayName $xColumn_ $yColumn_ xyexey < $tmpfile
       } elseif { $findXerr } {
          exec xpaset ds9 plot new name $DS9plotName $displayName $xColumn_ $yColumn_ xyex < $tmpfile
       } elseif { $findYerr } {
          exec xpaset ds9 plot new name $DS9plotName $displayName $xColumn_ $yColumn_ xyey < $tmpfile
       } else {
          exec xpaset ds9 plot new name $DS9plotName $displayName $xColumn_ $yColumn_ xy < $tmpfile
       }
    } 

    if { $monoflag == 0 } {
         exec xpaset -p ds9 plot line discrete diamond
         exec xpaset -p ds9 plot view line no
         exec xpaset -p ds9 plot view discrete yes
    } else {
         exec xpaset -p ds9 plot view line yes
         exec xpaset -p ds9 plot view discrete no
    }

#     if { $totalCol > 2 } {
#        exec xpaset ds9 analysis plot $displayName $xColumn_ $yColumn_ \
#                                     $totalCol < $tmpfile
#     } else {
#        exec xpaset ds9 analysis plot $displayName $xColumn_ $yColumn_ 2 < $tmpfile
#     }

     # file delete $tmpfile

     return DONE
}

itcl::body Table::_ds9MakePlot { rowNum_ fRow lRow xColumn_ yColumn_ {xeColumn_ ""} {yeColumn_ ""} } {
     global g_backupDir
     global DS9plotName
    
#puts _ds9MakePlot
#puts "_listPreSelectedColNames: $_listPreSelectedColNames"
#puts "xColumn_: $xColumn_"
#puts "yColumn_: $yColumn_"
     set tmpfile ${g_backupDir}/ds9PlotTemp.data

     if [file exists $tmpfile] {
        file delete -force $tmpfile
     }

     if { [catch {set newDataFile [open $tmpfile "w"]} err] } {
        error $err
        return
     }

     # load the data with specific rowNum_ into the tmpfile
     # set range "[expr $rowNum_ - 1]-[expr $rowNum_ - 1]"
     set range "$fRow-$lRow"
     set col 0
     set totalCol 2

     set findXColumn true
     set colIdx [lsearch $_listPreSelectedColNames $xColumn_]
#puts "colIdx: $colIdx"

     if { $colIdx < 0 } {
        set findXColumn false
     }

     set findYColumn true
     set colIdxY [lsearch $_listPreSelectedColNames $yColumn_]
     if { $colIdxY < 0 } {
        set findYColumn false
     }

     if { $findYColumn == "true" && $findXColumn == "false" } {
        set colIdx $colIdxY
     }
     set key [$fFile getKeyword TFORM[expr $colIdx + 1]]
     set value "[string trim [lindex [lindex $key 0] 1] {' ()}]"
     set value [string range $value 0 [expr [string length $value] - 2]]

     if { $value == "" } {
        set value 1
     }

     # take care x error or y error
     if { $xeColumn_ != "" ||  $yeColumn_ != "" } {
        if { $xeColumn_ != "" } {
           set errorIdx [lsearch $_listPreSelectedColNames $xeColumn_]
           set thirdCol $xeColumn_
        } else {
           set errorIdx [lsearch $_listPreSelectedColNames $yeColumn_]
           set thirdCol $yeColumn_
        }
        incr totalCol
     }

#puts "value: $value"
     for { set col 1 } { $col <= $value } {incr col} {
         set data [$fFile getVectorTableAsRawList [lindex $_listPreSelectedColNames $colIdx] $col $range]
         set tokens [split $data " "]

         if { $colIdxY >= 0 } {
            set data2 [$fFile getVectorTableAsRawList [lindex $_listPreSelectedColNames $colIdxY] $col $range]
            set tokens2 [split $data2 " "]
         }

         if { $xeColumn_ != "" || $yeColumn_ != "" } {
            if { $errorIdx >= 0 } {
               set errRata [$fFile getVectorTableAsRawList [lindex $_listPreSelectedColNames $errorIdx] $col $range]
               set errTokens [split $errData " "]
            }
         }

         for { set i 0 } { $i < [llength $tokens] } {incr i} {
             if { $findXColumn == "true" && $findYColumn == "true" } {
                set finalStr [format "%s %s" [lindex $tokens $i] [lindex $tokens2 $i]]
             } elseif { $xColumn_ == "RowNumber" } {
                set finalStr [format "%s %s" [expr $i + 1] [lindex $tokens $i]]
             } elseif  { $yColumn_ == "RowNumber" } {
                set finalStr [format "%s %s" [lindex $tokens $i] [expr $i + 1]]
             } else {
                if { $findXColumn == "false" } {
                   set finalStr [format "%s %s" $col [lindex $tokens $i]]
                } else {
                   set finalStr [format "%s %s" [lindex $tokens $i] $col]
                }
             }

             if { $xeColumn_ != "" || $yeColumn_ != "" } {
                if { $errorIdx >= 0 } {
                   set finalStr [format "%s %s" $finalStr [lindex $errTokens $i]]
                } else {
                   if { $xeColumn_ == "RowNumber" || $yeColumn_ == "RowNumber"} {
                      set finalStr [format "%s %s" $finalStr [expr $i + 1]]
                   } else {
                      set finalStr [format "%s %s" $finalStr $col]
                   }
                }
             }
             puts $newDataFile $finalStr
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
              return NO_YET
           }
           close $fFile
           return DONE
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
                    if { [tk_dialog .saoError "DS9 startup error.. Cannot start ds9!\nUse POW instead?" \
                             question 0 Yes No] == 0 } {
                       close $fFile
                       return NO_YET
                    }
                    close $fFile
                    return DONE
                 }
              }
        }

     }

     if { $rowNum_ == "NONE" } {
        set displayName [file tail $fileName]
     } else {
        set displayName [format "%s\(%s\)" [file tail $fileName] [expr $rowNum_ + 1]]
     }

#puts "displayName: $displayName"
#puts "tmpfile: $tmpfile"
     set DS9plotName ds9plot_[clock seconds]
     # if { $totalCol > 2 } {
     #    exec xpaset ds9 plot new name $displayName $displayName $xColumn_ $yColumn_ \
     #                                 $thirdCol $totalCol < $tmpfile
     # } else {
        exec xpaset ds9 plot new name $DS9plotName $displayName $xColumn_ $yColumn_ xy < $tmpfile
     # }

     file delete $tmpfile

     return DONE
}

itcl::body Table::_plotVectorTableRow { colIdx rowNum_ } {

    if { $fvPref::imgDisplayer == "DS9" } {
       # display is for DS9
       set flag "NOT_YET"
       # ds9 file start with 1 instead of 0
       set rowNum_ [expr $_firstRow + $rowNum_ - 1]
       set flag [_ds9MakePlot $rowNum_ [expr $rowNum_ + 1] [expr $rowNum_ + 1] \
                                       ElementNumber [lindex $_listPreSelectedColNames $colIdx]]
       if { $flag == "DONE" } return
    }

    # apparently, plot does not need to use sortlist after the row is sorted
    set rowNum_ [expr $_firstRow + $rowNum_ ]
    set selectCol [lindex $_listPreSelectedColNames $colIdx ]
    _plotCols ElementNumber {} $selectCol {} 0 $rowNum_ 0
}


itcl::body Table::addChild { child_ } {
   lappend _myChildren $child_
}


itcl::body Table::freeChild { child_ } {
   set pos [lsearch $_myChildren $child_]
   if { $pos != -1 } {
      set _myChildren [lreplace $_myChildren $pos $pos]
   }
}

itcl::body Table::passParamsToVectorTable {} {
    return [list $_numRows $isFailedToCopy $fileName $currentHDU]
}

itcl::body Table::_scrollChildren { firstRow {currentRow -1} } {

    set tmpRow $currentRow
    if { ($tmpRow == -1) && [info exist _currentRow] } {
	set tmpRow $_currentRow
    }

    # Check for locked children
    foreach child $_myChildren {
	catch { 
           if { [$child _isVScrollLocked] } {
	      $child _setVScroll [expr $firstRow-1]
#puts "firstrow=$firstRow tmpRow=$tmpRow"
#	      if { $tmpRow >= 0 } {
#		 $child _setStartMark 0 [expr $tmpRow-$firstRow+1] False
#	      }
	   }
        }
    }
}

itcl::body Table::_isVScrollLocked {} {
    return $_isVScrollLocked1
}
