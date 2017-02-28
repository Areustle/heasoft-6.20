itcl::class FitsFile {
     constructor {args} {}
     destructor {} 
     
     public method save {}
     public method saveAs { {saveFileName_ ""} }
     public method saveHDUsAsText { {saveFileName_ ""} }
     public method saveHDUsAs {}
     public method saveCHDUAs {ext_}
     public method revertFile {}
     public method writeHisKey {}
     public method getBackup {}
     public method getTableInfo {name_}
     public method getColInfo {colName_} 
     public method getExprInfo {expr_}
     public method getImgInfo {} 
     public method getImgType {} 
     public method getcolblock {col_ rowrange_} 
     public method getKeywords {keywords_} 
     public method dumpHeader {} 
     public method isReadOnly {}
     public method closeCmd { {force_ 0} } 
     public method cleanChild {}
     public method updateHighLight {extNum_ field_ value_}
     public method sort {column_ ascendFlag_ isMerge_}
     public method getWcs {{dest {}} {RAColNum_ {}} {DecColNum_ {}}}
     public method getColMinMax {colname_ felem_ rowrange_} 
     public method getColStat {colname_ felem_ rowrange_} 
     public method getColNum {colname_}
     public method getTLMinMax {colname_}
     public method makeHistogram {args}
     public method plotHisto {}
     public method addChild { child_ extNum_ }
     public method freeChild { extNum_ }
     public method countChanged {}
     public method getMaster {}

     public method displaySlice   { extNum_ slice }
     public method plotData       { extNum_ {xyNames_ {}} }
     public method plotData1       { extNum_ {xyNames_ {}} }
     public method openHeader     { extNum_ }
     public method openTable      { extNum_ {coor_ {}} {newTable_ {}} }
     public method openExtension  { extNum_ }
     public method closeExtension { extNum_ }
     public method skipTable      { extNum_ {coor_ {}} }
     public method callHistogram  { extNum_ fillFlag_ }
     public method checkXYCol     { idx }
     public method deleteExt { extNum_ }

     public method _transformFileName { name }
     public method _reverseTransformedFileName { name }
     public method _isEmptyDir { dir }
     public method getHeader2String {}
     public method translateKeyWords { colName rowNum dest tempFile }
     public method getDummyHeader2String {{dest {}} RAColNum_ DecColNum_}
     public method getHeaderKeyWord { str img }
     public method assembleWcsHeader { img {selection "DEFAULT"} } 
     public method assembleWcsLabel { img {selection "DEFAULT"} } 
     public method headerDebugDataPrint { title string }

# fhl = file highlight window
     private variable _oldName
     private variable _fhl
     private variable _loaded
     private variable _mBar
     private variable _subwin
     private variable _myExtensionChildren
     private variable _myExtNums 
     private variable _createTransformDirectory
     
     private common _smoothID
     public variable xwin
     public variable ywin 
     public variable dosmooth 

#public member
    public variable opened
    public variable filename
    public variable openedFileName

## this is variable "chdu" in fitsTcl
    public variable currentHDU

    public variable isMaster 0
    public variable isFailedToCopy 0 

# the actual variable...see fitsTcl2.1 user's guide for more info on fitsfiles
    public variable fitsfile
    public variable filePermission ""

    public method moveToHDU {numHDU_} 
    public method getNthKey {n_}
    public method copyCHDU {saveName_}
    public method insertKey {n_ rec_ fflag_}
    public method putNthKey {n_ rec_ fflag_}
    public method putHis {rec_} 
    public method delNthKey {n_}
    public method flush {isClear_} 
    public method loadColumn {col_ null_ i_}
    public method loadExpr {expr_ null_ {rows_ "-"}}
    public method loadImage {}
    public method loadImageFlip { direction }
    public method loadImageBlock {fRow_ nRows_ fCol_ nCols_ slice_}
    public method loadImageSlice {i_ r_}

    public method loadVectorTableToDataAddressForPOW {colName_}
    public method getVectorTableAsRawList {colName_ rows_ range_}
    public method getVectorTableRowAsFormattedList {colName_ row_ totalCols_}
    public method freeColumn {add_} 
    public method freeExpr {add_}
    public method freeImage  {add_}
    public method freeVTable {add_}
    public method loadTBlock {var_ cList_ fRow_ nRows_ fCol_ nCols_ i_} 
    public method delRows {n0_ nn_} 
    public method delRowsRange {rowrange_} 
    public method delRowsWithCondition {cond_} 
    public method delCols {colList_}
    public method selRowsWithCondition {cond_ fRow_ nRows_} 
    public method addColumn {name_ form_}
    public method addColumnFrExpr {name_ form_ expr_ rowrange_}
    public method addRow {nRows_}
    public method getImageAsList {firstElem_ nElem_}
    public method putImage {fElem_ fRow_ value_}
    public method putTable {name_ felem_ range_ value_}
    public method putKwd {rec_ fflag_} 
    public method saveTabToASCII {name_ n_ fRow_ nRows_ colList_ width_ ifFixedFormat_ ifCSV_ ifPrintRow_ sepString_}
    public method saveImgToASCII {name_ n_ fRow_ nRows_ fCol_ nCols_ width_ ifCSV_ ifPrintRow_ sepString_ { slice 1 } }
    public method saveVecToASCII {name_ n_ fRow_ nRows_ fCol_ nCols_ colName_ ifCSV_ ifPrintRow_ sepString_ ifVariableVec_}
    public method getKeyword {keyword_} 
    public method insertCol {index_ ttype_ tform_}
    public method insertRow {index_ nRows_}
    public method getHDUtype { hdu }
    public method getNumHdus { }
    public method getOrigName {}
    public method changeFile {}
    public method unchangeFile {}
    public method isFileChanged {}
    public method loadImageMeanCols {fCol_ lCol_ slice_} 
    public method loadImageMeanRows {fRow_ lRow_ slice_} 
    public method updateChecksum { {forceUpdate_ 0} }
    public method verifyChecksum {}
    public method  redrawHighLight {} 
	 
     # private member

# 1=readonly, 0=read/write
     private variable _fileMode

     private variable _isChanged 0
     private variable _numExts
     private variable _isFtp 0
     private variable _isHttp 0

# either this FitsFile represents the original file, or it's a draft copy
     private variable _isOriginalFile 1

# this variable is only set (in constructor)
#    note _origFileObj = "" if and only if _isOriginalFile is true
     private variable _origFileObj ""

     private variable _ftp_user
     private variable _ftp_pass
     private variable _ftp_file

# called by the constructor
     private method _createFileHighlightWindow {} 
     private method _redrawHighLight {} 
# called by _createFileHighlightWindow and _redrawHighLight
     private method _drawContent {}

# checks to make sure file is open, returns error if not
     private method _isOpen {} 
     private method _getNumHdus {} 

# both open and close the fitsfile class variable
     private method _openFitsFile {} 
     private method _closeFitsFile {}

     private method _ftpFile {fname_ saveAs_}
     private method _fetchFile {}
     private method _deleteHDUs {} 
     private method _copyHDUs {}
     private method _pasteHDUs {} 
     private method _writeHDUsToClipBoard {ifDelete_}
     private method _isHDUSelected {} 
     private method _isImgSelected {} 
     private method _isThereImgExt {} 
     private method _isTblSelected {} 
     private method _appendNewHDU {}
     private method _postMenus {}
     private method _smoothImgs {}
     private method _smoothOK {} 
     private method _plotHDUs {}

# whether an extension's check box button is on or off
     private variable _extCheck
# data of all the extension ($i,fieldname)
     private variable _extData
     private variable _openExtraTable 0
     private method _setNewTable { flag_ }

     private variable closeCmdSave "NOTYET"
}


itcl::body FitsFile::constructor {args} {
    global g_listObjs
    global isWin

    set fitsfile ""
    set opened 0

    set filename [lindex $args 0]
    set _fileMode [lindex $args 1]

    # Build full path to filename
    set rName [urlTail $filename]
    set dName [getFullDirPath $filename]
    set filename "${dName}${rName}"

    set _oldName $filename
    set filename [_transformFileName $filename]

    if { $isWin } {
	# Must change all \'s to /'s to avoid problems with escape chars
	regsub -all "\\\\" $filename "/" filename
    }

# BACKUP
    if { [llength $args] == 3 } {
       set _isOriginalFile 0

       # This indicates we want to work with a backup of the original file
       # see FitsExtension::constructor for when this is called
       #    because when we open a header editor window, the constructor
       #    opens a new scratch fFile (namely this one) with the original fFile
       #      as this one's backup, hence we call it the _origFileObj
       #
       # the call from FitsExtension::constructor is:
       #
       # set fFile [FitsFile #auto $fileName $fMode $_fatherFitsFile]
       #   where _fatherFitsFile is the original FitsFile object
       #   and this constructor is creating as a new scratch version of

       # _origFileObj = $_fatherFitsFile in declaration
       set _origFileObj [lindex $args 2]

       set openedFileName [lindex $args 0]
       if { $_fileMode == 0 } {
          set openedFileName [$_origFileObj getBackup]
       }

       # this doesn't do much besides open $filename and set $fitsfile
       _openFitsFile

# ORIGINAL
    } else {
       _fetchFile
       # note this is *not* done in the backup case above
       lappend g_listObjs $this
       set _myExtensionChildren {}
       set _myExtNums {}
       set isMaster 1

# most of the work is done here
#         note _openFitsFile is called within _createFileHighlightWindow
       _createFileHighlightWindow

       update idletasks
	    
       if { $fvPref::ifAutoPlotPrimary } {
          _openFitsFile
          moveToHDU 1
          set dims [getImgInfo]
          _closeFitsFile
          if { [llength $dims] == 2 } {
             plotData 1
          }
       }
    }

}

itcl::body FitsFile::destructor {} {
   global g_listObjs
   global g_backupDir
   
   if { $isMaster } {
      set pos [lsearch $g_listObjs $this]

      if { $pos != -1 } {
	 set g_listObjs [lreplace $g_listObjs $pos $pos]
      }

      
      # Get location of this file in the native format, so that we can
      # do a valid string comparison
      
      set nativeBackupDir [file nativename $g_backupDir]
      set nativeOpenedDir [file nativename [file dir $openedFileName]]

      
      if { $nativeOpenedDir == $nativeBackupDir } {
	 # File viewed/editted or FTP/HTTP file copied
           catch {_closeFitsFile} err
#          catch { fits close $openedFileName } err
	 file delete \"$openedFileName\"
      }
      destroy $_fhl
   } else {
      # This is an extension file handler... close and exit
      if { $_isChanged } {
	 writeHisKey
	 updateChecksum
      }
      if { [catch {_closeFitsFile} err] } {
         puts $err
      }
   }
}

itcl::body FitsFile::_transformFileName { name } {

     set inputName $name

     regsub -all \\( "$name" _ name
     regsub -all \\) "$name" _ name

     set newName $name

     set _createTransformDirectory false
     if { $_oldName != $newName } {
        set newdir [file dirname $newName]

        if ![file exists $newdir] {
           set errorFlag [ catch { 
               file mkdir $newdir 
           } err]
       
           if { $errorFlag } {
              error "fv does not supported file name with '(' or ')' in it."
           } else {
              set _createTransformDirectory true
           }
        }

        set newName [format "%s_%s" $newName [clock seconds]]
        file rename "$inputName" $newName
     }

     return $newName
}

itcl::body FitsFile::_isEmptyDir { dir } {
     set filenames [glob -nocomplain -tails -directory $dir * .*]
     return [expr {![llength [lsearch -all -not -regexp $filenames {^\.\.?$}]]}]
}

itcl::body FitsFile::_reverseTransformedFileName { name } {
     if { $_oldName != $name && [file exists $name] } {
        file rename $name $_oldName
     }

     if { [_isEmptyDir [file dirname $name]] == 1 } {
        file delete [file dirname $name]
     }
     return
}

# creates the "fv: Summary of blah.fit in /blahdir" window
#
itcl::body FitsFile::_createFileHighlightWindow {} {
    global isMac isWin
    global g_titleFont

# file highlight window - _fhl 
    set _fhl ._fhl_[namespace tail $this]

# see pow for documentation
    powToplevel $_fhl .dummy

# report to fvwinkeeper
    .fvwinkeeper register $_fhl "Highlight" [urlTail $filename] 0 $this
    
    if { $isMac } {
	set cmdkey "Cmd"
    } else {
	set cmdkey "Alt"
    }

    # registers tells $_fhl to close by calling closeCmd
    wm protocol $_fhl WM_DELETE_WINDOW [itcl::code $this closeCmd]

    bind $_fhl <<SaveFile>>    [itcl::code $this save]
    bind $_fhl <<SaveFileAs>>  [itcl::code $this saveAs]
    bind $_fhl <<Export>>      [itcl::code $this saveHDUsAs]
    bind $_fhl <<ExportAs>>    [itcl::code $this saveHDUsAsText]
    bind $_fhl <<RevertFile>>  [itcl::code $this revertFile]
    bind $_fhl <<CloseWindow>> [itcl::code $this closeCmd]

    bind $_fhl <<Copy>>        [itcl::code $this _copyHDUs]
    bind $_fhl <<Cut>>         [itcl::code $this _deleteHDUs]
    bind $_fhl <<Paste>>       [itcl::code $this _pasteHDUs]
    bind $_fhl <<Smooth>>      [itcl::code $this _smoothImgs]
    bind $_fhl <<PlotHDUs>>    [itcl::code $this _plotHDUs]
    bind $_fhl <<AppendHDU>>   [itcl::code $this _appendNewHDU]
    
    bind $_fhl <<PostMenus>>   [itcl::code $this _postMenus]
    bind $_fhl <Destroy>       [itcl::code $this _reverseTransformedFileName $filename]


    # Create toplevel MenuBar
    #   (Macs get one common menubar for each window type... finite number of cascade menus allowed.)

    if { $isMac } {
        set _mBar .mbar.summary
        set evtWndw ""
    } else {
        set _mBar $_fhl.mbar
        set evtWndw $_fhl
    }
    $_fhl config -menu $_mBar
    
    if { ![winfo exists $_mBar] } {
        menu $_mBar -font g_titleFont
        if { $isMac } {

            $_mBar add cascade -menu $_mBar.apple
            $_mBar add cascade -menu $_mBar.file   -label File
            $_mBar add cascade -menu $_mBar.edit   -label Edit
            $_mBar add cascade -menu $_mBar.tools  -label Tools
            $_mBar add cascade -menu .mbar.wind    -label Windows
            $_mBar add cascade -menu $_mBar.help   -label Help
            
            # APPLE menu
            
            buildAppleStyleMenu $_mBar.apple
            
        } else {

            $_mBar add cascade -menu $_mBar.file  -label File
            $_mBar add cascade -menu $_mBar.edit  -label Edit
            $_mBar add cascade -menu $_mBar.tools -label Tools
            $_mBar add cascade -menu $_mBar.help  -label Help
            
        }

        # FILE menu

        buildFileMenu $_mBar.file
        $_mBar.file entryconfig "Export"   -label "Export HDUs..."
        $_mBar.file entryconfig "ExportAs" -label "Make HDU Listing..." -state normal
        $_mBar.file entryconfig "Save As..." -state normal
        $_mBar.file entryconfig "Close" -state normal

        # EDIT menu

        buildEditMenu $_mBar.edit
        $_mBar.edit entryconfigure "Copy"  -label "Copy HDUs"
        $_mBar.edit entryconfigure "Cut"   -label "Cut HDUs"
        $_mBar.edit entryconfigure "Paste" -label "Paste HDUs"
        
        $_mBar.edit insert "Prefer*" command -label "New Extension..." \
            -font g_titleFont \
            -underline 4 -accelerator "$cmdkey+E" \
            -command "doMenuEvent <<AppendHDU>>"
        $_mBar.edit insert "Prefer*" separator

        # Tools menu
        buildToolsMenu $_mBar.tools
        $_mBar.tools entryconfigure "Smooth" -label "Smooth Image"
        $_mBar.tools entryconfigure "Plot" -label "Plot"

        # HELP menu

        buildHelpMenu $_mBar.help fileSummary "File Summary"
        
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

    frame  $_fhl.fsep   -height 3 -relief raised -borderwidth 1
    frame  $_fhl.labelframe 
    
    label  $_fhl.labelframe.findex   -text Index        -width 9  -justify center -font g_titleFont
    label  $_fhl.labelframe.fextName -text Extension    -width 16 -justify center -font g_titleFont
    label  $_fhl.labelframe.ftype    -text Type         -width 8  -justify center -font g_titleFont
    label  $_fhl.labelframe.fdims    -text Dimension    -width 30 -justify center -font g_titleFont
    label  $_fhl.labelframe.fbut     -text View         -width 25 -justify center -font g_titleFont
    label  $_fhl.labelframe.fspace   -text ""           -width 7  -justify center -font g_titleFont

    # grid config $_fhl.labelframe  -column 0 -row 1 -columnspan 7 -sticky "nw"

    grid config $_fhl.labelframe  -column 0 -row 1 -columnspan 7 -sticky "nw"
    grid config $_fhl.fsep        -column 0 -row 2 -columnspan 7 -sticky "nw"

    grid config $_fhl.labelframe.findex   -column 0 -row 0 -sticky "nw"
    grid config $_fhl.labelframe.fextName -column 1 -row 0 -sticky "news"
    grid config $_fhl.labelframe.ftype    -column 2 -row 0 -sticky "nw"
    grid config $_fhl.labelframe.fdims    -column 3 -row 0 -sticky "nw"
    grid config $_fhl.labelframe.fbut     -column 4 -row 0 -sticky "nw" -columnspan 3
    grid config $_fhl.labelframe.fspace   -column 7 -row 0 -sticky "nw"

    iwidgets::scrolledframe $_fhl.contentframe -relief ridge -borderwidth 2 \
	 -vscrollmode dynamic -hscrollmode none
    grid config $_fhl.contentframe -column 0 -row 3 -columnspan 7 -sticky "snew"

    grid rowconfigure $_fhl 3 -weight 5
    grid columnconfigure $_fhl 0 -weight 5

    # expand text entry
    grid columnconfigure $_fhl.labelframe 1 -weight 5

    # make sure all children of contentframe are also moveable
    # noted these are hardcoded by system .lwchildsite.clipper.canvas
    set _subwin [$_fhl.contentframe childsite]
    _openFitsFile

    _drawContent

    _closeFitsFile 
    grid columnconfigure $_fhl.contentframe 0 -weight 5

    grid config          $_fhl.contentframe.lwchildsite -sticky "news"
    grid columnconfigure $_fhl.contentframe.lwchildsite 0 -weight 5

    grid config          $_fhl.contentframe.lwchildsite.clipper -sticky "news"
    grid columnconfigure $_fhl.contentframe.lwchildsite.clipper 0 -weight 5

    grid config          $_fhl.contentframe.lwchildsite.clipper.canvas -sticky "news"
    grid columnconfigure $_fhl.contentframe.lwchildsite.clipper.canvas 0 -weight 5

    #grid config $_subwin -sticky "news"
#ziqin
}

itcl::body FitsFile::getNumHdus { } {
   return $_numExts
}

itcl::body FitsFile::_drawContent { } {
   global isWin isMac
   global _buttonDivider
   global g_titleFont

   set rName [urlTail $filename]
   set dName [getFullDirPath $filename]
   wm title $_fhl "fv: Summary of [file tail $_oldName] in $dName"

   set nhdus [_getNumHdus]

   set _numExts $nhdus

   if { $isMac } {
       set btnRelief flat
   } else {
       set btnRelief raised
   }
      
   set textMaxLength -99

   for {set i 1} {$i <= $nhdus} {incr i} {
      set tableFlag false

      catch {moveToHDU $i} err
      set _extData($i,dims) "0"
      if { $i == 1 } {
	 set _extData($i,extName) Primary
	 set _extData($i,type)    Image
	 set dimsl [getImgInfo]
	 set ndims [llength $dimsl]
	 if { $ndims != 0 } {
	    set _extData($i,dims) [lindex $dimsl 0] 
	    for {set k 1} { $k < $ndims } {incr k} {
	       set tmpDim [lindex $dimsl $k]
	       append _extData($i,dims) " X $tmpDim" 
	    }
	 }
	 
      } else {
	 if { [catch { set _extData($i,extName) \
	       [lindex [join [getKeyword EXTNAME]] 1] } ] } {
	    set _extData($i,extName) "NoName" 
	 } else {
            set _extData($i,extName) [string trim $_extData($i,extName) {' }]
         }
	 set _extData($i,type)    [lindex [join [getTableInfo hdutype]] 0]
	 if { ($_extData($i,type) == "Binary") || ($_extData($i,type) == "ASCII") } {
            set tableFlag true
	    set _extData($i,nrows)   [getTableInfo nrows]
	    set _extData($i,ncols)   [getTableInfo ncols]
            set _extData($i,colList) [getTableInfo column]
	    set _extData($i,dims) "$_extData($i,ncols) cols"
            append _extData($i,dims) " X $_extData($i,nrows) rows"
	 } else {
	    set dimsl [getImgInfo]
	    set ndims [llength $dimsl ]
	     if { $ndims != 0 } {
		 set _extData($i,dims) [lindex $dimsl 0] 
		 for {set k 1} { $k < $ndims } {incr k} {
		     set tmpDim [lindex $dimsl $k]
		     append _extData($i,dims) " X $tmpDim" 
		 }
	     }
	 } 
      }
      
      set currentTextMaxLength [expr [string length $_extData($i,extName)] + 10]

      if { $textMaxLength < $currentTextMaxLength } {
         set textMaxLength $currentTextMaxLength
      }

      checkbutton $_subwin.index$i  -text [expr $i-1]  -width 6 \
	    -variable  [itcl::scope _extCheck($i)] \
	    -selectcolor $fvPref::checkBBgColor  \
	    -activeforeground black -activebackground $fvPref::globalBgColor \
	    -justify center -font g_titleFont
      label  $_subwin.extName$i -text $_extData($i,extName)  \
                                -width $textMaxLength \
	                        -justify center -font g_titleFont
      label  $_subwin.type$i    -text $_extData($i,type)     -width 8 \
	    -justify center -font g_titleFont
      label  $_subwin.dims$i    -text $_extData($i,dims)     -width 30 \
	    -justify center -font g_titleFont
      
      button $_subwin.kwds$i -text "Header" \
            -borderwidth 2 -relief $btnRelief -width 5  \
	    -command  [itcl::code $this openHeader $i] -font g_titleFont

      button $_subwin.img$i -text "Hist" \
            -borderwidth 2 -relief $btnRelief -width 2 \
            -command  [itcl::code $this callHistogram $i false ] -font g_titleFont
      button $_subwin.plot$i -text "Plot" \
            -borderwidth 2 -relief $btnRelief -width 4 \
            -command  [itcl::code $this plotData $i] -font g_titleFont
#            -command  [itcl::code $this plotData1 $i] -font g_titleFont

      button $_subwin.tab$i  -text "All" \
            -borderwidth 2 -relief $btnRelief -width 6 \
            -command  [itcl::code $this openTable $i] -font g_titleFont
      button $_subwin.selcol$i  -text "Select" -font g_titleFont \
            -borderwidth 2 -relief $btnRelief -width 5 \
            -command  [itcl::code $this openTable $i]

      set errorFlag [ catch {
          set imgDim [getImgInfo]
      } err ]

      set noSelColFlag true
      set xyFlag false

      if { $errorFlag } {
         set noSelColFlag false
      }

      if { ($_extData($i,type) != "Binary") && ($_extData($i,type) != "ASCII") } { 
         if { [regexp X $_extData($i,dims)] } {
	    $_subwin.plot$i config -text "Image" -width 4
         }
         # else if image is 1-d (no "X") then the mid button should say "Plot"

	 # if image is empty, disable the image/table buttons
	 if { $_extData($i,dims) == 0 } {
	    $_subwin.plot$i    config -text "Image" -width 4
	    $_subwin.plot$i    configure -state disabled 
	    $_subwin.tab$i     configure -state disabled -text "Table"
            set noSelColFlag   true
	 }
      } else {
         # check if x and y are presented in column name list
         set xyFlag [checkXYCol $i]
      }
 
      bind $_subwin.selcol$i <ButtonRelease-1> \
            "[itcl::code $this _setNewTable 1]; tk::ButtonUp %W"
      if { $isMac } {
         bind $_subwin.selcol$i <ButtonPress-1> { tk::ButtonDown %W }
         bind $_subwin.selcol$i <ButtonRelease-1> \
               "[itcl::code $this _setNewTable 1]; tk::ButtonUp %W"
      }

      set colIdx 0
      grid config $_subwin.index$i     -column $colIdx -row [expr $i] -sticky "nw"
      incr colIdx
      grid config $_subwin.extName$i   -column $colIdx -row [expr $i] -sticky "new"
      set textCol $colIdx
      incr colIdx
      grid config $_subwin.type$i      -column $colIdx -row [expr $i] -sticky "nw"
      incr colIdx
      grid config $_subwin.dims$i      -column $colIdx -row [expr $i] -sticky "nw"
      incr colIdx
      grid config $_subwin.kwds$i      -column $colIdx -row [expr $i] -sticky "nw"
      incr colIdx

      if { $tableFlag == "true" } {
         $_fhl.labelframe.fbut  configure -width 35
         if { $xyFlag == "true" } {
            $_subwin.img$i configure -command  [itcl::code $this callHistogram $i true]
         }
         grid config $_subwin.img$i  -column $colIdx -row [expr $i] -sticky "nesw"
         incr colIdx
         $_subwin.plot$i configure -width 2
         grid config $_subwin.plot$i -column $colIdx -row [expr $i] -sticky "snew"
         incr colIdx
      } else {
         grid config $_subwin.plot$i -column $colIdx -row [expr $i] -sticky "snew" -columnspan 2
         set colIdx [expr $colIdx + 2]
      }

      if { $noSelColFlag == "false" } {
         $_subwin.tab$i configure -width 2
         grid config $_subwin.tab$i     -column $colIdx -row [expr $i] -sticky "nsew"
         incr colIdx
         grid config $_subwin.selcol$i  -column $colIdx -row [expr $i] -sticky "nsew"
      } else {
         $_subwin.tab$i configure -text "Table"
         grid config $_subwin.tab$i  -column $colIdx -row [expr $i] -sticky "wnes" -columnspan 2
      }
   }

   # expand text entry
   $_fhl.labelframe.fextName configure -width $textMaxLength
   grid columnconfigure $_subwin $textCol -weight 5

   set lineHeight [expr [winfo reqheight $_subwin.tab1] ]
   if { $_numExts <= 10} {
      $_fhl.labelframe.fspace   configure -width 3 
      $_fhl.contentframe configure -height [expr 10 + $_numExts*$lineHeight]
   } else {
      $_fhl.contentframe configure -height [expr 10 + 10*$lineHeight]
   }

}

itcl::body FitsFile::_fetchFile {} {
   global g_backupDir
   global isWin isMac

   #####
   # Copy FTP and HTTP files to backup directory for accessing
   #####

# FTP FILE
   if { [string range $filename 0 5] == "ftp://" } {

      #    Handle FTP files

      set _isFtp 1

      # Make copy and check that the name is unique
      set cnt 0
      set endName [urlTail $filename]
      set openedFileName [file join ${g_backupDir} $endName]
      while { [file exists $openedFileName] } {
	 incr cnt
	 set openedFileName [file join $g_backupDir f${cnt}_$endName]
      }
      
      # Copy ftp file using RemoteClass ftp method, rather than
      # leave it to CFITSIO, which doesn't have drivers for Windows
      
      _ftpFile $filename $openedFileName

# HTTP FILE
   } elseif { [string range $filename 0 6] == "http://" } {
      
      #    Handle HTTP files

      set _isHttp 1

      # Make copy, but check that the name is unique
      set cnt 0
      set endName [urlTail $filename]
      set openedFileName [file join ${g_backupDir} $endName]
      while { [file exists $openedFileName] } {
	 incr cnt
	 set openedFileName [file join ${g_backupDir} h${cnt}_$endName]
      }
	 
      set tmpFileName ${filename}\($openedFileName\)
# not sure what the purpose of this is
      set f [fits open $tmpFileName]
      $f close

# ORDINARY DISK FILE	 
   } else {
      set openedFileName $filename
   }

   if { $isWin } {
      set filePermission [file attributes $openedFileName -system]
   } elseif { $isMac } {
      set filePermissions ""
   } else {
      set filePermission [file attributes $openedFileName -permissions]
   }
}

itcl::body FitsFile::_ftpFile { fname_ saveAs_ } {
   global g_backupDir

   set ftpC ftp_[namespace tail $this]
   RemoteClass $ftpC

#  Decode this file syntax: "ftp://userName:passwd@host/path/file.fits"

   set fname_ [string range $fname_ 6 end]
   set idx [string first "/" $fname_]
   if { $idx == -1 || [string range $fname_ end end] == "/" } {
      # Not pointing to a file... error out
      error "File name points to a host or directory, not a file"
   }
   set path [string range $fname_ $idx end]
   incr idx -1
   set loginInfo [string range $fname_ 0 $idx]
   set idx [string last "/" $path]
   set file [string range $path [expr $idx+1] end]
   set path [string range "$path" 1 [expr $idx-1]]

   set idx [string last "@" $loginInfo]
   if { $idx == -1 } {
      set host $loginInfo
      set user anonymous
      set pass "fv@fv.gsfc.nasa.gov"
   } else {
      set host [string range $loginInfo [expr $idx+1] end]
      set user [string range $loginInfo 0 [expr $idx-1]]
      set idx [string first ":" $user]
      if { $idx == -1 } {
	 set pass "fv@fv.gsfc.nasa.gov"
      } else {
	 set pass [string range $user [expr $idx+1] end]
	 set user [string range $user 0 [expr $idx-1]]
      }
   }

   $ftpC openConn $host $user $pass
   $ftpC cd $path
   if { ![$ftpC get ${file} $saveAs_] } {
      itcl::delete object $ftpC
      error "Unable to copy $file to local disk"
   }
   itcl::delete object $ftpC

   # save FTP information for later

   set _ftp_user $user
   set _ftp_pass $pass
   if { $path == "" } {
      set _ftp_file "${host}/${file}"
   } else {
      set _ftp_file "${host}/${path}/${file}"
   }
   if { $user == "anonymous" } {
      set filename "ftp://${_ftp_file}"
   } else {
      set filename "ftp://${user}@${_ftp_file}"
   }
}

## actually opens $filename and sets $fitsfile
#
itcl::body FitsFile::_openFitsFile {} {
   if { $isMaster || $isFailedToCopy } {
      # Read ONLY
      set rw 0
   } else {
      set rw 1
   }

   if { [string first "ftp:" $filename] >= 0 } {
      set rw 0
   }
   set errorFlag [ catch {
       set fitsfile [fits open "$openedFileName" $rw]
   } err ]

   if { $errorFlag } {
      if { $rw == 1 } { 
         set rw 0 
      } elseif { $rw == 0 } { set rw 0 }

      set errorFlag [ catch {
          set fitsfile [fits open "$openedFileName" $rw]
      } err ]

      if { $errorFlag } {
         error "Cannot open file: $openedFileName, error: $err"
      }
   }

   set opened 1
}

## argh -- poorly documented method!!
#
itcl::body FitsFile::getBackup {} {
   global g_backupDir
   global isWin isMac

   # Get location of this file in the native format, so that we can
   # do a valid string comparison... do not create backup if opened
   # file is already in the backup directory... eg, histogram
      
   set nativeBackupDir [file nativename $g_backupDir]
   set nativeOpenedDir [file nativename [file dir $openedFileName]]

   if { $openedFileName == $filename && !$_isFtp && !$_isHttp \
	 && $nativeOpenedDir != $nativeBackupDir } {
      
      set root [file rootname [urlTail $filename]]
      set ext  [file extension $filename]
   
      # make the filename "." and ":" free for Win32
      regsub -all ":|\\." $root "_" tmpFileName
      
      # Make sure the name is unique
      set cnt 0
      set backupfilename [file join ${g_backupDir} ${tmpFileName}$ext]
      while { [file exists $backupfilename] } {
	 incr cnt
	 set backupfilename [file join ${g_backupDir} b${cnt}_${tmpFileName}$ext]
      }
      
      # Try to make a copy in the g_backupDir
      
      # If a compressed file, use CFITSIO to copy/uncompress
      if { [isFits $filename] == 2 } {
         set fName "${filename}(${backupfilename})"
         set errFlag [catch {set f [fits open $fName 0]} err]
         if { !$errFlag } {$f close}
      } else {
         set errFlag [catch {file copy $filename $backupfilename} err]
      }

      if { $errFlag } {
         set _fileMode 1
         set isFailedToCopy 1
       puts "Error: $err"
       puts "Cannot make backup, please set FVTMP to some place"
       puts "which has bigger disk space. Open as readonly this time."
      } else {
	 # chmod to 666 for read and write
	 if { $isWin || $isMac } {
	    file attributes $backupfilename -readonly 0
	 } else {

# most common situation
	    file attributes $backupfilename -permissions 00666
	 }
# most common situation
	 set openedFileName $backupfilename
      }
   }
   return $openedFileName
}

itcl::body FitsFile::_closeFitsFile {} {

    if { [catch {$fitsfile close} error] == 1 } {
	error  $error
    }

    set opened 0
    set _loaded 0
}

itcl::body FitsFile::countChanged {} {
   # Count number of extensions changed

   set nChanged 0
   foreach child $_myExtensionChildren {
      set childFObj ::FitsExtension::[$child cget -fFile]
      if { [$childFObj isFileChanged] } {
         incr nChanged
      }
   }
   return $nChanged
}

itcl::body FitsFile::save {} {
   global isWin isMac g_backupDir
   global g_fileIsClosing

   if { ![isFileChanged] } return

   if { !$_isOriginalFile } {

      set nChanged [$_origFileObj countChanged]
      if { $nChanged > 1 } {
	 set feedback [promptMsg "WARNING: This will save changes made\
	       to multiple extensions.  Continue?" \
	       return Yes No]
	 if { $feedback == "CANCEL" } return
      }
      $_origFileObj save
      return
   }

   if { [string first $g_backupDir $filename] >= 0 } {
      # current file is created out of original file and it is in the temp directory
      set result [saveAs]
      if { $result == "NOTSAVE" } return
   }

   # Update/flush each subordinate FITS file still opened

   foreach child $_myExtensionChildren {
      set childFObj ::FitsExtension::[$child cget -fFile]
      if { [$childFObj isFileChanged] } {
	 $childFObj writeHisKey
	 $childFObj updateChecksum
	 $childFObj flush noClear 
	 $child updateDisps ""
      }
   }

   # Try to make a backup
   if ![info exists g_fileIsClosing] {

      if { [catch {file copy -force $filename $filename~} err] } {
         puts "Warning: cannot make a backup of the original file:\n $err"
      } 

      # Copy file

      if { [catch {file copy -force $openedFileName $filename} err ] } {
         error $err
         return
      }
   }

   unchangeFile
   # copy permissions of original
   if { $isWin } {
      file attributes $filename -system $filePermission
   } elseif { $isMac } {
       # Make file fvEd/FITS type
       file attributes $filename -creator "fvEd" -type "FITS"
   } else {
      file attributes $filename -permissions $filePermission
   }
   _redrawHighLight
}

itcl::body FitsFile::saveHDUsAsText { {saveFileName_ ""} } {
   global isWin isMac
   if { !$_isOriginalFile } {
      $_origFileObj saveAs $saveFileName_
      return
   }

   if { $saveFileName_ == "" } {
      set t [urlTail $filename]
      set r [file root $t]
      set e [file ext $t]
      if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
         set e [file ext $r]
         set r [file root $r]
      }
      set sugName "${r}_hdu.txt"
      set saveFileName_ [getSelectedFileName $sugName]
      if { $saveFileName_ == "" } return
      if { [file exist $saveFileName_] } {
         set feedback [promptMsg "File $saveFileName_ exists\n overwrite?" \
               return Yes No]
         if { $feedback == "CANCEL" } return
      }
   }

   set f [open $saveFileName_ "w"]

   puts $f [format "%-5s   %-30s   %-10s   %-20s" Index Extension Type Dimension]
   puts $f [format "%-5s   %-30s   %-10s   %-20s" "=====" \
                                                  "==============================" \
                                                  "==========" \
                                                  "===================="]
   for {set i 1} {$i <= $_numExts} {incr i} {
       puts $f [format "%-5s   %-30s   %-10s   %-20s" $i $_extData($i,extName) $_extData($i,type) $_extData($i,dims)] 
   }

   close $f

   # copy permissions of original, but make read/write
   if { $isWin } {
      file attributes $saveFileName_ -system $filePermission
      file attributes $saveFileName_ -readonly 0
      set filePermission [file attributes $saveFileName_ -system]
   } elseif { $isMac } {
       # Make file fvEd/FITS type
       file attributes $saveFileName_ -creator "fvEd" -type "FITS"
   } else {
      set filePermission "006[string range $filePermission 3 end]"
      file attributes $saveFileName_ -permissions $filePermission
   }
   set filename $saveFileName_
   .fvwinkeeper signoff  $_fhl
   .fvwinkeeper register $_fhl "Highlight" [urlTail $filename] 0 $this
   set _fileMode 0
   foreach child $_myExtensionChildren {
      $child setFileName $filename
   }
   _redrawHighLight

}

itcl::body FitsFile::saveAs { {saveFileName_ ""} } {
   global isWin isMac

   # This function should be called only from the Master FitsFile
   # PDW 2000/11/16: SaveAs does exist in Mac File menu, so this
   #                 needs to work there, too, so pass it to _origFileObj
   if { !$_isOriginalFile } {
      $_origFileObj saveAs $saveFileName_
      return 
   }

   if { $saveFileName_ == "" } {
      set t [urlTail $filename]
      set r [file root $t]
      set e [file ext $t]
      if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
         set e [file ext $r]
         set r [file root $r]
      }
      set sugName "${r}_2${e}"
      set saveFileName_ [getSelectedFileName $sugName]
      if { $saveFileName_ == "" } {
          return NOTSAVE
      }
      if { [file exist $saveFileName_] } {
         set feedback [promptMsg "File $saveFileName_ exists\n overwrite?" \
               return Yes No]
         if { $feedback == "CANCEL" } {
            return NOTSAVE
         }
      }
   }

   # Update/flush each subordinate FITS file still opened

   foreach child $_myExtensionChildren {
      set childFObj ::FitsExtension::[$child cget -fFile]
      if { [$childFObj isFileChanged] } {
         $childFObj writeHisKey
         $childFObj updateChecksum
         $childFObj flush noClear 
      }
   }

   file copy -force $openedFileName $saveFileName_

   unchangeFile
   # copy permissions of original, but make read/write
   if { $isWin } {
      file attributes $saveFileName_ -system $filePermission
      file attributes $saveFileName_ -readonly 0
      set filePermission [file attributes $saveFileName_ -system]
   } elseif { $isMac } {
       # Make file fvEd/FITS type
       file attributes $saveFileName_ -creator "fvEd" -type "FITS"
   } else {
      set filePermission "006[string range $filePermission 3 end]"
      file attributes $saveFileName_ -permissions $filePermission
   }
   set filename $saveFileName_
   .fvwinkeeper signoff  $_fhl
   .fvwinkeeper register $_fhl "Highlight" [urlTail $filename] 0 $this
   set _fileMode 0
   foreach child $_myExtensionChildren {
      $child setFileName $filename
   }
   _redrawHighLight
   wm title $_fhl "fv: Summary of [file tail $saveFileName_] in [file dirname $saveFileName_]"
   return [file tail $saveFileName_]
}

itcl::body FitsFile::saveCHDUAs {ext} {
   global isWin isMac

   set t [urlTail $filename]
   set r [file root $t]
   set e [file ext $t]
   if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
      set e [file ext $r]
      set r [file root $r]
   }
   set sugName "${r}_hdu${e}"
   set saveFileName [getSelectedFileName $sugName]
   if { $saveFileName == "" } return
   if { [file exist $saveFileName] } {
      set feedback [promptMsg "File $saveFileName exists\n overwrite?" \
                              return Yes No]
      if { $feedback == "CANCEL" } return
   }

   file delete \"$saveFileName\"
   set tmpfits [fits open $saveFileName 2 tmpfits]
   $tmpfits put ihd -p 
   $tmpfits close
   
   set tmpfitsfile [fits open $openedFileName 0]

   set nhdus [$tmpfitsfile info nhdu]
   set _extCheck(2) 1
   for {set i 1} {$i <= $nhdus} {incr i} {
      if { $_extCheck($i) } {
# means to move to current HDU
         $tmpfitsfile move $i
         if { $_extData($i,extName) == "Primary" } {
            $tmpfitsfile copy $saveFileName
         } else {
            $tmpfitsfile append $saveFileName
         }
      }
      set _extCheck($i) 0
   }

   $tmpfitsfile close
   
   # copy permissions of original but make writeable
   if { $isWin } {
      file attributes $saveFileName -system $filePermission
      file attributes $saveFileName -readonly 0
      set filePermission [file attributes $saveFileName -system]
   } elseif { $isMac } {
       # Make file fvEd/FITS type
       file attributes $saveFileName -creator "fvEd" -type "FITS"
   } else {
      set filePermission "006[string range $filePermission 3 end]"
      file attributes $saveFileName -permissions $filePermission
   }
}




itcl::body FitsFile::saveHDUsAs {} {
   global isWin isMac

   if { ![_isHDUSelected] } {
      error "Please select HDU first"
      return
   }

   set t [urlTail $filename]
   set r [file root $t]
   set e [file ext $t]
   if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
      set e [file ext $r]
      set r [file root $r]
   }
   set sugName "${r}_hdu${e}"
   set saveFileName [getSelectedFileName $sugName]
   if { $saveFileName == "" } return
   if { [file exist $saveFileName] } {
      set feedback [promptMsg "File $saveFileName exists\n overwrite?" \
                              return Yes No]
      if { $feedback == "CANCEL" } return
   }

   file delete \"$saveFileName\"
   set tmpfits [fits open $saveFileName 2 tmpfits]
   $tmpfits put ihd -p 
   $tmpfits close
   
   set tmpfitsfile [fits open $openedFileName 0]

   set nhdus [$tmpfitsfile info nhdu]
   for {set i 1} {$i <= $nhdus} {incr i} {
      if { $_extCheck($i) } {
# means to move to current HDU
         $tmpfitsfile move $i
         if { $_extData($i,extName) == "Primary" } {
            $tmpfitsfile copy $saveFileName
         } else {
            $tmpfitsfile append $saveFileName
         }
      }
      set _extCheck($i) 0
   }

   $tmpfitsfile close
   
   # copy permissions of original but make writeable
   if { $isWin } {
      file attributes $saveFileName -system $filePermission
      file attributes $saveFileName -readonly 0
      set filePermission [file attributes $saveFileName -system]
   } elseif { $isMac } {
       # Make file fvEd/FITS type
       file attributes $saveFileName -creator "fvEd" -type "FITS"
   } else {
      set filePermission "006[string range $filePermission 3 end]"
      file attributes $saveFileName -permissions $filePermission
   }
}


itcl::body FitsFile::flush {isClear_} {
    if { $isClear_ == "clear"} {
	$fitsfile flush clear
    } else {
	$fitsfile flush
    }
}

itcl::body FitsFile::revertFile {} {

   if { ![isFileChanged] } return

   set feedback [promptMsg \
	 "All changes since last save will be lost.  Continue?" \
	 return Yes No]
   if {$feedback == "CANCEL"} return

   cleanChild

   if { $filename != $openedFileName } {
      file delete \"$openedFileName\"
   }
   if { $_isFtp } {
      # Must reconstruct original filename
      set filename "ftp://${_ftp_user}:${_ftp_pass}@${_ftp_file}"
   }
   _fetchFile
   unchangeFile
   _redrawHighLight
}

itcl::body FitsFile::writeHisKey {} {
   global env
   global isWin

# write the HISTORY keyword
   if { $fvPref::ifWriteHisKey == 1 && [isFileChanged] && !$_isOriginalFile } {
      set date [clock format [clock seconds] \
	    -format %Y-%m-%dT%H:%M:%S] 
      
      # Search through the various enviroment variables for a user name
      if { [info exists env(USER)] } {
	 set uname $env(USER)
      } elseif { [info exists env(USERNAME)] } {
	 set uname $env(USERNAME)
      } elseif { [info exists env(LOGNAME)] } {
	 set uname $env(LOGNAME)
      } else {
	 set uname "???"
      }

      putHis "File modified by user \'$uname\' with fv  on $date" 
   }
}


# the following methods are wrappers around $fitsfile, which is a FitsTcl object
# all fv code uses these wrappers with two exceptions
# (1) $fitsfile load iblock
# (2) $fitsfile load tblock
# in both cases, the local code's $fitsfile will create a local 2d Tcl Array
#   since Tcl does not pass arrays, we cannot create the arrays here in FitsFile
# instead, the array is created locally and the local code grabs $fitsfile from here,
#   which is why FitsFile keeps $fitsfile as a public variable

itcl::body FitsFile::_getNumHdus {} {
    _isOpen
    $fitsfile info nhdu
}

itcl::body FitsFile::getTableInfo {name_} {
    $fitsfile info $name_
}


itcl::body FitsFile::getColInfo {colName_} {
    $fitsfile info column -exact $colName_
}

# unclear what it returns since it's not documented in fitsTcl
itcl::body FitsFile::getExprInfo {expr_} {
    $fitsfile info expr $expr_
}

itcl::body FitsFile::getImgInfo {} {
    $fitsfile info imgdim
}

itcl::body FitsFile::getImgType {} {
    $fitsfile info imgType
}


itcl::body FitsFile::moveToHDU {numHDU_} {
    # change the currentHDU to numHDU_

    _isOpen
    if { [catch {$fitsfile move $numHDU_} ] == 1 } {
	error "Cannot access HDU extension [expr $numHDU_ -1]. Does the file still exist?"
	return
     } else {
	set currentHDU $numHDU_
     }
    
}

itcl::body FitsFile::getcolblock {col_ rowrange_} {
    _isOpen
    if { [catch {
       set x [$fitsfile get table -c -noformat $col_ $rowrange_]
    } err] }  {
       puts "Cannot read table block of column $col_: $err"
       return {}
    }
    return $x
}

itcl::body FitsFile::getKeyword {keyword} {
    # get $keyword
    _isOpen
    $fitsfile get keyword ^$keyword\$
}

itcl::body FitsFile::getWcs {{dest {}} {RAColNum_ {}} {DecColNum_ {}} } {
    _isOpen

    if { $RAColNum_ == "" || $DecColNum_ == "" } {
       if { [catch {set wcs [$fitsfile get wcs -m $dest]} err] } {
          return ""
       } else {
          return $wcs
       }
    } else {
	set wcs [$fitsfile get wcs -m $dest $RAColNum_ $DecColNum_]
	return $wcs
    }
    
}

itcl::body FitsFile::getKeywords {keywords_} {
    # return a list of all the values for $keywords_

    set values {}

    _isOpen
    set currentHDU [$fitsfile info chdu]
    
    set curext 0

    for {set i 1} {$i <= [llength $keywords_]} {incr i} {
	set retval([lindex $keywords_ [expr $i-1]]) {}
    }
    <
    while {1} {
	
	incr curext
	if {[catch {moveToHDU $curext}]} {
	    #we're done.
	    break
	}
	
	for {set i 1} {$i <= [llength $keywords_]} {incr i} {
	    set keyword [lindex $keywords_ [expr $i-1]]
	    if {[catch {getKeyword $keyword} result]} {
		# didn't find it
		continue
	    }
	    
	    set result [lindex [lindex $result 0] 1]
	    set result [string trim $result ']
	    set result [string trim $result]
	    
	    set retval($keyword) [concat $retval($keyword) \
				      [list $result]]
	    set values [concat $values [list $result]]
	}
    }
    moveToHDU $currentHDU

    set values {}
    for {set i 1} {$i <= [llength $keywords_]} {incr i} {
	set values [concat $values \
			[list $retval([lindex $keywords_ [expr $i-1]])]]
    }
    

    return $values
}

itcl::body FitsFile::dumpHeader {} {
    _isOpen
    set header [$fitsfile dump -e]
    return $header
# header is a string separated by newline characters
}

itcl::body FitsFile::_isOpen {} {
    if {$opened == 0} {
	error "FitsFile $this : $filename not open"
    }
}

itcl::body FitsFile::isReadOnly {} {
   if { $_isOriginalFile } {
# 1 means readonly, 0 means read/write
      return $_fileMode
   } else {
      return [$_origFileObj isReadOnly]
   }
}

itcl::body FitsFile::closeCmd { {force_ 0} } {
   global env
   global fvHOME
   global g_fileIsClosing
   global notSaveAll

   # Check for modifications needing saving
   set g_fileIsClosing 1

   if { [isFileChanged] } {
      if { $_isFtp || $_isHttp || [isReadOnly] } {
	 set cmd [itcl::code $this saveAs]
      } else {
	 set cmd [itcl::code $this save]
      }
      if { $force_ } { set cTxt "None" } else { set cTxt "Cancel" }

      if { [string first $env($fvHOME) $filename] > 0 } {
      } else {
         if ![info exists notSaveAll] {
            set feedback [promptMsg \
	          "File $filename has changed.\n\
	          Do you want to save the file before closing?" \
	          $cmd Yes $cTxt "No to All"]
            if {!$force_ && $feedback == "BREAK"} {
	       return 1
            }
            if { $feedback != "CANCEL" } {
               # Pan Chai: pop up directory list to save the file
               saveAs
            }
         }
      }
   }

   cleanChild
   .fvwinkeeper signoff $_fhl
   itcl::delete object $this
   unset g_fileIsClosing
   return 0
}

itcl::body FitsFile::cleanChild {} {
   foreach child $_myExtensionChildren {
      $child closeCmd
   }
}


itcl::body FitsFile::_postMenus {} {
   global isMac

   if { [isFileChanged] } {
      if { [isReadOnly] } {
	 $_mBar.file entryconfigure Save -state disabled
      } else {
	 $_mBar.file entryconfigure Save -state normal
      }
      $_mBar.file entryconfigure Revert -state normal
   } else {
      $_mBar.file entryconfigure Save -state disabled
      $_mBar.file entryconfigure Revert -state disabled
   }

   if { [_isHDUSelected] } {
      $_mBar.file entryconfigure "Export*" -state normal
      $_mBar.edit entryconfigure "Copy*" -state normal
      $_mBar.edit entryconfigure "Cut*" -state normal
   } else {
      $_mBar.file entryconfigure "Export*" -state disabled
      $_mBar.edit entryconfigure "Copy*" -state disabled
      $_mBar.edit entryconfigure "Cut*" -state disabled
   }
   
   if { [_isImgSelected] } {
      $_mBar.tools entryconfigure "Smooth*" -state normal
      $_mBar.tools entryconfigure "Plot*" -state normal
   } else {
      if [_isThereImgExt] {
         $_mBar.tools entryconfigure "Smooth*" -state normal
      } else {
         $_mBar.tools entryconfigure "Smooth*" -state disabled
      }
      if { [_isTblSelected] } {
         $_mBar.tools entryconfigure "Plot*" -state normal
      } else {
         $_mBar.tools entryconfigure "Plot*" -state disabled
      }
   }

   if { [lindex [fvClipBoard report] 0] == "HDUs" } {
      $_mBar.edit entryconfigure "Paste*" -state normal
   } else {
      $_mBar.edit entryconfigure "Paste*" -state disabled
   }
   update idle
}

itcl::body FitsFile::updateHighLight {extNum_ field_ value_} {
    switch $field_ {
	"DIMENSION" {
	    $_subwin.dims$extNum_ configure \
		-text "[lindex $value_ 0] cols X [lindex $value_ 1] rows" 
	}
	"EXTNAME" {
	    $_subwin.extName$extNum_ configure -text $value_
	}
	default {return}
    }
}

itcl::body FitsFile::getNthKey {n_} {
    _isOpen
    $fitsfile get keyword -num $n_
}

itcl::body FitsFile::copyCHDU {saveName_} {
    $fitsfile copy $saveName_
} 

itcl::body FitsFile::insertKey {n_ rec_ fflag_} {
    $fitsfile insert keyword $n_ $rec_ $fflag_
}

itcl::body FitsFile::putNthKey {n_ rec_ fflag_} {
    $fitsfile put keyword -num $n_ $rec_ $fflag_
}

itcl::body FitsFile::putHis {rec_} {
    $fitsfile put history $rec_
}

itcl::body FitsFile::delNthKey {n_} {
    $fitsfile delete keyword $n_
}

itcl::body FitsFile::loadColumn {col_ null_ i_} {
    $fitsfile load column $col_ $null_ $i_
}

# evaluate arithemetic expression $expr_ on each row in $rows_
# using $null_ as result of any null results
# Returns: dataAddress dataType numElements
itcl::body FitsFile::loadExpr {expr_ null_ {rows_ "-"} } {
    $fitsfile load expr -rows $rows_ $expr_ $null_
}

itcl::body FitsFile::loadImage {} {
    $fitsfile load image
}

itcl::body FitsFile::loadImageFlip { direction } {
    $fitsfile load image flip $direction
}

# loads image block into a memory data address for POW to use
# the memory address is returned to the caller
itcl::body FitsFile::loadImageBlock {fRow_ nRows_ fCol_ nCols_ slice_} {
    $fitsfile load iblock -- $fRow_ $nRows_ $fCol_ $nCols_ $slice_
}

itcl::body FitsFile::loadImageSlice {i_ r_} {
    $fitsfile load image $i_ $r_
}

# loads the entire vector table contained in $colName_
#   to a data address for POW to plot
itcl::body FitsFile::loadVectorTableToDataAddressForPOW {colName_} {
    $fitsfile load vtable $colName_
}

itcl::body FitsFile::getVectorTableAsRawList {colName_ rows_ range_} {
    $fitsfile get vtable -noformat $colName_ $rows_ $range_
}

# returns a list of totalCol_ elements in the row_ row of a vector table
# based on the colName_ of its parent table
itcl::body FitsFile::getVectorTableRowAsFormattedList {colName_ row_ totalCols_} {
    set tmpList {}
    for {set col 0} {col < totalCols_} {incr col} {
	lappend tmpList [$fitsfile get vtable $colName_ $col_ $row_]
    }
    return $tmpList
}

itcl::body FitsFile::freeColumn {add_} {
    $fitsfile free column $add_
}

itcl::body FitsFile::freeExpr {add_} {
    $fitsfile free expr $add_
}

itcl::body FitsFile::freeVTable {add_} {
    $fitsfile free vtable $add_
}

itcl::body FitsFile::loadTBlock {var_ cList_ fRow_ nRows_ fCol_ nCols i_} {
    $fitsfile load tblock $var_ $cList_ $fRow_ $nRows_ $fCol_ $nCols $i_
}

itcl::body FitsFile::delRows {n0_ nn_} {
    $fitsfile delete rows $n0_ $nn_
}

itcl::body FitsFile::delRowsRange {rowrange_} {
    $fitsfile delete rows -range $rowrange_
}

itcl::body FitsFile::selRowsWithCondition {cond_ fRow_ nRow_} {
    $fitsfile select rows -expr $cond_ $fRow_ $nRow_
}

itcl::body FitsFile::delRowsWithCondition {cond_} {
    $fitsfile delete rows -expr $cond_
}

itcl::body FitsFile::addColumn {name_ form_} {
    $fitsfile add column $name_ $form_
}

itcl::body FitsFile::addColumnFrExpr {name_ form_ expr_ rowrange_} {
    $fitsfile add column $name_ $form_ $expr_ $rowrange_
}

itcl::body FitsFile::addRow {nRows_} {
    $fitsfile add row $nRows_
}

itcl::body FitsFile::insertCol {index_ ttype_ tform_} {
    $fitsfile insert column $index_ $ttype_ $tform_
}

itcl::body FitsFile::insertRow {index_ nRows_} {
    $fitsfile insert row $index_ $nRows_
}

itcl::body FitsFile::delCols {colList_} {
    $fitsfile delete cols $colList_

}

itcl::body FitsFile::getImageAsList {firstElem_ nElem_} {
    $fitsfile get image $firstElem_ $nElem_
}

itcl::body FitsFile::putImage {fElem_ fRow_ value_} {
    $fitsfile put image $fElem_ $fRow_ $value_
}

itcl::body FitsFile::putTable {name_ felem_ range_ value_}  {
    $fitsfile put table $name_ $felem_ $range_ $value_
}

itcl::body FitsFile::putKwd {rec_ fflag_} {
    $fitsfile put keyword $rec_ $fflag_
}

itcl::body FitsFile::saveTabToASCII {name_ n_ fRow_ nRows_ colList_ width_ ifFixedFormat_ ifCSV_ ifPrintRow_ sepString_} {
#okbox [list $colList_ $width_]
    $fitsfile sascii table $name_ $n_ $fRow_ $nRows_ $colList_ $width_ $ifFixedFormat_ $ifCSV_ $ifPrintRow_ $sepString_
}

itcl::body FitsFile::saveImgToASCII {name_ n_ fRow_ nRows_ fCol_ nCols_ width_ ifCSV_ ifPrintRow_ sepString_ { slice_ 1 }} {
    $fitsfile sascii image $name_ $n_ $fRow_ $nRows_ $fCol_ $nCols_ $width_ $ifCSV_ $ifPrintRow_ $sepString_ $slice_
}

itcl::body FitsFile::saveVecToASCII {name_ n_ fRow_ nRows_ fCol_ nCols_ colName_ ifCSV_ ifPrintRow_ sepString_ ifVariableVec_} {
    $fitsfile sascii vector $name_ $n_ $fRow_ $nRows_ $fCol_ $nCols_ $colName_ $ifCSV_ $ifPrintRow_ $sepString_ $ifVariableVec_
}

itcl::body FitsFile::freeImage {add_} {
    $fitsfile free image $add_
}

itcl::body FitsFile::sort {column_ ascendFlag_ isMerge_} {
# column_ is a list of all the column names used in sorting 
# ascendFlag_ is a list of 1/0 to indicate ascending/decending 
#    sorting for each column
#  isMerge_ == 1, then merge all identical rows
    if { $isMerge_ == 1 } {
	$fitsfile sort -merge $column_ $ascendFlag_
    } else {
	$fitsfile sort $column_ $ascendFlag_ 
    }
}

itcl::body FitsFile::getMaster {} {
   if { $_isOriginalFile } {
      return $this
   } else {
      return $_origFileObj
   }
}

itcl::body FitsFile::getHDUtype { hdu } {
     if [info exists _extData($hdu,type)] {
        return $_extData($hdu,type)
     } else {
        return UNKNOWN
     }

}

itcl::body FitsFile::getOrigName {} {
   if { $_isOriginalFile } {
      return $filename
   } else {
      return [$_origFileObj getOrigName]
   }
}

itcl::body FitsFile::changeFile {} {
   set _isChanged 1
   if { !$_isOriginalFile } {
      $_origFileObj changeFile
   }
}

itcl::body FitsFile::unchangeFile {} {

   set _isChanged 0
   if { $_isOriginalFile } {
      foreach child $_myExtensionChildren {
         set childFObj ::FitsExtension::[$child cget -fFile]
         $childFObj unchangeFile
      }
   }
}

itcl::body FitsFile::isFileChanged {} {
   return $_isChanged
}

itcl::body FitsFile::_deleteHDUs {} {

    if { [_isHDUSelected] != 1 } {
	error "Please select HDU first"
	return
    }

    if { $_extCheck(1) } {
	tk_messageBox -message "You cannot delete the primary array\n"
	return
    }

    if { [llength $_myExtensionChildren] != 0 } {
	tk_messageBox -message \
	    "There are extensions opened for this file. Please close all\
	    other fv windows related to this file before deleting HDUs."
	return
    }

# cut the HDUs to the clipboard file
    _writeHDUsToClipBoard 1

    _redrawHighLight
}

itcl::body FitsFile::redrawHighLight {} {
   _redrawHighLight
}

itcl::body FitsFile::_redrawHighLight {} {
    
    foreach i [winfo children $_subwin] {
	catch {destroy $i}
    }

    # Update required to let the TK event handlers actually handle the
    # destruction of this objects before we recreate them
    update idletasks

    _openFitsFile
    _drawContent
    _closeFitsFile
}

itcl::body FitsFile::_copyHDUs {} {
# just copy
    if { ! [_isHDUSelected] } {
	error "Please select HDU first"
	return
    }
    _writeHDUsToClipBoard 0
    
}

itcl::body FitsFile::_pasteHDUs {} {
    global g_backupDir

    set tmp [fvClipBoard report]
# check if there are hdus in the clipboard
    if { [lindex $tmp 0] != "HDUs" } {
	error "Sorry, there are no HDUs in the clipboard"
	return
    }
    set cbfilename [file join ${g_backupDir} cb.fits]
    fits open $cbfilename 0 tmpcbfits
    set nhdu [tmpcbfits info nhdu]

# skip the first header
    for { set i $nhdu} { $i > 1} {incr i -1} {
	tmpcbfits move $i
	tmpcbfits append $openedFileName
    }
    tmpcbfits close
# update the total _numExts
    set _numExts [expr $_numExts+$nhdu-1]
    _redrawHighLight
    changeFile
}

itcl::body FitsFile::_smoothImgs {} {
    global g_titleFont
    global g_backupDir
    global g_fitsFileMode


    if ![_isImgSelected] {
       tk_messageBox -type ok -icon warning \
                     -message "You must first select which image extension to be smoothed."
       return
    }

# open the smooth dialog 
    if ![info exist xwin] {
       set xwin 3
    }
    if ![info exist ywin] {
       set ywin 3
    }


    if ![info exist _smoothID] {
       set _smoothID 1
    }

    set dosmooth 0 
    if [winfo  exists .smthdlg ] {
        focus .smthdlg 
        raise .smthdlg 
        return
    }

    toplevel .smthdlg -class Dialog 
    wm title .smthdlg "fv: Smooth Image"

    label .smthdlg.title -text "Size of smoothing box:" -font g_titleFont \
       -anchor w
    pack  .smthdlg.title -pady 5 -ipadx 35 -anchor w

    frame .smthdlg.smthx -relief flat
    label .smthdlg.smthx.px -text "Width: " -anchor w -font g_titleFont
    entry .smthdlg.smthx.ex -width 4 \
            -textvariable [itcl::scope xwin] -relief sunken   -font g_titleFont
    pack  .smthdlg.smthx.px .smthdlg.smthx.ex  -side left 
    label .smthdlg.smthx.unit -text " pixel" -anchor w -font g_titleFont
    pack  .smthdlg.smthx.px .smthdlg.smthx.ex .smthdlg.smthx.unit -side left 
    pack  .smthdlg.smthx -pady 3 -padx 3 -anchor w

    frame .smthdlg.smthy -relief flat
    label .smthdlg.smthy.py -text "Height:" -anchor w -font g_titleFont
    entry .smthdlg.smthy.ey -width 4  \
            -textvariable [itcl::scope ywin] -relief sunken   -font g_titleFont
    label .smthdlg.smthy.unit -text " pixel" -anchor w -font g_titleFont
    pack  .smthdlg.smthy.py .smthdlg.smthy.ey .smthdlg.smthy.unit -side left 
    pack  .smthdlg.smthy -side top -pady 3 -padx 3 -anchor w

    label .smthdlg.note -text "(must be odd numbers)" -font g_titleFont
    pack  .smthdlg.note -pady 5 -anchor w

    frame .smthdlg.smthbt -relief flat
    button .smthdlg.smthbt.ok -text OK  \
           -command [itcl::code $this _smoothOK]
    button .smthdlg.smthbt.cancel -text Cancel -command {destroy .smthdlg}
    pack .smthdlg.smthbt.ok  -side left 
    pack .smthdlg.smthbt.cancel -side right 
    pack .smthdlg.smthbt -side top -pady 5 -padx 5 -expand true -fill x

    tkwait win .smthdlg
    set window $xwin
    lappend window $ywin 

    if {$dosmooth == 0 } {
        return 0
    }

#   smooth all the selected extensions 

    set tmpfits [fits open $filename 0]
    set numexts $_numExts
    set smfile [file join $g_backupDir smooth.tmp$_smoothID]
    incr _smoothID   
    if [file exists $smfile] {
          file delete $smfile
    }
    for {set i 1} {$i <= $numexts} {incr i} {
        if { $_extCheck($i) || $numexts == 1} {
            set htype [$tmpfits move $i]
            if {$htype == 0} { 
              $tmpfits smooth $window $smfile 
            }
        }
    }
    $tmpfits close

    set oldMode $g_fitsFileMode
    # Set Read-Only flag
    set g_fitsFileMode 1
# note this is an fedit.tcl openFitsFile proc, not to be confused with _openFitsFile
    set smtmp [openFitsFile $smfile] 
    $smtmp changeFile
    set g_fitsFileMode $oldMode
    return 0
}

itcl::body FitsFile::_smoothOK {} {
    set dosmooth 1 
    destroy .smthdlg 
}

itcl::body FitsFile::_plotHDUs {} {
    global g_titleFont

    set numexts $_numExts

    if {$numexts == 1} {
        plotData 1
        return 0
    }

    set tmpfits [fits open $filename 0]
    for {set i 1} {$i <= $numexts} {incr i} {
        if { $_extCheck($i) } {
            set htype [$tmpfits move $i]
            if {$htype == 0} {
               set  dimsl [$tmpfits info imgdim]
               set  ndims [llength $dimsl ]
	       if { $ndims > 0} {  
                   plotData $i
               }
            } else {
               plotData $i
            }    
        }
    }
    $tmpfits close
    return 0
}

itcl::body FitsFile::_isHDUSelected {} {

   for {set i 1} {$i <= $_numExts} {incr i} {
      if { $_extCheck($i) } {
         return 1
      }
   }
   return 0
}

itcl::body FitsFile::_isThereImgExt {} {
   for {set i 1} {$i <= $_numExts} {incr i} {
       if { $_extData($i,type) == "Image" && $_extData($i,dims) != "0" } {
          return 1
       }
   }
   return 0
}

itcl::body FitsFile::_isImgSelected {} {
    
   for {set i 1} {$i <= $_numExts} {incr i} {
      if { $_extCheck($i) || $_numExts == 1 } {
         if { $_extData($i,type) == "Image" && $_extData($i,dims) != "0" } {
            return 1
         }
      }
   }
   return 0
}

itcl::body FitsFile::_isTblSelected {} {

   for {set i 1} {$i <= $_numExts} {incr i} {
      if { $_extCheck($i) && $_extData($i,type) != "Image" } {
         return 1
      }
   }
   return 0
}

itcl::body FitsFile::_writeHDUsToClipBoard {ifDelete_} {
    global g_backupDir

   getBackup

# ifDelete_ = 1 : cut
# ifDelete_ = 0 : copy

    set cbfilename [file join ${g_backupDir} cb.fits]
    file delete $cbfilename
    set tmpcbfits [fits open $cbfilename 2 tmpcbfits]
    $tmpcbfits put ihd -p 
    $tmpcbfits close
    
    set tmpfits [fits open [getBackup] 1]

    set hduList {}
    for {set i $_numExts} {$i > 0} {incr i -1} {
	if { $_extCheck($i) } {
	    $tmpfits move $i
	    $tmpfits append $cbfilename
	    lappend hduList "[expr $i-1],"
	    if { $ifDelete_ } {
		$tmpfits delete chdu
	    }
	}
	set _extCheck($i) 0
    }
    
    # register to the clipboard. The first arg is a dummy arg
    fvClipBoard register "HDUs" $filename $hduList $hduList
    
    set _numExts [$tmpfits info nhdu]	
    $tmpfits close
    if { $ifDelete_ } changeFile
}


itcl::body FitsFile::getColNum { colname_ } {
    set collist [$fitsfile info column]
    set colnum [lsearch -exact $collist $colname_]
    incr colnum
    return $colnum
}

itcl::body FitsFile::getTLMinMax {colname_} {
    set colnum [getColNum $colname_]
    if { $colnum == 0} {
	return ""
    }
    if { [catch {set tmpmin [getKeyword "TLMIN$colnum"]}] == 1} return ""
    set min [lindex [lindex $tmpmin 0] 1]
    if { [catch {set tmpmax [getKeyword "TLMAX$colnum"]}] == 1} return ""
    set max [lindex [lindex $tmpmax 0] 1]
    #  Make sure min/max is a valid number
    regsub {[dD]} $min E min
    regsub {[dD]} $max E max
    if { [catch {set min [expr $min]}] } { set min "" }
    if { [catch {set max [expr $max]}] } { set max "" }
    return "$min $max"
}

itcl::body FitsFile::getColMinMax {colname_ felem_ rowrange_} {
    $fitsfile info column -minmax $colname_ $felem_ $rowrange_
}

itcl::body FitsFile::getColStat {colname_ felem_ rowrange_} {
    $fitsfile info column -stat $colname_ $felem_ $rowrange_
}

itcl::body FitsFile::callHistogram { extNum_ fillFlag_ } {
# Ziqin
#     set fT [openTable $extNum_ ]
#     return [$fT callHistogramDirectly $extNum_  $fillFlag_ ]
    set fE [openExtension $extNum_ ]
    return [$fE callHistogram $extNum_  $fillFlag_ ] 
}

itcl::body FitsFile::makeHistogram {args} {
   eval $fitsfile histogram $args
}

itcl::body FitsFile::loadImageMeanCols {fCol_ lCol slice_} {
    $fitsfile load icols $fCol_ $lCol $slice_
}

itcl::body FitsFile::loadImageMeanRows {fRow_ lRow_ slice_} {
    $fitsfile load irows $fRow_ $lRow_ $slice_
}

itcl::body FitsFile::plotHisto { } {
    global g_histoPlotId powPlotParam
    global xCount yCount
    global powWCSList powWCSLabel powWCSName


    _openFitsFile

# if not a 1D histogram abort.

   if { [catch {set xval [getKeyword "CRVAL1"]} ] } {_closeFitsFile; return}
   if { [catch {set xpix [getKeyword "CRPIX1"]} ] } {_closeFitsFile; return}
   if { [catch {set xbin [getKeyword "CDELT1"]} ] } {_closeFitsFile; return}

   set imgDim [getImgInfo]
   set numX [lindex $imgDim 0]
   if { [llength $imgDim]>2 \
	 || ([llength $imgDim] == 2 && [lindex $imgDim 1] != 1) } {_closeFitsFile; return}
# 

   set xval [lindex [lindex $xval 0] 1]
   set xpix [lindex [lindex $xpix 0] 1]
   set xbin [lindex [lindex $xbin 0] 1]

   set xval [expr $xval + (1.0-$xpix)*$xbin]

    incr g_histoPlotId
    set graphID 1D_Histogram_${g_histoPlotId}
    set dataInfo [loadImageBlock 1 1 1 $numX 1]
    set dataAddressForPOW    [lindex $dataInfo 0]
    set dataType   [lindex $dataInfo 1]
    set dataLength [lindex $dataInfo 2]

    if { [winfo exist .pow.pow] != 1 } { 
       powInit .dummy
    }
    powCreateVectorEN x_$graphID x_$graphID $dataLength $xval $xbin NULL
    powCreateData y_d_$graphID $dataAddressForPOW $dataType $dataLength 1
    freeImage $dataAddressForPOW
    powCreateVector y_$graphID y_d_$graphID 0 $dataLength 1

    if { [catch {set xunt [getKeyword "CUNIT1"]} ] } {
       set xUnit Pixels
    } else {
       set xUnit [lindex [lindex $xunt 0] 1]
       set xUnit [string trim $xUnit { '}]
    }

    if { [catch {set xlbl [getKeyword "CTYPE1"]} ] } {
       set xLabel Pixels
    } else {
       set xLabel [lindex [lindex $xlbl 0] 1]
       set xLabel [string trim $xLabel { '}]
       set dsh [string first - $xLabel]
       if { $dsh > 0 } {
          incr dsh -1
          set xLabel [string range $xLabel 0 $dsh]
       }
    }

    set yUnit  NULL
    set yLabel Counts

    set powWCSLabel(xlabel,$imgHandle,DEFAULT) $xLabel
    set powWCSLabel(ylabel,$imgHandle,DEFAULT) $yLabel
    set powWCSLabel(xunit,$imgHandle,DEFAULT) $xUnit
    set powWCSLabel(yunit,$imgHandle,DEFAULT) $yUnit

    set powWCSName($graphID) 0
    set powWCSName(${graphID}scope) 0
    set powWCS($graphID) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
    set powFitsHeader($graphID) ""
    set powFitsHeaderCnt($graphID) 0

    powCreateCurve $graphID x_$graphID NULL y_$graphID NULL NULL NULL

    # Draw as a histogram
    powSetCurveOptions $graphID $graphID lStep Yes lDisp Yes

    set powPlotParam(graphType,$graphID) [string tolower [lindex [getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${graphID}scope) $powPlotParam(graphType,$graphID)
    set powPlotParam(zoomed,$graphID) 0
    set powPlotParam(zoomed,${graphID}scope) 0

    set xCount($graphID) 0
    set yCount($graphID) 0
    set xCount(${graphID}scope) 0
    set yCount(${graphID}scope) 0

    powCreateGraph $graphID $graphID NULL $xUnit $yUnit \
	$xLabel $yLabel \
	[lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1]  

    set powWCSList($graphID) {}
    lappend powWCSList($graphID) 1
    lappend powWCSList($graphID) {}

    _closeFitsFile
}


itcl::body FitsFile::_appendNewHDU {} {
    if { [winfo exists .newhdu] } {
        itcl::delete object .newhdu
    }
    NewExtension .newhdu [getBackup] -title "fv: New HDU"

    tkwait window .newhdu

    _redrawHighLight
    changeFile
}

itcl::body FitsFile::updateChecksum { {forceUpdate_ 0} } {

   
   if { [isFileChanged] && ! $forceUpdate_} {
      _isOpen
      switch $fvPref::ifAutoUpdateChecksum {
	 0 {return}
	 1 {
	    if { [$fitsfile checksum verify]<0 } { 
	       set forceUpdate_ 1
	    }
	 }
	 2 {set forceUpdate_ 1} 
      }
   }
   if { $forceUpdate_ } {
      $fitsfile checksum update
      changeFile
   }
}

itcl::body FitsFile::verifyChecksum {} {
    _isOpen
# return 1 checksum ok
#        0 checksum keyword not there
#       -1 dooh!
    $fitsfile checksum verify
}


#######################################
#
#          Open extensions
#
#######################################


itcl::body FitsFile::openHeader { extNum_ } {
   set fE [openExtension $extNum_]
   return [$fE dispHeader]
}

## when (the left) mouse button is pushed, only extNum_ is passed in
##  these extra args are called from somewhere else
#
itcl::body FitsFile::openTable { extNum_ {coor_ {}} {newTable_ {}} } {


   if { $newTable_ == "" } {
# standard case
# _openExtraTable is 1 if and only if third mouse button was pressed
      set newTable_ $_openExtraTable
   }
   set _openExtraTable 0

   set fE [openExtension $extNum_]
# when clicking on a POW image, $coor_ is the image pixels clicked on
   return [$fE dispTable $extNum_ $coor_ $newTable_]
}
itcl::body FitsFile::plotData { extNum_ {xyNames_ {}} } {
#    set fT [openTable $extNum_]
#    $fT scplotCmd $xyNames_

   set fE [openExtension $extNum_]
#   Ziqin
#   $fE dispTable "" "" 
   $fE plotData $xyNames_
}	

itcl::body FitsFile::plotData1 { extNum_ {xyNames_ {}} } {
    set fT [openTable $extNum_]
    $fT scplotCmd $xyNames_

##   set fE [openExtension $extNum_]
#   Ziqin
#   $fE dispTable "" "" 
##   $fE plotData $xyNames_
}	

itcl::body FitsFile::deleteExt { extNum_ } {

    incr extNum_



    if { ($extNum_ < 2) || ($extNum_ > $_numExts) } {
      error "Extension num is invalid"
    } else {
      for {set i 1} { $i <= $_numExts } { incr i} {
         set _extCheck($i) 0
      }
      set _extCheck($extNum_) 1
    }
    _deleteHDUs
}




itcl::body FitsFile::checkXYCol { idx } {
    set token [split $_extData($idx,colList) " "]

    set xPos [lsearch -exact [string tolower $token] "x"]
    set yPos [lsearch -exact [string tolower $token] "y"]

    if { $xPos >= 0 && $xPos >= 0 } {
       return true
    } else {
       return false
    }
}

itcl::body FitsFile::displaySlice { extNum_ slice } {
    set fE [openExtension $extNum_ ]
    return [$fE displaySlice $extNum_ $slice]
}

itcl::body FitsFile::skipTable { extNum_ {coor_ {}} } {
    set fE [openExtension $extNum_ ]
    return [$fE skipTable $coor_ ]
}	

##  if the FitsExtension hasn't been created, create it
##  if it already has been create, simply return it
#
itcl::body FitsFile::openExtension { extNum_ } {
   if { !$_isOriginalFile } {
      return [$_origFileObj openExtension $extNum_]
   }
   set pos [lsearch $_myExtNums $extNum_]
   if { $pos == -1 } {
      set fE [FitsExtension #auto $_fileMode $extNum_ $filename $this]
   } else {
      set fE [lindex $_myExtensionChildren $pos]
   }
   return $fE
}


itcl::body FitsFile::closeExtension { extNum_ } {
   if { !$_isOriginalFile } {
      $_origFileObj closeExtension $extNum_
   } else {
      if { $extNum_ < 0 } {
         # Close ALL extensions
         foreach fE $_myExtensionChildren {
            $fE closeCmd
         }
      } else {
         set pos [lsearch $_myExtNums $extNum_]
         if { $pos != -1 } {
            set fE [lindex $_myExtensionChildren $pos]
            $fE closeCmd
         }
      }
   }
}


itcl::body FitsFile::_setNewTable { flag_ } {
   set _openExtraTable $flag_
}

itcl::body FitsFile::addChild { child_ extNum_ } {
   set pos [lsearch $_myExtNums $extNum_]
   if { $pos == -1 } {
      lappend _myExtensionChildren $child_
      lappend _myExtNums  $extNum_
   } else {
      puts "$extNum_ already exists: [lindex $_myExtensionChildren $pos]"
   }
}

itcl::body FitsFile::freeChild { extNum_ } {
   set pos [lsearch $_myExtNums $extNum_]
   if { $pos == -1 } {
      puts "[expr $extNum_ - 1]  does not exist"
   } else {
      set _myExtensionChildren [lreplace $_myExtensionChildren $pos $pos]
      set _myExtNums  [lreplace $_myExtNums $pos $pos]
   }
}

itcl::body FitsFile::translateKeyWords { colName rowNum dest tempFile } {
   # wrapper to call fits_copy_cell2image in cfitsio
   # return new header string and wcs info
   return [$fitsfile get translatedKeywords $colName $rowNum $dest $tempFile]
}

itcl::body FitsFile::getHeader2String {} {
   # wrapper to call ffhdr2str in cfitsio
   return [$fitsfile get header2str]
}

itcl::body FitsFile::getDummyHeader2String {{dest {}} RAColNum_ DecColNum_} {
   # wrapper to call to get dummy
   return [$fitsfile get dummy2str $dest $RAColNum_ $DecColNum_]
}

itcl::body FitsFile::getHeaderKeyWord { str img } {
   global powHeaderWcsKeyWord powWCSInfo powWCSToken powWCS

   if [info exists powHeaderWcsKeyWord($img,DEFAULT)] {

      foreach letter [list a b c d e f g h i j k l m n o p q r s t u v w x y z] {
         if [info exists powHeaderWcsKeyWord($img,$letter)] {
            unset powHeaderWcsKeyWord($img,$letter)
         }
      }
      if [info exists powHeaderWcsKeyWord($img,NONE)] {
         unset powHeaderWcsKeyWord($img,NONE)
      }
      if [info exists powHeaderWcsKeyWord($img,END)] {
         unset powHeaderWcsKeyWord($img,END)
      }
      if [info exists powHeaderWcsKeyWord($img,DEFAULT)] {
         unset powHeaderWcsKeyWord($img,DEFAULT)
      }
      unset powHeaderWcsKeyWord
   }

   set i 0
   set numCard 0
   set numCoord 0
   set numCardInCoord 0
   set powWCSToken($img) { DEFAULT }
   while { 1 } {
      set currentStr [string range $str $i [expr $i + 79]]
      incr i 80
      if { [string trim $currentStr] == "" } {
         if { $i > [string length $str] } break
         continue
      }
      set currentStrToken [split $currentStr "="]
      set header [string trim [lindex $currentStrToken 0]]
      incr numCard

      if { [llength $currentStrToken] == 2 } {

         switch -regexp -- $header {
             {CTYPE[0-9][A-Z]?} -
             {[0-9]CTYP[0-9][A-Z]?} -
             {CUNIT[0-9][A-Z]?} -
             {[0-9]CUNI[0-9][A-Z]?} -
             {CRVAL[0-9][A-Z]?} -
             {CRPIX[0-9][A-Z]?} -
             {CD[0-9][_][0-9][A-Z]?} -
             {CDELT[0-9][A-Z]?} -
             {[0-9]CDLT[0-9][A-Z]?} -
             {CROTA[0-9][A-Z]?} -
             {TCTYP[0-9][A-Z]?} -
             {TCUNI[0-9][A-Z]?} -
             {TCRVL[0-9][A-Z]?} -
             {TCRPX[0-9][A-Z]?} -
             {TCDLT[0-9][A-Z]?} -
             {TCD[0-9][A-Z]?} -
             {TCROT[0-9][A-Z]?} -
             {OFFSET[0-9][A-Z]?} {
                incr numCardInCoord
                set lastChar [string toupper [string range $header end end]]
                regsub {[A-Z]} $lastChar {} testChar
                if { $testChar == "" } {
                   set headerLength [string length [lindex $currentStrToken 0]]
                   set newHeader [string range $header 0 [expr [string length $header] - 2]]
                   set newStr [format "%-${headerLength}s=%s" $newHeader \
                                                                [lindex $currentStrToken 1]]
                   if { ![info exists powHeaderWcsKeyWord] || \
                        ![info exists powHeaderWcsKeyWord($img,$lastChar)] } {
                      incr numCoord
                      set powHeaderWcsKeyWord($img,$lastChar) $newStr
                      set powWCSInfo($img,$lastChar) [getWcs $lastChar]
                      lappend powWCSToken($img) $lastChar
                   } else {
                      set powHeaderWcsKeyWord($img,$lastChar) \
                          [format "%s%s" $powHeaderWcsKeyWord($img,$lastChar) $newStr]
                   }
                } else {
                   if { ![info exists powHeaderWcsKeyWord] || \
                        ![info exists powHeaderWcsKeyWord($img,DEFAULT)] } {
                      incr numCoord
                      set powWCSInfo($img,DEFAULT) $powWCS($img)
                      set powHeaderWcsKeyWord($img,DEFAULT) $currentStr
                   } else {
                      set powHeaderWcsKeyWord($img,DEFAULT) \
                          [format "%s%s" $powHeaderWcsKeyWord($img,DEFAULT) $currentStr]
                   }
                }
             }
             default {
                if { ![info exists powHeaderWcsKeyWord] || \
                     ![info exists powHeaderWcsKeyWord($img,NONE)] } {
                   set powHeaderWcsKeyWord($img,NONE) $currentStr
                } else {
                   set powHeaderWcsKeyWord($img,NONE) \
                       [format "%s%s" $powHeaderWcsKeyWord($img,NONE) $currentStr]
                }
             }
         }
      } else {
         switch -glob -- $header {
             "END*" {
                set powHeaderWcsKeyWord($img,END) $currentStr
             }
             default {
                if { ![info exists powHeaderWcsKeyWord] || \
                     ![info exists powHeaderWcsKeyWord($img,NONE)] } {
                   set powHeaderWcsKeyWord($img,NONE) $currentStr
                } else {
                   set powHeaderWcsKeyWord($img,NONE) \
                       [format "%s%s" $powHeaderWcsKeyWord($img,NONE) $currentStr]
                }
             }
         }
      }
      if { $i > [string length $str] } break
   }

   if { $numCoord > 0 } {
      set numCardPerCoord [expr $numCardInCoord / $numCoord]

      return [list $numCard $numCard]
   } else {
      return [list 0 $numCard]
   }
}

itcl::body FitsFile::assembleWcsLabel { img {selection "DEFAULT"} } {
    global powWCSLabel

    if { $selection == " " } return
    set powWCSLabel(xlabel,$img,$selection) ""
    set powWCSLabel(ylabel,$img,$selection) ""
    set powWCSLabel(xunit,$img,$selection) ""
    set powWCSLabel(yunit,$img,$selection) ""

    set x_label ""
    set y_label ""
    set x_unit "pixels"
    set y_unit "pixels"
    if { ![catch {set tmp [getKeyword CTYPE1$selection]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set x_label [string trim $v {' }]
       set powWCSLabel(xlabel,$img,$selection) $x_label
    }
    if { ![catch {set tmp [getKeyword CTYPE2$selection]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set y_label [string trim $v {' }]
       set powWCSLabel(ylabel,$img,$selection) $y_label
    }
    if { ![catch {set tmp [getKeyword CUNIT1$selection]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set x_unit [string trim $v {' }]
       set powWCSLabel(xunit,$img,$selection) $x_unit
    }
    if { ![catch {set tmp [getKeyword CUNIT2$selection]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set y_unit [string trim $v {' }]
       set powWCSLabel(yunit,$img,$selection) $y_unit
    }

    set z_label "counts"
    if { ![catch {set tmp [getKeyword BUNIT$selection]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set z_label [string trim $v {' }]
    }
    set powWCSLabel(zlabel,$img,$selection) $z_label
}

itcl::body FitsFile::assembleWcsHeader { img {selection "DEFAULT"} } {
   global powHeaderWcsKeyWord 

   # regular header
   if { $selection == "NOWCS" } {
      return [format "%s%s" $powHeaderWcsKeyWord($img,NONE) \
                            $powHeaderWcsKeyWord($img,END)]
   } else {
headerDebugDataPrint "NONE STRING" $powHeaderWcsKeyWord($img,NONE)
headerDebugDataPrint "selection: $selection" $powHeaderWcsKeyWord($img,$selection)
headerDebugDataPrint "END STRING" $powHeaderWcsKeyWord($img,END)
      return [format "%s%s%s" $powHeaderWcsKeyWord($img,NONE) \
                              $powHeaderWcsKeyWord($img,$selection) \
                              $powHeaderWcsKeyWord($img,END)]
   }
}

itcl::body FitsFile::headerDebugDataPrint { title string } {
     puts "$title"
     set k 0
     for {set i 0} {$i < [string length $string]} {incr i 80} {
        set currentStr [string range $string $i [expr $i + 79]]
        puts "<$currentStr>"
        incr k
     }
     puts "count: $k"
}

