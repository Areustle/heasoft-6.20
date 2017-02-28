proc _getDataForAxis { filehandle axis_ axisExpression_ whichRows_ } {
    if { $axisExpression_ == "RowNumber" } {
        set axisExpression_ "#ROW"
        set dataType 41
        set nelem [range count $whichRows_ $_numRows]
        set dim 1
    } else {
        if { [catch {set axis_info [$filehandle info expr $axisExpression_]} err] } {
            error "Cannot plot expression for $axis_.\n\n$err"
        }
        set dataType [lindex $axis_info 0]
        set nelem    [lindex $axis_info 1]
        set dim      [lindex $axis_info 2]

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
    set dataInfoForPOW [$filehandle load expr -rows $whichRows_ $axisExpression_ NULL]
    set dataPtr     [lindex $dataInfoForPOW 0]
    set dataType    [lindex $dataInfoForPOW 1]
    set numElements [lindex $dataInfoForPOW 2]

    return [list $dataPtr $dataType $numElements $nelem $dim]
}

proc powDebugDataPrint { title string } {
     puts "$title"
     set k 0
     for {set i 0} {$i < [string length $string]} {incr i 80} {
        set currentStr [string range $string $i [expr $i + 79]]
        puts "<$currentStr>"
        incr k
     }
     puts "count: $k"
}

proc assembleWcsLabel { filehandle img flag {selection "DEFAULT"} } {
    global powWCSLabel

    set target $selection
    if { $selection == " " || $selection == "DEFAULT" } {
       set target "DEFAULT"
       set selection ""
    }

    set powWCSLabel(xunit,$img,$target) ""
    set powWCSLabel(yunit,$img,$target) ""

    set x_label ""
    set y_label ""
    set x_unit "pixels"
    set y_unit "pixels"
    if { $flag == "image" } {
       set powWCSLabel(xlabel,$img,$target) ""
       set powWCSLabel(ylabel,$img,$target) ""
       if { ![catch {set tmp [getKeyword $filehandle CTYPE1$selection]}] } {
          set v [lindex [lindex $tmp 0] 1]
          set x_label [string trim $v {' }]
          set powWCSLabel(xlabel,$img,$target) $x_label
       }
       if { ![catch {set tmp [getKeyword $filehandle TTYPE1$selection]}] } {
          set v [lindex [lindex $tmp 0] 1]
          set x_label [string trim $v {' }]
          set powWCSLabel(xlabel,$img,$target) $x_label
       }
       if { ![catch {set tmp [getKeyword $filehandle CTYPE2$selection]}] } {
          set v [lindex [lindex $tmp 0] 1]
          set y_label [string trim $v {' }]
          set powWCSLabel(ylabel,$img,$target) $y_label
       }
       if { ![catch {set tmp [getKeyword $filehandle TTYPE2$selection]}] } {
          set v [lindex [lindex $tmp 0] 1]
          set y_label [string trim $v {' }]
          set powWCSLabel(ylabel,$img,$target) $y_label
       }
       if { ![catch {set tmp [getKeyword $filehandle CUNIT1$selection]}] } {
          set v [lindex [lindex $tmp 0] 1]
          set x_unit [string trim $v {' }]
          set powWCSLabel(xunit,$img,$target) $x_unit
       }
       if { ![catch {set tmp [getKeyword $filehandle TUNIT1$selection]}] } {
          set v [lindex [lindex $tmp 0] 1]
          set x_unit [string trim $v {' }]
          set powWCSLabel(xunit,$img,$target) $x_unit
       }
       if { ![catch {set tmp [getKeyword $filehandle CUNIT2$selection]}] } {
          set v [lindex [lindex $tmp 0] 1]
          set y_unit [string trim $v {' }]
          set powWCSLabel(yunit,$img,$target) $y_unit
       }
       if { ![catch {set tmp [getKeyword $filehandle TUNIT2$selection]}] } {
          set v [lindex [lindex $tmp 0] 1]
          set y_unit [string trim $v {' }]
          set powWCSLabel(yunit,$img,$target) $y_unit
       }
    } else {
       set i 0
       set findUnit 0
       while (1) {
          incr i
          if { ![catch {set tmp [getKeyword $filehandle CTYPE${i}$selection]}] } {
             set v [lindex [lindex $tmp 0] 1]
             set label [string trim $v {' }]
             if { $label == $powWCSLabel(xlabel,$img,$target) } {
                set powWCSLabel(xunit,$img,$target) "counts"
                if { ![catch { set tmp [getKeyword $filehandle CUNIT${i}$selection]}] } {
                   set v [lindex [lindex $tmp 0] 1]
                   set x_unit [string trim $v {' }]
                   set powWCSLabel(xunit,$img,$target) $x_unit
                }
                incr findUnit
             } elseif { $label == $powWCSLabel(ylabel,$img,$target) } {
                set powWCSLabel(yunit,$img,$target) "counts"
                if { ![catch { set tmp [getKeyword $filehandle CUNIT${i}$selection]}] } {
                   set v [lindex [lindex $tmp 0] 1]
                   set y_unit [string trim $v {' }]
                   set powWCSLabel(yunit,$img,$target) $y_unit
                }
                incr findUnit
             }
             if { $findUnit >= 2 } break
          } elseif { ![catch {set tmp [getKeyword $filehandle TTYPE${i}$selection]}] } {
             set v [lindex [lindex $tmp 0] 1]
             set label [string trim $v {' }]
             if { $label == $powWCSLabel(xlabel,$img,$target) } {
                set powWCSLabel(xunit,$img,$target) "counts"
                if { ![catch { set tmp [getKeyword $filehandle TUNIT${i}$selection]}] } {
                   set v [lindex [lindex $tmp 0] 1]
                   set x_unit [string trim $v {' }]
                   set powWCSLabel(xunit,$img,$target) $x_unit
                }
                incr findUnit
             } elseif { $label == $powWCSLabel(ylabel,$img,$target) } {
                set powWCSLabel(yunit,$img,$target) "counts"
                if { ![catch { set tmp [getKeyword $filehandle TUNIT${i}$selection]}] } {
                   set v [lindex [lindex $tmp 0] 1]
                   set y_unit [string trim $v {' }]
                   set powWCSLabel(yunit,$img,$target) $y_unit
                }
                incr findUnit
             }
             if { $findUnit >= 2 } break
          } else {
             break
          }
       }
       if { $findUnit < 2 } {
          puts "not enough information to plot"
          exit
       } 
    }

    set z_label "counts"
    if { ![catch {set tmp [getKeyword $filehandle BUNIT$selection]}] } {
       set v [lindex [lindex $tmp 0] 1]
       set z_label [string trim $v {' }]
    }
    set powWCSLabel(zlabel,$img,$target) $z_label
}

proc getWcs {{dest {}} {RAColNum_ {}} {DecColNum_ {}} } {
    global filehandle
    if { $RAColNum_ == "" || $DecColNum_ == "" } {
       if { [catch {set wcs [$filehandle get wcs -m $dest]}] } {
          return ""
       } else {
          return $wcs
       }
    } else {
        set wcs [$filehandle get wcs -m $dest $RAColNum_ $DecColNum_]
        return $wcs
    }

}

proc rotationSend { str img } {
   global powHeaderWcsKeyWord powWCSInfo powWCSToken powWCS

   set rotationRate 80
   set i 0
   while { 1 } {
      set currentStr [string range $str $i [expr $i + [expr $rotationRate - 1]]]
      incr i $rotationRate
      if { [string trim $currentStr] == "" } {
         if { $i > [string length $str] } break
         continue
      }
      ::powCmds::wcsHeader $img $rotationRate $currentStr continue
   }
}

proc getHeaderKeyWord { str img } {
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
             {CUNIT[0-9][A-Z]?} -
             {CRVAL[0-9][A-Z]?} -
             {CRPIX[0-9][A-Z]?} -
             {CD[0-9][_][0-9][A-Z]?} -
             {CDELT[0-9][A-Z]?} -
             {CROTA[0-9][A-Z]?} -
             {TTYPE[0-9][A-Z]?} -
             {TUNIT[0-9][A-Z]?} -
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

proc assembleWcsHeader { img {selection "DEFAULT"} } {
   global powHeaderWcsKeyWord

   # regular header
   if { $selection == "NOWCS" } {
      return [format "%s%s" $powHeaderWcsKeyWord($img,NONE) \
                            $powHeaderWcsKeyWord($img,END)]
   } else {
      return [format "%s%s%s" $powHeaderWcsKeyWord($img,NONE) \
                              $powHeaderWcsKeyWord($img,$selection) \
                              $powHeaderWcsKeyWord($img,END)]
   }
}

proc getKeyword {filehandle keyword} {
    # get $keyword
    # _isOpen
    $filehandle get keyword ^$keyword\$
}

set helpMsg "Usage: POWplot ?-cmap code? ?-display client? ?fitsimage?
 code: 0 Chose the best colormap
       1 Force to install private pseudo colormap
       2 Force to use truecolor colormap (if don't have it will crash!) 
       3 Force to use screen default colormap
 client: XPA entry point of form IP_Address:portNumber, pointing to
         a remote POW session to which plot commands should be sent"


global g_backupDir tcl_platform
global powFitsHeaderCnt powFitsHeader 
global powHeaderWcsKeyWord powWCSInfo powWCSToken powWCS
global powWCSName powWCSList powWCSLabel
global filehandle

if { $tcl_platform(platform) == "windows" } {
   set fvTmp "fv_tmp"
} elseif { $tcl_platform(platform) == "macintosh" } {
   set fvTmp "fv_temp_folder"
} else {
   set fvTmp ".fvtmp"
}

set g_backupDir $env(HOME)/$fvTmp

if ![file exists $g_backupDir] {
   file mkdir $g_backupDir
   file attributes $g_backupDir -permissions 00775
}

set ppFitsDataType(8) 0
set ppFitsDataType(16) 1
set ppFitsDataType(32) 2
set ppFitsDataType(-32) 3
set ppFitsDataType(-64) 4

set instPos [lsearch $argv "-cmap"]
set colorCode 0

# if install colormap
if { $instPos != -1} {
    if { [catch {set colorCode [lindex $argv [expr $instPos+1]]}] == 1} {
	puts $helpMsg
	exit
    }
    if { ($colorCode != 0) && ($colorCode != 1) && ($colorCode != 2) \
	&& ($colorCode != 3) } {
	puts $helpMsg
	exit
    }	
    set argv [lreplace $argv $instPos [expr $instPos+1]]
    incr argc -2 
}

wm withdraw .
update idletask

set plotFlag [lsearch $argv "-plot"]
if { $plotFlag >= 0 } {
   set argv [lreplace $argv $plotFlag $plotFlag]
   incr argc
   set plotFlag true
} else {
   set plotFlag false
}

#
# Load libraries
#

set POWLIB "$env(POW_LIBRARY)/.."
set env(POW_HELPDIR) $env(POW_LIBRARY)

package require Itcl
package require Itk
package require Iwidgets

load [glob $POWLIB/libfitstcl.{s\[ol\]*,dylib}]
load [glob $POWLIB/libpow.{s\[ol\]*,dylib}]

# Look for client/server flag

set instPos [lsearch $argv "-display"]
if { $instPos != -1 } {
   set client [lindex $argv [expr $instPos+1]]
   set argv [lreplace $argv $instPos [expr $instPos+1]]
   incr argc -2
   ::powCmds::remote $client
} else {
   # None specified, so ask POW for default value
   set client [::powCmds::remote]
}

if ![info exists g_titleFont] {
    if { $tcl_platform(platform) == "windows" } {
       set isWin 1
       set isMac 0
       font create g_titleFont -family Arial             -size -12
       font create g_notebookTitleFont -family Arial     -size -14
    } elseif { $tcl_platform(platform) == "macintosh" } {
       set isWin 0
       set isMac 1
       font create g_titleFont -family system    -size -12
       font create g_notebookTitleFont -family system -size -14
    } else {
       set isWin 0
       set isMac 0
       font create g_titleFont -family Helvetica -size -12 -weight bold
       font create g_notebookTitleFont -family Helvetica -size -14 -weight bold
    }

    font create g_entryFont -family Courier -size -12
    set g_charPix      [font measure g_entryFont m]

}

set globalBgColor        #cccccc
set globalFgColor        black
set activeBgColor        #eeeeee
set activeFgColor        black
set checkBBgColor        #ff3366

option add *Background          $globalBgColor
option add *Foreground          $globalFgColor
option add *HighlightBackground $globalBgColor
option add *activeForeground    $activeFgColor
option add *activeBackground    $activeBgColor
option add *selectForeground    $activeFgColor
option add *selectBackground    $activeBgColor
option add *selectColor         $checkBBgColor

::powCmds::init 30 $colorCode

if { $plotFlag == true } {
   if { $argc < 5 } {
      tk_messageBox -icon error -type ok \
                    -message "usage: POWplot -plot <file> <ext> <xCol> <yCol> ?<range>?"
      exit
   }
   set filename  [lindex $argv 0]
   set extension [lindex $argv 1]
   set xColumn_  [lindex $argv 2]
   set yColumn_  [lindex $argv 3]

   set range ""
   if { $argc == 7 } {
      set token [split [lindex $argv 6] "-"]
      if { $token != 2 } {
         tk_messageBox -icon error -type ok \
                       -message "Range specified in format:\n\nstart-end"
         exit
      }
      set range [lindex $argv 6]
   }

   regsub -all {[^a-zA-Z0-9.]} [file tail $filename] "_" gname
   set filehandle [fits open $filename 0]

   # find extension
   while (1) {
     set errorFlag [ catch {
        $filehandle move +1
     } err ]

     if { $errorFlag } {
        break
     } else {
        set currentHDU [split [$filehandle get keyword EXTNAME] " "]
        set checkExt [string tolower [string trim [lindex $currentHDU 1] "{'}"]]
        if { $checkExt == [string tolower $extension] } {
           break
        }
     }
   }

   set wcsinfo [getWcs]
   if { [lindex $wcsinfo 4] != "none" } {
      set x_label [lindex [lindex $wcsinfo 3] 0]
      set y_label [lindex [lindex $wcsinfo 3] 1]
   }

   set x_unit  "counts"
   set y_unit  "counts"

   set _numRows [$filehandle info nrows]
   if { $range == "" } {
      set range [format "1-%s" $_numRows]
   }
   
   set graphName ${gname}_plot
   set curveName ${gname}_curve

   set result [$filehandle get header2str]
   set cntList($graphName) [getHeaderKeyWord [lindex $result 0] $graphName]
   set powFitsHeaderCnt($graphName) [lindex $cntList($graphName) 1]
   set powFitsHeaderCnt($curveName) [lindex $cntList($graphName) 1]
   set powWCSName($graphName) 0
   set powWCSName(${graphName}scope) 0
   set powWCSName($curveName) 0
   set powWCSName(${curveName}scope) 0

   if { [lindex $cntList($graphName) 0] > 0 } {
      set powFitsHeader($graphName) [lindex $result 0]
      set powFitsHeader($curveName) [lindex $result 0]
   } else {
      set powFitsHeader($graphName) ""
      set powFitsHeader($curveName) ""
   }

   set currentStr $powFitsHeader($graphName)
   ::powCmds::wcs $graphName $wcsinfo
   ::powCmds::wcsHeader $graphName 0 NONE start

   set idx 0
   set headerLen [string length $currentStr]
   if { $idx + $headerLen <= 3000 } {
      ::powCmds::wcsHeader $graphName $headerLen $currentStr continue
   } else {
      rotationSend [lindex $result 0] $graphName
   }
   ::powCmds::wcsHeader $graphName 0 NONE done
   ::powCmds::wcsHeaderCnt $graphName $powFitsHeaderCnt($graphName)

   set currentStr $powFitsHeader($curveName)
   ::powCmds::wcs $curveName $wcsinfo
   ::powCmds::wcsHeader $curveName 0 NONE start

   set idx 0
   set headerLen [string length $currentStr]
   if { $idx + $headerLen <= 3000 } {
      ::powCmds::wcsHeader $curveName $headerLen $currentStr continue
   } else {
      rotationSend [lindex $result 0] $curveName
   }
   ::powCmds::wcsHeader $curveName 0 NONE done
   ::powCmds::wcsHeaderCnt $curveName $powFitsHeaderCnt($curveName)


   if ![info exists powWCSList($graphName)] {
      set powWCSList($graphName) {}
      lappend powWCSList($graphName) 1
      lappend powWCSList($graphName) DEFAULT
   }

   foreach name [lindex $powWCSList($graphName) 1] {
      # plot is against columns input
      set powWCSLabel(xlabel,$graphName,$name) $xColumn_
      set powWCSLabel(ylabel,$graphName,$name) $yColumn_

      assembleWcsLabel $filehandle $graphName "plot" $name

      ::powCmds::wcsLabel $graphName "xlabel" $name $powWCSLabel(xlabel,$graphName,$name)
      ::powCmds::wcsLabel $graphName "ylabel" $name $powWCSLabel(ylabel,$graphName,$name)
      ::powCmds::wcsLabel $graphName "xunit" $name $powWCSLabel(xunit,$graphName,$name)
      ::powCmds::wcsLabel $graphName "yunit" $name $powWCSLabel(yunit,$graphName,$name)

      ::powCmds::wcsLabel $graphName "zlabel" $name $powWCSLabel(zlabel,$graphName,$name)
      ::powCmds::wcsLabel $curveName "xlabel" $name $powWCSLabel(xlabel,$graphName,$name)
      ::powCmds::wcsLabel $curveName "ylabel" $name $powWCSLabel(ylabel,$graphName,$name)
      ::powCmds::wcsLabel $curveName "xunit" $name $powWCSLabel(xunit,$graphName,$name)
      ::powCmds::wcsLabel $curveName "yunit" $name $powWCSLabel(yunit,$graphName,$name)
      ::powCmds::wcsLabel $curveName "zlabel" $name $powWCSLabel(zlabel,$graphName,$name)
   }

   set x_unit  $powWCSLabel(xunit,$graphName,DEFAULT)
   set y_unit  $powWCSLabel(yunit,$graphName,DEFAULT)

   set powWCSList(${graphName}scope) $powWCSList($graphName)
   set powWCSList(${curveName}scope) $powWCSList($graphName)

   # value
   set wcsliststr "[lindex $powWCSList($graphName) 0]"

   foreach name [lindex $powWCSList($graphName) 1] {
       set wcsliststr [format "%s|%s" $wcsliststr $name]
   }

   ::powCmds::wcsSetList $graphName $wcsliststr
   ::powCmds::wcsSetList $curveName $wcsliststr

   set data [_getDataForAxis $filehandle "x" $xColumn_ $range]
   set xcol_data [ptr2lst [lindex $data 0] [lindex $data 1] [lindex $data 2]]

   set data [_getDataForAxis $filehandle "y" $yColumn_ $range]
   set ycol_data [ptr2lst [lindex $data 0] [lindex $data 1] [lindex $data 2]]

   #::powCmds::create data xdataName $xcol_data
   #::powCmds::create data ydataName $ycol_data

   set sendBlockSize 200

   ::powCmds::create data xdataName NONE Start
   if { [llength $xcol_data] <= $sendBlockSize } {
      ::powCmds::create data xdataName $xcol_data continue
   } else {
      while { [llength $xcol_data] > 0 } {
          set currentBatch [lrange $xcol_data 0 [expr $sendBlockSize - 1]]
          set xcol_data [lreplace $xcol_data 0 [expr $sendBlockSize - 1]]
          ::powCmds::create data xdataName $currentBatch continue
      }
   }
   ::powCmds::create data xdataName NONE Done

   ::powCmds::create data ydataName NONE Start
   if { [llength $ycol_data] <= $sendBlockSize } {
      ::powCmds::create data ydataName $ycol_data continue
   } else {
      while { [llength $ycol_data] > 0 } {
          set currentBatch [lrange $ycol_data 0 [expr $sendBlockSize - 1]]
          set ycol_data [lreplace $ycol_data 0 [expr $sendBlockSize - 1]]
          ::powCmds::create data ydataName $currentBatch continue
      }
   }
   ::powCmds::create data ydataName NONE Done

   ::powCmds::create curve $curveName xdataName ydataName
   ::powCmds::create graph $graphName $curveName NULL
   ::powCmds::size 300 300

   ::powCmds::graph -name $graphName \
            xlabel $xColumn_ \
            ylabel $yColumn_ \
            xunits $x_unit \
            yunits $y_unit

   ::powCmds::curve -name $curveName pDisp No lDisp Yes
   ::powCmds::select graph $graphName
   
   $filehandle close

} else {
   foreach filename $argv {
      regsub -all {[^a-zA-Z0-9.]} [file tail $filename] "_" gname
      set filehandle [fits open $filename 0]
      set imghandle [$filehandle load image]
      set dims [$filehandle info imgdim]
      set n1 [lindex $dims 0]
      if { [llength $dims]==1 } {
         set n2 1
      } else {
         set n2 [lindex $dims 1]
      }
      set data_type [lindex [lindex [$filehandle get keyword BITPIX] 0] 1]
      set data_type $ppFitsDataType($data_type)
      if { ([catch {$filehandle get keyword BZERO}] == 0) ||
           ([catch {$filehandle get keyword BSCALE}] == 0) } {
          set data_type 4
      }
     
     #    powCreateData ${gname}_data $imghandle $data_type [expr $n1 * $n2] 0
     
      set lstData [list $imghandle $data_type [list $n1 $n2] ]
      ::powCmds::array $lstData ${gname}_data PTR $tcl_platform(byteOrder)
     
      # Try to get WCS (or scaling) information
     
      set x0 1
      set y0 1
      set xinc 1
      set yinc 1
      set x_label ""
      set y_label ""
      set x_unit  "pixels"
      set y_unit  "pixels"
      set z_unit  "counts"
     
      set wcsinfo [getWcs]
      if { [lindex $wcsinfo 4] != "none" } {
         set x_label [lindex [lindex $wcsinfo 3] 0]
         set y_label [lindex [lindex $wcsinfo 3] 1]
         set x_unit "deg"
         set y_unit "deg"
      }
     
      set result [$filehandle get header2str]
      set cntList(${gname}_img) [getHeaderKeyWord [lindex $result 0] ${gname}_img]
      set powFitsHeaderCnt(${gname}_img) [lindex $cntList(${gname}_img) 1]
      set graphName ${gname}_img
      set powWCSName($graphName) 0
      set powWCSName(${graphName}scope) 0
     
      if { [lindex $cntList($graphName) 0] > 0 } {
         set powFitsHeader($graphName) [lindex $result 0]
      } else {
         set powFitsHeader($graphName) ""
      }
     
      set currentStr $powFitsHeader($graphName)
      ::powCmds::wcs $graphName $wcsinfo
      ::powCmds::wcsHeader $graphName 0 NONE start
     
      set idx 0
      set headerLen [string length $currentStr]
      if { $idx + $headerLen <= 3000 } {
         ::powCmds::wcsHeader $graphName $headerLen $currentStr continue
      } else {
         rotationSend [lindex $result 0] $graphName
      }
      ::powCmds::wcsHeader $graphName 0 NONE done
     
      ::powCmds::wcsHeaderCnt $graphName $powFitsHeaderCnt($graphName)
      ::powCmds::create image $graphName \
                              ${gname}_data $n1 $n2
     
      if [info exists powWCSList($graphName)] {
         foreach name [lindex $powWCSList($graphName) 1] {
            assembleWcsLabel $filehandle $graphName "image" $name
            if { $name == " " } {
               set name "DEFAULT"
            }
            ::powCmds::wcsLabel $graphName "xlabel" $name $powWCSLabel(xlabel,$graphName,$name)
            ::powCmds::wcsLabel $graphName "ylabel" $name $powWCSLabel(ylabel,$graphName,$name)
            ::powCmds::wcsLabel $graphName "xunit" $name $powWCSLabel(xunit,$graphName,$name)
            ::powCmds::wcsLabel $graphName "yunit" $name $powWCSLabel(yunit,$graphName,$name)
            ::powCmds::wcsLabel $graphName "zlabel" $name $powWCSLabel(zlabel,$graphName,$name)
         }
      } else {
         set powWCSList($graphName) {}
         lappend powWCSList($graphName) 1
         lappend powWCSList($graphName) {DEFAULT}
      }
   
      set powWCSList(${graphName}scope) $powWCSList($graphName)
   
      # value
      set wcsliststr "[lindex $powWCSList($graphName) 0]"
    
      foreach name [lindex $powWCSList($graphName) 1] {
         set wcsliststr [format "%s|%s" $wcsliststr $name]
      }
   
      ::powCmds::wcsSetList $graphName $wcsliststr
   
      ::powCmds::graph -name $graphName \
            xlabel $x_label \
            ylabel $y_label \
            xunits $x_unit \
            yunits $y_unit
   
      ::powCmds::create graph $graphName NULL $graphName
   
      $filehandle close
   }
}

bind . <<powExit>> {
    exit
}

if { $client != "" } {
   exit
}
