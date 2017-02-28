#
#  This file contains code which makes POW more scriptable either from
#  TCL or via XPA entry points
#

namespace eval powCmds {
   variable currcrv ""
   variable remoteServer ""
   variable mouseClicked ""

   proc helpPage { args } {
        variable remoteServer
        global g_showpow_flag powDWP g_backupDir

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer helpPage $str ]
        }

        set g_showpow_flag "noshow"

        if { ![winfo exists .pow] } {
           powInit .dummy
        }

        if { [string range $args [expr [string length $args] - 5] end] == ".html" } {
           set fileName $args
        } else {
           set fileName $g_backupDir/helpPage_[clock seconds].html
           set f [::open $fileName w+]
           set data [split [lindex $args 0] \n]
   
           foreach line $data {
              catch { puts $f $line } err
           }
   
           catch { ::close $f } err
        }
        catch { powHelp $fileName } err
   }

   proc getXRange { args } {
        global currgn powDWP
        global xRangeParam
        variable remoteServer

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer getXRange $str ]
        }

        if { ![info exist xRangeParam(rgns)] } return {}
        set theRgns [$xRangeParam(rgns) regions]
        set regionStr {}
        foreach rgn $theRgns {
           foreach [list sign shape descr] \
                   [$xRangeParam(rgns) buildRegionStr $rgn $xRangeParam(degreeFormat)]\
                   {}

           set descr "[format "%.15G" [lindex $descr 0]] [format "%.15G" [lindex $descr 2]]"
           lappend regionStr $descr
        }
        return $regionStr
   }

   proc getRegion { args } {
        global currgn regionParam powDWP
        variable remoteServer

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer getRegion $str ]
        }

        set propertyOrder [lindex $args 0]

        if { ![info exist regionParam(rgns)] } return {}
        set theRgns [$regionParam(rgns) regions]
        set regionStr {}
        foreach rgn $theRgns {
           if { [$rgn getPropertyOrder] == $propertyOrder } {
              foreach [list sign shape descr] \
                    [$regionParam(rgns) buildRegionStr $rgn $regionParam(degreeFormat)]\
                    {}
              set descr [format "%s%s%s" [string trim $sign] [string tolower $shape] ([join $descr {, }])]
              lappend regionStr $descr
           }
        }
        return $regionStr
   }

   proc regions { args } {
        variable remoteServer
        global currgn regionParam powDWP

        
        if { [llength $args] == 1 && [file exists [lindex $args 0]] } {
           set f [open [lindex $args 0] r]
           set str [read $f [file size [lindex $args 0]]]
           ::close $f 
        } else {
           set str ""
           for {set i 0} {$i < [llength $args]} {incr i} {
               set str [format "%s%s" $str [lindex $args $i]]
           }
        }
   
        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer regions $str ]
        }

        if { ![winfo exists ${powDWP}region] } {
           powRegion
        }

        if { ![info exists regionParam(rgns)] } {
           set regionParam(rgns) [gRegionList $currgn .pow.pow]
        }

        catch { $regionParam(rgns) readFromStr $str } err
        return Done
   }

   proc regionName { args } {
        global regionOutputFileName
        variable remoteServer

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer regionName $args]
        }

        set regionOutputFileName [lindex $args 0]
        powUpdateRegionTitle $regionOutputFileName
        return Done
   }

   proc setRegionFormat { args } {
        variable remoteServer
        global regionParam

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer regionName $args]
        }

        set plainformats [$regionParam(rgns) getPlainFormats]
        set allformats [$regionParam(rgns) getAllFormats]
        set idx [lsearch -exact $plainformats [lindex $args 0]] 
     
        set newFormat [lindex $args 0]
        if { $idx >= 0 } {
           set newFormat [lindex $allformats $idx]
        } 
        
        powChangeFormat $newFormat
        return Done
   }

   proc binFactorTool { args } {
        global binFactorSendFlag powDWP
        global defaultBinFactor

        variable remoteServer

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer binFactorTool $args]
        }

        if { [llength $args] == 1 } {
           switch -- [lindex $args 0] {
               "-wait" {
                  set token [split $binFactorSendFlag " "]
                  if { [lindex $token 0] != "NOT_YET" } {
                     set returnFlag $binFactorSendFlag
                     set binFactorSendFlag "NOT_YET 0"
                     return $returnFlag
                  }
                  return $binFactorSendFlag
               }
               "-open" {
                  powBinFactorSelect
                  set binFactorSendFlag "NOT_YET 0"
                  return Done
               }
               "-close" {
                  powApplyBinFactor DONE
                  catch { destroy ${powDWP}binFactorSelect }
                  return Done
               }
               default {
                  error "[lindex $args 0] is not a valid option for binFactorTool"
               }
           }
        } elseif { [llength $args] == 2 } {
           switch -- [lindex $args 0] {
               "-value" {
                  powSetBinFactor [lindex $args 1]
                  set defaultBinFactor [lindex $args 1]
               }
               default {
                  error "[lindex $args 0] is not a valid option for binFactorTool"
               }
           }
        } else {
           error "$args are not a valid options for binFactorTool"
        }
   }

   proc regionTool { args } {
        global currgn regionParam
        global regionOutputFileName waitFlag powDWP

        variable remoteServer

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer regionTool $args]
        }

        if { [llength $args] == 1 } {
           switch -- [lindex $args 0] {
               "-wait" {
                  if ![info exists waitFlag] {
                     return "NOT_YET"
                  }
                  if { $waitFlag == "save" } {
                     return 0
                  } elseif { $waitFlag == "unsave" } {
                     return 1
                  } else {
                     return "NOT_YET"
                  }
               }
               "-open" {
                  set regionParam(rgns) [gRegionList $currgn .pow.pow]
                  powRegion
                  set waitFlag "NOT_YET"
                  return Done
               }
               "-close" {
                  catch { destroy ${powDWP}region }
                  return Done
               }
               "-clearAll" {
                  catch { powClearRegions clearAll } err
                  return Done
               }
               default {
                  error "[lindex $args 0] is not a valid option for regionTool"
               }
           }
        } else {
           error "$args are not a valid options for regionTool"
        }
   }

   proc xranges { args } {
        variable remoteServer
        global currgn xRangeParam powDWP

        if { [llength $args] == 1 && [file exists [lindex $args 0]] } {
           set f [open [lindex $args 0] r]
           set str [read $f [file size [lindex $args 0]]]
           ::close $f 
        } else {
           set str ""
           for {set i 0} {$i < [llength $args]} {incr i} {
               set str [format "%s%s" $str [lindex $args $i]]
           }
        }

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer xranges $str ]
        }

        if { ![info exists ${powDWP}xRange] } {
           powXRange 
        }

        if { ![info exists xRangeParam(rgns)] } {
           set xRangeParam(rgns) [gRegionList $currgn .pow.pow]

        }

        $xRangeParam(rgns) setStaticFlag "y"

        catch { xrangeReadDataStr $str } err
        return Done
   }

   proc xrangeName { args } {
        global xrangeOutputFileName
        variable remoteServer

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer xrangeName $args]
        }

        set xrangeOutputFileName [lindex $args 0]
        powUpdateXRangeTitle $xrangeOutputFileName
        return Done
   }

   proc xrangeTool { args } {
        global currgn xRangeParam
        global xrangeOutputFileName waitFlag powDWP

        variable remoteServer

        if { $remoteServer != "" } {
           return [::powXPA::server $remoteServer xrangeTool $args]
        }

        if { [llength $args] == 1 } {
           switch -- [lindex $args 0] {
               "-wait" {
                  if { $waitFlag == "save" } {
                     return 0
                  } elseif { $waitFlag == "unsave" } {
                     return 1
                  } else {
                     return "NOT_YET"
                  }
               }
               "-open" {
                  set xRangeParam(rgns) [gRegionList $currgn .pow.pow]
                  powXRange
                  set waitFlag "NOT_YET" 
                  return Done
               }
               "-close" {
                  catch { destroy ${powDWP}xRange }
                  return Done
               }
               default {
                  error "[lindex $args 0] is not a valid option for xrangeTool"
               }
           }
        } else {
           error "$args are not a valid options for xrangeTool"
        }
   }

   proc draw { args } {
      init
      eval .pow.pow create $args
   }

   ########################
   #    Graph Commands    #
   ########################

   proc add { objType objName } {
      global currgn
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer add $objType $objName]
      }

      switch -- $objType {
         curve {
            powAddCurves $currgn $objName
         }
         image {
            powAddImages $currgn $objName
         }
         default {
            error "Unrecognized object type: $objType"
         }
      }
   }

   proc axes { {xscale ""} {yscale ""} } {
      global currgn powPlotParam
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer axes $xscale $yscale]
      }

      if { $xscale=="" && $yscale=="" } {
         return [list $powPlotParam(xTickScal,$currgn) \
               $powPlotParam(yTickScal,$currgn)]
      } elseif { $yscale=="" } {
         if { [regexp (\W)-(\W) $xscale dmy xsc ysc] } {
            powLogGraph $currgn $xsc $ysc
         } else {
            powLogGraph $currgn $xscale $xscale
         } 
      } else {
         powLogGraph $currgn $xscale $yscale
      }
   }

   proc bounds { args } {
      global powPlotParam currgn currimg
      variable currcrv
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer bounds $args]
      }

      setCurrCurve
      set mode "wcs"
      set argc [llength $args]
      if { $argc==0 || ($argc==1 && [lindex $args 0]!="reset") } {

         foreach {x0 y0 x1 y1} [list \
               $powPlotParam(xBot,$currgn) \
               $powPlotParam(yBot,$currgn) \
               $powPlotParam(xTop,$currgn) \
               $powPlotParam(yTop,$currgn) ] {}
         if { $argc==1 } {set mode $args}
         switch -glob $mode {
            pix* {
               if { [info exists currimg] || $currcrv!="" } {
                  if { [info exists currimg] } {
                     set obj $currimg
                     set isImg 1
                  } else {
                     set obj $currcrv
                     set isImg 0
                  }
                  foreach {x0 y0} [powGraphToPixel $obj \
                        $powPlotParam(xBot,$currgn) \
                        $powPlotParam(yBot,$currgn)] {}
                  foreach {x1 y1} [powGraphToPixel $obj \
                        $powPlotParam(xTop,$currgn) \
                        $powPlotParam(yTop,$currgn)] {}
                  if { $isImg } {
                     set x0 [expr $x0+1]
                     set y0 [expr $y0+1]
                     set x1 [expr $x1+1]
                     set y1 [expr $y1+1]
                  }
               }
            }
            wc* {
               # No need to do anything
            }
            default {
               error "Unrecognized conversion mode: $mode"
            }
         }
         return [list $x0 $y0 $x1 $y1]

      } elseif { [lindex $args 0]=="reset" } {

         set powPlotParam(xBot,$currgn) NULL
         set powPlotParam(yBot,$currgn) NULL
         set powPlotParam(xTop,$currgn) NULL
         set powPlotParam(yTop,$currgn) NULL
         powEraseGraph $currgn 1
         powMapGraph $currgn

      } elseif { [lindex $args 0]=="zoom" } {

         if { $argc==2 || $argc==3 } {
            set xmag [lindex $args 1]
            if { $argc==3 } {
               set ymag [lindex $args 2]
            } else {
               set ymag $xmag
            }
            if { $xmag<=0 || $ymag<=0 } {
               error "Zoom factor out of range"
            }
            foreach {x0 y0 x1 y1} [bounds pixels] {}
            set halfwdth [expr 0.5*($x1-$x0)]
            set halfhght [expr 0.5*($y1-$y0)]
            set x0 [expr $x0 + $halfwdth - $halfwdth/$xmag]
            set x1 [expr $x1 - $halfwdth + $halfwdth/$xmag]
            set y0 [expr $y0 + $halfhght - $halfhght/$ymag]
            set y1 [expr $y1 - $halfhght + $halfhght/$ymag]
            bounds $x0 $y0 $x1 $y1 pixels
         } else {
            error "Usage: bounds zoom xMag ?yMag?"
         }

      } elseif { $argc==4 || $argc==5 } {

         foreach {x0 y0 x1 y1} [lrange $args 0 3] {}
         if { $argc==5 } {set mode [lindex $args 4]}
         switch -glob $mode {
            pix* {
               if { [info exists currimg] || $currcrv!="" } {
                  if { [info exists currimg] } {
                     set obj $currimg
                     set x0 [expr $x0-1]
                     set y0 [expr $y0-1]
                     set x1 [expr $x1-1]
                     set y1 [expr $y1-1]
                  } else {
                     set obj $currcrv
                  }
                  foreach {x0 y0} [powPixelToGraph $obj $x0 $y0] {}
                  foreach {x1 y1} [powPixelToGraph $obj $x1 $y1] {}
               }
            }
            wc* {
               # No need to do anything
            }
            default {
               error "Unrecognized conversion mode: $mode"
            }
         }
         set powPlotParam(xBot,$currgn) $x0
         set powPlotParam(yBot,$currgn) $y0
         set powPlotParam(xTop,$currgn) $x1
         set powPlotParam(yTop,$currgn) $y1
         powEraseGraph $currgn 1
         powMapGraph $currgn

      } else {
         error "Syntax: bounds ?xLft yBtm xRgt yTop? ?mode?"
      }
   }

   proc cursor { } {
      global currgn
      variable mouseClicked

      foreach {x1 y1 x2 y2} [.pow.pow coords ${currgn}box] {}
      set boxid [.pow.pow create polygon $x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1 \
	    -fill {}]
      
      .pow.pow bind $boxid <Button> \
            "powCmds::cursor_callback $currgn %x %y %b"
      set oldBind [bind .pow <KeyPress>]
      bind .pow <KeyPress> \
            "powCmds::cursor_callback $currgn %x %y -%N"

      vwait ::powCmds::mouseClicked
      .pow.pow delete $boxid
      bind .pow <KeyPress> $oldBind
      return $::powCmds::mouseClicked
   }

   proc cursor_callback { gn x y b } {
      variable mouseClicked

      if { $b=="" || $b<-255 } return
      if { $b < 0 } {
         # Keypress... need to translate x/y coordinate to .pow.pow
         set x [expr $x - [winfo x .pow.pow]]
         set y [expr $y - [winfo y .pow.pow]]
      }
      set gcoords [powCanvasToGraph $gn \
            [.pow.pow canvasx $x] [.pow.pow canvasy $y]]
      set rx [lindex $gcoords 0]
      set ry [lindex $gcoords 1]
      set ::powCmds::mouseClicked [list $rx $ry $b]
   }

   proc graph { args } {
#puts "graph: args: $args"
      global powPlotParam currgn
      variable currcrv
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer graph $args]
      }
      
      init
      set argc [llength $args]
      if { $argc==0 } {
         error "Usage: graph ?-name gName? ?param value? ?param value? ..."
      }

      if { [lindex $args 0] == "-name" } {
         if { $argc==1 } {
            return $currgn
         } else {
            set gn [lindex $args 1]
            set args [lrange $args 2 end]
            incr argc -2
            if { $gn == "default" } {
               set gn "powDef"
            }
         }
      } else {
         set gn $currgn
      }

      if { $argc==1 || [expr $argc % 2]==0 } {
         return [eval powGraphOptions $gn $args]
      } else {
         error "Usage: graph ?-name gName? ?param value? ?param value? ..."
      }
   }

   proc position { args } {
      global powPlotParam currgn
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer position $args]
      }

      set argc [llength $args]
      if { $argc==0 } {

         return [list $powPlotParam(xo,$currgn) $powPlotParam(yo,$currgn)]

      } elseif { [lindex $args 0]=="offset" } {

         if { $argc==3 } {
            set dx [lindex $args 1]
            set dy [lindex $args 2]
            powMoveGraph $currgn $dx $dy
         } else {
            error "Usage: position offset ?dx dy?"
         }
            
      } elseif { $argc==2 } {

         set x [lindex $args 0]
         set y [lindex $args 1]
         powMoveGraphTo $currgn $x $y

      } else {
         error "Usage: position ?x y?|?offset dx dy?"
      }
   }

   proc refresh { args } {
      global currgn
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer refresh]
      }

      if { [llength $args]>0 } {
         error "Usage: refresh"
      } else {
         powEraseGraph $currgn 1
         powMapGraph $currgn
      }
   }

   proc remove { args } {
      global currgn
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer remove $args]
      }

      set argc [llength $args]
      if { $argc==4 && [lindex $args 0] == "-name" } {
         set gn [lindex $args 1]
         set args [lrange $args 2 3]
      } elseif { $argc==2 } {
         set gn $currgn
      } else {
         error "Usage: remove ?-name graphName? curve|image objName"
      }

      foreach [list objType objName] $args {}
      switch -- $objType {
         curve {
            powRemoveCurves $gn $objName
         }
         image {
            powRemoveImages $gn $objName
         }
         default {
            error "Unrecognized object type: $objType"
         }
      }
   }

   proc size { args } {
      global powPlotParam currgn
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer size $args]
      }

      set argc [llength $args]
      if { $argc==0 } {

         return [list [tagXdim .pow.pow ${currgn}box] \
               [tagYdim .pow.pow ${currgn}box]]

      } elseif { [lindex $args 0]=="stretch" } {

         if { $argc==1 } {
            return [list $powPlotParam(xmagstep,$currgn) \
                  $powPlotParam(ymagstep,$currgn)]
         } elseif { [lindex $args 1]=="to" } {
            if { $argc==3 } {
               set xMag [lindex $args 2]
               powMagGraph $currgn $xMag $xMag
            } elseif { $argc==4 } {
               set xMag [lindex $args 2]
               set yMag [lindex $args 3]
               powMagGraph $currgn $xMag $yMag
            } else {
               error "Usage: size stretch to xMag ?yMag?"
            }
         } elseif { $argc==2 } {
            set xmag [lindex $args 1]
            powResizeGraph $currgn $xmag $xmag
         } elseif { $argc==3 } {
            set xmag [lindex $args 1]
            set ymag [lindex $args 2]
            powResizeGraph $currgn $xmag $ymag
         } else {
            error "Usage: size stretch ?xMag ?yMag??"
         }

      } elseif { $argc==2 } {

         powStretchGraphToSize $currgn [lindex $args 0] [lindex $args 1]

      } else {
         error "Usage: size ?width height?|?stretch xfactor yfactor?"
      }

   }


   ########################
   #    Image Commands    #
   ########################

   proc colorbar { args } {
      global color_bar_img currimg currgn
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer colorbar $args]
      }

      set argc [llength $args]
      if { $argc==0 } {
         tk_messageBox -icon error -type ok -message "usage: pow colorbar create/delete"
         return
      }

      if { [lindex $args 0] == "create" } {
         powColorbar
      } elseif { [lindex $args 0] == "delete" } {
         set color_bar_img [lindex [powGetColorbarLink $currgn $currimg] 0]
         powDeleteGraph $color_bar_img quite
      } else {
         tk_messageBox -icon error -type ok -message "Invalid option. usage: pow colorbar create/delete"
      }
   }

   proc colormap { args } {
      global powImageParam currgn currimg
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer colormap $args]
      }

      if { ![info exists currimg] } {
         error "No image selected"
      }

      set argc [llength $args]
      if { $argc==0 }  {
         return $powImageParam(colormap${currimg},$currgn)
      }

      if { [lindex $args 0]=="-current" } {
         set doAll 0
         set args [lrange $args 1 end]
         incr argc -1
         if { $argc==0 } {
            return $powImageParam(colormap${currimg},$currgn)
         }
      } else {
         set doAll 1
      }

      if { [lindex $args 0]=="invert" } {

         if { $argc==1 } {
            return $powImageParam(invert${currimg},$currgn)
         } elseif { $argc==2 } {
            # Do it like this to catch colorbar, too
            if { $doAll } {
               powSetCurrImageOpts invert [lindex $args 1]
            } else {
               powSetImageOptions $currgn $currimg invert [lindex $args 1]
            }
         } else {
            error "Usage: colormap invert ?yes|no?"
         }

      } elseif { [lindex $args 0]=="scale" } {

         set availModes [list linear sqrt log histo]
         if { $argc==1 } {

            return [list $powImageParam(scale${currimg},$currgn) \
                  $powImageParam(RBmin${currimg},$currgn) \
                  $powImageParam(RBmax${currimg},$currgn)]
            
         } elseif { $argc==2 } {

            set mode [lindex $args 1]
            if { [lsearch $availModes $mode]!=-1 } {
               if { $doAll } {
                  powSetCurrImageOpts scale $mode
               } else {
                  powSetImageOptions $currgn $currimg scale $mode
               }
            } else {
               error "Unrecognized scale mode: $mode"
            }

         } elseif { $argc==3 } {

            powSetRanges $currgn $currimg [lindex $args 1] [lindex $args 2]

         } elseif { $argc==4 } {
            
            set mode [lindex $args 1]
            if { [lsearch $availModes $mode]!=-1 } {
               powSetRanges $currgn $currimg [lindex $args 2] [lindex $args 3]
               if { $doAll } {
                  powSetCurrImageOpts scale $mode
               } else {
                  powSetImageOptions $currgn $currimg scale $mode
               }
            } else {
               error "Unrecognized scale mode: $mode"
            }

         } else {
            error "Syntax: colormap scale ?mode|min max?"
         }

      } elseif { [lindex $args 0]=="add" } {

         if { $argc < 3 } {
            error "Usage: colormap add cmapName cmapList"
         } elseif { $argc == 3 } {
            powAddCustomLut [lindex $args 1] [lindex $args 2]
         } else {
            powAddCustomLut [lindex $args 1] [lrange $args 2 end]
         }

      } elseif { $argc==1 } {

         set allMaps {}
         foreach map $powImageParam(allMaps,powDef) {
            eval lappend allMaps [lrange $map 1 end]
         }
         if { [lsearch -exact $allMaps $args]==-1 } {
            error "$args is not a valid colormap"
         } else {
            if { $doAll } {
               powSetCurrImageOpts colormap $args
            } else {
               powSetImageOptions $currgn $currimg colormap $args
            }
         }

      } else {
         error "Usage: colormap ?cmap?"
      }
   }


   ########################
   #    Curve Commands    #
   ########################

   proc curve { args } {
      global powPlotParam powCurveParam currgn
      variable currcrv
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer curve $args]
      }
      
      set argc [llength $args]
      setCurrCurve
      set gn $currgn
      if { [lindex $args 0] == "-name" } {
         if { $argc==1 } {
            return $currcrv
         } else {
            # set gn  "powDef"
            set crv  [lindex $args 1]
            set args [lrange $args 2 end]
            incr argc -2
            if { $crv == "default" } {
               set crv ""
            }
         }
      } else {
         set crv $currcrv
      }

      if { $argc==0 } {
         return [powSetCurveOptions $gn $crv]
      } elseif { $argc==1 } {
         return [powSetCurveOptions $gn $crv [lindex $args 0]]
      } elseif { [expr $argc % 2]==0 } {
         eval {powSetCurveOptions $gn $crv} $args
      } else {
         error "Usage: curve ?-name crv? ?param value? ?param value? ..."
      }
   }


   ########################
   #    Misc. Commands    #
   ########################

   proc array { dchan dName {bitpix "LIST"} {byteOrder ""} } {
#puts "dchan: $dchan"
#puts "dName: $dName"
#puts "bitpix: $bitpix"
      global tcl_platform
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer array \
               $dchan $dName $bitpix $byteOrder]
      }

      if { $byteOrder=="FITS" || $byteOrder=="fits" || $byteOrder=="IEEE" } {
         # FITS data is stored in bigEndian format
         set byteOrder "bigEndian"
      }
      if { $byteOrder=="" || $byteOrder==$tcl_platform(byteOrder) } {
         # Use Native byteOrder... just copy it into the array
         set byteOrder 1
      } else {
         # Else, we have to reverse the order of the bytes
         set byteOrder -1
      }

      if { $bitpix=="LIST" || $bitpix=="list" } {
#puts "List"
         set buffer [$dchan readChannel]
         $dchan closeChannel
         set nPts 0
         set nCols [llength $dName]
         foreach aLine [split $buffer \n] {
            set i [string first # $aLine]
            if { $i>0 } {
               set aLine [string range $aLine 0 [expr $i-1]]
            } elseif { $i==0 } {
               set aLine ""
            }
            if { [llength $aLine]>0 } {
               set idx 0
               foreach entry $aLine {
                  if { $nCols==1 } {
                     lappend column(0) $entry
                  } elseif { $idx<$nCols } {
                     lappend column($idx) $entry
                  } else {
                     break
                  }
                  incr idx
               }
               while { $idx<$nCols } {
                  if { $nPts>0 } {
                     lappend column($idx) [lindex $column($idx) [expr $nPts-1]]
                  } else {
                     error "First line contains too few elements"
                  }
                  incr idx
               }
               incr nPts
            }
         }
         set idx 0
         foreach col $dName {
            powCreateDataFromList $col $column($idx)
            incr idx
         }
      } elseif { $bitpix=="PTR" || $bitpix=="ptr" } {
#puts "PTR"
         powCreateDataFromPtr $dchan $dName $byteOrder
      } else {
#puts "else"
         switch -regexp -- $bitpix {
            ^(0|8|BYTE|byte)$               {   set bitpix 0   }
            ^(1|16|SHORTINT|shortint)$      {   set bitpix 1   }
            ^(2|32|INT|int)$                {   set bitpix 2   }
            ^(3|-32|REAL|real|FLOAT|float)$ {   set bitpix 3   }
            ^(4|-64|DOUBLE|double)$         {   set bitpix 4   }
            default {
               error "Unrecognized BITPIX value"
            }
         }
         set buffer [$dchan readChannel]
         set len [string length $buffer]
#puts "len = $len"
         powCreateDataFromBuffer $buffer $len $dName $bitpix $byteOrder
         $dchan closeChannel
         # fconfigure $dchan -translation binary
         # powCreateDataFromChannel $dchan $dName $bitpix $byteOrder
      }
   }

   proc print { args } {
      variable remoteServer
      global powPlacement
      global powPostOrient
      global powStretch
      global tcl_platform
      global powConvertFunction
      global powOutputFileName
      global powOutputFileType
      global powSelectDirectory

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer print $args]
      }

      set powPlacement FOOP
      set powStretch no
      set powPostOrient 0
      set fileName ""

      if { [llength $args] > 0 } {
         for {set i 0} {$i < [llength $args]} {incr i} {
             switch -- [lindex $args $i] {
                 "-landscape" {
                    set powPostOrient 1
                 }
                 "-stretch" {
                    set powStretch yes
                 }
                 "-file" {
                    set fileName [lindex $args [expr $i + 1]]
                    incr i
                 }
                 "-multipage" {
                    set powPlacement OGPP
                 }
                 default {
                    tk_messageBox -icon error -type ok \
                         "Usage: pow print ?-file <destination file>? ?-landscape? ?-stretch? ?-multipage?"
                    return
                 }
             }
         }
      }

      if { $fileName != "" } {
         set format [lindex [split [file tail $fileName] "."] 1]

         set findFlag false
         foreach functionList $powConvertFunction {
             if { $format == [lindex $functionList 2] } {
                set powOutputFileType $functionList
                set findFlag true
                break
             }
         }

         if { $findFlag == "false" } {
            puts "Save format not recongized."
            return
         }

         powShowHandles 0
         set powOutputFileName [file tail $fileName]
         set powOutputDirectory [pwd]
         if { $tcl_platform(platform)=="windows"  } {
         } else {
            if { [string range $fileName 0 0] == "/" } {
               set powSelectDirectory [file dirname $fileName]
            }
         }
         powSave
         powShowHandles 1

      } else {
         powShowHandles 0
         powPrint
         powShowHandles 1
      }

   }

   proc calculate { newData args } {
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer calculate $newData $args]
      }

      if { [llength $args]>0 } {
         powExpr $newData $args
      } else {
         error "Usage: calculate resultName expression"
      }
   }

   proc close {} {
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer close]
      }

      ::powEvents::ExitPOW
   }

   proc contour { args } {
      global currimg
      global powPlotParam
      global powWCS powFitsHeader powFitsHeaderCnt xCount yCount
      global powWCSName powWCSList powWCSLabel
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer create $objType $objName $args]
      }

      # crvName imgName lvls {res 1}
      set argc [llength $args]
      if { $argc < 2 } {
         error "Usage: contour ?-res n? ?-image img? crvName level1 level2 ..."
      }
      set idx 0
      set done 0
      while { !$done } {
         switch -glob -- [lindex $args $idx] {
            "-res*" {
               incr idx
               set res [lindex $args $idx]
            }
            "-im*" {
               incr idx
               set img [lindex $args $idx]
            }
            default {
               set done 1
               break
            }
         }
         incr idx
      }
      if { ![info exists res] } {
         set res 1
      }
      if { ![info exists img] } {
         if { ![info exists currimg] } {
            error "No image selected"
         }
         set img $currimg
      }
      if { [expr $idx+1] >= $argc } {
         error "Usage: contour ?-res n? ?-image img? crvName level1 level2 ..."
      }
      set crvName [lindex $args $idx]
      incr idx
      set lvls [lrange $args $idx end]
      set powWCSName($img) 0
      set powWCSName(${img}scope) 0

      set powWCSList($img) {}
      lappend powWCSList($img) 1
      lappend powWCSList($img) {}

      set powWCSList(${img}scope) $powWCSList($img)

      set powWCS($img) {{0.0 0.0)} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
      set powFitsHeader($img) ""
      set powFitsHeaderCnt($img) 0

      set powWCS(${img}scope) {{0.0 0.0)} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
      set powFitsHeader(${img}scope) ""
      set powFitsHeaderCnt(${img}scope) 0

      set powWCSLabel(xlabel,$img,DEFAULT) ""
      set powWCSLabel(ylabel,$img,DEFAULT) ""
      set powWCSLabel(xunit,$img,DEFAULT) ""
      set powWCSLabel(yunit,$img,DEFAULT) ""

      set powPlotParam(graphType,$img) "image"
      set powPlotParam(graphType,${img}scope) "image"
      set powPlotParam(zoomed,$img) 0
      set powPlotParam(zoomed,${img}scope) 0
      set xCount($img) 0
      set yCount($img) 0
      set xCount(${img}scope) 0
      set yCount(${img}scope) 0

      set powWCS($crvName) {{0.0 0.0)} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
      set powWCSName($crvName) 0
      set powWCSName(${crvName}scope) 0

      set powWCSList($crvName) {}
      lappend powWCSList($crvName) 1
      lappend powWCSList($crvName) {}

      set powWCSList(${crvName}scope) $powWCSList($crvName)

      set powFitsHeader($crvName) ""
      set powFitsHeaderCnt($crvName) 0

      set powPlotParam(graphType,$crvName) "binary"
      set powPlotParam(graphType,${crvName}scope) "binary"
      set powPlotParam(zoomed,$crvName) 0
      set powPlotParam(zoomed,${crvName}scope) 0
      set xCount($crvName) 0
      set yCount($crvName) 0
      set xCount(${crvName}scope) 0
      set yCount(${crvName}scope) 0

      powCreateContour $crvName $img $lvls $res
      powSetCurveOptions powDef $crvName lDisp Yes pDisp No lStyle " "
   }

   proc create { objType objName args } {
      global powPlotParam
      global powWCS powFitsHeader powFitsHeaderCnt xCount yCount
      global powWCSName powWCSList powWCSLabel
      global powDataList
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer create \
               $objType $objName $args]
      }

      set argc [llength $args]
      switch -- $objType {
         data {
            if { $argc > 0 } {
               set list [lindex $args 0]
               set option [lindex $args 1]
               if { $argc == 2 && \
                    ($option == "Start" || $option == "continue" || \
                     $option == "Done") } {
                  # this is from remote
                  if { $option == "Start" } {
                     set powDataList($objName) {}
                  } elseif { $option == "continue" } {
                     set args [lreplace $args 0 0]
                     for { set i 0 } { $i < [llength $list] } { incr i } {
                         lappend powDataList($objName) [lindex $list $i]
                     }
                     set argc [llength $list]
                  }
               } else {
                  set powDataList($objName) $args
                  set option Done
               }
                
#puts "powDataList($objName): $powDataList($objName)"
#puts "option: $option"
               if { $option == "Done" } {
#puts "result: [llength $powDataList($objName)]"
                  powCreateDataFromList $objName $powDataList($objName)
               }
            } else {
               error "Usage: create data data_name data_list option"
            }
         }
         curve {
            if { $argc == 2 } {
               foreach {x y} $args {}
               powCreateVector ${x}_tmpV $x 0 NULL NULL
               powCreateVector ${y}_tmpV $y 0 NULL NULL
               powCreateCurve $objName ${x}_tmpV NULL ${y}_tmpV NULL
            } elseif { $argc==4 } {
               foreach {x xe y ye} $args {}
               powCreateVector ${x}_tmpV  $x  0 NULL NULL
               powCreateVector ${y}_tmpV  $y  0 NULL NULL
               powCreateVector ${xe}_tmpV $xe 0 NULL NULL
               powCreateVector ${ye}_tmpV $ye 0 NULL NULL
               powCreateCurve $objName ${x}_tmpV ${xe}_tmpV ${y}_tmpV ${ye}_tmpV
            } else {
               error "Usage: create curve crvName xData ?xeData? yData ?yeData?"
            }
         }
         image {
            if { $argc == 3 } {
               if ![info exists powWCS($objName)] {
                  set powWCS($objName) {{0.0 0.0)} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
                  set powFitsHeader($objName) ""
                  set powFitsHeaderCnt($objName) 0
               }

               set powWCSName($objName) 0
               set powWCSName(${objName}scope) 0

               set powWCSList($objName) {}
               lappend powWCSList($objName) 1
               lappend powWCSList($objName) {}

               set powWCSLabel(xlabel,$objName,DEFAULT) ""
               set powWCSLabel(ylabel,$objName,DEFAULT) ""
               set powWCSLabel(xunit,$objName,DEFAULT) ""
               set powWCSLabel(yunit,$objName,DEFAULT) ""

               set powWCS(${objName}scope) $powWCS($objName)
               set powFitsHeader(${objName}scope) $powFitsHeader($objName)
               set powFitsHeaderCnt(${objName}scope) $powFitsHeaderCnt($objName)

               set powPlotParam(graphType,$objName) "image"
               set powPlotParam(graphType,${objName}scope) "image"
               set powPlotParam(zoomed,$objName) 0
               set powPlotParam(zoomed,${objName}scope) 0
               set xCount($objName) 0
               set yCount($objName) 0
               set xCount(${objName}scope) 0
               set yCount(${objName}scope) 0

               foreach {data width height} $args {}
               powCreateImage $objName $data 0 0 $width $height \
                     1 1 1 1 NULL NULL NULL
            } else {
               error "Usage: create image imgName dataName width height"
            }
         }
         graph {
            if { $argc == 2 || $argc == 4 } {
               if ![info exists powWCS($objName)] {
                  set powWCS($objName) {{0.0 0.0)} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
                  set powFitsHeader($objName) ""
                  set powFitsHeaderCnt($objName) 0
               }
   
               set powWCSName($objName) 0
               set powWCSName(${objName}scope) 0

               set powWCSList($objName) {}
               lappend powWCSList($objName) 1
               lappend powWCSList($objName) {}

               set powWCSList(${objName}scope) $powWCSList($objName)

               set powWCS(${objName}scope) $powWCS($objName)
               set powFitsHeader(${objName}scope) $powFitsHeader($objName)
               set powFitsHeaderCnt(${objName}scope) $powFitsHeaderCnt($objName)

               set powPlotParam(graphType,$objName) "binary"
               set powPlotParam(graphType,${objName}scope) "binary"
               set powPlotParam(zoomed,$objName) 0
               set powPlotParam(zoomed,${objName}scope) 0
               set xCount($objName) 0
               set yCount($objName) 0
               set xCount(${objName}scope) 0
               set yCount(${objName}scope) 0
            }

            init
            foreach opt [list xlabel ylabel xunit yunit] {
               if { [info exist powPlotParam($opt,$objName)] } {
                  set $opt $powPlotParam($opt,$objName)
               } else {
                  switch $opt {
                      "xlabel" {
                         set powWCSLabel(xlabel,$objName,DEFAULT) [lindex [lindex $powWCS($objName) 3] 0]
                      }
                      "ylabel" {
                         set powWCSLabel(ylabel,$objName,DEFAULT) [lindex [lindex $powWCS($objName) 3] 1]
                      }
                      "xunit" {
                         if { [lindex $powWCS($objName) 4] != "none" } {
                            set $opt ""
                            set new_opt ""
                         } else {
                            set $opt "pixels"
                            set new_opt "pixels"
                         }
                         set powWCSLabel(xunit,$objName,DEFAULT) $new_opt
                      }
                      "yunit" {
                         if { [lindex $powWCS($objName) 4] != "none" } {
                            set $opt ""
                            set new_opt ""
                         } else {
                            set $opt "pixels"
                            set new_opt "pixels"
                         }
                         set powWCSLabel(yunit,$objName,DEFAULT) $new_opt
                      }
                  }
                  set $opt $powWCSLabel($opt,$objName,DEFAULT)
               }
            }

            if { $argc == 2 } {
               foreach {c i} $args {}
               powCreateGraph $objName $c $i $xunit $yunit $xlabel $ylabel
            } elseif { $argc == 4 } {
               foreach {c i w h} $args {}
               powCreateGraph $objName $c $i $xunit $yunit $xlabel $ylabel \
                     $w $h
            } elseif { $argc == 5 } {
               foreach {c i w hs hcnt} $args {}
               powCreateGraph $objName $c $i $xunit $yunit $xlabel $ylabel
            } else {
               error "Usage: create graph grphName crvName imgName ?wdth hght?"
            }
         }
      }
   }

   proc delete { args } {
      global powPlotParam
      variable remoteServer

      if { $remoteServer != "" } {
         return [eval ::powXPA::server $remoteServer delete $args]
      }

      set argc [llength $args]
      if { $argc<2 || $argc>3 } {
         error "Usage: delete ?-propogate? objType objName"
      }

      if { [string match "-pro*" [lindex $args 0]] } {
         set args [lrange $args 1 end]
         incr argc -1
         set propogate 1
      } else {
         set propogate 0
      }
      if { $argc != 2 } {
         error "Usage: delete ?-propogate? objType objName"
      }

      foreach [list objType objName] $args {}

      switch -- $objType {
         data {
            # Check if data is still referenced by an object
            set stillInUse 0
            foreach v [powListVectors] {
               set d [lindex [powFetchVectorInfoHash $v] 1]
               if { $objName == $d } {
                  set stillInUse 1
                  break
               }
            }
            foreach i [powListImages] {
               set d [lindex [powFetchImageInfoHash $i] 1]
               if { $objName == $d } {
                  set stillInUse 1
                  break
               }
            }
            if { ! $stillInUse } {
               powDestroyData $objName
               # Cannot propogate further
            }
         }
         vector {
            # Make sure vector is not depended upon by a curve
            set stillInUse 0
            foreach c [powListCurves] {
               foreach [list axis v] [powFetchCurveInfoHash $c] {
                  if { $v != "NULL" && $objName == $v } {
                     set stillInUse 1
                     break
                  }
               }
            }
            if { ! $stillInUse } {
               set vectorInfo [powFetchVectorInfoHash $objName]
               powDestroyVector $objName
               if { $propogate } {
                  ::powCmds::delete data [lindex $vectorInfo 1]
               }
            }
         }
         curve {
            # Remove curve from all graphs first
            foreach gn [powListGraphs] {
               if { [string match *scope $gn] } continue
               set idx [lsearch $powPlotParam(curves,$gn) $objName]
               if { $idx!=-1 } {
                  remove -name $gn curve $objName
               }
            }
            set curveInfo [powFetchCurveInfoHash $objName]
            powDestroyCurve $objName
            if { $propogate } {
               foreach [list axis v] $curveInfo {
                  if { $v != "NULL" } {
                     ::powCmds::delete -propogate vector $v
                  }
               }
            }
         }
         image {
            # Remove image from all graphs first
            foreach gn [powListGraphs] {
               if { [string match *scope $gn] } continue
               set idx [lsearch $powPlotParam(images,$gn) $objName]
               if { $idx!=-1 } {
                  remove -name $gn image $objName
               }
            }
            set imageInfo [powFetchImageInfoHash $objName]
            powDestroyImage $objName
            if { $propogate } {
               ::powCmds::delete data [lindex $imageInfo 1]
            }
         }
         graph {
            powDestroyGraph $objName
            if { $propogate } {
               foreach c $powPlotParam(curves,$objName) {
                  if { $c != "NULL" } {
                     ::powCmds::delete -propogate curve $c
                  }
               }
               foreach i $powPlotParam(images,$objName) {
                  if { $i != "NULL" } {
                     ::powCmds::delete -propogate image $i
                  }
               }
            }
         }
      }
   }

   proc init { {ncolors 30} {colorMode 0} } {
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer init $ncolors $colorMode]
      }

      if { [winfo exists .pow.pow] } return

      if { ![winfo exists .dummy] } {
         # try to avoid the flash. 
         powSetupColormap .dummy $ncolors $colorMode
         wm withdraw .dummy
      }
      powInit .dummy
   }

   proc remote { args } {
      variable remoteServer

      if { [llength $args]==0 } {
         return $remoteServer
      }
      if { [info commands xpaset] == "" } {
         error "XPA library is not available"
      }
      set remoteServer [lindex $args 0]
   }

   proc scope { {dx ""} {dy ""} } {
      global powScopeWidth powScopeHeight
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer scope $dx $dy]
      }

      if { $dx=="" && $dy=="" } {
         return [list $powScopeWidth $powScopeHeight]
      } elseif { $dy=="" } {
         powResizeScope $dx $dx
      } else {
         powResizeScope $dx $dy
      }
   }

   proc select { obj {name ""} } {
      global currgn currimg powPlotParam
      variable currcrv
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer select $obj $name]
      }

      if { $name=="" } {

         switch -exact $obj {
            graph {
               return $currgn
            }
            image {
               if { [info exists currimg] } {
                  return $currimg
               } else {
                  return ""
               }
            }
            curve {
               setCurrCurve
               return $currcrv
            }
         }

      } else {

         switch -exact $obj {
            graph {
               powSelectGraph $name
            }
            image {
               set imgName [findListMember $name $powPlotParam(images,$currgn)]
               if { $imgName=="" } {
                  error "$name is not a member of graph $currgn"
               }
               powSelectImage $currgn $imgName
            }
            curve {
               set crvName [findListMember $name $powPlotParam(curves,$currgn)]
               if { $crvName=="" } {
                  error "$name is not a member of graph $currgn"
               }
               set currcrv $crvName
            }
         }

      }
   }

   proc version {} {
      variable remoteServer

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer version]
      }

      return [powGetVersion]
   }

   proc wcs { object {wcs} } {
      global powWCS
      variable remoteServer
      if { $remoteServer != "" } {
         if { [info exists wcs] } {
            return [::powXPA::server $remoteServer wcs $object $wcs]
         } else {
            return [::powXPA::server $remoteServer wcs $object]
         }
      }

      if { [info exists wcs] } {
         set powWCS($object) $wcs
      } elseif { [info exists powWCS($object)] } {
         return $powWCS($object)
      } else {
         return ""
      }
   }

   proc wcsHeader { gn strlen {str} flag } {
      variable remoteServer
      global powFitsHeader powWCSName

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer wcsHeader $gn $strlen $str $flag]
      }

      set powWCSName($gn) 0
      set powWCSName(${gn}scope) 0

      if { $flag == "start" } {
         set powFitsHeader($gn) ""
      } elseif { $flag == "continue" } {
         set powFitsHeader($gn) [format "%s%-${strlen}s" $powFitsHeader($gn) $str]
      }
   }

   proc wcsHeaderCnt { gn cnt } {
      variable remoteServer
      global powFitsHeaderCnt powFitsHeader

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer wcsHeaderCnt $gn $cnt]
      }
      set powFitsHeaderCnt($gn) $cnt
   }

   proc wcsLabel { gn label name value } {
      variable remoteServer
      global powWCSLabel
      global powPlotParam

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer wcsLabel $gn $label $name $value]
      }

      set powWCSLabel($label,$gn,$name) $value
      set powPlotParam($label,$gn) $value
   }

   proc wcsSetList { gn list} {
      variable remoteServer
      global powWCSList 

      if { $remoteServer != "" } {
         return [::powXPA::server $remoteServer wcsSetList $gn $list]
      }

      set token [split $list "|"]
      set entry {}
      lappend entry [lindex $token 0]
      set tokenList {}
      for {set i 1} {$i < [llength $token]} {incr i} {
          if { [lindex $token $i] == "DEFAULT" } {
             lappend tokenList " "
          } else {
             lappend tokenList [lindex $token $i]
          }
      }
      lappend entry $tokenList
      set powWCSList($gn) $entry
      set powWCSList(${gn}scope) $entry
   }

   ########################
   # Utility routines, not public command
   ########################

   proc findListMember { fnd lst } {
      if { ![regexp {[^0-9]} $fnd] } {
         set objName [lindex $lst [expr $fnd-1]]
         if { $objName=="" || $objName=="NULL" } {
            set objName ""
         }
      } else {
         set objName $fnd
         if { [lsearch -exact $lst $objName]==-1 } {
            set objName ""
         }
      }
      return $objName
   }

   proc setCurrCurve {} {
      global powPlotParam currgn
      variable currcrv

      if { $powPlotParam(curves,$currgn)=="NULL" } {
         set currcrv ""
      } elseif { $currcrv=="" || \
            [lsearch $powPlotParam(curves,$currgn) $currcrv]==-1 } {
         set currcrv [lindex $powPlotParam(curves,$currgn) 0]
      }
   }

}



####################   XPA  Entry Points   ####################
# note to myself.. We might not need XPA entry points here for 
# send and receive anymore 
namespace eval powXPA {
   variable xpaPt
   variable cmds

   proc init { { user "" } } {
      global env
      variable xpaPt

      if { [catch {package require tclxpa} err ] } {
         if { [catch {load [file join $env(LHEASOFT)/lib libtclxpa[info sharedlibextension]]} err1 ] && \
              [catch {load "" xpa} err2 ] } return
      }

      if { $user == "" } {
         set xpaPt [xpacmdnew "" pow]
      } else {
         set xpaPt [xpacmdnew "" pow_$user]
      }

      # Create a safe interpretter in which one can evaluate commands:
      interp create -safe powSafe
      interp share {} stdout powSafe

      foreach {cmd snd rcv hlp} [list                                    \
            add        0 1 "Add an object to the current graph"          \
            array      0 1 "Imports data into POW"                       \
            axes       1 1 "Sets axes as log or linear"                  \
            bounds     1 1 "Modifies graph bounding box"                 \
            calculate  0 1 "Perform a calculation on existing data"      \
            close      0 1 "Close/Exit POW"                              \
            colormap   1 1 "Manipulate colormap of images"               \
            colorbar   0 1 "Crate colorbar of images"                    \
            contour    0 1 "Create a contour map of an image"            \
            create     0 1 "Create data/curves/images/graphs"            \
            cursor     1 0 "Get a mouse click on the current graph"      \
            curve      1 1 "Manipulate curve options"                    \
            draw       0 1 "Do raw draws onto canvas"                    \
            delete     0 1 "Delete data/curves/images/graphs"            \
            graph      1 1 "Manipulate graph options"                    \
            init       0 1 "Startup pow window"                          \
            print      0 1 "print graph(s)"                              \
            position   1 1 "Moves current graph around the canvas"       \
            refresh    0 1 "Redraws current graph"                       \
            remote     1 1 "Set the XPA access point to use"             \
            remove     0 1 "Remove a curve/image from a graph"           \
            scope      1 1 "Sets scope window size"                      \
            select     1 1 "Select a curve/image/graph for manipulation" \
            size       1 1 "Set graph size"                              \
            tcl        0 1 "Execute tcl code"                            \
            binFactorTool 1 1 "open/close bin factor editor on POW"      \
            regionTool 1 1 "open/close region edit panel on POW"         \
            getXRange  1 1 "get region info, X direction only."          \
            getRegion  1 1 "get region info"                             \
            regions    1 1 "receive region info and update POW"          \
            regionName 1 1 "set output region file name"                 \
            setRegionFormat 1 1 "set region format"                      \
            xrangeTool 1 1 "open/close select X axis Range panel on POW" \
            xranges    1 1 "receive X axis Range info and update POW"    \
            xrangeName 1 1 "set output X axis Range file name"           \
            helpPage   0 1 "Display Help Page"                           \
            version    1 0 "Return POW version number"                   \
            wcs        1 1 "Set/Get WCS information for object"          \
            wcsHeader    1 1 "Set/Get WCS information for object"        \
            wcsHeaderCnt 1 1 "Set/Get WCS information for object"        \
            wcsSetList   1 1 "Set/Get WCS information for object"        \
            wcsLabel     1 1 "Set/Get WCS information for object"        \
            ] {
         register $cmd $snd $rcv $hlp
         powSafe alias $cmd ::powCmds::$cmd
      }

      # Check if we are running in client/server mode
      if { [info exists env(POW_DISPLAY)] && $env(POW_DISPLAY)!="" } {
         ::powCmds::remote $env(POW_DISPLAY)
      }
   }


   proc register { cmd snd rcv hlp } {
      variable xpaPt
      variable cmds
      
      if { $snd } {
         set sndParam [list powXPA::send $cmd ""]
      } else {
         set sndParam [list "" "" ""]
      }
      if { $rcv } {
         set rcvParam [list powXPA::recv $cmd "fillbuf=false"]
      } else {
         set rcvParam [list "" "" ""]
      }
      eval xpacmdadd $xpaPt $cmd {$hlp} $sndParam $rcvParam
      set cmds(canSend,$cmd) $snd
      set cmds(canRcve,$cmd) $rcv
      set cmds(cmdHelp,$cmd) $hlp
   }


   proc send { xpa client_data paramlist } {
      switch -exact $client_data {
         default {
            if { [powSafe alias $client_data]!="" } {
               xpasetbuf $xpa [powSafe eval $client_data $paramlist]
            } else {
               error "$client_data is an invalid command"
            }
         }
      }
   }


   proc recv { xpa client_data paramlist buf len } {
      global g_backupDir g_showpow_flag
 
      switch -exact $client_data {
         array {
            set dchan [xparec $xpa datachan]
            eval powSafe eval array \$dchan $paramlist
            close $dchan
         }
         tcl {
            set dchan [xparec $xpa datachan]
            set scrpt [read $dchan]
            close $dchan
            powSafe eval $scrpt
         }
         helpPage {
             set dchan [xparec $xpa datachan]
             set regdata [read $dchan]
             close $dchan

             set param $paramlist
             set g_showpow_flag "noshow"
             eval powSafe eval init

             if { [llength $paramlist] <= 0 } {
                set fileName $g_backupDir/helpPage_[clock seconds].html
                set f [::open $fileName w+]
                puts $f $regdata
                close $f
                set param $fileName
             }
             eval powSafe eval $client_data $param
         }
         regions -
         xranges {
            if { [llength $paramlist] <= 0 } {
               set dchan [xparec $xpa datachan]
               set regdata [read $dchan]
               close $dchan
            } else {
               set f [open [lindex $paramlist 0] r]
               set regdata [read $f]
               ::close $f 
            }
            set regtoken [split $regdata \n]
            set regstr ""

            for {set i 0} {$i < [llength $regtoken]} {incr i} {
                if { [string length [lindex $regtoken $i]] <= 0 } continue
                set resultStr ""
                for {set j 0} {$j < [string length [lindex $regtoken $i]]} {incr j} {
                    set currentChar [string range  [lindex $regtoken $i] $j $j]
                    if { $currentChar == ";" } {
                       set currentChar "|"
                    }
                    set resultStr [format "%s%s" $resultStr $currentChar]
                }
                if { $regstr == "" } {
                   set regstr $resultStr
                } else {
                   set regstr [format "%s|%s" $regstr $resultStr]
                }
            }
            eval powSafe eval $client_data $regstr
         }
         create {
            if {[lindex $paramlist 0] == "data" } {
               if { [llength $paramlist] == 2 } {
                  catch { set dchan [xparec $xpa datachan] } err
                  catch { set regdata [read $dchan] } err
                  catch { close $dchan } err
                  eval powSafe eval $client_data $paramlist $regdata
               } else {
                  eval powSafe eval $client_data $paramlist
               }
            } else {
               eval powSafe eval $client_data $paramlist
            }
         }
         default {
            if { [powSafe alias $client_data]!="" } {
               eval powSafe eval $client_data $paramlist
            } else {
               error "$client_data is an invalid command"
            }
         }
      }
   }


   proc server { remoteXPA args } {
      variable cmds

      set cmd [lindex $args 0]
      switch -exact $cmd {
         array {
            if { [llength $args]!=5 } {
               error "In client/server mode, must supply all\
                     array parameters"
            }
            set dchan  [lindex $args 1]
            set dname  [lindex $args 2]
            set bitpix [lindex $args 3]
            set byteOr [lindex $args 4]
            if { $bitpix=="LIST" || $bitpix=="list" } {
               set buf [read $dchan]
            } elseif { $bitpix=="PTR" || $bitpix=="ptr" } {
               foreach [list ptr bitpix naxes] $dchan {}
               set buf [powCreateStrFromPtr $ptr $bitpix $naxes]
            } else {
               fconfigure $dchan -translation binary
               set buf [read $dchan]
            }
            set newArgs [list array $dname $bitpix $byteOr]
            set len [string bytelength $buf]
            set got [xpaset "" $remoteXPA $newArgs "" $buf $len names errs 1]
         }
         default {
            if { $cmds(canSend,$cmd) } {
               # Can return a value to caller
               set got [xpaget "" $remoteXPA $args "" bufs lens names errs 1]
            } else {
               # Can only receive data, so no return value
               set got [xpaset "" $remoteXPA $args "" "" 0 names errs 1]
            }
         }
      }
      set errs [lindex $errs 0]
      if { $errs != "" } {
         error $errs
      } elseif { $got==0 } {
         error "Unable to connect to client."
      }
      if { $cmds(canSend,$cmd) } {
         return [lindex $bufs 0]
      }
   }

}
